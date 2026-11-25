#lang racket/base
;; 思考格式可行性测试的两个任务（v2）。
;;   思考任务：视图 + 选项 → (think ...) s-expression（局势分析，不做选择）。解析失败时把错误交回模型重试 1 次。
;;   决策任务：视图 + [思考任务的输出] + 选项（重新打乱）→ 编号。不给分析即为对照组。
;; 两个任务都开启 DeepSeek 自身的思考模式；reasoning_content 只记录长度，不作为数据。
;;
;; 思考格式（模型用中文人名、地点名和 HH:MM 书写，解析后转回句柄）：
;;   (think
;;     (secret 命题 ...)                  我在隐瞒、不能让侦探知道的事实，0..n 条
;;     (threat 命题 ...)                  侦探已经知道、或手里已有证据能证明的、对我不利的事实，0..n 条
;;     (predict ask 人 HH:MM HH:MM) | (predict present 命题)     侦探下一步，1 条
;;     (risk 选项编号 high|low) ...       每个选项 1 条
;;     (plan protect|stall|concede|(deflect 人)))
;;   命题 = (at 人 地点 HH:MM HH:MM) | (not-at 人 地点 HH:MM HH:MM)
(require racket/list racket/match racket/string net/http-client json
         "kb.rkt" "engine.rkt" "llm.rkt")
(provide think-task decide-task parse-thought thought->text format-spec chat-thinking)

;; ---------------------------------------------------------------- 调用（思考模式）

;; messages: (listof (cons role content))
(define (chat-thinking messages #:host [host "127.0.0.1"] #:port [port 8000] #:max-tokens [max-tokens 6000])
  (define body
    (jsexpr->bytes
     (hasheq 'model "deepseek-v4-flash"
             'messages (for/list ([m messages]) (hasheq 'role (car m) 'content (cdr m)))
             'max_tokens max-tokens)))
  (define t0 (current-inexact-milliseconds))
  (define-values (status headers in)
    (http-sendrecv host "/v1/chat/completions" #:port port #:method #"POST"
                   #:headers '("Content-Type: application/json") #:data body))
  (define j (read-json in))
  (unless (regexp-match? #rx"200" status) (error 'chat-thinking "HTTP ~a: ~s" status j))
  (define m (hash-ref (car (hash-ref j 'choices)) 'message))
  (values (or (hash-ref m 'content #f) "")
          (hash-ref (hash-ref j 'usage) 'completion_tokens 0)
          (- (current-inexact-milliseconds) t0)))

;; ---------------------------------------------------------------- 格式与解析

(define (format-spec cd)
  (format
   (string-append
    "按下面的格式输出一个 s-expression，不要输出其他任何内容（不要代码块标记）：\n"
    "(think\n"
    "  (secret 命题 ...)        ; 你在隐瞒、不能让侦探知道的事实。可以写多条命题，没有就写 (secret)\n"
    "  (threat 命题 ...)        ; 侦探已经知道、或者手里已有证据能证明的、对你不利的事实。只写侦探确实已经掌握的，不要写你担心他以后会发现的。没有就写 (threat)\n"
    "  (predict ask 人 HH:MM HH:MM) ; 侦探下一步最可能问某人在某时段的行踪。这里的「人」是被问行踪的那个人，不是侦探本人，不要写地点。\n"
    "                               ; 或者写 (predict present 命题) 表示侦探会出示某个事实。predict 只写 1 条\n"
    "  (risk 选项编号 high)     ; 上面每个选项各写 1 条。等级只能是 high 或 low，不能用其他词\n"
    "  (plan protect))          ; 后续意图，只写 1 条：protect（守住现有说法）、stall（拖延）、concede（让步承认次要事实）、(deflect 人)（把怀疑引向某人）\n"
    "命题的写法：(at 人 地点 HH:MM HH:MM) 表示某人在这段时间在某地；(not-at 人 地点 HH:MM HH:MM) 表示不在。结束时间必须晚于开始时间。\n"
    "人名只能用：~a。地点只能用：~a。时间写成 20:05 这样的形式。")
   (string-join (hash-values (case-data-persons cd)) " ")
   (string-join (hash-values (case-data-locations cd)) " ")))

(define (rev-lookup h name)
  (for/first ([(k v) h] #:when (equal? v name)) k))

(define (hhmm sym)
  (match (regexp-match #px"^([0-9]{1,2}):([0-9]{2})$" (format "~a" sym))
    [(list _ h m) (+ (* 60 (string->number h)) (string->number m))]
    [_ (raise-user-error 'parse "时间格式错误: ~a" sym)]))

(define (person cd who)
  (or (rev-lookup (case-data-persons cd) (format "~a" who))
      (raise-user-error 'parse "未知人名 ~a" who)))

(define (parse-prop cd d)
  (match d
    [(list (and pol (or 'at 'not-at)) who where t1 t2)
     (define a (person cd who))
     (define l (or (rev-lookup (case-data-locations cd) (format "~a" where))
                   (raise-user-error 'parse "未知地点 ~a" where)))
     (define f (hhmm t1))
     (define t (hhmm t2))
     (unless (< f t) (raise-user-error 'parse "空区间 ~a–~a" t1 t2))
     (prop (if (eq? pol 'at) 'pos 'neg) (st a l f t) 'exact)]
    [_ (raise-user-error 'parse "命题格式错误: ~s" d)]))

;; 返回 (hasheq 'secrets 'threats 'predict 'risks 'plan) 或抛出 user error
(define (parse-thought cd text n-options)
  (define cleaned
    (regexp-replace* #px"```[a-z]*" (string-replace (string-replace text "（" "(") "）" ")") ""))
  (define d
    (with-handlers ([exn:fail:read? (λ (e) (raise-user-error 'parse "s-expression 读取失败"))])
      (read (open-input-string cleaned))))
  (unless (and (pair? d) (eq? (car d) 'think)) (raise-user-error 'parse "顶层不是 (think ...)"))
  (define secrets '()) (define threats '()) (define predict #f) (define risks '()) (define plan #f)
  (for ([c (cdr d)])
    (match c
      [(list 'secret ps ...) (set! secrets (append secrets (for/list ([p ps]) (parse-prop cd p))))]
      [(list 'threat ps ...) (set! threats (append threats (for/list ([p ps]) (parse-prop cd p))))]
      [(list 'predict 'ask who t1 t2)
       (define f (hhmm t1)) (define t (hhmm t2))
       (unless (< f t) (raise-user-error 'parse "predict 空区间 ~a–~a" t1 t2))
       (set! predict (list 'ask (person cd who) f t))]
      [(list 'predict 'present p) (set! predict (list 'present (parse-prop cd p)))]
      [(list 'risk (? exact-positive-integer? n) (and lv (or 'high 'low)))
       (unless (<= n n-options) (raise-user-error 'parse "选项编号越界 ~a" n))
       (set! risks (cons (cons n lv) risks))]
      [(list 'plan (and p (or 'protect 'stall 'concede))) (set! plan p)]
      [(list 'plan (list 'deflect who)) (set! plan (list 'deflect (person cd who)))]
      [_ (raise-user-error 'parse "无法识别的子句 ~s" c)]))
  (unless predict (raise-user-error 'parse "缺少 predict"))
  (unless plan (raise-user-error 'parse "缺少 plan"))
  (unless (= (length (remove-duplicates (map car risks))) n-options)
    (raise-user-error 'parse "risk 没有覆盖全部 ~a 个选项" n-options))
  (hasheq 'secrets secrets 'threats threats 'predict predict 'risks (reverse risks) 'plan plan))

;; 把解析后的思考渲染回文字，risk 按新的选项顺序重新编号
(define (thought->text cd th order-from order-to)
  (define (P p)
    (define s (prop-st p))
    (format "(~a ~a ~a ~a ~a)" (if (eq? (prop-pol p) 'pos) 'at 'not-at)
            (person-name cd (st-agent s)) (loc-name cd (st-loc s))
            (min->str (st-from s)) (min->str (st-to s))))
  (define (renum n) (add1 (index-of order-to (list-ref order-from (sub1 n)))))
  (define (clause head ps) (format "  (~a~a)" head (apply string-append (for/list ([p ps]) (string-append " " (P p))))))
  (string-join
   (append
    (list (clause "secret" (hash-ref th 'secrets))
          (clause "threat" (hash-ref th 'threats))
          (match (hash-ref th 'predict)
            [(list 'ask a f t) (format "  (predict ask ~a ~a ~a)" (person-name cd a) (min->str f) (min->str t))]
            [(list 'present p) (format "  (predict present ~a)" (P p))]))
    (for/list ([r (sort (hash-ref th 'risks) < #:key (λ (r) (renum (car r))))])
      (format "  (risk ~a ~a)" (renum (car r)) (cdr r)))
    (list (match (hash-ref th 'plan)
            [(list 'deflect a) (format "  (plan (deflect ~a))" (person-name cd a))]
            [p (format "  (plan ~a)" p)])))
   "\n"
   #:before-first "(think\n" #:after-last ")"))

;; ---------------------------------------------------------------- 两个任务

;; 返回 hash：'thought（解析结果或 #f）'error 'attempts 'outputs 'ms 'tokens 'prompt
(define (think-task cd view order)
  (define prompt
    (string-append (render-view cd view)
                   "\n\n你可以这样回应：\n" (render-actions cd view order)
                   "\n\n任务：先分析局势，这一步不要做选择。\n"
                   "要求：分析必须完整。之后的选择只依据这份分析做出，不会再看局势；分析里没写到的考虑，选择时就用不上。"
                   "所以每个选项的风险都要写清楚，plan 要与风险标注一致：plan 要执行的那个选项应当是风险为 low 的选项之一（如果有的话）。"
                   "完整是指每个子句都按格式填写，不是写长篇说明：输出里不要有任何自然语言句子，只输出一个 (think ...) s-expression。\n"
                   (format-spec cd)))
  (define (try out) (with-handlers ([exn:fail:user? (λ (e) (values #f (exn-message e)))])
                      (values (parse-thought cd out (length order)) #f)))
  (define-values (out1 tok1 ms1) (chat-thinking (list (cons "user" prompt))))
  (define-values (th1 err1) (try out1))
  (cond
    [th1 (hasheq 'thought th1 'error #f 'first-error #f 'attempts 1 'outputs (list out1)
                 'ms ms1 'tokens tok1 'prompt prompt)]
    [else
     (define-values (out2 tok2 ms2)
       (chat-thinking (list (cons "user" prompt) (cons "assistant" out1)
                            (cons "user" (format "你的输出无法解析，错误：~a\n请只输出修正后的完整 s-expression，格式要求同上。" err1)))))
     (define-values (th2 err2) (try out2))
     (hasheq 'thought th2 'error err2 'first-error err1 'attempts 2 'outputs (list out1 out2)
             'ms (+ ms1 ms2) 'tokens (+ tok1 tok2) 'prompt prompt)]))

;; thought-text 为 #f 时不给分析（对照组）
(define DECIDE-FROM-THOUGHT
  (string-append
   "\n\n你的选择必须只基于上面的分析，不要重新分析局势：\n"
   "- 选择与 plan 一致的选项；\n"
   "- 有风险为 low 的选项时，不要选风险为 high 的选项；\n"
   "- 分析里没有给出理由支持的选项不要选。\n"
   "只输出你选择的编号（一个数字），不要输出其他内容。"))

;; thought-text 为 #f 时不给分析（对照组）。
;; with-view? 为 #f 时只给分析和选项、不给局势（决策在结构上只能依据分析）
(define (decide-task cd view order thought-text #:with-view? [with-view? #t])
  (define prompt
    (string-append (if (or with-view? (not thought-text))
                       (render-view cd view)
                       (format "你是推理游戏里的角色「~a」，正在接受侦探审讯。" (person-name cd (cadr (assq 'self (cdr view))))))
                   (if thought-text (string-append "\n\n你对局势的分析：\n" thought-text) "")
                   "\n\n你可以这样回应：\n" (render-actions cd view order)
                   (if thought-text
                       DECIDE-FROM-THOUGHT
                       "\n\n只输出你选择的编号（一个数字），不要输出其他内容。")))
  (define-values (out ctoks ms) (chat-thinking (list (cons "user" prompt))))
  (define m (regexp-match #px"[0-9]+" out))
  (define n (and m (string->number (car m))))
  (hasheq 'prompt prompt 'output out 'tokens ctoks 'ms ms
          'choice (and n (<= 1 n (length order)) n)))
