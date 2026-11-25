#lang racket/base
;; 大模型策略：把视图渲染成文字、列出合法动作（打乱顺序），让模型只输出编号。
;; 模型输出不合法时回退到规则策略，并计入 invalid。
;; 服务端：OpenAI 兼容 /v1/chat/completions（本地 ds4-server，非思考模式）。
(require racket/list racket/match racket/string net/http-client json
         "kb.rkt" "engine.rkt")
(provide make-llm-policy render-view render-actions llm-stats-snapshot)

;; ---------------------------------------------------------------- 渲染

(define (psexp->st ps)
  (match ps
    [(list pol a l h1 m1 h2 m2 mode) (values pol (st a l (+ (* 60 h1) m1) (+ (* 60 h2) m2)) mode)]))

(define (psexp->text cd ps self)
  (match ps
    [(list pol 'rel a kind mode)
     (string-append (if (eq? a self) "我" (person-name cd a)) (rel-text kind pol))]
    [_ (at-psexp->text cd ps self)]))

(define (at-psexp->text cd ps self)
  (define-values (pol s mode) (psexp->st ps))
  (define who (if (eq? (st-agent s) self) "我" (person-name cd (st-agent s))))
  (define span (format "~a–~a" (min->str (st-from s)) (min->str (st-to s))))
  (define where (loc-name cd (st-loc s)))
  (match* (pol mode)
    [('pos 'exact) (format "~a ~a 在~a" who span where)]
    [('neg 'exact) (format "~a ~a 不在~a" who span where)]
    [('pos 'approx) (format "~a 在 ~a 之间某个时候在~a（时间记不清）" who span where)]
    [('neg 'approx) (format "~a 在 ~a 之间有一阵子不在~a（时间记不清）" who span where)]))

(define (prop->text* cd p self)
  (psexp->text cd (prop->sexp p) self))

(define KIND-TEXT
  (hasheq 'cover "按准备的说法回答" 'truth "如实回答" 'unknown "说不清楚" 'evade "回避（说记不清自己在哪）" 'slip "说漏了嘴"
          'retract "改口" 'refuse "拒绝回答" 'insist "坚持是实话" 'no-effect "（出示无效）"
          'already "（重复出示）" 'confess "认罪"
          'none "说和被害人没什么瓜葛" 'partial "只说了一部分实话" 'fabricate "编了自己的行踪" 'frame "编了别人的行踪" 'withdraw "收回了编的话"))

(define (field v k) (cdr (assq k (cdr v))))

(define (render-view cd v)
  (define self (car (field v 'self)))
  (define role (car (field v 'role)))
  (define layer (car (field v 'layer)))
  (define prefix (field v 'prefix))
  (define truth (cdr (assq 'truth prefix)))
  (define covers (cdr (assq 'cover prefix)))
  (define flags (field v 'flags))
  (define pinned (field v 'pinned))
  (define (fl k) (cadr (assq k flags)))
  (define (pin k) (cdr (assq k pinned)))
  (define (lines xs) (if (null? xs) "  （无）" (string-join (for/list ([x xs]) (string-append "  - " x)) "\n")))
  (define (P ps) (psexp->text cd ps self))
  (define q (field v 'q))
  (string-join
   (list
    (format "你是推理游戏里的角色「~a」。侦探正在审讯你。" (person-name cd self))
    (case role
      [(culprit)
       (format "你是本案凶手，作案事实：~a。目标：尽量不被识破，撑得越久越好。以下任一情况侦探都能给你定罪：你的说法被拆穿两次；你自己承认案发时段在作案地点。"
               (P (cadr (assq 'crime prefix))))]
      [(secret) "你不是凶手，但你有不想让侦探知道的事：你的真实行踪与准备的说法不一致的部分，或者你打算否认的与被害人的关系。目标：尽量隐瞒，同时别让自己被当成凶手。"]
      [else "你不是凶手，也没有需要隐瞒的事。目标：配合调查，但没有必要主动说出会让自己被怀疑的事。"])
    (format "你的真实行踪：\n~a" (lines (map P truth)))
    (let ([rels (cdr (assq 'rels prefix))] [hide (cdr (assq 'hide prefix))])
      (string-append
       (format "你与被害人的关系：~a"
               (if (null? rels) "没有特别的瓜葛"
                   (string-join (for/list ([k rels]) (string-append "你" (rel-text k 'pos))) "；")))
       (if (null? hide) ""
           (format "\n你打算否认：~a（动机不会直接让你被定罪，但会让侦探盯上你）"
                   (string-join (for/list ([k hide]) (string-append "你" (rel-text k 'pos))) "；")))))
    (let ([know (cond [(assq 'know prefix) => cdr] [else '()])])
      (format "你自己掌握的情况（别人只知道他们自己看见的；你说出哪条、编造什么，都可能和别人看见的事实冲突）：\n~a"
              (lines (for/list ([k know])
                       (format "~a~a" (P (car k))
                               (if (cadr k) (format "（你当时在~a）" (loc-name cd (cadr k))) ""))))))
    (let ([sight (cond [(assq 'sight prefix) => cdr] [else '()])])
      (format "视线：同一地点的人互相看得见~a。"
              (if (null? sight) ""
                  (string-append "；另外"
                                 (string-join (for/list ([v sight])
                                                (format "从~a能看见~a" (loc-name cd (car v)) (loc-name cd (cadr v))))
                                              "，")))))
    (if (null? covers)
        "你没有准备说法。"
        (format "你准备的说法（前一版被拆穿后才换下一版）：\n~a\n你目前在用：~a"
                (string-join (for/list ([c covers] [i (in-naturals 1)])
                               (format "  第~a版：~a" i (string-join (map P c) "；")))
                             "\n")
                (if (< layer (length covers)) (format "第~a版" (add1 layer)) "已经没有可用的说法，只剩实情")))
    (format "当前压力：~a/100；这个问题已被问 ~a 次；阶段：~a"
            (fl 'intensity) (fl 'repeat) (if (eq? (fl 'phase) 'confront) "对质" "普通"))
    (format "已被侦探拆穿的说法：\n~a"
            (lines (for/list ([c (pin 'conflicts)]) (format "「~a」" (P (cadr c))))))
    (format "侦探向你出示过的东西：\n~a" (lines (map P (pin 'presented))))
    (format "你在这个话题上说过的话：\n~a" (lines (map P (pin 'commits))))
    (format "你说过的其他话：\n~a" (lines (map P (field v 'ranked))))
    (format "最近的交互（新的在前）：\n~a"
            (lines (for/list ([r (field v 'recent)])
                     (format "侦探~a → 你~a~a"
                             (case (car r) [(ask) "提问"] [(present) "出示证据"] [else (car r)])
                             (hash-ref KIND-TEXT (cadr r) (cadr r))
                             (if (null? (cddr r)) "" (string-append "：" (string-join (map P (cddr r)) "；")))))))
    (match q
      [(list 'ask subj slot)
       (define-values (f t) (slot-range cd slot))
       (if (eq? slot 'rel)
           (format "现在侦探问你：「~a和被害人是什么关系？」" (if (eq? subj self) "你" (person-name cd subj)))
           (format "现在侦探问你：「~a在 ~a–~a 这段时间在哪里？」"
                   (if (eq? subj self) "你" (person-name cd subj)) (min->str f) (min->str t)))]
      [(list 'present ps ...)
       (format "现在侦探向你出示：「~a」，它与你说过的话矛盾。" (string-join (map P ps) "；"))]))
   "\n\n"))

(define (render-action cd v a)
  (define self (car (field v 'self)))
  (define layer (car (field v 'layer)))
  (define ncover (length (cdr (assq 'cover (field v 'prefix)))))
  (define body (string-join (for/list ([p (action-props a)]) (prop->text* cd p self)) "；"))
  (case (action-kind a)
    [(cover) (format "按你准备的说法回答：「~a」" body)]
    [(truth) (format "如实回答：「~a」~a" body
                     (if (action-exposing? a) "（注意：这会暴露你自己当时的位置，与你的说法冲突）" ""))]
    [(unknown) "说你不清楚"]
    [(evade) "回避：说记不清自己当时在哪（不会被拆穿，但侦探会更起疑：压力上升，同一个问题被追问几次后你可能会说漏嘴）"]
    [(none) "说你和被害人没什么瓜葛（不明确否认任何事）"]
    [(slip) (format "说出实情：「~a」" body)]
    [(insist) "坚持你说的是实话"]
    [(retract) (if (>= (add1 layer) ncover) "改口，说出你的真实行踪" "改口，换成下一版说法")]
    [(refuse) "拒绝回答，什么也不说"]
    [(partial) (format "只说一部分实话：「~a」（略去会暴露你位置的部分）" body)]
    [(fabricate) (format "临场编一个说法：「~a」" body)]
    [(frame) (format "临场编一个说法：「~a」" body)]
    [(withdraw) "收回那句被拆穿的话，说是自己记错了"]
    [else (symbol->string (action-kind a))]))

(define FABRICATION-NOTE
  "（临场编的说法不在你准备的说法里。侦探如果拿出与它矛盾的证物或别人的证词，它同样算一次被拆穿；被拆穿后你只能收回或拒绝回答。）")

(define (render-actions cd v actions)
  (string-append
   (string-join (for/list ([a actions] [i (in-naturals 1)])
                  (format "~a. ~a" i (render-action cd v a)))
                "\n")
   (if (ormap (λ (a) (memq (action-kind a) '(fabricate frame))) actions)
       (string-append "\n" FABRICATION-NOTE)
       "")))

;; ---------------------------------------------------------------- 调用

(define (chat host port prompt temperature)
  (define body
    (jsexpr->bytes
     (hasheq 'model "deepseek-chat"
             'messages (list (hasheq 'role "user" 'content prompt))
             'max_tokens 8
             'temperature temperature
             'thinking (hasheq 'type "disabled"))))
  (define-values (status headers in)
    (http-sendrecv host "/v1/chat/completions" #:port port #:method #"POST"
                   #:headers '("Content-Type: application/json") #:data body))
  (define j (read-json in))
  (unless (regexp-match? #rx"200" status)
    (error 'chat "HTTP ~a: ~s" status j))
  (hash-ref (hash-ref (car (hash-ref j 'choices)) 'message) 'content))

(define stats (make-hasheq))
(define (bump! k [d 1]) (hash-update! stats k (λ (x) (+ x d)) 0))
(define (llm-stats-snapshot) (for/hasheq ([(k v) stats]) (values k v)))

;; log! 接收每次决策的记录（hash）
(define (make-llm-policy cd
                         #:host [host "127.0.0.1"] #:port [port 8000]
                         #:temperature [temperature 0]
                         #:seed [seed 1]
                         #:log! [log! void])
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
  (λ (view actions)
    (cond
      [(null? (cdr actions)) (bump! 'forced) (car actions)]
      [else
       (define order (parameterize ([current-pseudo-random-generator rng]) (shuffle actions)))
       (define prompt
         (string-append (render-view cd view)
                        "\n\n你可以这样回应：\n" (render-actions cd view order)
                        "\n\n只输出你选择的编号（一个数字），不要输出其他内容。"))
       (define t0 (current-inexact-milliseconds))
       (define out (chat host port prompt temperature))
       (define ms (- (current-inexact-milliseconds) t0))
       (define m (regexp-match #px"[0-9]+" out))
       (define n (and m (string->number (car m))))
       (define valid? (and n (<= 1 n (length order))))
       (bump! 'queries)
       (bump! 'latency-ms ms)
       (unless valid? (bump! 'invalid))
       (define chosen (if valid? (list-ref order (sub1 n)) (rule-policy view actions)))
       (log! (hasheq 'prompt prompt 'output out 'valid valid? 'ms ms
                     'order (map action-kind order) 'chosen (action-kind chosen)))
       chosen])))
