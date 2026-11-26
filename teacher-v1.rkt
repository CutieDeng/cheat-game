#lang racket/base
;; v1 教师生成（spec-v1.md §8.4 第 2 步）：让本地 DeepSeek V4 Flash（ds4-server）给凶手编故事。
;; 输入只含凶手知道的东西（spec §5.1）：地图与通行时间、视线规则、自己的真实行踪、自己的观察、公开物证、公开的案情。
;; 输出格式：(story (地点id 离开HHMM) ...)，进入时刻由 parse-story 按步行分钟推出（2026-11-26 起；此前为 (地点id 进入 离开)）
(require racket/list racket/match racket/string racket/port json net/http-client
         "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "text-v1.rkt" "engine-v1.rkt")
(provide culprit-prompt parse-story chat-once)

(define (hm m) (format "~a~a" (quotient m 60) (let ([r (remainder m 60)]) (if (< r 10) (format "0~a" r) r))))
(define (hhmm m) (string->number (format "~a~a" (quotient m 60) (let ([r (remainder m 60)]) (if (< r 10) (format "0~a" r) r)))))

(define (culprit-prompt c)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (define t0 (case1-t0 c))
  (define t1 (case1-t1 c))
  (match-define (list cn clo chi cd) (case1-crime c))
  (define truth (findf (λ (p) (eq? (path-who p) who)) (case1-paths c)))
  ;; 自己的观察；略去不到 2 分钟、且什么也没看见的看守（经过走廊时的一瞬间）
  (define own-obs
    (for/list ([o (case1-observations c)]
               #:when (eq? (obs-observer o) who)
               #:unless (and (memq (obs-kind o) '(saw-only doors-only))
                             (null? (obs-subject o))
                             (< (- (obs-to o) (obs-from o)) 2)))
      (obs->claim 'x o)))
  (define (kind k) (case k [(room) "房间"] [(corridor) "走廊"] [(outdoor) "户外"]))
  (string-join
   (list
    (format "你是~a，今晚的凶手。侦探在调查。你要为自己编一条今晚的行踪（假的），供侦探盘问时使用。" (person-name c who))
    ""
    "## 地图"
    (string-append "地点（id=名称）："
                   (string-join (for/list ([n (node-ids m)]) (format "~a=~a（~a）" n (place-name c n) (kind (kind-of m n)))) "，"))
    (string-append "通道与步行分钟数（每个地点能直接走到哪里）：\n"
                   (string-join (for/list ([a (node-ids m)])
                                  (format "- ~a → ~a" a (string-join (for/list ([e (neighbors m a)]) (format "~a（~a 分钟）" (car e) (cdr e))) "，")))
                                "\n"))
    (if (null? (gmap-windows m)) "没有窗户。"
        (string-append "窗户（单向看得见对面屋里）：" (string-join (for/list ([w (gmap-windows m)]) (format "从 ~a 看得见 ~a" (car w) (cdr w))) "，")))
    "视线：同一地点的人互相看得见；站在走廊上的人能看见相邻房间的门口（看得见谁进出，看不见屋里）；在路上走的那几分钟谁也看不见。"
    ""
    "## 公开的案情（侦探知道）"
    (format "被害人在 ~a（~a）遇害，案发时段 ~a–~a，作案至少要在那里连续待 ~a 分钟。" cn (place-name c cn) (hm clo) (hm chi) cd)
    (format "时间窗 ~a–~a。" (hm t0) (hm t1))
    (string-append "公开物证：" (string-join (for/list ([e (case1-evidence c)]) (format "~a（~a）" (second e) (claim->text c (third e)))) "；"))
    ""
    "## 你知道的事（只有你知道）"
    ;; 作案段挖掉，只给时段：照抄真实路径在 sft-v1b 中是自认候选的过半来源（8 人局 32/60）
    (string-append "你今晚真实的行踪（进入–离开；作案那段已挖掉，要你编）："
                   (string-join (for/list ([s (path-segs truth)])
                                  (if (and (eq? (seg-node s) cn) (< (max (seg-in s) clo) (min (seg-out s) chi)))
                                      (format "【~a–~a 作案，这段要编一个别的去处】" (hm (seg-in s)) (hm (seg-out s)))
                                      (format "~a ~a–~a" (seg-node s) (hm (seg-in s)) (hm (seg-out s)))))
                                " → "))
    "你看见的情况（你看见的人也看见了你；但你不知道谁在别处看见了你）："
    (string-join (for/list ([cl own-obs]) (string-append "- " (claim->text c cl))) "\n")
    ""
    "## 侦探怎么查"
    "侦探会问每个人：某时段你在哪、见过某人吗、在某地见过谁，并把证词互相对照、和物证对照。别人说的话有几分钟误差，你说自己的行踪按 ±3 分钟算。"
    (format "如果你说的行踪与 ~a 个不同的人（或物证）看到的事实矛盾，你就被定罪。" EXPOSE-TO-WIN)
    "所以：凡是你看见过的人，他们也看见了你，那段时间你最好照实说；离开、经过走廊、进出房间都可能被人看见。"
    (format "最重要：你必须编造。整个 ~a–~a 时段里，故事都不能让你待在 ~a——哪怕只重叠几分钟，也等于认罪。照抄真实行踪就是认罪。" (hm clo) (hm chi) cn)
    "除了挖掉的那段，其余时间照抄上面的真实行踪，一分钟都不要改：那些时段你真的被人看见过，改了就会和别人的证词矛盾。只编挖掉的那段，以及进出它要经过的走廊。"
    "先想好这段时间你说自己在哪里（选一个不太会被人看见、或者看见你的人恰好也能为你作证的地方），再写整条故事。"
    ""
    "## 输出格式（只输出这一个 s-expression，不要任何其他文字）"
    "(story (地点id 离开时刻) ...)：每段只写你离开那个地点的时刻（HHMM 整数，例如 1930）。"
    (format "进入时刻不用写，程序自动算：第一段从 ~a 进入；之后每段的进入时刻 = 上一段的离开时刻 + 两地之间的步行分钟数。" (hhmm t0))
    "相邻两段的地点必须在地图上直接相连（看上面的列表）：房间和房间之间通常不直接相连，要经过走廊，走廊那一段也要写出来。只是经过某处，就把离开时刻写成和进入时刻一样。"
    (format "最后一段的离开时刻写 ~a。" (hhmm t1))
    ;; 例子取地图上第一条边、与真实路径无关的时刻，避免引导照抄
    (let* ([a (first (node-ids m))] [e (first (neighbors m a))] [b (car e)] [w (cdr e)] [x (+ t0 17)])
      (format "格式示例（与你无关）：(story (~a ~a) (~a ~a) ...) 表示 ~a–~a 在 ~a，走 ~a 分钟，~a 进入 ~a、立即离开（只是经过）。"
              a (hhmm x) b (hhmm (+ x w)) (hm t0) (hm x) a w (hm (+ x w)) b)))
   "\n"))

;; 解析模型输出：取第一个 (story ...) 形式。返回 path 或错误字符串
;; 两种段格式：
;;   (地点 离开)        —— 新格式：进入时刻 = 上一段离开 + 步行分钟（首段 = t0）；离开早于进入按"只是经过"处理（离开 = 进入）
;;   (地点 进入 离开)   —— 旧格式（sft-v1 数据），原样读取
;; 新格式下相邻两段不相连时无法推出进入时刻：按 0 分钟推，交给 V2 报不相邻
(define (parse-story c text)
  (define who (case1-culprit c))
  (define m (case1-map c))
  (define i (let ([r (regexp-match-positions #rx"\\(story" text)]) (and r (caar r))))
  (cond
    [(not i) "没有找到 (story ...)"]
    [else
     (with-handlers ([exn:fail? (λ (e) (format "读取失败：~a" (exn-message e)))])
       (define d (read (open-input-string (substring text i))))
       (match d
         [(list 'story segs ...)
          (unless (pair? segs) (raise-user-error 'parse "故事为空"))
          (define out
            (for/fold ([acc '()] #:result (reverse acc)) ([s segs])
              (match s
                [(list (? symbol? n) (? exact-nonnegative-integer? a) (? exact-nonnegative-integer? b))
                 (cons (seg n (hhmm->min a) (hhmm->min b)) acc)]
                [(list (? symbol? n) (? exact-nonnegative-integer? b))
                 (define in (if (null? acc) (case1-t0 c)
                                (+ (seg-out (car acc)) (or (and (memq (seg-node (car acc)) (node-ids m))
                                                                (memq n (node-ids m))
                                                                (edge-w m (seg-node (car acc)) n))
                                                           0))))
                 (if (and (pair? acc) (eq? n (seg-node (car acc))))
                     ;; 连续两段同一地点：合并成一段（停留到较晚的离开时刻）
                     (cons (seg n (seg-in (car acc)) (max (seg-out (car acc)) (hhmm->min b))) (cdr acc))
                     (cons (seg n in (max in (hhmm->min b))) acc))]
                [_ (raise-user-error 'parse "段格式不对：~s" s)])))
          (path who out)]
         [_ "顶层不是 (story ...)"]))]))

;; 一次非思考模式（或思考模式）调用。返回 (values 文本 用时毫秒 completion-tokens)
(define (chat-once prompt #:temperature [temp 1.0] #:max-tokens [max-tokens 800] #:thinking? [thinking? #f]
                   #:host [host "127.0.0.1"] #:port [port 8000])
  (define body
    (jsexpr->bytes
     (let ([h (hasheq 'model "deepseek-v4-flash"
                      'messages (list (hasheq 'role "user" 'content prompt))
                      'max_tokens max-tokens 'temperature temp)])
       (if thinking? h (hash-set h 'thinking (hasheq 'type "disabled"))))))
  (define t0 (current-inexact-milliseconds))
  (define-values (status headers in)
    (http-sendrecv host "/v1/chat/completions" #:port port #:method #"POST"
                   #:headers '("Content-Type: application/json") #:data body))
  (define j (read-json in))
  (unless (regexp-match? #rx"200" status) (error 'chat-once "HTTP ~a: ~s" status j))
  (define msg (hash-ref (car (hash-ref j 'choices)) 'message))
  (values (or (hash-ref msg 'content #f) "") (- (current-inexact-milliseconds) t0)
          (hash-ref (hash-ref j 'usage (hasheq)) 'completion_tokens 0)))
