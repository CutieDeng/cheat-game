#lang racket/base
;; Web 服务：静态文件（static/）+ JSON API。最小 HTTP/1.1，只用 racket/tcp、openssl、json。
;; 规则一律经 engine.rkt / kb.rkt / text.rkt 的接口访问，本文件不含规则。
;;
;; 用法：racket server.rkt [--host 127.0.0.1] [--port 8080] [--https --cert 证书.pem --key 私钥.pem]
;; HTTP/2、HTTP/3（QUIC）由前置反向代理处理，见 design.md「web 服务」。
;;
;; API（请求体与响应体均为 JSON；POST 要求 Content-Type: application/json）
;;   GET  /api/cases                  可选案件 [{file title npcs ap_budget}]
;;   POST /api/games                  {case, seed?, ext?}           -> 局面
;;   GET  /api/games/<id>             局面
;;   POST /api/games/<id>/ask         {npc, subject, slot}          slot 为时间段下标或 "rel"
;;   POST /api/games/<id>/present     {npc, item}                   item 为证物 id（字符串）或声明 id（整数）
;;   POST /api/games/<id>/accuse      {npc}
;; 动作的响应：{response: {kind text clauses cost claim_ids}, state: 局面}
(require racket/list racket/string racket/match racket/cmdline racket/tcp racket/random
         racket/runtime-path net/uri-codec json openssl
         "kb.rkt" "engine.rkt" "text.rkt")
(provide handle-api state->jsexpr)

(define-runtime-path BASE-DIR ".")
(define STATIC-DIR (simplify-path (build-path BASE-DIR "static")))
(define CASES-DIR (build-path BASE-DIR "cases"))
(define DEFAULT-CASE "case-manor.rktd")

;; ---------------------------------------------------------------- 上限

(define MAX-LINE 8192)       ; 请求行与单个头部行的字节数
(define MAX-HEADERS 100)
(define MAX-BODY 65536)      ; 请求体字节数
(define CONN-TIMEOUT 30)     ; 单个连接（含 TLS 握手）的秒数
(define MAX-CONNS 256)       ; 同时处理的连接数
(define MAX-GAMES 1000)      ; 内存中的对局数，超过时淘汰最久未访问的

;; ---------------------------------------------------------------- 错误

(struct http-error (code msg))
(define (fail code fmt . args) (raise (http-error code (apply format fmt args))))

(define (reason code)
  (case code
    [(200) "OK"] [(201) "Created"] [(400) "Bad Request"] [(404) "Not Found"]
    [(405) "Method Not Allowed"] [(409) "Conflict"] [(413) "Payload Too Large"]
    [(415) "Unsupported Media Type"] [(431) "Request Header Fields Too Large"]
    [(500) "Internal Server Error"] [(501) "Not Implemented"] [else "Unknown"]))

;; ---------------------------------------------------------------- 案件

(define (case-files)
  (append
   (if (file-exists? (build-path BASE-DIR DEFAULT-CASE)) (list DEFAULT-CASE) '())
   (if (directory-exists? CASES-DIR)
       (sort (for/list ([f (directory-list CASES-DIR)]
                        #:when (regexp-match? #rx"[.]rktd$" (path->string f)))
               (string-append "cases/" (path->string f)))
             string<?)
       '())))

;; 只接受 case-files 列出的名字，防止读任意文件
(define (load-case/checked name)
  (unless (member name (case-files)) (fail 404 "没有这个案件：~a" name))
  (define cd (load-case (build-path BASE-DIR name)))
  (define errs (validate-case cd #:lies-to-win LIES-TO-WIN))
  (unless (null? errs) (fail 500 "案件校验失败：~a" (string-join errs "；")))
  cd)

(define (cases-json)
  (for/list ([f (case-files)])
    (with-handlers ([exn:fail? (λ (e) (hasheq 'file f 'error (exn-message e)))])
      (define cd (load-case (build-path BASE-DIR f)))
      (hasheq 'file f 'title (case-data-title cd)
              'npcs (length (case-data-npcs cd)) 'ap_budget (case-data-turn-limit cd)))))

;; ---------------------------------------------------------------- 会话

;; log：新的在前，每项是一个 jsexpr
(struct sess (id file seed g lock [log #:mutable] [touched #:mutable]))
(define sessions (make-hash))
(define sessions-lock (make-semaphore 1))

(define (new-id) (apply string-append (for/list ([b (crypto-random-bytes 16)])
                                        (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (add-session! s)
  (call-with-semaphore
   sessions-lock
   (λ ()
     (when (>= (hash-count sessions) MAX-GAMES)
       (define oldest (argmin sess-touched (hash-values sessions)))
       (hash-remove! sessions (sess-id oldest)))
     (hash-set! sessions (sess-id s) s))))

(define (get-session id)
  (define s (call-with-semaphore sessions-lock (λ () (hash-ref sessions id #f))))
  (unless s (fail 404 "对局不存在或已过期：~a" id))
  (set-sess-touched! s (current-inexact-milliseconds))
  s)

;; ---------------------------------------------------------------- 局面 → JSON
;; 字段名一律 snake_case。接口按 design.md「说话分层」预留：
;;   NPC 回答为 {kind text clauses cost}；局面用行动点 {ap_used ap_budget costs}。

;; 每个动作的行动点消耗。引擎实现行动点（「说话分层」§2）之前一律按 1，与 game-turn 每个动作加 1 一致；
;; 届时 ap_used / ap_budget / costs 改为从引擎读取。
(define ACTION-COSTS (hasheq 'ask 1 'present 1 'accuse 1))
(define (ap-used g) (game-turn g))
(define (ap-budget g) (turn-limit g))
(define (costs-json) (for/hasheq ([(k v) (in-hash ACTION-COSTS)]) (values k v)))

(define (slot-label cd i)
  (define-values (f t) (slot-range cd i))
  (format "~a–~a" (min->str f) (min->str t)))

;; 回答类别对玩家公开到什么程度：cover / truth / partial / fabricate / frame 是否说谎属于隐藏信息，
;; 统一报 answer；其余类别本来就会体现在文本里（与 text.rkt 的措辞一致）
(define (public-response-kind k)
  (if (memq k '(cover truth partial fabricate frame)) "answer" (symbol->string k)))
(define (public-claim-kind k)
  (if (memq k '(retract slip confess)) (symbol->string k) "said"))

;; 命题 → 声明层子句（「说话分层」§3 的写法），时间写作 HHMM。
;; approx 命题写成 (approx <子句>)（design.md「说话分层」§3「修饰」行）。
(define (hhmm m) (+ (* 100 (quotient m 60)) (remainder m 60)))
(define (prop->clause p)
  (define s (prop-st p))
  (define c
    (if (rl? s)
        (list (if (eq? (prop-pol p) 'pos) 'rel 'no-rel) (rl-agent s) (rl-kind s))
        (list (if (eq? (prop-pol p) 'pos) 'at 'not-at) (st-agent s) (st-loc s) (hhmm (st-from s)) (hhmm (st-to s)))))
  (if (eq? (prop-mode p) 'approx) (list 'approx c) c))
(define (clause->string c) (format "~s" c))

;; 一次回答的子句：retracted 是这次动作中从该 NPC 承诺里撤下的声明
(define (response-clauses npc r retracted)
  (define props (for/list ([c (response-claims r)]) (prop->clause (claim-prop c))))
  (define drops (for/list ([c retracted]) (list 'retract (claim-id c))))
  (case (response-kind r)
    [(unknown evade) '((dunno))]
    [(refuse) '((refuse))]
    ;; "没什么瓜葛"：对每种关系都否认（引擎不把它记为承诺）
    [(none) (for/list ([k REL-KINDS]) (list 'no-rel npc k))]
    [(retract withdraw) (append drops props)]
    [else props]))

(define (claim->jsexpr cd c)
  (hasheq 'id (claim-id c) 'step (claim-turn c)
          'speaker (symbol->string (claim-speaker c)) 'speaker_name (person-name cd (claim-speaker c))
          'kind (public-claim-kind (claim-kind c))
          'clause (clause->string (prop->clause (claim-prop c)))
          'text (prop->text cd (claim-prop c)) 'note (claim->note cd c)))

(define (state->jsexpr s)
  (define g (sess-g s))
  (define cd (game-cd g))
  (define (nm n) (person-name cd n))
  (hasheq
   'id (sess-id s) 'case (sess-file s) 'seed (sess-seed s) 'ext (game-ext? g)
   'title (case-data-title cd) 'briefing (case-data-briefing cd)
   'status (symbol->string (game-status g))
   'ap_used (ap-used g) 'ap_budget (ap-budget g) 'costs (costs-json)
   'phase (symbol->string (phase g)) 'phase_ap (phase-turn g)
   'npcs (for/list ([n (case-data-npcs cd)])
           (hasheq 'id (symbol->string n) 'name (nm n)
                   'conflicts (for/sum ([c (game-exposed g)]) (if (eq? (conflict-npc c) n) 1 0))))
   'slots (for/list ([i (vector-length (case-data-slots cd))])
            (hasheq 'index i 'label (slot-label cd i)))
   'evidence (for/list ([e (case-data-evidence cd)])
               (hasheq 'id (symbol->string (first e)) 'name (second e)
                       'clause (clause->string (prop->clause (third e)))
                       'text (prop->text cd (third e)) 'note (evidence->note cd e)))
   'claims (for/list ([c (reverse (game-claims g))]) (claim->jsexpr cd c))
   'exposed (for/list ([c (game-exposed g)])
              (hasheq 'npc (symbol->string (conflict-npc c)) 'npc_name (nm (conflict-npc c))
                      'note (conflict->note cd c)))
   'evaded (for/list ([h (reverse (game-history g))] #:when (eq? (fourth h) 'evade))
             (define slot (third (third h)))
             (hasheq 'step (first h) 'npc (symbol->string (second h)) 'npc_name (nm (second h))
                     'slot (if (number? slot) slot "rel")
                     'note (format "第 ~a 次行动：~a 说不清自己 ~a 在哪" (first h) (nm (second h))
                                   (if (number? slot) (slot-label cd slot) "（关系）"))))
   'log (reverse (sess-log s))))

;; ---------------------------------------------------------------- 输入校验

(define (field body k) (hash-ref body k (λ () (fail 400 "缺少字段 ~a" k))))

(define (npc-arg cd body k)
  (define v (field body k))
  (define sym (and (string? v) (string->symbol v)))
  (unless (and sym (memq sym (case-data-npcs cd))) (fail 400 "~a 不是本案的 NPC：~s" k v))
  sym)

(define (slot-arg cd body)
  (define v (field body 'slot))
  (cond [(equal? v "rel") 'rel]
        [(and (exact-nonnegative-integer? v) (< v (vector-length (case-data-slots cd)))) v]
        [else (fail 400 "slot 应为时间段下标或 \"rel\"：~s" v)]))

(define (item-arg g body)
  (define v (field body 'item))
  (define cd (game-cd g))
  (cond [(and (string? v) (assq (string->symbol v) (case-data-evidence cd))) (string->symbol v)]
        [(and (exact-integer? v) (for/or ([c (game-claims g)]) (= (claim-id c) v))) v]
        [else (fail 400 "item 应为证物 id 或已获得的声明 id：~s" v)]))

;; ---------------------------------------------------------------- 动作

(define (question-text g kind npc args)
  (define cd (game-cd g))
  (define (nm n) (person-name cd n))
  (match* (kind args)
    [('ask (list subj slot))
     (define who (if (eq? subj npc) "你" (nm subj)))
     (if (eq? slot 'rel)
         (format "~a和被害人是什么关系？" who)
         (format "~a ~a 在哪里？" who (slot-label cd slot)))]
    [('present (list item))
     (format "（出示）~a"
             (if (symbol? item)
                 (evidence->note cd (assq item (case-data-evidence cd)))
                 (claim->note cd (findf (λ (c) (= (claim-id c) item)) (game-claims g)))))]
    [('accuse '()) (format "我认为凶手就是你，~a。" (nm npc))]))

(define (do-action! s kind body)
  (define g (sess-g s))
  (define cd (game-cd g))
  (unless (eq? (game-status g) 'playing) (fail 409 "这局已经结束（~a）" (game-status g)))
  (define npc (npc-arg cd body 'npc))
  (define args
    (case kind
      [(ask) (list (npc-arg cd body 'subject) (slot-arg cd body))]
      [(present) (list (item-arg g body))]
      [(accuse) '()]))
  (define q (question-text g kind npc args))
  (define step (add1 (game-turn g)))
  (define cost (hash-ref ACTION-COSTS kind))
  (define commits-before (hash-ref (game-commits g) npc '()))
  (define r
    (case kind
      [(ask) (ask! g npc (first args) (second args))]
      [(present) (present! g npc (first args))]
      [(accuse) (accuse! g npc)]))
  (define commits-after (hash-ref (game-commits g) npc '()))
  (define retracted (filter (λ (c) (not (memq c commits-after))) commits-before))
  (define rj (hasheq 'kind (public-response-kind (response-kind r))
                     'text (response->text cd npc r)
                     'clauses (map clause->string (response-clauses npc r retracted))
                     'cost cost
                     'claim_ids (for/list ([c (response-claims r)]) (claim-id c))))
  (set-sess-log! s (cons (hasheq 'step step 'action (symbol->string kind) 'cost cost
                                 'ap_used (ap-used g)
                                 'npc (symbol->string npc) 'npc_name (person-name cd npc)
                                 'question q 'response rj)
                         (sess-log s)))
  (hasheq 'response rj 'state (state->jsexpr s)))

(define (create-game body)
  (define file (hash-ref body 'case DEFAULT-CASE))
  (unless (string? file) (fail 400 "case 应为字符串"))
  (define seed (hash-ref body 'seed (λ () (random 1 2147483647 (make-pseudo-random-generator)))))
  (unless (and (exact-nonnegative-integer? seed) (< seed 2147483648)) (fail 400 "seed 应为 0 到 2^31-1 的整数"))
  (define ext? (hash-ref body 'ext #f))
  (unless (boolean? ext?) (fail 400 "ext 应为 true/false"))
  (define cd (load-case/checked file))
  (define s (sess (new-id) file seed (new-game cd #:seed seed #:ext? ext?) (make-semaphore 1)
                  '() (current-inexact-milliseconds)))
  (add-session! s)
  (state->jsexpr s))

;; method：字符串；segs：路径去掉 "/api/" 后按 / 切分；body：jsexpr 或 #f
;; 返回 (values 状态码 jsexpr)
(define (handle-api method segs body)
  (define (need-body) (unless (hash? body) (fail 400 "请求体应为 JSON 对象")) body)
  (define (locked s thunk) (call-with-semaphore (sess-lock s) thunk))
  (match* (method segs)
    [("GET" '("cases")) (values 200 (cases-json))]
    [("POST" '("games")) (values 201 (create-game (need-body)))]
    [("GET" (list "games" id)) (let ([s (get-session id)]) (values 200 (locked s (λ () (state->jsexpr s)))))]
    [("POST" (list "games" id (and act (or "ask" "present" "accuse"))))
     (define s (get-session id))
     (define b (need-body))
     (values 200 (locked s (λ () (do-action! s (string->symbol act) b))))]
    [(_ (or '("cases") '("games") (list "games" _) (list "games" _ (or "ask" "present" "accuse"))))
     (fail 405 "不支持的方法 ~a" method)]
    [(_ _) (fail 404 "没有这个接口")]))

;; ---------------------------------------------------------------- HTTP/1.1

(struct req (method path query headers body))

;; 读一行（LF 结尾，去掉 CR）；行首就是 EOF 时返回 eof
(define (read-line/limit in)
  (define buf (open-output-bytes))
  (let loop ([n 0])
    (define b (read-byte in))
    (cond [(eof-object? b) (if (zero? n) eof (fail 400 "请求不完整"))]
          [(= b 10)
           (define bs (get-output-bytes buf))
           (define len (bytes-length bs))
           (bytes->string/utf-8 (if (and (> len 0) (= 13 (bytes-ref bs (sub1 len)))) (subbytes bs 0 (sub1 len)) bs)
                                #\?)]
          [(>= n MAX-LINE) (fail 431 "行过长")]
          [else (write-byte b buf) (loop (add1 n))])))

(define (read-request in)
  (define line (read-line/limit in))
  (cond
    [(eof-object? line) eof]
    [else
     (define parts (string-split line " " #:trim? #f))
     (unless (= 3 (length parts)) (fail 400 "请求行格式错误"))
     (match-define (list method target version) parts)
     (unless (regexp-match? #rx"^HTTP/1[.][01]$" version) (fail 400 "不支持的协议版本 ~a" version))
     (unless (regexp-match? #rx"^/" target) (fail 400 "请求目标应以 / 开头"))
     (define-values (raw-path query)
       (match (regexp-match #rx"^([^?]*)(?:[?](.*))?$" target)
         [(list _ p q) (values p (or q ""))]))
     (define headers
       (let loop ([acc (hash)] [n 0])
         (define h (read-line/limit in))
         (cond [(eof-object? h) (fail 400 "请求头不完整")]
               [(string=? h "") acc]
               [(>= n MAX-HEADERS) (fail 431 "请求头过多")]
               [else
                (match (regexp-match #rx"^([!#$%&'*+.^_`|~0-9A-Za-z-]+):[ \t]*(.*?)[ \t]*$" h)
                  [(list _ k v)
                   (define key (string-downcase k))
                   (when (and (equal? key "content-length") (hash-has-key? acc key)
                              (not (equal? (hash-ref acc key) v)))
                     (fail 400 "Content-Length 重复且不一致"))
                   (loop (hash-set acc key v) (add1 n))]
                  [#f (fail 400 "请求头格式错误")])])))
     (when (hash-has-key? headers "transfer-encoding") (fail 501 "不支持 Transfer-Encoding"))
     (define len
       (match (hash-ref headers "content-length" #f)
         [#f 0]
         [(regexp #rx"^[0-9]+$" (list v)) (string->number v)]
         [_ (fail 400 "Content-Length 格式错误")]))
     (when (> len MAX-BODY) (fail 413 "请求体超过 ~a 字节" MAX-BODY))
     (define body (if (zero? len) #"" (read-bytes len in)))
     (unless (and (bytes? body) (= (bytes-length body) len)) (fail 400 "请求体不完整"))
     (req method (uri-decode raw-path) query headers body)]))

(define (write-response out code ctype body #:head? [head? #f] #:extra [extra '()])
  (fprintf out "HTTP/1.1 ~a ~a\r\n" code (reason code))
  (for ([h (append (list (cons "Content-Type" ctype)
                         (cons "Content-Length" (bytes-length body))
                         (cons "Connection" "close")
                         (cons "X-Content-Type-Options" "nosniff"))
                   extra)])
    (fprintf out "~a: ~a\r\n" (car h) (cdr h)))
  (write-bytes #"\r\n" out)
  (unless head? (write-bytes body out))
  (flush-output out))

(define (json-response out code v)
  (write-response out code "application/json; charset=utf-8" (jsexpr->bytes v)
                  #:extra '(("Cache-Control" . "no-store"))))

;; ---------------------------------------------------------------- 静态文件

(define CONTENT-TYPES
  (hash "html" "text/html; charset=utf-8" "css" "text/css; charset=utf-8"
        "js" "text/javascript; charset=utf-8" "json" "application/json; charset=utf-8"
        "svg" "image/svg+xml" "png" "image/png" "ico" "image/x-icon" "txt" "text/plain; charset=utf-8"))

;; 路径段不得为空以外的 . / ..、不得以 . 开头、不得含 \ 或 NUL；解析结果必须仍在 STATIC-DIR 下
(define (static-file path)
  (define segs (string-split path "/"))
  (define segs* (if (null? segs) '("index.html") segs))
  (for ([s segs*])
    (when (or (regexp-match? #rx"^[.]" s) (regexp-match? #rx"[\\\\\0]" s))
      (fail 404 "没有这个文件")))
  (define p (simplify-path (apply build-path STATIC-DIR segs*)))
  (define root (path->string (path->directory-path STATIC-DIR)))
  (unless (and (string-prefix? (path->string p) root) (file-exists? p)) (fail 404 "没有这个文件"))
  p)

(define (serve-static out r head?)
  (define p (static-file (req-path r)))
  (define ext (cond [(regexp-match #rx"[.]([a-z0-9]+)$" (path->string p)) => cadr] [else ""]))
  (write-response out 200 (hash-ref CONTENT-TYPES ext "application/octet-stream")
                  (call-with-input-file p (λ (in) (read-bytes (file-size p) in)) #:mode 'binary)
                  #:head? head?
                  #:extra (if (equal? ext "html")
                              '(("Content-Security-Policy" . "default-src 'self'; style-src 'self'; script-src 'self'")
                                ("Cache-Control" . "no-cache"))
                              '(("Cache-Control" . "no-cache")))))

;; ---------------------------------------------------------------- 连接

(define (route out r)
  (define path (req-path r))
  (define method (req-method r))
  (cond
    [(regexp-match #rx"^/api/(.*)$" path)
     => (λ (m)
          (define body
            (cond [(equal? method "POST")
                   (unless (regexp-match? #rx"^application/json" (hash-ref (req-headers r) "content-type" ""))
                     (fail 415 "POST 请求体应为 application/json"))
                   (with-handlers ([exn:fail? (λ (e) (fail 400 "JSON 解析失败"))])
                     (define v (bytes->jsexpr (req-body r)))
                     (when (eof-object? v) (fail 400 "请求体为空"))
                     v)]
                  [else #f]))
          (define-values (code v) (handle-api method (string-split (cadr m) "/") body))
          (json-response out code v))]
    [(member method '("GET" "HEAD")) (serve-static out r (equal? method "HEAD"))]
    [else (fail 405 "不支持的方法 ~a" method)]))

(define (log-line fmt . args)
  (apply eprintf (string-append "[~a] " fmt "\n") (current-seconds) args))

(define (serve-connection in out)
  (define r #f)
  (with-handlers ([http-error?
                   (λ (e)
                     (log-line "~a ~a -> ~a ~a" (if r (req-method r) "-") (if r (req-path r) "-")
                               (http-error-code e) (http-error-msg e))
                     (with-handlers ([exn:fail? void])
                       (json-response out (http-error-code e) (hasheq 'error (http-error-msg e)))))]
                  [exn:fail?
                   (λ (e)
                     (log-line "内部错误：~a" (exn-message e))
                     (with-handlers ([exn:fail? void])
                       (json-response out 500 (hasheq 'error "内部错误"))))])
    (define x (read-request in))
    (unless (eof-object? x)
      (set! r x)
      (route out x)
      (log-line "~a ~a -> ok" (req-method x) (req-path x)))))

(define (run-server #:host host #:port port #:tls tls-ctx)
  (define listener (tcp-listen port 64 #t host))
  (define slots (make-semaphore MAX-CONNS))
  (define-values (_a bound-port _b _c) (tcp-addresses listener #t))
  (printf "listening on ~a://~a:~a/\n" (if tls-ctx "https" "http") host bound-port)
  (flush-output)
  (let loop ()
    (semaphore-wait slots)
    (define-values (tin tout) (tcp-accept listener))
    (thread
     (λ ()
       (define cust (make-custodian))
       (define worker
         (parameterize ([current-custodian cust])
           (thread (λ ()
                     (with-handlers ([exn:fail? (λ (e) (log-line "连接出错：~a" (exn-message e)))])
                       (if tls-ctx
                           (let-values ([(in out) (ports->ssl-ports tin tout #:mode 'accept #:context tls-ctx
                                                                    #:close-original? #t)])
                             (serve-connection in out)
                             (close-output-port out)
                             (close-input-port in))
                           (serve-connection tin tout)))))))
       (unless (sync/timeout CONN-TIMEOUT worker) (log-line "连接超时，已关闭"))
       (custodian-shutdown-all cust)
       (with-handlers ([exn:fail? void]) (close-output-port tout))
       (with-handlers ([exn:fail? void]) (close-input-port tin))
       (semaphore-post slots)))
    (loop)))

(define (make-tls-context cert key)
  (define ctx (ssl-make-server-context 'auto))
  (ssl-load-certificate-chain! ctx cert)
  (ssl-load-private-key! ctx key)
  (ssl-seal-context! ctx)
  ctx)

(module+ main
  (define host (make-parameter "127.0.0.1"))
  (define port (make-parameter 8080))
  (define https? (make-parameter #f))
  (define cert (make-parameter #f))
  (define key (make-parameter #f))
  (command-line
   #:once-each
   ["--host" h "监听地址（默认 127.0.0.1）" (host h)]
   ["--port" p "端口（默认 8080）"
    (port (or (let ([n (string->number p)]) (and (exact-nonnegative-integer? n) (< n 65536) n))
              (raise-user-error 'server "端口应为 0–65535：~a" p)))]
   ["--https" "启用 HTTPS（需要 --cert 与 --key）" (https? #t)]
   ["--cert" f "证书链 PEM" (cert f)]
   ["--key" f "私钥 PEM" (key f)])
  (when (and (https?) (not (and (cert) (key))))
    (raise-user-error 'server "--https 需要同时给出 --cert 与 --key"))
  (run-server #:host (host) #:port (port)
              #:tls (and (https?) (make-tls-context (cert) (key)))))
