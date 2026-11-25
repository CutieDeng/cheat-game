#lang racket/base
;; v1（地图版）JSON API。规则全部经 engine-v1.rkt；本文件只做参数校验、会话与 JSON 转换。
;;
;;   POST /api/v1/games                 {npcs?: 4..8, seed?, planner?: "omni"|"local"}  -> 局面（201）
;;   GET  /api/v1/games/<id>            局面
;;   POST /api/v1/games/<id>/ask        {kind: "self"|"about"|"place", npc, arg?, slot}
;;                                      arg：about 时为人物 id，place 时为地点 id
;;   POST /api/v1/games/<id>/present    {npc, item}   item：物证 id 或听到的声明 id（字符串）
;;   POST /api/v1/games/<id>/accuse     {npc}
;; 动作的响应：{result: {...}, state: 局面}
;; 局面不含排除状态（用户决定：由玩家自己判断）；对局结束后附 reveal（凶手、真实路径、凶手的故事）。
(require racket/list racket/match racket/string racket/random
         "http-error.rkt" "map.rkt" "world.rkt" "claims.rkt" "case1.rkt"
         "engine-v1.rkt" "liar-v1.rkt" "text-v1.rkt" "gen-v1.rkt")
(provide handle-api-v1)

(define MAX-GAMES 1000)

;; log：新的在前
(struct sess (id seed planner g lock [log #:mutable] [touched #:mutable]))
(define sessions (make-hash))
(define sessions-lock (make-semaphore 1))

(define (new-id) (apply string-append (for/list ([b (crypto-random-bytes 16)])
                                        (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (add-session! s)
  (call-with-semaphore
   sessions-lock
   (λ ()
     (when (>= (hash-count sessions) MAX-GAMES)
       (hash-remove! sessions (sess-id (argmin sess-touched (hash-values sessions)))))
     (hash-set! sessions (sess-id s) s))))

(define (get-session id)
  (define s (call-with-semaphore sessions-lock (λ () (hash-ref sessions id #f))))
  (unless s (fail 404 "对局不存在或已过期：~a" id))
  (set-sess-touched! s (current-inexact-milliseconds))
  s)

;; ---------------------------------------------------------------- JSON

(define (sym x) (symbol->string x))

(define (slot-label g i)
  (match (slot-range g i) [(cons a b) (format "~a–~a" (fmt-time a) (fmt-time b))]))

(define (claim->json c cl)
  (hasheq 'id (sym (claim-id cl))
          'speaker (if (claim-speaker cl) (sym (claim-speaker cl)) "")
          'speaker_name (if (claim-speaker cl) (person-name c (claim-speaker cl)) "物证")
          'text (claim->text c cl)
          'clause (format "~s" (claim-clause cl))))

(define (path->json c p)
  (for/list ([s (path-segs p)])
    (hasheq 'place (sym (seg-node s)) 'place_name (place-name c (seg-node s))
            'from (fmt-time (seg-in s)) 'to (fmt-time (seg-out s)))))

(define (state->json s)
  (define g (sess-g s))
  (define c (game1-c g))
  (define m (case1-map c))
  (match-define (list cn clo chi cd) (case1-crime c))
  (define done? (memq (game1-status g) '(won lost)))
  (hash-set*
   (hasheq
    'id (sess-id s) 'seed (sess-seed s) 'planner (sess-planner s)
    'title (case1-title c) 'briefing (case1-briefing c)
    'status (sym (game1-status g))
    'ap_used (game1-ap g) 'ap_budget (game1-budget g)
    'costs (for/hasheq ([(k v) (in-hash (game1-costs g))]) (values k v))
    'expose_to_win EXPOSE-TO-WIN
    'crime (hasheq 'place (sym cn) 'place_name (place-name c cn)
                   'from (fmt-time clo) 'to (fmt-time chi) 'min_minutes cd)
    'map (hasheq 'nodes (for/list ([n (node-ids m)])
                          (hasheq 'id (sym n) 'name (place-name c n) 'kind (sym (kind-of m n))))
                 'edges (remove-duplicates
                         (for*/list ([a (node-ids m)] [e (neighbors m a)] #:when (symbol<? a (car e)))
                           (hasheq 'a (sym a) 'b (sym (car e)) 'minutes (cdr e))))
                 'windows (for/list ([w (gmap-windows m)]) (hasheq 'from (sym (car w)) 'to (sym (cdr w)))))
    'persons (for/list ([p (person-ids c)]) (hasheq 'id (sym p) 'name (person-name c p)))
    'slots (for/list ([i (length (game1-slots g))]) (hasheq 'index i 'label (slot-label g i)))
    'evidence (for/list ([e (case1-evidence c)])
                (hasheq 'id (sym (first e)) 'name (second e) 'text (claim->text c (third e))))
    'heard (for/list ([cl (game1-heard g)]) (claim->json c cl))
    'exposed (for/list ([e (game1-exposed g)])
               (match-define (list who cl src) e)
               (hasheq 'person (sym who) 'person_name (person-name c who)
                       'claim_id (sym (claim-id cl)) 'claim_text (claim->text c cl)
                       'source (sym src)
                       'source_name (if (assq src (case1-persons c)) (person-name c src) (sym src))))
    'log (reverse (sess-log s)))
   'reveal
   (if done?
       (hasheq 'culprit (sym (case1-culprit c)) 'culprit_name (person-name c (case1-culprit c))
               'paths (for/list ([p (case1-paths c)])
                        (hasheq 'person (sym (path-who p)) 'person_name (person-name c (path-who p))
                                'segs (path->json c p)))
               'story (path->json c (game1-story g)))
       (json-null))))

(define (json-null) 'null)

;; ---------------------------------------------------------------- 参数

(define (person-arg c v what)
  (define x (and (string? v) (string->symbol v)))
  (unless (and x (memq x (person-ids c))) (fail 400 "~a 不是在场者：~s" what v))
  x)
(define (place-arg c v)
  (define x (and (string? v) (string->symbol v)))
  (unless (and x (memq x (node-ids (case1-map c)))) (fail 400 "不是地图上的地点：~s" v))
  x)
(define (slot-arg g v)
  (unless (and (exact-nonnegative-integer? v) (< v (length (game1-slots g)))) (fail 400 "时段下标无效：~s" v))
  v)
(define (item-arg g v)
  (define c (game1-c g))
  (define x (and (string? v) (string->symbol v)))
  (unless (and x (or (assq x (case1-evidence c)) (for/or ([cl (game1-heard g)]) (eq? (claim-id cl) x))))
    (fail 400 "没有这件物证或这条听到的证词：~s" v))
  x)

(define (check-action! g kind)
  (unless (active1? g) (fail 409 "这局已经结束"))
  (unless (or (eq? kind 'accuse) (eq? (game1-status g) 'playing)) (fail 409 "行动点已用完，只能指认"))
  (unless (affordable1? g kind) (fail 409 "行动点不足")))

;; ---------------------------------------------------------------- 动作

(define (log! s entry) (set-sess-log! s (cons entry (sess-log s))))

(define (do-ask! s b)
  (define g (sess-g s))
  (define c (game1-c g))
  (check-action! g 'ask)
  (define kind (hash-ref b 'kind #f))
  (define npc (person-arg c (hash-ref b 'npc #f) "npc"))
  (define slot (slot-arg g (hash-ref b 'slot #f)))
  (define q
    (match kind
      ["self" (list 'self npc slot)]
      ["about" (let ([y (person-arg c (hash-ref b 'arg #f) "arg")])
                 (when (eq? y npc) (fail 400 "about 的对象不能是被问的人自己"))
                 (list 'about npc y slot))]
      ["place" (list 'place npc (place-arg c (hash-ref b 'arg #f)) slot)]
      [_ (fail 400 "kind 应为 self / about / place")]))
  (define cls (ask1! g q))
  (define r (hasheq 'kind "ask" 'npc (sym npc) 'npc_name (person-name c npc)
                    'question (question->text c (λ (i) (slot-label g i)) q)
                    'answers (for/list ([cl cls]) (claim->json c cl))
                    'cost (hash-ref (game1-costs g) 'ask) 'ap_used (game1-ap g)))
  (log! s r)
  r)

(define (do-present! s b)
  (define g (sess-g s))
  (define c (game1-c g))
  (check-action! g 'present)
  (define npc (person-arg c (hash-ref b 'npc #f) "npc"))
  (define item (item-arg g (hash-ref b 'item #f)))
  (define before (length (exposure-sources g npc)))
  (define found (present1! g npc item))
  (define item-text
    (cond [(assq item (case1-evidence c)) => (λ (e) (format "~a（~a）" (second e) (claim->text c (third e))))]
          [else (let ([cl (findf (λ (x) (eq? (claim-id x) item)) (game1-heard g))])
                  (format "~a说：「~a」" (person-name c (claim-speaker cl)) (claim->text c cl)))]))
  (define r (hasheq 'kind "present" 'npc (sym npc) 'npc_name (person-name c npc)
                    'item (sym item) 'item_text item-text
                    'conflicts (for/list ([f found])
                                 (hasheq 'claim (claim->json c (first f))
                                         'type (case (second f) [(evidence) "物证"] [(sight) "视线"]
                                                 [(location) "位置"] [(travel) "来不及走到"])))
                    'new_exposure (> (length (exposure-sources g npc)) before)
                    'cost (hash-ref (game1-costs g) 'present) 'ap_used (game1-ap g)))
  (log! s r)
  r)

(define (do-accuse! s b)
  (define g (sess-g s))
  (define c (game1-c g))
  (check-action! g 'accuse)
  (define npc (person-arg c (hash-ref b 'npc #f) "npc"))
  (define culprit? (eq? npc (case1-culprit c)))
  (define ok (accuse1! g npc))
  (define gaps (if (and culprit? (not ok)) (accusation-gaps g npc) '()))
  (define r (hasheq 'kind "accuse" 'npc (sym npc) 'npc_name (person-name c npc) 'won ok
                    'text (cond [ok (if (>= (length (exposure-sources g npc)) EXPOSE-TO-WIN)
                                        "破案：他的行踪说法已被多方拆穿。"
                                        "破案：其他人都已被排除。")]
                                [culprit? (accuse-shortfall-text c g npc gaps)]
                                [else (format "指认错误：~a不是凶手。" (person-name c npc))])
                    'gaps (for/list ([x gaps])
                            (hasheq 'person (sym (car x)) 'person_name (person-name c (car x))
                                    'reason (sym (cdr x))))
                    'cost 0 'ap_used (game1-ap g)))
  (log! s r)
  r)

;; 指认了凶手但证据不足时的说明：谁没被排除、为什么；拆穿差几个来源
(define (accuse-shortfall-text c g z gaps)
  (string-append
   "证据不足。\n"
   (string-join
    (for/list ([x gaps])
      (format "· ~a没有被排除：~a" (person-name c (car x))
              (case (cdr x)
                [(self-only) "你没有听到别人关于他去向的证词（他本人的话不算），需要别人的证词或物证。"]
                [else "有别人的证词提到他，但还不足以说明他不可能在案发时段待够作案所需的时间。"])))
    "\n")
   (format "\n· 拆穿：~a的行踪说法被 ~a 个不同来源拆穿，需要 ~a 个。"
           (person-name c z) (length (exposure-sources g z)) EXPOSE-TO-WIN)))

(define (create-game b)
  (define n (hash-ref b 'npcs 5))
  (unless (memv n '(4 5 6 7 8)) (fail 400 "npcs 应为 4–8"))
  (define seed (hash-ref b 'seed (λ () (random 1 100000 (make-pseudo-random-generator)))))
  (unless (and (exact-positive-integer? seed) (< seed 1000000)) (fail 400 "seed 应为 1–999999 的整数"))
  (define planner (hash-ref b 'planner "omni"))
  (unless (member planner '("omni" "local")) (fail 400 "planner 应为 omni 或 local"))
  (define-values (d attempts errs) (generate-case1 seed #:npcs n))
  (unless d (fail 500 "生成失败：~a" errs))
  (define c (datum->case1 d))
  (define g (new-game1 c #:planner (if (equal? planner "omni") omniscient-planner local-planner)))
  (define s (sess (new-id) seed planner g (make-semaphore 1) '() (current-inexact-milliseconds)))
  (add-session! s)
  (state->json s))

;; method：字符串；segs：去掉 "/api/v1/" 后的路径段；body：jsexpr 或 #f。返回 (values 状态码 jsexpr)
(define (handle-api-v1 method segs body)
  (define (need-body) (unless (hash? body) (fail 400 "请求体应为 JSON 对象")) body)
  (define (locked s thunk) (call-with-semaphore (sess-lock s) thunk))
  (match* (method segs)
    [("POST" '("games")) (values 201 (create-game (need-body)))]
    [("GET" (list "games" id)) (let ([s (get-session id)]) (values 200 (locked s (λ () (state->json s)))))]
    [("POST" (list "games" id (and act (or "ask" "present" "accuse"))))
     (define s (get-session id))
     (define b (need-body))
     (values 200 (locked s (λ ()
                             (define r ((case act [("ask") do-ask!] [("present") do-present!] [else do-accuse!]) s b))
                             (hasheq 'result r 'state (state->json s)))))]
    [(_ (or '("games") (list "games" _) (list "games" _ (or "ask" "present" "accuse"))))
     (fail 405 "不支持的方法 ~a" method)]
    [(_ _) (fail 404 "没有这个接口")]))
