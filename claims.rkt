#lang racket/base
;; v1 声明、容限与冲突（spec-v1.md §4）
;;
;; 声明子句（文件里时间写作 HHMM，内部为分钟）：
;;   (at 谁 节点 起 止)          谁在某处（说自己或说别人）
;;   (saw 谁 节点 起 止)         说话人看见谁在某处
;;   (saw-door 谁 房间 in|out 时刻)  说话人在走廊看见谁进 / 出某房间
;;   (saw-only 节点 起 止 (人 ...))   说话人能看见该节点，期间除自己外只看见这些人
;;   (saw-none 节点 起 止)            = (saw-only 节点 起 止 ())
;;   (doors-only 房间 起 止 (人 ...)) 说话人在走廊看着该房间门口，期间进出的只有这些人
;;   (move 谁 从 到 时刻)        谁约在某时刻从一处出发去另一处
;; 容限 tol（分钟）只由声明种类决定：物证 0；关于自己 3；关于别人 8（含 saw-only / doors-only）。
;;
;; 每条声明对某个人的路径产生约束：
;;   in-c    在 [lo, hi] 每一分钟都位于 node（at / saw 按容限收缩：[起 + tol, 止 − tol]，非空时）
;;           收缩后为空（短暂停留、容限大）时改为 visit-c [起 − tol, 止 + tol]：至少在附近某时刻到过那里
;;   out-c   在 [lo, hi] 每一分钟都不在 node（saw-only，对说话人和名单以外的每个人）
;;   nocross-c 在 [lo, hi] 内没有进出房间 node（doors-only，对说话人和名单以外的每个人）
;;   visit-c 在 [lo, hi] 中至少有一分钟位于 node（saw-door、move，按容限放宽）
;; 判定：时间展开图上的可达性（节点 × 分钟，含通行途中的分钟），见 satisfiable?。
;; 两条声明冲突 = 各自可满足、合起来对某人不可满足。冲突种类只用于向玩家解释：
;;   evidence（涉及物证）> sight（涉及 saw-only / doors-only）> location（时间区间重叠）> travel（来不及走到）
(require racket/list racket/match "map.rkt" "world.rkt")
(provide (struct-out claim) (struct-out in-c) (struct-out out-c) (struct-out visit-c) (struct-out nocross-c)
         TOL-SELF TOL-OTHER default-tol datum->claim claim-subjects constraints-on
         satisfiable? path-satisfies? person-satisfiable? conflict-type obs->claim)

(define TOL-SELF 3)
(define TOL-OTHER 5)

;; clause 内部时间为分钟。source: testimony | evidence
(struct claim (id speaker clause tol source) #:transparent)
(struct in-c (node lo hi) #:transparent)
(struct out-c (node lo hi) #:transparent)
(struct visit-c (node lo hi) #:transparent)
(struct nocross-c (node lo hi) #:transparent)
(define NEGATIVE '(saw-only doors-only))

(define (clause-subject cl)
  (match cl
    [(list (or 'at 'saw 'saw-door 'move) who _ ...) who]
    [(list (or 'saw-only 'doors-only) _ ...) #f]))

(define (default-tol speaker cl source)
  (cond [(eq? source 'evidence) 0]
        [(and (memq (car cl) '(at move)) (eq? (clause-subject cl) speaker)) TOL-SELF]
        [else TOL-OTHER]))

;; 文件中的子句（HHMM）→ claim
(define (datum->claim id speaker d #:source [source 'testimony] #:tol [tol #f])
  (define cl
    (match d
      [(list (and k (or 'at 'saw)) who n a b) (list k who n (hhmm->min a) (hhmm->min b))]
      [(list 'saw-door who r dir t) (list 'saw-door who r dir (hhmm->min t))]
      [(list 'saw-none n a b) (list 'saw-only n (hhmm->min a) (hhmm->min b) '())]
      [(list (and k (or 'saw-only 'doors-only)) n a b ps) (list k n (hhmm->min a) (hhmm->min b) ps)]
      [(list 'move who from to t) (list 'move who from to (hhmm->min t))]))
  (claim id speaker cl (or tol (default-tol speaker cl source)) source))

;; 观察 → 说出它的声明（诚实且无误差时）
(define (obs->claim id o #:tol [tol #f])
  (define cl
    (match o
      [(obs 'saw _ s n f t _) (list 'saw s n f t)]
      [(obs 'saw-door _ s n f _ d) (list 'saw-door s n d f)]
      [(obs (and k (or 'saw-only 'doors-only)) _ ps n f t _) (list k n f t ps)]))
  (claim id (obs-observer o) cl (or tol (default-tol (obs-observer o) cl 'testimony)) 'testimony))

;; 这条声明约束了哪些人的路径
(define (claim-subjects c persons)
  (match (claim-clause c)
    [(list (or 'saw-only 'doors-only) _ _ _ ps)
     (filter (λ (p) (not (or (eq? p (claim-speaker c)) (memq p ps)))) persons)]
    [cl (list (clause-subject cl))]))

;; 声明对 who 的路径产生的约束列表
(define (constraints-on m c who)
  (define tol (claim-tol c))
  (define (shrink n a b)
    (if (<= (+ a tol) (- b tol))
        (list (in-c n (+ a tol) (- b tol)))
        (list (visit-c n (- a tol) (+ b tol)))))
  (match (claim-clause c)
    [(list (or 'at 'saw) (== who) n a b) (shrink n a b)]
    [(list 'saw-door (== who) r _ t) (list (visit-c r (- t tol) (+ t tol)))]
    [(list 'move (== who) from to t)
     (list (visit-c from (- t tol) (+ t tol))
           (visit-c to (- t tol) (+ t tol (dist m from to))))]
    [(list (and k (or 'saw-only 'doors-only)) n a b ps)
     (if (and (not (eq? who (claim-speaker c))) (not (memq who ps)) (<= (+ a tol) (- b tol)))
         (list ((if (eq? k 'saw-only) out-c nocross-c) n (+ a tol) (- b tol)))
         '())]
    [_ '()]))

;; ---------------------------------------------------------------- 可满足性
;; cs：约束列表；[t0, t1]：时间窗。opportunity：#f 或 (list 节点 lo hi d)，
;; 要求路径在 [lo, hi] 内于该节点连续停留至少 d 分钟（作案机会）。
(define (satisfiable? m t0 t1 cs #:opportunity [opp #f])
  (define n-t (add1 (- t1 t0)))
  (define must (make-vector n-t #f))   ; #f 或 节点
  (define forbid (make-vector n-t '()))
  (define ok
    (for/and ([c cs])
      (match c
        [(in-c n lo hi)
         (for/and ([t (in-range (max lo t0) (add1 (min hi t1)))])
           (define i (- t t0))
           (define cur (vector-ref must i))
           (cond [(not cur) (vector-set! must i n) #t]
                 [else (eq? cur n)]))]
        [(out-c n lo hi)
         (for ([t (in-range (max lo t0) (add1 (min hi t1)))])
           (vector-set! forbid (- t t0) (cons n (vector-ref forbid (- t t0)))))
         #t]
        [(or (visit-c _ _ _) (nocross-c _ _ _)) #t])))
  (and ok
       (let ()
         (define visits (for/list ([c cs] #:when (visit-c? c)) c))
         (define nocross (for/list ([c cs] #:when (nocross-c? c)) c))
         ;; 在 t 离开 a、在 arr 到达 b：出门事件在 t（a 是房间），进门事件在 arr（b 是房间）
         (define (crossing-ok? a t b arr)
           (for/and ([x nocross])
             (not (or (and (eq? (nocross-c-node x) a) (<= (nocross-c-lo x) t (nocross-c-hi x)))
                      (and (eq? (nocross-c-node x) b) (<= (nocross-c-lo x) arr (nocross-c-hi x)))))))
         (define full (sub1 (arithmetic-shift 1 (length visits))))
         (define (allowed? n t)
           (define i (- t t0))
           (and (let ([mu (vector-ref must i)]) (or (not mu) (eq? mu n)))
                (not (memq n (vector-ref forbid i)))))
         (define (transit-ok? t) (not (vector-ref must (- t t0))))
         (define (flags-at n t fl)
           (for/fold ([fl fl]) ([v visits] [k (in-naturals)])
             (if (and (eq? (visit-c-node v) n) (<= (visit-c-lo v) t (visit-c-hi v)))
                 (bitwise-ior fl (arithmetic-shift 1 k))
                 fl)))
         (define d (if opp (fourth opp) 0))
         (define (run-at n t prev-run)   ; prev-run：上一分钟（在同一节点）时的连续分钟数；刚到达时传 0
           (if (and opp (eq? n (first opp)) (<= (second opp) t (third opp))) (min d (add1 prev-run)) 0))
         ;; 状态：(vector 节点 flags run done)
         (define reach (make-vector n-t #f))
         (for ([i n-t]) (vector-set! reach i (make-hash)))
         ;; 某个 visit 的时间窗已过而没到过：这个状态不可能满足，直接丢弃（状态数只随同时开放的窗口数增长）
         (define (missed? t fl)
           (for/or ([v visits] [k (in-naturals)])
             (and (< (visit-c-hi v) t) (not (bitwise-bit-set? fl k)))))
         (define (add! t n fl run done)
           (unless (missed? t fl)
             (hash-set! (vector-ref reach (- t t0)) (vector n fl run done) #t)))
         (for ([n (node-ids m)] #:when (allowed? n t0))
           (define r (run-at n t0 0))
           (add! t0 n (flags-at n t0 0) r (and opp (>= r d))))
         (for ([t (in-range t0 t1)])
           (for ([st (in-hash-keys (vector-ref reach (- t t0)))])
             (match-define (vector n fl run done) st)
             ;; 停留
             (when (allowed? n (add1 t))
               (define r (run-at n (add1 t) run))
               (add! (add1 t) n (flags-at n (add1 t) fl) r (or done (and opp (>= r d)))))
             ;; 沿边移动：在 t 离开，t + w 到达；中间的分钟在路上
             (for ([e (neighbors m n)])
               (define arr (+ t (cdr e)))
               (when (and (<= arr t1)
                          (for/and ([u (in-range (add1 t) arr)]) (transit-ok? u))
                          (allowed? (car e) arr)
                          (crossing-ok? n t (car e) arr))
                 (define r (run-at (car e) arr 0))
                 (add! arr (car e) (flags-at (car e) arr fl) r (or done (and opp (>= r d))))))))
         (for/or ([st (in-hash-keys (vector-ref reach (- t1 t0)))])
           (and (= (vector-ref st 1) full) (or (not opp) (vector-ref st 3)))))))

;; 直接检查一条具体路径是否满足约束（用于与 satisfiable? 对照）
(define (path-satisfies? m p cs)
  (define evs (door-events m p))
  (for/and ([c cs])
    (match c
      [(nocross-c n lo hi) (not (for/or ([ev evs]) (and (eq? (first ev) n) (<= lo (third ev) hi))))]
      [(in-c n lo hi) (for/and ([t (in-range lo (add1 hi))]) (eq? (at-node p t) n))]
      [(out-c n lo hi) (for/and ([t (in-range lo (add1 hi))]) (not (eq? (at-node p t) n)))]
      [(visit-c n lo hi) (for/or ([t (in-range lo (add1 hi))]) (eq? (at-node p t) n))])))

;; 一组声明对 who 是否可满足
(define (person-satisfiable? m t0 t1 who claims #:opportunity [opp #f])
  (satisfiable? m t0 t1 (append-map (λ (c) (constraints-on m c who)) claims) #:opportunity opp))

;; 两条声明对 who 是否冲突；冲突时返回种类，否则 #f
(define (conflict-type m t0 t1 who c1 c2)
  (define (sat? cs) (person-satisfiable? m t0 t1 who cs))
  (and (sat? (list c1)) (sat? (list c2)) (not (sat? (list c1 c2)))
       (cond
         [(or (eq? (claim-source c1) 'evidence) (eq? (claim-source c2) 'evidence)) 'evidence]
         [(or (memq (car (claim-clause c1)) NEGATIVE) (memq (car (claim-clause c2)) NEGATIVE)) 'sight]
         [(intervals-overlap? (constraints-on m c1 who) (constraints-on m c2 who)) 'location]
         [else 'travel])))

(define (c-lo c) (match c [(in-c _ lo _) lo] [(out-c _ lo _) lo] [(visit-c _ lo _) lo] [(nocross-c _ lo _) lo]))
(define (c-hi c) (match c [(in-c _ _ hi) hi] [(out-c _ _ hi) hi] [(visit-c _ _ hi) hi] [(nocross-c _ _ hi) hi]))
(define (intervals-overlap? as bs)
  (for*/or ([a as] [b bs]) (and (<= (c-lo a) (c-hi b)) (<= (c-lo b) (c-hi a)))))
