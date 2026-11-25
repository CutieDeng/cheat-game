#lang racket/base
;; v1 诚实记忆（spec-v1.md §3.2）：由真实路径和精确观察采样一次，存入案件文件。
;; 诚实 NPC 的证词就是自己的记忆；同一 NPC 复述时一致。
;;
;; 误差（参数见 DEFAULT-NOISE）：
;;   自己的路径：每段端点各偏移 U[−3, 3]；停留 < 3 分钟的段以 p-skip-self 漏掉
;;   对别人的观察：端点各偏移 U[−5, 5]（= TOL-OTHER）；整条以 p-forget 不记得；
;;                 saw / saw-door 以 p-confuse 把地点记成"相近的同类地点"（同类且最短距离 ≤ 2）；
;;                 saw-only / doors-only 名单中每个人以 p-miss 没注意到（从名单里漏掉）
;; 偏移 ≤ 容限，所以只有"记混地点"和"没注意到某人"会让诚实证词与真相冲突。
(require racket/list racket/match "map.rkt" "world.rkt" "claims.rkt")
(provide DEFAULT-NOISE sample-memory memory->claims clause->datum)

(define DEFAULT-NOISE
  (hasheq 'shift-self TOL-SELF 'shift-other TOL-OTHER
          'p-skip-self 0.3 'p-forget 0.15 'p-confuse 0.025 'p-miss 0.025))

(define (uniform rng k) (- (random (add1 (* 2 k)) rng) k))   ; U[−k, k]，整数
(define (chance rng p) (< (random rng) p))

(define (confusable m n)
  (for/list ([x (node-ids m)]
             #:when (and (not (eq? x n)) (eq? (kind-of m x) (kind-of m n)) (<= (dist m n x) 2)))
    x))

;; 返回 hasheq：人 -> 子句列表（内部时间为分钟）
;; window：(cons t0 t1)，给出时把记忆的时刻截到时间窗内（真相在窗内，截断只会更接近真相，不引入新错误）
(define (sample-memory m paths observations rng #:noise [noise DEFAULT-NOISE] #:window [window #f])
  (define (N k) (hash-ref noise k))
  (define (clip t) (if window (min (cdr window) (max (car window) t)) t))
  (define (span a b k)
    (define a* (clip (+ a (uniform rng k))))
    (define b* (clip (+ b (uniform rng k))))
    (values a* (max a* b*)))
  (define (maybe-confuse n)
    (define cs (confusable m n))
    (if (and (pair? cs) (chance rng (N 'p-confuse))) (list-ref cs (random (length cs) rng)) n))
  (define (thin ps) (filter (λ (_) (not (chance rng (N 'p-miss)))) ps))
  (for/hasheq ([p paths])
    (define who (path-who p))
    (define own
      (for*/list ([s (path-segs p)]
                  #:unless (and (< (- (seg-out s) (seg-in s)) 3) (chance rng (N 'p-skip-self))))
        (define-values (a b) (span (seg-in s) (seg-out s) (N 'shift-self)))
        (list 'at who (seg-node s) a b)))
    (define seen
      (for*/list ([o observations]
                  #:when (eq? (obs-observer o) who)
                  #:unless (chance rng (N 'p-forget)))
        (match o
          [(obs 'saw _ s n f t _)
           (define-values (a b) (span f t (N 'shift-other)))
           (list 'saw s (maybe-confuse n) a b)]
          [(obs 'saw-door _ s n f _ d)
           (list 'saw-door s (maybe-confuse n) d (clip (+ f (uniform rng (N 'shift-other)))))]
          [(obs (and k (or 'saw-only 'doors-only)) _ ps n f t _)
           (define-values (a b) (span f t (N 'shift-other)))
           (list k n a b (thin ps))])))
    (values who (append own seen))))

;; 记忆 → 声明（说话人即记忆的主人；容限按种类）
(define (memory->claims mem #:only [only #f])
  (append*
   (for/list ([who (sort (hash-keys mem) symbol<?)]
              #:when (or (not only) (memq who only)))
     (for/list ([cl (hash-ref mem who)] [i (in-naturals)])
       (claim (string->symbol (format "~a#~a" who i)) who cl (default-tol who cl 'testimony) 'testimony)))))

;; 子句（分钟）→ 文件格式（HHMM）
(define (clause->datum cl)
  (match cl
    [(list (and k (or 'at 'saw)) who n a b) (list k who n (min->hhmm a) (min->hhmm b))]
    [(list 'saw-door who n d t) (list 'saw-door who n d (min->hhmm t))]
    [(list (and k (or 'saw-only 'doors-only)) n a b ps) (list k n (min->hhmm a) (min->hhmm b) ps)]
    [(list 'move who from to t) (list 'move who from to (min->hhmm t))]))
