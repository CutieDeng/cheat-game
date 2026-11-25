#lang racket/base
;; v1 排除判定（spec-v1.md §6）与诚实冲突统计
;;
;; Y 已被排除：存在约束的一个子集 S（物证始终在内），S 对 Y 可满足，且在 S 下 Y 没有作案机会。
;; 约束越多可行集越小，所以只需检查极大可满足子集（MSS）。
;; MSS 的枚举：S 可满足则 S 本身；否则取一个极小不可满足核 K（逐个删除法），
;; 对 K 中每个非物证元素 k 递归 S \ {k}。每个 MSS 至少漏掉 K 的一个元素，所以都能被枚举到。
(require racket/list racket/set "claims.rkt")
(provide relevant-claims min-core conflict-cores satisfiable-subsets excluded? MSS-LIMIT)

(define MSS-LIMIT 256)   ; 递归展开的状态数上限；超出时报错（说明冲突过多，案件不该被接受）

(define (relevant-claims m who claims)
  (filter (λ (c) (pair? (constraints-on m c who))) claims))

(define (evidence? c) (eq? (claim-source c) 'evidence))

;; S 对 who 不可满足时，返回一个极小不可满足子集（物证尽量保留在核外不重要：核里可以有物证）
(define (min-core m t0 t1 who S)
  (define (sat? cs) (person-satisfiable? m t0 t1 who cs))
  (let loop ([core S] [i 0])
    (if (>= i (length core))
        core
        (let ([without (append (take core i) (drop core (add1 i)))])
          (if (sat? without) (loop core (add1 i)) (loop without i))))))

;; 枚举可满足子集（包含全部 MSS）与途中发现的不可满足核
(define (explore m t0 t1 who claims)
  (define S0 (relevant-claims m who claims))
  (define seen (mutable-set))
  (define sats '())
  (define cores '())
  (let go ([removed (set)])
    (unless (set-member? seen removed)
      (set-add! seen removed)
      (when (> (set-count seen) MSS-LIMIT) (error 'exclude "~a 的冲突过多（超过 ~a 个分支）" who MSS-LIMIT))
      (define S (filter (λ (c) (not (set-member? removed (claim-id c)))) S0))
      (cond
        [(person-satisfiable? m t0 t1 who S) (set! sats (cons S sats))]
        [else
         (define K (min-core m t0 t1 who S))
         (set! cores (cons (sort (map claim-id K) symbol<?) cores))
         (for ([k K] #:unless (evidence? k))
           (go (set-add removed (claim-id k))))])))
  (values sats (remove-duplicates cores)))

(define (satisfiable-subsets m t0 t1 who claims)
  (define-values (sats _) (explore m t0 t1 who claims))
  sats)

;; 关于 who 的极小冲突（每个是声明 id 的列表）
(define (conflict-cores m t0 t1 who claims)
  (define-values (_ cores) (explore m t0 t1 who claims))
  cores)

;; crime：(list 节点 lo hi d)
(define (excluded? m t0 t1 who claims crime)
  (for/or ([S (satisfiable-subsets m t0 t1 who claims)])
    (not (person-satisfiable? m t0 t1 who S #:opportunity crime))))
