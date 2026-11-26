#lang racket/base
;; v1 校验器（spec-v1.md §8.4 第 1 步）：给凶手的一个故事打分
;;
;;   V1 格式与实体：是凶手本人的路径、节点都在地图上
;;   V2 可行性：相邻、通行时间相符、覆盖时间窗（world.rkt path-errors）
;;   V3 可被拆穿：故事里他关于自己行踪的每一段，与诚实证词（非凶手的记忆）或物证的冲突；按来源（说话人 / 物证）去重
;;   V4 自认：故事里有他在案发时段待在作案地点（与引擎的自认法同一判定 admits-claims?，按容限收缩后与案发窗口重叠）
;;   存活分：公开信息侦探（宽预算）对这个故事下一局：是否被拆穿定罪、破案所需行动点
;; V5（引擎契约）在对局内由 engine-v1.rkt 强制，不在故事层面检查。
;;
;; 排序键（越大越好，按字典序）：合法（V1、V2 通过且不自认）> 未被拆穿定罪 > 侦探所需行动点 > −拆穿来源数
;; 自认算不合法：自认即定罪（引擎的自认法）；而且若不排除，照实说出作案行程的故事会因为"没有矛盾"排在前面（教师冒烟测试中出现）
(require racket/list racket/match
         "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt" "engine-v1.rkt" "detective-v1.rkt")
(provide verify-story story-key key>? story-self-claims)

(define (story-self-claims c story)
  (define who (case1-culprit c))
  (for/list ([s (path-segs story)] [i (in-naturals)])
    (claim (string->symbol (format "S#~a" i)) who (list 'at who (seg-node s) (seg-in s) (seg-out s)) TOL-SELF 'testimony)))

(define (verify-story c story #:detective? [detective? #t] #:budget [budget 400])
  (define m (case1-map c))
  (define who (case1-culprit c))
  (define v1
    (append (if (eq? (path-who story) who) '() (list "不是凶手本人的路径"))
            (for/list ([s (path-segs story)] #:unless (memq (seg-node s) (node-ids m)))
              (format "未知地点 ~a" (seg-node s)))))
  (define v2 (if (null? v1) (path-errors m story (case1-t0 c) (case1-t1 c)) '()))
  (define ok (and (null? v1) (null? v2)))
  (define honest (append (memory->claims (case1-memory* c) #:only (remq who (person-ids c)))
                         (map third (case1-evidence c))))
  (define refutations
    (if ok
        (for*/list ([sc (story-self-claims c story)]
                    [h honest]
                    [k (in-value (conflict-type m (case1-t0 c) (case1-t1 c) who sc h))] #:when k)
          (list sc h k))
        '()))
  (define sources
    (remove-duplicates (for/list ([r refutations])
                         (define h (second r))
                         (if (eq? (claim-source h) 'evidence) (claim-id h) (claim-speaker h)))))
  (define admits (and ok (admits-claims? c who (story-self-claims c story))))
  (define survival
    (and ok detective?
         (let* ([g (new-game1 c #:planner (λ (_) story) #:budget budget)]
                [st (detective-run! g)])
           (hasheq 'status st 'ap (game1-ap g)
                   'exposed (and (eq? st 'won) (>= (length (exposure-sources g who)) EXPOSE-TO-WIN))))))
  (hasheq 'v1 v1 'v2 v2 'ok ok
          'refutations (length refutations) 'sources (length sources)
          'admits admits 'survival survival))

;; 排序键：(合法且不自认 未被拆穿定罪 侦探行动点 −来源数)，布尔按 1/0
(define (story-key r)
  (define s (hash-ref r 'survival))
  (list (if (and (hash-ref r 'ok) (not (hash-ref r 'admits))) 1 0)
        (if (and s (not (hash-ref s 'exposed))) 1 0)
        (if s (hash-ref s 'ap) 0)
        (- (hash-ref r 'sources))))

(define (key>? a b)
  (cond [(null? a) #f]
        [(> (car a) (car b)) #t]
        [(< (car a) (car b)) #f]
        [else (key>? (cdr a) (cdr b))]))
