#lang racket/base
;; 证据网分析：静态可达性、拆穿深度、最小割、动态消融。
;;
;; 静态模型（近似，用于生成时快速筛选）：
;;   可用来源 = 证物 + 不撒谎的 NPC 的位置观察 + 已被完全拆穿的撒谎者的位置观察
;;   撒谎者第 i 层可被拆穿 ⇔ 第 i-1 层已拆穿，且某个可用来源与第 i 层冲突
;;   终点 = 凶手被拆穿的层数 ≥ LIES-TO-WIN
;;   深度 = 到达终点所需的轮数（每轮每个撒谎者最多多拆穿一层）
;; 动态消融：逐个删掉一个相关来源，用检查器（引擎 + 规则策略）重新对局，统计仍能破案的比例。
(require racket/list "kb.rkt" "engine.rkt" "solve.rkt")
(provide relevant-sources static-reach min-cut ablation remove-sources chain-depth)

;; 来源：(cons 'k kitem) | (cons 'ev 证物)。只保留与某层掩护说法冲突的
(define (source-prop s) (if (eq? (car s) 'k) (kitem-prop (cdr s)) (third (cdr s))))
(define (source-id s) (if (eq? (car s) 'k) (kitem-id (cdr s)) (first (cdr s))))

(define (relevant-sources cd)
  (define layers (append* (hash-values (case-data-cover cd))))
  (for/list ([s (append (map (λ (k) (cons 'k k)) (case-data-knowledge cd))
                        (map (λ (e) (cons 'ev e)) (case-data-evidence cd)))]
             #:when (for*/or ([layer layers] [x layer])
                      (conflict-kind (prop 'pos x 'exact) (source-prop s))))
    s))

(define (remove-sources cd ids)
  (struct-copy case-data cd
               [knowledge (filter (λ (k) (not (memq (kitem-id k) ids))) (case-data-knowledge cd))]
               [evidence (filter (λ (e) (not (memq (first e) ids))) (case-data-evidence cd))]))

;; 返回 (values 到达终点? 深度或 #f)
(define (static-reach cd [removed '()])
  (define cover (case-data-cover cd))
  (define culprit (case-data-culprit cd))
  (define broken (make-hasheq))
  (define (nb e) (hash-ref broken e 0))
  (define (open? e) (>= (nb e) (length (hash-ref cover e '()))))
  (define (goal?) (>= (nb culprit) LIES-TO-WIN))
  (define srcs (filter (λ (s) (not (memq (source-id s) removed))) (relevant-sources cd)))
  (let loop ([round 1])
    (define avail
      (for/list ([s srcs]
                 #:when (or (eq? (car s) 'ev) (open? (kitem-owner (cdr s)))))
        (source-prop s)))
    (define newly
      (for/list ([(e layers) (in-hash cover)]
                 #:when (< (nb e) (length layers))
                 #:when (for*/or ([x (list-ref layers (nb e))] [p avail])
                          (conflict-kind (prop 'pos x 'exact) p)))
        e))
    (for ([e newly]) (hash-update! broken e add1 0))
    (cond [(goal?) (values #t round)]
          [(null? newly) (values #f #f)]
          [else (loop (add1 round))])))

;; 最小割：删掉几个相关来源能让终点不可达。返回 0、1、2，或 3（表示 ≥3）
(define (min-cut cd)
  (define ids (map source-id (relevant-sources cd)))
  (define (reach? removed) (let-values ([(ok _) (static-reach cd removed)]) ok))
  (cond
    [(not (reach? '())) 0]
    [(for/or ([a ids]) (not (reach? (list a)))) 1]
    [(for*/or ([a ids] [b ids] #:when (symbol<? a b)) (not (reach? (list a b)))) 2]
    [else 3]))

;; 动态消融：返回 (values 仍可破案数 相关来源数 失败的来源 id)
(define (ablation cd)
  (define ids (map source-id (relevant-sources cd)))
  ;; 失败项：(list 来源 id 结局 轮数)
  (define fails
    (for*/list ([a ids]
                [r (in-value (oracle-solve (remove-sources cd (list a))))]
                #:unless (eq? 'won (hash-ref r 'status)))
      (list a (hash-ref r 'status) (hash-ref r 'turns))))
  (values (- (length ids) (length fails)) (length ids) fails))

;; 链长：拆穿凶手第 LIES-TO-WIN 层之前，至少要先依次打开几个撒谎者。
;;   d(证物) = d(不撒谎者的观察) = 0
;;   d(撒谎者 X 的观察) = 1 + d(X 被完全拆穿)
;;   d(X 第 i 层) = max(d(第 i-1 层), min_{与第 i 层冲突的来源 s} d(s))
;; 用迭代松弛求不动点；不可达为 +inf.0。凶手自己的观察不参与（凶手被拆穿就是终点）。
(define (chain-depth cd [removed '()])
  (define cover (case-data-cover cd))
  (define culprit (case-data-culprit cd))
  (define srcs (filter (λ (s) (not (memq (source-id s) removed))) (relevant-sources cd)))
  (define d-layer (make-hash))   ; (cons e i) -> 深度
  (define (layer-d e i) (if (< i 0) 0 (hash-ref d-layer (cons e i) +inf.0)))
  (define (open-d e) (layer-d e (sub1 (length (hash-ref cover e '())))))
  (define (src-d s)
    (cond [(eq? (car s) 'ev) 0]
          [(null? (hash-ref cover (kitem-owner (cdr s)) '())) 0]
          [(eq? (kitem-owner (cdr s)) culprit) +inf.0]
          [else (+ 1 (open-d (kitem-owner (cdr s))))]))
  (let loop ()
    (define changed #f)
    (for* ([(e layers) (in-hash cover)] [i (in-range (length layers))])
      (define best
        (for/fold ([m +inf.0]) ([s srcs]
                                #:when (for/or ([x (list-ref layers i)])
                                         (conflict-kind (prop 'pos x 'exact) (source-prop s))))
          (min m (src-d s))))
      (define v (max (layer-d e (sub1 i)) best))
      (when (< v (layer-d e i)) (hash-set! d-layer (cons e i) v) (set! changed #t)))
    (when changed (loop)))
  (layer-d culprit (sub1 LIES-TO-WIN)))
