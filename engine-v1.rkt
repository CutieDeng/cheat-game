#lang racket/base
;; v1 对局（spec-v1.md §6）
;;
;; 提问（1 点）：
;;   (self 人 时段)        问某人自己这段时间在哪
;;   (about 人 某人 时段)  问某人这段时间有没有见过另一个人
;;   (place 人 地点 时段)  问某人这段时间在某地见过谁（含"只看见谁""门口只有谁进出"）
;; 回答：从说话人的记忆里取出与该时段重叠的相关子句；诚实者用案件里的诚实记忆，凶手用假记忆（liar-v1.rkt）。
;; 出示（2 点）：把物证或听到的某条声明给某人看，报告它与此人说过的话的冲突；对方承认可能记错（M3 不改变状态）。
;; 指认（0 点）：指认的是凶手，且满足其一（用户决定 2026-11-25）：
;;   排除法：其他每个人都已被排除（只用说话人不是被排除者、也不是被指认者的声明，加物证）；
;;   拆穿法：他关于自己行踪的说法，已被至少 EXPOSE-TO-WIN 个不同来源（别人的证词按说话人计、物证按件计）
;;           经出示证明矛盾。诚实者自己的行踪只有容限内的时间偏差，要被两个不同来源拆穿，需要两个人都记错他。
(require racket/list racket/match "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt" "exclude.rkt" "liar-v1.rkt")
(provide (struct-out game1) new-game1 COSTS1 SLOT-MIN slot-range question-claims
         ask1! present1! accuse1! excluded-now? can-win? active1? affordable1? ap-left1 all-questions
         default-budget1 budget-for-npcs EXPOSE-TO-WIN exposure-sources accusation-gaps)

(define COSTS1 (hasheq 'ask 1 'present 2 'accuse 0))
(define EXPOSE-TO-WIN 2)
(define SLOT-MIN 30)

;; mem：人 -> 声明列表（id 稳定，重复提问得到同样的声明）
;; heard：已听到的声明（按听到的先后，去重）；asked：问过的问题 -> #t
;; status：playing accuse-only won lost
;; exposed：(listof (list 人 他被拆穿的声明 来源))，来源是出示的声明的说话人或物证 id
(struct game1 (c slots costs budget mem story
               [ap #:mutable] [heard #:mutable] asked [status #:mutable] [log #:mutable] [exposed #:mutable])
  #:transparent)

;; 预算 ⌈1.2 · 人数²⌉（用户决定 2026-11-25，依据 runs/budget1-v1.txt：公开信息侦探所需行动点约随人数平方增长）
(define (budget-for-npcs n) (ceiling (* 6/5 n n)))
(define (default-budget1 c) (budget-for-npcs (length (person-ids c))))

;; 每段 SLOT-MIN 分钟，最后一段包含 t1
(define (make-slots t0 t1)
  (define los (for/list ([lo (in-range t0 t1 SLOT-MIN)]) lo))
  (for/list ([lo los] [i (in-naturals)])
    (cons lo (if (= i (sub1 (length los))) t1 (+ lo SLOT-MIN -1)))))

(define (new-game1 c #:planner [planner local-planner] #:budget [budget #f] #:costs [costs COSTS1])
  (define culprit (case1-culprit c))
  (define story (planner c))
  (define honest (case1-memory* c))
  (define mem
    (for/hasheq ([who (person-ids c)])
      (define cls (if (eq? who culprit) (fake-memory c story) (hash-ref honest who)))
      (values who
              (for/list ([cl cls] [i (in-naturals)])
                (claim (string->symbol (format "~a#~a" who i)) who cl (default-tol who cl 'testimony) 'testimony)))))
  (game1 c (make-slots (case1-t0 c) (case1-t1 c)) costs (or budget (default-budget1 c)) mem story
         0 '() (make-hash) 'playing '() '()))

(define (slot-range g i) (list-ref (game1-slots g) i))

;; ---------------------------------------------------------------- 回答

(define (overlaps? a b lo hi) (and (<= a hi) (<= lo b)))

(define (question-claims g q)
  (match-define (list kind who arg slot) (if (= (length q) 3) (list (first q) (second q) #f (third q)) q))
  (match-define (cons lo hi) (slot-range g slot))
  (for/list ([cl (hash-ref (game1-mem g) who)]
             #:when (match* (kind (claim-clause cl))
                      [('self (list 'at (== who) _ a b)) (overlaps? a b lo hi)]
                      [('about (list 'saw (== arg) _ a b)) (overlaps? a b lo hi)]
                      [('about (list 'saw-door (== arg) _ _ t)) (<= lo t hi)]
                      [('place (list 'saw _ (== arg) a b)) (overlaps? a b lo hi)]
                      [('place (list (or 'saw-only 'doors-only) (== arg) a b _)) (overlaps? a b lo hi)]
                      [(_ _) #f]))
    cl))

;; 所有合法问题（终端菜单与检查器用）
(define (all-questions g)
  (define c (game1-c g))
  (define ids (person-ids c))
  (define nodes (node-ids (case1-map c)))
  (define ns (range (length (game1-slots g))))
  (append (for*/list ([p ids] [s ns]) (list 'self p s))
          (for*/list ([p ids] [y ids] #:unless (eq? p y) [s ns]) (list 'about p y s))
          (for*/list ([p ids] [n nodes] [s ns]) (list 'place p n s))))

;; ---------------------------------------------------------------- 行动点

(define (cost g kind) (hash-ref (game1-costs g) kind))
(define (ap-left1 g) (- (game1-budget g) (game1-ap g)))
(define (affordable1? g kind) (<= (cost g kind) (ap-left1 g)))
(define (active1? g) (and (memq (game1-status g) '(playing accuse-only)) #t))

(define (spend! g kind)
  (unless (or (eq? (game1-status g) 'playing) (and (eq? kind 'accuse) (eq? (game1-status g) 'accuse-only)))
    (error 'engine-v1 "游戏已结束"))
  (unless (affordable1? g kind) (error 'engine-v1 "行动点不足：~a 需要 ~a，剩 ~a" kind (cost g kind) (ap-left1 g)))
  (set-game1-ap! g (+ (game1-ap g) (cost g kind))))

(define (end-check! g)
  (when (and (eq? (game1-status g) 'playing) (not (affordable1? g 'ask)) (not (affordable1? g 'present)))
    (set-game1-status! g (if (affordable1? g 'accuse) 'accuse-only 'lost))))

(define (log! g entry) (set-game1-log! g (cons (cons (game1-ap g) entry) (game1-log g))))

;; ---------------------------------------------------------------- 动作

;; 返回听到的声明列表（空表 = 不知道）
(define (ask1! g q)
  (spend! g 'ask)
  (hash-set! (game1-asked g) q #t)
  (define cls (question-claims g q))
  (define new (filter (λ (cl) (not (memq cl (game1-heard g)))) cls))
  (set-game1-heard! g (append (game1-heard g) new))
  (log! g (list 'ask q (map claim-id cls)))
  (end-check! g)
  cls)

(define (find-item g item)
  (or (for/first ([e (case1-evidence (game1-c g))] #:when (eq? (first e) item)) (third e))
      (for/first ([cl (game1-heard g)] #:when (eq? (claim-id cl) item)) cl)
      (error 'engine-v1 "未知证物或声明 ~a" item)))

;; 出示物的来源：物证按 id，声明按说话人
(define (source-of g item it)
  (if (eq? (claim-source it) 'evidence) item (claim-speaker it)))

;; 返回 (listof (list 此人说过的声明 冲突种类))
;; 冲突的是此人关于自己行踪的声明（(at 此人 ...)）且来源不是他自己时，记为一次拆穿
(define (present1! g who item)
  (spend! g 'present)
  (define c (game1-c g))
  (define m (case1-map c))
  (define it (find-item g item))
  (define ids (person-ids c))
  (define found
    (for*/list ([h (game1-heard g)] #:when (eq? (claim-speaker h) who) #:unless (eq? h it)
                [y (remove-duplicates (append (claim-subjects h ids) (claim-subjects it ids)))]
                [k (in-value (conflict-type m (case1-t0 c) (case1-t1 c) y it h))] #:when k)
      (list h k)))
  (define src (source-of g item it))
  (for ([f found])
    (define h (first f))
    (when (and (not (eq? src who))
               (match (claim-clause h) [(list 'at (== who) _ ...) #t] [_ #f])
               (not (member (list who h src) (game1-exposed g))))
      (set-game1-exposed! g (append (game1-exposed g) (list (list who h src))))))
  (log! g (list 'present who item (for/list ([f found]) (list (claim-id (first f)) (second f)))))
  (end-check! g)
  found)

;; 排除状态。accused：假定被指认的人（他的话不算）；#f 表示只去掉 y 自己的话
(define (excluded-now? g y #:accused [z #f])
  (define c (game1-c g))
  (define cls (append (filter (λ (cl) (not (memq (claim-speaker cl) (list y z)))) (game1-heard g))
                      (map third (case1-evidence c))))
  (excluded? (case1-map c) (case1-t0 c) (case1-t1 c) y cls (case1-crime c)))

;; 指认 z 时还挡着排除法的人：(listof (cons 人 原因))
;;   原因 self-only：听到的话里没有别人（不算他本人和 z）能约束他的；insufficient：有第三方证词，但不足以排除
;; 只在对局结束后给玩家看（局中不显示排除状态）
(define (accusation-gaps g z)
  (define c (game1-c g))
  (define m (case1-map c))
  (for/list ([y (person-ids c)]
             #:unless (eq? y z)
             #:unless (excluded-now? g y #:accused z))
    (define others
      (for/list ([cl (append (game1-heard g) (map third (case1-evidence c)))]
                 #:unless (memq (claim-speaker cl) (list y z))
                 #:when (pair? (constraints-on m cl y)))
        cl))
    (cons y (if (null? others) 'self-only 'insufficient))))

(define (can-win? g z)
  (for/and ([y (person-ids (game1-c g))] #:unless (eq? y z)) (excluded-now? g y #:accused z)))

;; z 的自身行踪说法被拆穿的不同来源
(define (exposure-sources g z)
  (remove-duplicates (for/list ([e (game1-exposed g)] #:when (eq? (first e) z)) (third e))))

(define (accuse1! g z)
  (spend! g 'accuse)
  (define ok (and (eq? z (case1-culprit (game1-c g)))
                  (or (>= (length (exposure-sources g z)) EXPOSE-TO-WIN) (can-win? g z))))
  (set-game1-status! g (if ok 'won 'lost))
  (log! g (list 'accuse z (game1-status g)))
  ok)
