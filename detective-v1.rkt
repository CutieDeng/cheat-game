#lang racket/base
;; v1 侦探（spec-v1.md §7.1 第三条、§7.3）
;;
;; 全知检查器：知道每个问题的答案。从"所有非凶手答得出的问题"出发，逐个尝试删掉问题，
;;   删掉后排除法仍成立就删；剩下的问题数就是它需要的行动点（近似最少，不保证最优）。
;; 公开信息侦探：不知道谁是凶手，只用听到的声明和物证。
;;   目标：未排除的人（只去掉本人的话）。只剩一人 z 时：能赢就指认；不能赢说明有人只靠 z 的话才被排除，
;;   目标改为"去掉 z 的话后仍未排除的人"。
;;   提问顺序（按阶段，每阶段遍历所有目标，取第一个没问过的问题；时段只取案发时段附近）：
;;     A 目标自己在哪；A2 其他每个人自己在哪；
;;     B 自称与目标在同一地点的人：问他"那时在该地见过谁"；
;;     C 问其他每个人"那时见过目标吗"；
;;     D 问其他每个人"那时在目标自称的地点见过谁"；
;;     E 问每个人"那时在你自称的地点见过谁"；
;;     F 兜底：其余所有问题（案发时段附近的优先）。
;;   B、D 跟着目标的自称地点走：凶手的谎从这里影响行动点消耗；
;;   凶手关于别人的话也会让侦探误以为某人已被排除（指认时这些话不算），之后要补问。
;;   拆穿（expose? 为真时）：每轮先检查听到的话里，某人关于自己行踪的说法是否与别人的证词或物证冲突，
;;   有就把冲突的那条出示给他（2 点）；某人被 ≥ EXPOSE-TO-WIN 个不同来源拆穿就指认他（可能指认错：诚实者也可能被两人记错）。
(require racket/list racket/match racket/format
         "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt" "exclude.rkt" "engine-v1.rkt" "liar-v1.rkt")
(provide oracle-questions detective-run! crime-slots)

(define (crime-slots g)
  (match-define (list _ lo hi _) (case1-crime (game1-c g)))
  (for/list ([s (game1-slots g)] [i (in-naturals)]
             #:when (and (<= (car s) (+ hi 15)) (<= (- lo 15) (cdr s))))
    i))

;; ---------------------------------------------------------------- 全知检查器

(define (oracle-questions g)
  (define c (game1-c g))
  (define m (case1-map c))
  (define culprit (case1-culprit c))
  (define ids (person-ids c))
  (define ev (map third (case1-evidence c)))
  (define qs
    (for/list ([q (all-questions g)]
               #:unless (eq? (second q) culprit)
               #:when (pair? (question-claims g q)))
      q))
  (define answers (for/hash ([q qs]) (values q (question-claims g q))))
  (define (claims-of qset) (remove-duplicates (append* (for/list ([q qset]) (hash-ref answers q)))))
  (define (excl? y cls)
    (excluded? m (case1-t0 c) (case1-t1 c) y
               (append (filter (λ (cl) (not (memq (claim-speaker cl) (list y culprit)))) cls) ev)
               (case1-crime c)))
  (define innocents (remq culprit ids))
  (unless (for/and ([y innocents]) (excl? y (claims-of qs)))
    (error 'oracle "全部问题问完仍不能排除所有非凶手"))
  ;; 删掉问题 q 只可能影响与 q 的回答有关的人
  (define (affects q)
    (for/list ([y innocents]
               #:when (for/or ([cl (hash-ref answers q)])
                        (and (not (memq (claim-speaker cl) (list y culprit))) (pair? (constraints-on m cl y)))))
      y))
  (let loop ([keep qs] [todo qs])
    (cond
      [(null? todo) keep]
      [else
       (define q (car todo))
       (define without (remove q keep))
       (define cls (claims-of without))
       (if (for/and ([y (affects q)]) (excl? y cls))
           (loop without (cdr todo))
           (loop keep (cdr todo)))])))

;; ---------------------------------------------------------------- 公开信息侦探

(define (c-span cs)   ; 约束列表的时间范围
  (for/fold ([lo +inf.0] [hi -inf.0]) ([k cs])
    (define-values (a b)
      (match k [(in-c _ a b) (values a b)] [(out-c _ a b) (values a b)]
             [(visit-c _ a b) (values a b)] [(nocross-c _ a b) (values a b)]))
    (values (min lo a) (max hi b))))

(define (detective-run! g #:expose? [expose? #t])
  (define c (game1-c g))
  (define m (case1-map c))
  (define ids (person-ids c))
  (define cs (crime-slots g))
  (define checked (make-hash))   ; (cons 被拆穿声明 出示物) -> 冲突种类或 #f
  (define presented (make-hash)) ; (cons 人 出示物 id) -> #t
  (define evidence (map third (case1-evidence c)))
  ;; 找一条可出示的矛盾：(cons 人 出示物 id)
  (define (find-contradiction)
    (for*/first ([h (game1-heard g)]
                 [k (in-value (claim-clause h))]
                 #:when (and (eq? (first k) 'at) (eq? (second k) (claim-speaker h)))
                 [p (in-value (claim-speaker h))]
                 #:when (< (length (exposure-sources g p)) EXPOSE-TO-WIN)
                 [it (append evidence (game1-heard g))]
                 #:unless (eq? (claim-speaker it) p)
                 #:unless (hash-ref presented (cons p (claim-id it)) #f)
                 #:unless (member (if (eq? (claim-source it) 'evidence) (claim-id it) (claim-speaker it))
                                  (exposure-sources g p))
                 #:when (let ([a (constraints-on m h p)] [b (constraints-on m it p)])
                          (and (pair? a) (pair? b)
                               (let-values ([(l1 h1) (c-span a)] [(l2 h2) (c-span b)])
                                 (and (<= l1 (+ h2 20)) (<= l2 (+ h1 20))))))
                 #:when (hash-ref! checked (cons h it)
                                   (λ () (conflict-type m (case1-t0 c) (case1-t1 c) p it h))))
      (cons p (claim-id it))))
  (define (asked? q) (hash-ref (game1-asked g) q #f))
  ;; 某人自称所在的地点（从听到的他自己的声明）
  (define (claimed y)
    (remove-duplicates
     (for*/list ([cl (game1-heard g)]
                 #:when (eq? (claim-speaker cl) y)
                 [k (in-value (claim-clause cl))]
                 #:when (and (eq? (first k) 'at) (eq? (second k) y)))
       (third k))))
  (define (candidates targets)
    (append
     (for*/list ([y targets] [s cs]) (list 'self y s))
     (for*/list ([p ids] [s cs]) (list 'self p s))
     (for*/list ([y targets] [n (claimed y)] [p (remq y ids)] #:when (memq n (claimed p)) [s cs]) (list 'place p n s))
     (for*/list ([y targets] [p (remq y ids)] [s cs]) (list 'about p y s))
     (for*/list ([y targets] [n (claimed y)] [p (remq y ids)] [s cs]) (list 'place p n s))
     (for*/list ([p ids] [n (claimed p)] [s cs]) (list 'place p n s))
     ;; F 兜底：其余所有问题（案发时段附近的优先）。生成器只保证"问遍所有诚实问题"时可破案
     (let ([all (all-questions g)])
       (append (filter (λ (q) (memv (last q) cs)) all)
               (filter (λ (q) (not (memv (last q) cs))) all)))))
  (let loop ()
    (when (active1? g)
      (define open (filter (λ (y) (not (excluded-now? g y))) ids))
      (define z (and (= (length open) 1) (car open)))
      (define exposed-enough
        (and expose? (for/first ([y ids] #:when (>= (length (exposure-sources g y)) EXPOSE-TO-WIN)) y)))
      (define contra (and expose? (affordable1? g 'present) (find-contradiction)))
      (cond
        [(and z (can-win? g z)) (accuse1! g z)]
        [exposed-enough (accuse1! g exposed-enough)]
        [contra
         (hash-set! presented contra #t)
         (present1! g (car contra) (cdr contra))
         (loop)]
        [(eq? (game1-status g) 'accuse-only)
         (accuse1! g (argmax (λ (y) (length (exposure-sources g y))) open))]
        [else
         (define targets
           (if z
               (filter (λ (y) (and (not (eq? y z)) (not (excluded-now? g y #:accused z)))) ids)
               open))
         (define next (findf (λ (q) (not (asked? q))) (candidates targets)))
         (cond
           [next (ask1! g next) (loop)]
           [else (accuse1! g (car open))])])))
  (game1-status g))

;; ---------------------------------------------------------------- 批量评测

(module+ main
  (require racket/cmdline "gen-v1.rkt")
  (define seeds (make-parameter 20))
  (define npcs (make-parameter "4 5 6 8"))
  (define big (make-parameter 400))
  (command-line #:once-each
                ["--seeds" s "每个人数的种子数" (seeds (string->number s))]
                ["--npcs" s "人数列表" (npcs s)]
                ["--big" b "侦探评测用的宽预算" (big (string->number b))]
                ["--p-join" p "凶手与别人同住的概率（生成器参数）" (p-culprit-joins (string->number p))])
  (define (mean xs) (if (null? xs) +nan.0 (/ (apply + xs) (exact->inexact (length xs)))))
  (define (quant xs q) (let ([v (sort xs <)]) (list-ref v (min (sub1 (length v)) (floor (* q (length v)))))))
  (for ([n (map string->number (regexp-split #rx" +" (npcs)))])
    (define t0 (current-inexact-milliseconds))
    (define rows
      (for/list ([s (in-range 1 (add1 (seeds)))])
        (define-values (d a _) (generate-case1 s #:npcs n))
        (define c (datum->case1 d))
        (define oracle-ap (length (oracle-questions (new-game1 c #:budget (big)))))
        (define (det planner expose?)
          (define g (new-game1 c #:planner planner #:budget (big)))
          (define st (detective-run! g #:expose? expose?))
          (define accused (for/first ([e (game1-log g)] #:when (eq? (car (cdr e)) 'accuse)) (cadr (cdr e))))
          (hasheq 'status st 'ap (game1-ap g)
                  'route (cond [(not (eq? st 'won)) #f]
                               [(>= (length (exposure-sources g accused)) EXPOSE-TO-WIN) 'expose]
                               [else 'exclude])))
        (list oracle-ap
              (det local-planner #f) (det omniscient-planner #f)
              (det local-planner #t) (det omniscient-planner #t))))
    (define o (map first rows))
    (printf "npcs ~a（~a 案，默认预算 ~a）：\n" n (length rows) (budget-for-npcs n))
    (printf "  全知检查器所需 AP：均值 ~a，中位 ~a，p90 ~a，最大 ~a\n"
            (~r (mean o) #:precision 1) (quant o 1/2) (quant o 9/10) (apply max o))
    (for ([k '(1 2 3 4)] [label '("只排除 vs 局部" "只排除 vs 全知" "排除+拆穿 vs 局部" "排除+拆穿 vs 全知")])
      (define rs (map (λ (r) (list-ref r k)) rows))
      (define won (filter (λ (r) (eq? 'won (hash-ref r 'status))) rs))
      (define aps (map (λ (r) (hash-ref r 'ap)) won))
      (printf "  ~a：胜 ~a/~a（拆穿路线 ~a），AP 均值 ~a，中位 ~a，p90 ~a\n"
              label (length won) (length rs) (count (λ (r) (eq? 'expose (hash-ref r 'route))) won)
              (~r (mean aps) #:precision 1)
              (if (null? aps) "-" (quant aps 1/2)) (if (null? aps) "-" (quant aps 9/10))))
    (printf "  逐案 AP 差（排除+拆穿，全知 − 局部）：~a\n"
            (for/list ([r rows]) (- (hash-ref (list-ref r 4) 'ap) (hash-ref (list-ref r 3) 'ap))))
    (printf "  用时 ~a s\n" (round (/ (- (current-inexact-milliseconds) t0) 1000)))
    (flush-output)))
