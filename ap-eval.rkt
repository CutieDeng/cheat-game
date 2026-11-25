#lang racket/base
;; 行动点倍率标定。固定提问 1、指认 0，扫描出示倍率与预算（案件回合上限 L 的倍数），比较四种玩家的破案率：
;;   oracle     检查器，规则 NPC（读取完整案情，近似最优路径）
;;   oracle-adv 检查器 + 卡住后追问，对抗 NPC（用不可拆穿的编造）
;;   heur       启发式玩家（只用公开信息，出示前检查与对方说法是否矛盾），规则 NPC，每案 5 个玩家种子
;;   brute      穷举玩家（只用公开信息，不做矛盾判断）：先问遍每个人自己每个时段的行踪，
;;              再把每件证物和每条听到的说法依次出示给每个人（自己的说法除外），再问其余话题，规则 NPC
;; 目标：oracle 接近 100%，brute 明显低于 heur。
;; racket ap-eval.rkt [--seeds N] [--out 文件]
(require racket/list racket/cmdline racket/format racket/pretty racket/math
         "kb.rkt" "engine.rkt" "eval.rkt" "solve.rkt" "gen.rkt" "gen-net.rkt")

(define GEN '((B 4 4 2 1) (C 5 4 3 1) (D 6 5 3 2) (E 8 5 3 3)))
(define NET '((N5 5 4 2 0) (N6 6 5 3 0) (N8 8 5 3 0) (N5d1 5 4 2 1) (N6d2 6 5 3 2)))

;; 每个配置先跑一行按轮计数（UNIT-COSTS，全部为 1，预算 L）作对照
(define PRESENT '(1 3/2 2 3 4))
(define FACTORS '(1 5/4 3/2 2))

(define (costs-for pc) (hasheq 'ask 1 'present pc 'accuse 0))

;; ---------------------------------------------------------------- 穷举玩家
(define (brute-run cd costs budget)
  (define g (new-game cd #:seed 1 #:costs costs #:budget budget))
  (define ps (pstate (make-hash) (make-hasheq) (make-pseudo-random-generator) 0))
  (define npcs (case-data-npcs cd))
  (define crime (case-data-crime cd))
  (define nslots (vector-length (case-data-slots cd)))
  (define topics
    (append (for*/list ([n npcs] [s (in-range nslots)]) (list n n s))
            (for*/list ([n npcs] [o npcs] #:unless (eq? n o) [s (in-range nslots)]) (list n o s))
            (if (null? (case-data-rels cd)) '() (for*/list ([n npcs] [o npcs]) (list n o 'rel)))))
  (define pressed 0)
  (define (next-move)
    (define top (argmax (λ (n) (hash-ref (pstate-score ps) n 0)) npcs))
    (define self-placed
      (for/first ([c (heard g)]
                  #:when (let ([p (claim-prop c)])
                           (and (st? (prop-st p)) (eq? (prop-pol p) 'pos) (eq? (prop-mode p) 'exact)
                                (eq? (st-agent (prop-st p)) (claim-speaker c))
                                (eq? (st-loc (prop-st p)) (st-loc crime))
                                (overlap? (prop-st p) crime))))
        (claim-speaker c)))
    (define items (append (map first (case-data-evidence cd)) (map claim-id (heard g))))
    (cond
      [self-placed (list 'accuse self-placed)]
      [(>= (hash-ref (pstate-score ps) top 0) LIES-TO-WIN) (list 'accuse top)]
      [(eq? (game-status g) 'accuse-only) (list 'accuse top)]
      [(< (- (ap-left g) (cost g 'ask)) (cost g 'accuse)) (list 'accuse top)]
      [(findf (λ (t) (and (eq? (first t) (second t)) (zero? (hash-ref (game-asked g) t 0)) (affordable? g 'ask (first t)))) topics)
       => (λ (t) (cons 'ask t))]
      [(and (affordable? g 'present)
            (for*/first ([it items] [n npcs]
                         #:when (affordable? g 'present n)
                         #:unless (hash-ref (pstate-presented ps) (cons n it) #f)
                         #:unless (and (number? it) (eq? (claim-speaker (find-claim g it)) n)))
              (list 'present n it)))
       => values]
      [(findf (λ (t) (and (zero? (hash-ref (game-asked g) t 0)) (affordable? g 'ask (first t)))) topics) => (λ (t) (cons 'ask t))]
      [(not (affordable? g 'ask top)) (list 'accuse top)]
      [else (set! pressed (add1 pressed)) (list 'ask top top (modulo pressed nslots))]))
  (let loop ()
    (when (active? g) (apply-move! g ps (next-move)) (loop)))
  (hasheq 'status (game-status g) 'ap (game-ap g)
          'present-ap (for/sum ([h (game-history g)]) (if (eq? (first (third h)) 'present) (cost g 'present) 0))))

;; ---------------------------------------------------------------- 汇总
(define (won? s) (eq? s 'won))

;; 所需行动点：预算放宽到 BIG × L，记录获胜局用掉的 AP / L；未获胜记为 +inf.0。
;; 超过 BIG × L 的需求与预算选择（≤ 2L）无关，记为 ∞。预算很大时对质阶段（2/3 预算处）实际不会到来，所以这里的数是"不靠对质加成"的需求。
(define BIG (make-parameter 6))
(define (needs cds pc)
  (define costs (costs-for pc))
  (define (B cd) (* (BIG) (case-data-turn-limit cd)))
  (define (need r cd) (if (won? (hash-ref r 'status)) (/ (hash-ref r 'ap) (case-data-turn-limit cd)) +inf.0))
  (define bs (for/list ([cd cds]) (brute-run cd costs (B cd))))
  (hasheq 'oracle (for/list ([cd cds]) (need (oracle-solve cd #:costs costs #:budget (B cd)) cd))
          'oracle-adv (for/list ([cd cds]) (need (adversarial-solve cd #:costs costs #:budget (B cd)) cd))
          'heur (for*/list ([cd cds] [p (in-range 1 6)])
                  (need (run-game cd (λ (_) rule-policy) p 0.2 #:costs costs #:budget (B cd)) cd))
          'brute (for/list ([r bs] [cd cds]) (need r cd))
          'brute-present-share (let ([w (filter (λ (r) (won? (hash-ref r 'status))) bs)])
                                 (if (null? w) #f
                                     (/ (for/sum ([r w]) (/ (hash-ref r 'present-ap) (hash-ref r 'ap)))
                                        (exact->inexact (length w)))))))
(define (quant xs q)
  (define v (sort xs <))
  (list-ref v (min (sub1 (length v)) (inexact->exact (floor (* q (length v)))))))
(define (within xs f) (/ (count (λ (x) (<= x f)) xs) (exact->inexact (length xs))))
(define (fmt-need xs)
  (define (q x) (if (infinite? x) "∞" (~r (exact->inexact x) #:precision 2)))
  (format "中位 ~a  p90 ~a  ≤1L ~a%  ≤1.5L ~a%  ≤2L ~a%"
          (q (quant xs 1/2)) (q (quant xs 9/10))
          (pct (within xs 1)) (pct (within xs 3/2)) (pct (within xs 2))))

(define (cell cds pc f)
  (define costs (costs-for pc))
  (define (B cd) (ceiling (* f (case-data-turn-limit cd))))
  (define o (for/list ([cd cds]) (oracle-solve cd #:costs costs #:budget (B cd))))
  (define oa (for/list ([cd cds]) (adversarial-solve cd #:costs costs #:budget (B cd))))
  (define h (for*/list ([cd cds] [p (in-range 1 6)])
              (hash-ref (run-game cd (λ (_) rule-policy) p 0.2 #:costs costs #:budget (B cd)) 'status)))
  (define b (for/list ([cd cds]) (hash-ref (brute-run cd costs (B cd)) 'status)))
  (define (rate xs) (/ (count won? xs) (exact->inexact (length xs))))
  (define ow (filter (λ (r) (won? (hash-ref r 'status))) o))
  (hasheq 'present pc 'factor f
          'oracle (rate (map (λ (r) (hash-ref r 'status)) o))
          'oracle-adv (rate (map (λ (r) (hash-ref r 'status)) oa))
          'heur (rate h) 'brute (rate b)
          ;; 检查器获胜局用掉的行动点占预算的比例
          'oracle-ap-frac (if (null? ow) #f
                              (/ (for/sum ([r ow] [cd cds] #:when (won? (hash-ref r 'status)))
                                   (/ (hash-ref r 'ap) (B cd)))
                                 (exact->inexact (length ow))))))

(define (pct x) (~r (* 100 x) #:precision 0))

(module+ main
  (define seeds (make-parameter 20))
  (define out (make-parameter #f))
  (command-line #:once-each
                ["--seeds" s "每个配置的种子数" (seeds (string->number s))]
                ["--out" f "输出 .rktd" (out f)]
                ["--big" b "测所需 AP 时的预算倍数（缺省 6）" (BIG (string->number b))])
  (define configs
    (append
     (for/list ([c GEN])
       (define-values (name n S L K) (apply values c))
       (cons name (for*/list ([s (in-range 1 (add1 (seeds)))]
                              [d (in-value (let-values ([(d a) (generate-case* s #:npcs n #:slots S #:layers L #:secrets K)]) d))]
                              #:when d)
                    (datum->case d))))
     (for/list ([c NET])
       (define-values (name n S L md) (apply values c))
       (cons name (for*/list ([s (in-range 1 (add1 (seeds)))]
                              [d (in-value (let-values ([(d a m) (generate-net-case* s #:npcs n #:slots S #:layers L #:min-depth md)]) d))]
                              #:when d)
                    (datum->case d))))))
  (define rows
    (for/list ([c configs])
      (define name (car c))
      (define cds (cdr c))
      (define t0 (current-inexact-milliseconds))
      (printf "\n== ~a：~a 个案件，L = ~a ==\n" name (length cds) (remove-duplicates (map case-data-turn-limit cds)))
      (printf "出示  预算    oracle  oracle-adv  heur  brute  oracle用掉AP\n")
      ;; 现行：全部为 1，预算 L
      (define base
        (let* ([o (for/list ([cd cds]) (hash-ref (oracle-solve cd #:costs UNIT-COSTS) 'status))]
               [oa (for/list ([cd cds]) (hash-ref (adversarial-solve cd #:costs UNIT-COSTS) 'status))]
               [h (for*/list ([cd cds] [p (in-range 1 6)]) (hash-ref (run-game cd (λ (_) rule-policy) p 0.2 #:costs UNIT-COSTS) 'status))]
               [b (for/list ([cd cds]) (hash-ref (brute-run cd UNIT-COSTS (case-data-turn-limit cd)) 'status))]
               [rate (λ (xs) (/ (count won? xs) (exact->inexact (length xs))))])
          (hasheq 'present 'baseline 'factor 1 'oracle (rate o) 'oracle-adv (rate oa) 'heur (rate h) 'brute (rate b))))
      (printf "现行（全 1）1L   ~a%    ~a%        ~a%   ~a%\n"
              (pct (hash-ref base 'oracle)) (pct (hash-ref base 'oracle-adv)) (pct (hash-ref base 'heur)) (pct (hash-ref base 'brute)))
      (flush-output)
      (define need-rows
        (for/list ([pc PRESENT])
          (define r (needs cds pc))
          (printf "所需 AP / L（出示 ~a，预算 ~aL）：\n" pc (BIG))
          (for ([k '(oracle oracle-adv heur brute)])
            (printf "  ~a ~a\n" (~a k #:min-width 10) (fmt-need (hash-ref r k))))
          (printf "  brute 获胜局中出示占 AP 的比例：~a\n"
                  (let ([x (hash-ref r 'brute-present-share)]) (if x (~r x #:precision 2) "n/a")))
          (flush-output)
          (hash-set r 'present pc)))
      (define cells
        (for*/list ([pc PRESENT] [f FACTORS])
          (define r (cell cds pc f))
          (printf "~a    ~aL   ~a%    ~a%        ~a%   ~a%   ~a\n"
                  (~a pc #:min-width 3) (~a (exact->inexact f) #:min-width 4)
                  (pct (hash-ref r 'oracle)) (pct (hash-ref r 'oracle-adv)) (pct (hash-ref r 'heur)) (pct (hash-ref r 'brute))
                  (let ([x (hash-ref r 'oracle-ap-frac)]) (if x (~r x #:precision 2) "n/a")))
          (flush-output)
          r))
      (printf "（本配置用时 ~a s）\n" (round (/ (- (current-inexact-milliseconds) t0) 1000)))
      (hasheq 'config name 'cases (length cds) 'baseline base 'needs need-rows 'cells cells)))
  (when (out) (with-output-to-file (out) #:exists 'replace (λ () (for ([r rows]) (write r) (newline))))))
