#lang racket/base
;; 扩展动作空间的统计：生成案件 × 启发式玩家 × 随机合法策略 NPC。
;; 检查引擎在扩展动作下不出错，并统计选项数、编造选项中可被拆穿的比例。
;; racket ext-stats.rkt [--seeds N] [--players P]
(require racket/list racket/cmdline racket/format racket/string
         "kb.rkt" "engine.rkt" "gen.rkt" "eval.rkt")

(define CONFIGS '((A 3 3 2 0) (B 4 4 2 1) (C 5 4 3 1) (D 6 5 3 2) (E 8 5 3 3)))
(define (pct a b) (if (zero? b) "n/a" (string-append (~r (* 100. (/ a b)) #:precision 1) "%")))
(define (mean xs) (if (null? xs) "n/a" (~r (exact->inexact (/ (apply + xs) (length xs))) #:precision 1)))

(define (random-policy seed)
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed (+ 7 seed)))
  (λ (view actions) (list-ref actions (random (length actions) rng))))

(module+ main
  (define seeds (make-parameter 30))
  (define players (make-parameter 5))
  (command-line #:once-each
                ["--seeds" s "每个配置的案件数" (seeds (string->number s))]
                ["--players" s "每案局数" (players (string->number s))])
  (printf "配置 | 选项数 均值/最大 | 含编造选项的决策点 | 编造选项可被拆穿 | 随机 NPC：玩家破案 | 规则 NPC：玩家破案 | 规则 NPC 与关闭扩展时结果相同\n")
  (for ([cfg CONFIGS])
    (define-values (name n S L K) (apply values cfg))
    (define rows
      (for*/list ([cs (in-range 1 (add1 (seeds)))] [ps (in-range 1 (add1 (players)))])
        (define cd (datum->case (generate-case cs #:npcs n #:slots S #:layers L #:secrets K)))
        (define rnd (run-game cd random-policy ps 0.2 #:ext? #t))
        (define rule-ext (run-game cd (λ (_) rule-policy) ps 0.2 #:ext? #t))
        (define rule-base (run-game cd (λ (_) rule-policy) ps 0.2))
        (list rnd rule-ext rule-base)))
    (define ds (append-map (λ (r) (hash-ref (first r) 'decisions)) rows))
    (define free (filter (λ (d) (> (hash-ref d 'n) 1)) ds))
    (define fab-points (filter (λ (d) (for/or ([k (hash-ref d 'kinds)]) (memq k '(fabricate frame)))) free))
    (define fab-refs (for*/list ([d fab-points] [k (hash-ref d 'kinds)] [r (hash-ref d 'refutable)]
                                 #:when (memq k '(fabricate frame)))
                       r))
    (define (wins i) (count (λ (r) (eq? (hash-ref (list-ref r i) 'status) 'won)) rows))
    (printf "~a | ~a/~a | ~a | ~a | ~a | ~a | ~a\n"
            name (mean (map (λ (d) (hash-ref d 'n)) free)) (apply max 0 (map (λ (d) (hash-ref d 'n)) free))
            (pct (length fab-points) (length free))
            (pct (count values fab-refs) (length fab-refs))
            (pct (wins 0) (length rows)) (pct (wins 1) (length rows))
            (pct (count (λ (r) (equal? (hash-ref (second r) 'moves) (hash-ref (third r) 'moves))) rows) (length rows)))
    (flush-output)))
