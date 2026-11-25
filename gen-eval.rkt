#lang racket/base
;; 批量生成案件并评测：静态校验、检查器可解性、启发式玩家（规则策略 NPC）破案率、决策点构成。
;; racket gen-eval.rkt [--seeds N] [--players P] [--out 文件]
(require racket/list racket/cmdline racket/format racket/string
         "kb.rkt" "engine.rkt" "gen.rkt" "solve.rkt" "eval.rkt")

(define CONFIGS
  ;; 名称 npcs slots layers secrets
  '((A 3 3 2 0) (B 4 4 2 1) (C 5 4 3 1) (D 6 5 3 2) (E 8 5 3 3)))

(define (mean xs) (if (null? xs) +nan.0 (exact->inexact (/ (apply + xs) (length xs)))))
(define (f1 x) (~r x #:precision 1))
(define (pct a b) (if (zero? b) "n/a" (string-append (~r (* 100. (/ a b)) #:precision 1) "%")))

(define (eval-one cfg seed players)
  (define-values (name n S L K) (apply values cfg))
  (define d (generate-case seed #:npcs n #:slots S #:layers L #:secrets K))
  (define cd (datum->case d))
  (define errs (validate-case cd))
  (define o (oracle-solve cd))
  (define games (for/list ([p (in-range 1 (add1 players))]) (run-game cd (λ (_) rule-policy) p 0.2)))
  (define ds (append-map (λ (r) (hash-ref r 'decisions)) games))
  (hasheq 'config name 'seed seed 'errors errs
          'knowledge (length (case-data-knowledge cd)) 'critical (count kitem-critical? (case-data-knowledge cd))
          'liars (hash-count (case-data-cover cd)) 'evidence (length (case-data-evidence cd))
          'oracle-status (hash-ref o 'status) 'oracle-turns (hash-ref o 'turns) 'view-max (hash-ref o 'view-max)
          'player-wins (count (λ (r) (eq? (hash-ref r 'status) 'won)) games) 'players players
          'decisions (length ds)
          'exposing-points (count (λ (x) (hash-ref x 'exposing-available)) ds)
          'option-counts (map (λ (x) (hash-ref x 'n)) ds)))

(module+ main
  (define seeds (make-parameter 30))
  (define players (make-parameter 5))
  (define out (make-parameter "runs/gen-v1.rktd"))
  (command-line
   #:once-each
   ["--seeds" s "每个配置的案件数" (seeds (string->number s))]
   ["--players" s "每个案件的启发式玩家局数" (players (string->number s))]
   ["--out" f "逐案记录" (out f)])
  (define o (open-output-file (out) #:exists 'replace))
  (printf "配置 | NPC/槽/层/干扰 | 校验通过 | 检查器破案 | 检查器轮数 均值/最大 | 视图最大 token | 观察/critical | 启发式玩家破案 | 每局含暴露选项的决策点 | 选项数分布(1/2/3/4+)\n")
  (for ([cfg CONFIGS])
    (define rs (for/list ([s (in-range 1 (add1 (seeds)))])
                 (define r (eval-one cfg s (players)))
                 (write r o) (newline o)
                 r))
    (define n (length rs))
    (define won (filter (λ (r) (eq? (hash-ref r 'oracle-status) 'won)) rs))
    (define opts (append-map (λ (r) (hash-ref r 'option-counts)) rs))
    (define games (* n (players)))
    (printf "~a | ~a | ~a | ~a | ~a/~a | ~a | ~a/~a | ~a | ~a | ~a\n"
            (car cfg) (string-join (map ~a (cdr cfg)) "/")
            (pct (count (λ (r) (null? (hash-ref r 'errors))) rs) n)
            (pct (length won) n)
            (f1 (mean (map (λ (r) (hash-ref r 'oracle-turns)) won)))
            (apply max 0 (map (λ (r) (hash-ref r 'oracle-turns)) won))
            (apply max 0 (map (λ (r) (hash-ref r 'view-max)) rs))
            (f1 (mean (map (λ (r) (hash-ref r 'knowledge)) rs)))
            (f1 (mean (map (λ (r) (hash-ref r 'critical)) rs)))
            (pct (apply + (map (λ (r) (hash-ref r 'player-wins)) rs)) games)
            (f1 (/ (apply + (map (λ (r) (hash-ref r 'exposing-points)) rs)) games))
            (string-join (for/list ([k '(1 2 3)]) (pct (count (λ (x) (= x k)) opts) (length opts))) "/"))
    (for ([r rs] #:unless (and (null? (hash-ref r 'errors)) (eq? (hash-ref r 'oracle-status) 'won)))
      (printf "  失败：seed ~a 校验 ~s 检查器 ~a（~a 轮）\n" (hash-ref r 'seed) (hash-ref r 'errors)
              (hash-ref r 'oracle-status) (hash-ref r 'oracle-turns)))
    (flush-output))
  (close-output-port o))
