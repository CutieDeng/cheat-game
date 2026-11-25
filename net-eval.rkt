#lang racket/base
;; 网状案件批量评测：筛选代价、深度、最小割（静态）、动态消融、检查器轮数、启发式玩家破案率。
;; racket net-eval.rkt [--seeds N] [--players P] [--out 文件]
(require racket/list racket/cmdline racket/format racket/string
         "kb.rkt" "engine.rkt" "gen-net.rkt" "net.rkt" "solve.rkt" "eval.rkt")

(define CONFIGS
  ;; 名称 npcs slots layers 链长下限
  '((N4 4 4 2 0) (N5 5 4 2 0) (N6 6 5 3 0) (N8 8 5 3 0) (N5d1 5 4 2 1) (N6d2 6 5 3 2) (N8d2 8 5 3 2)))
(define (pct a b) (if (zero? b) "n/a" (string-append (~r (* 100. (/ a b)) #:precision 1) "%")))
(define (mean xs) (if (null? xs) "n/a" (~r (exact->inexact (/ (apply + xs) (length xs))) #:precision 1)))
(define (dist xs) (sort (hash->list (for/fold ([h (hash)]) ([x xs]) (hash-update h x add1 0))) < #:key car))

(module+ main
  (define seeds (make-parameter 20))
  (define players (make-parameter 5))
  (define out (make-parameter "runs/net-v1.rktd"))
  (command-line #:once-each
                ["--seeds" s "每个配置的案件数" (seeds (string->number s))]
                ["--players" s "每案启发式玩家局数" (players (string->number s))]
                ["--out" f "逐案记录" (out f)])
  (define o (open-output-file (out) #:exists 'replace))
  (printf "配置 | NPC/槽/层/链长下限 | 生成成功 | 平均尝试次数 | 链长分布 | 最小割分布 | 相关来源数 | 消融后仍可破案 | 所有单点删除都可破案的案件 | 检查器轮数 均值/最大 | 关系数/否认数/证物数 | 启发式玩家破案\n")
  (for ([cfg CONFIGS])
    (define-values (name n S L md) (apply values cfg))
    (define rs
      (for/list ([seed (in-range 1 (add1 (seeds)))])
        (define-values (d attempts m) (generate-net-case* seed #:npcs n #:slots S #:layers L #:min-depth md))
        (define r
          (cond
            [(not d) (hasheq 'ok #f 'attempts attempts)]
            [else
             (define cd (datum->case d))
             (define-values (alive total fails) (ablation cd))
             (define games (for/list ([p (in-range 1 (add1 (players)))]) (run-game cd (λ (_) rule-policy) p 0.2)))
             (hasheq 'ok #t 'seed seed 'attempts attempts
                     'depth (hash-ref m 'depth) 'min-cut (hash-ref m 'min-cut) 'sources total
                     'alive alive 'fails fails 'oracle-turns (hash-ref m 'oracle-turns)
                     'rels (length (case-data-rels cd))
                     'denials (apply + (map length (hash-values (case-data-rel-cover cd))))
                     'evidence (length (case-data-evidence cd))
                     'wins (count (λ (g) (eq? (hash-ref g 'status) 'won)) games))]))
        (write r o) (newline o)
        r))
    (define ok (filter (λ (r) (hash-ref r 'ok)) rs))
    (define (f k) (map (λ (r) (hash-ref r k)) ok))
    (printf "~a | ~a | ~a | ~a | ~s | ~s | ~a | ~a | ~a | ~a/~a | ~a/~a/~a | ~a\n"
            name (string-join (map ~a (cdr cfg)) "/")
            (pct (length ok) (length rs)) (mean (map (λ (r) (hash-ref r 'attempts)) rs))
            (dist (f 'depth)) (dist (f 'min-cut)) (mean (f 'sources))
            (pct (apply + (f 'alive)) (apply + (f 'sources)))
            (pct (count (λ (r) (null? (hash-ref r 'fails))) ok) (length ok))
            (mean (f 'oracle-turns)) (apply max 0 (f 'oracle-turns))
            (mean (f 'rels)) (mean (f 'denials)) (mean (f 'evidence))
            (pct (apply + (f 'wins)) (* (players) (length ok))))
    (define fl (append-map (λ (r) (hash-ref r 'fails)) ok))
    (unless (null? fl)
      (printf "  消融失败 ~a 次：~s\n" (length fl) (map (λ (f) (cdr f)) fl)))
    (flush-output))
  (close-output-port o))
