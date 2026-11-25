#lang racket/base
;; v1 预算标定：公开信息侦探（排除 + 拆穿）在宽预算下所需的行动点分布，换算成各预算下的胜率。
;; 指认不耗行动点，侦探不按预算调整策略，所以"预算 B 下胜"≈"所需行动点 ≤ B"。
;; racket budget1.rkt [--seeds 100] [--npcs "4 5 6 8"] [--out runs/budget1-v1.rktd]
(require racket/list racket/cmdline racket/format racket/pretty
         "case1.rkt" "engine-v1.rkt" "liar-v1.rkt" "detective-v1.rkt" "gen-v1.rkt")

(define (quant xs q) (let ([v (sort xs <)]) (list-ref v (min (sub1 (length v)) (floor (* q (length v)))))))

(module+ main
  (define seeds (make-parameter 100))
  (define npcs (make-parameter "4 5 6 8"))
  (define out (make-parameter #f))
  (command-line #:once-each
                ["--seeds" s "每个人数的种子数" (seeds (string->number s))]
                ["--npcs" s "人数列表" (npcs s)]
                ["--out" f "输出 .rktd" (out f)])
  (define rows
    (for/list ([n (map string->number (regexp-split #rx" +" (npcs)))])
      (define t0 (current-inexact-milliseconds))
      (define needs
        (for/list ([s (in-range 1 (add1 (seeds)))])
          (define-values (d a _) (generate-case1 s #:npcs n))
          (define c (datum->case1 d))
          (for/list ([planner (list local-planner omniscient-planner)])
            (define g (new-game1 c #:planner planner #:budget 400))
            (if (eq? (detective-run! g) 'won) (game1-ap g) +inf.0))))
      (define loc (map first needs))
      (define omn (map second needs))
      (define (rate xs b) (/ (count (λ (x) (<= x b)) xs) (exact->inexact (length xs))))
      (printf "npcs ~a（~a 案）：\n" n (length needs))
      (for ([label '("局部" "全知")] [xs (list loc omn)])
        (printf "  侦探 vs ~a规划器：分位 p50 ~a  p60 ~a  p70 ~a  p80 ~a  p90 ~a；预算 30/40/50/60/80 下胜率 ~a\n"
                label (quant xs 1/2) (quant xs 6/10) (quant xs 7/10) (quant xs 8/10) (quant xs 9/10)
                (for/list ([b '(30 40 50 60 80)]) (~r (* 100 (rate xs b)) #:precision 0))))
      (printf "  用时 ~a s\n" (round (/ (- (current-inexact-milliseconds) t0) 1000)))
      (flush-output)
      (hasheq 'npcs n 'local loc 'omni omn)))
  (when (out) (with-output-to-file (out) #:exists 'replace (λ () (for ([r rows]) (write r) (newline))))))
