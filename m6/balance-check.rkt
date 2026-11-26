#lang racket/base
;; v1 平衡验证：公开信息侦探在真实对局预算（engine-v1.rkt default-budget1）下，对 4 种凶手各下一局。
;;   racket m6/balance-check.rkt --strong m6/big-ckpt-rl3.jsonl --weak m6/big-ckpt-v2.jsonl [--shard i --of n] --out x.rktd
;;   racket m6/balance-check.rkt --from x.rktd
;; 凶手：强 = 第一个采样文件的贪心故事，弱 = 第二个文件的贪心故事，局部规划器，全知规划器。
;; 与 m6/budget-sweep.rkt 的离线重算互相校对（离线版用宽预算 400 下的所需 AP）。
(require racket/list racket/match racket/cmdline racket/format json
         "../world.rkt" "../case1.rkt" "../gen-v1.rkt" "../engine-v1.rkt" "../liar-v1.rkt" "../detective-v1.rkt")

(define (rel->path c st)
  (path (case1-culprit c)
        (for/list ([s st]) (match s [(list n a b) (seg (string->symbol n) (+ a (case1-t0 c)) (+ b (case1-t0 c)))]))))

(define (play c story)
  (define g (new-game1 c #:planner (λ (_) story)))     ; 缺省预算 = budget-for-npcs
  (define st (detective-run! g))
  (define z (case1-culprit c))
  (hasheq 'won (eq? st 'won) 'ap (game1-ap g)
          'expo (>= (length (exposure-sources g z)) EXPOSE-TO-WIN) 'excl (and (can-win? g z) #t)))

(define (read-rows file) (with-input-from-file file (λ () (for/list ([x (in-port read)]) x))))

(define (summary rows)
  (define (pct a b) (format "~a%" (~r (* 100. (/ a (max b 1))) #:precision 0)))
  (for ([n (sort (remove-duplicates (map (λ (r) (hash-ref r 'npcs)) rows)) <)])
    (define rs (filter (λ (r) (= n (hash-ref r 'npcs))) rows))
    (define N (length rs))
    (define (det who) (count (λ (r) (hash-ref (hash-ref r who) 'won)) rs))
    (define (expo who) (count (λ (r) (let ([x (hash-ref r who)]) (and (hash-ref x 'won) (hash-ref x 'expo) (not (hash-ref x 'excl))))) rs))
    (printf "~a 人（预算 ~a，~a 案）：侦探胜率 vs 强 ~a，弱 ~a，局部 ~a，全知 ~a；凶手胜率差（强 − 局部）~a 个百分点\n"
            n (budget-for-npcs n) N (pct (det 'strong) N) (pct (det 'weak) N) (pct (det 'local) N) (pct (det 'omni) N)
            (~r (* 100. (/ (- (det 'local) (det 'strong)) N)) #:precision 0))
    (printf "  侦探只靠拆穿破案（排除不成立）：vs 强 ~a，弱 ~a，局部 ~a，全知 ~a\n"
            (pct (expo 'strong) N) (pct (expo 'weak) N) (pct (expo 'local) N) (pct (expo 'omni) N))))

(module+ main
  (define strong (make-parameter #f)) (define weak (make-parameter #f))
  (define shard (make-parameter 0)) (define of (make-parameter 1))
  (define out (make-parameter #f)) (define from (make-parameter #f))
  (command-line #:once-each
                ["--strong" f "强凶手采样 JSONL" (strong f)]
                ["--weak" f "弱凶手采样 JSONL" (weak f)]
                ["--shard" i "分片下标" (shard (string->number i))]
                ["--of" k "分片数" (of (string->number k))]
                ["--out" f "逐案结果" (out f)]
                ["--from" f "只汇总" (from f)])
  (cond
    [(from) (summary (read-rows (from)))]
    [else
     (define (lines f) (for/list ([l (in-lines (open-input-file f))] [i (in-naturals)] #:when (= (modulo i (of)) (shard)))
                         (string->jsexpr l)))
     (define rows
       (for/list ([js (lines (strong))] [jw (lines (weak))])
         (unless (and (= (hash-ref js 'npcs) (hash-ref jw 'npcs)) (= (hash-ref js 'seed) (hash-ref jw 'seed)))
           (error 'balance "两个采样文件的案件顺序不一致"))
         (define n (hash-ref js 'npcs)) (define s (hash-ref js 'seed))
         (define-values (d a e) (generate-case1 s #:npcs n))
         (define c (datum->case1 d))
         (hasheq 'npcs n 'seed s
                 'strong (play c (rel->path c (hash-ref js 'greedy)))
                 'weak (play c (rel->path c (hash-ref jw 'greedy)))
                 'local (play c (local-planner c))
                 'omni (play c (omniscient-planner c)))))
     (with-output-to-file (out) #:exists 'replace (λ () (for ([r rows]) (write r) (newline))))
     (summary rows)]))
