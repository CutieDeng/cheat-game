#lang racket/base
;; v1 M6：逐案归因——侦探是靠排除还是靠拆穿破案，凶手的输赢有多少由故事决定。
;;   racket m6/attrib.rkt --model m6/big-ckpt-rl3.jsonl [--shard i --of n] --out runs/m6-attrib-part.rktd
;;   racket m6/attrib.rkt --from runs/m6-attrib-rl3.rktd        只汇总
;; 每个案件（模型采样文件的一行：贪心 + 8 个采样故事）：
;;   正常侦探（可拆穿）对贪心故事：破案时三条路线哪些成立（排除 can-win? / 拆穿 ≥2 来源 / 自认）、所需 AP
;;   只用排除的侦探（detective-run! #:expose? #f）对贪心与 8 个采样故事：所需 AP，取最大值 E-max
;; 排除法只用 Y、Z 以外的人的声明，基本不依赖凶手的故事；E-max ≤ 预算 说明这 9 个故事都逃不过排除法。
;; 分类（预算 B = engine-v1.rkt budget-for-npcs；§16.5 的结果用的是旧预算 ⌈1.2·n²⌉）：
;;   win          正常侦探所需 AP > B
;;   structural   E-max ≤ B（9 个故事里没有一个能靠"不被拆穿"撑过预算）
;;   exposure     E-max > B 但正常侦探在 B 内破案（至少有一个故事在不被拆穿时能撑过预算）
(require racket/list racket/match racket/cmdline racket/format json
         "../world.rkt" "../case1.rkt" "../gen-v1.rkt" "../engine-v1.rkt" "../detective-v1.rkt")

(define BUDGET 400)

(define (rel->path c st)
  (path (case1-culprit c)
        (for/list ([s st]) (match s [(list n a b) (seg (string->symbol n) (+ a (case1-t0 c)) (+ b (case1-t0 c)))]))))

(define (run c story expose?)
  (define g (new-game1 c #:planner (λ (_) story) #:budget BUDGET))
  (define st (detective-run! g #:expose? expose?))
  (define z (case1-culprit c))
  (hasheq 'status st 'ap (game1-ap g)
          'excl (and (can-win? g z) #t)
          'expo (>= (length (exposure-sources g z)) EXPOSE-TO-WIN)
          'adm (and (admitted? g z) #t)))

(define (one j)
  (define n (hash-ref j 'npcs)) (define s (hash-ref j 'seed))
  (define-values (d a e) (generate-case1 s #:npcs n))
  (define c (datum->case1 d))
  (define g (rel->path c (hash-ref j 'greedy)))
  (define normal (run c g #t))
  (define e-greedy (hash-ref (run c g #f) 'ap))
  (define e-samples (for/list ([st (hash-ref j 'samples)]) (hash-ref (run c (rel->path c st) #f) 'ap)))
  (hasheq 'npcs n 'seed s 'normal normal 'e-greedy e-greedy 'e-samples e-samples))

(define (read-rows file) (with-input-from-file file (λ () (for/list ([x (in-port read)]) x))))

(define (summary rows)
  (define (pct a b) (format "~a%" (~r (* 100. (/ a (max b 1))) #:precision 0)))
  (define (median xs) (let ([v (sort xs <)]) (if (null? v) +nan.0 (list-ref v (quotient (length v) 2)))))
  (for ([n (sort (remove-duplicates (map (λ (r) (hash-ref r 'npcs)) rows)) <)])
    (define rs (filter (λ (r) (= n (hash-ref r 'npcs))) rows))
    (define B (budget-for-npcs n))
    (define (ap r) (hash-ref (hash-ref r 'normal) 'ap))
    (define (won-by-det? r) (eq? 'won (hash-ref (hash-ref r 'normal) 'status)))
    (define (emax r) (apply max (hash-ref r 'e-greedy) (hash-ref r 'e-samples)))
    (define (cat r) (cond [(or (not (won-by-det? r)) (> (ap r) B)) 'win]
                          [(<= (emax r) B) 'structural]
                          [else 'exposure]))
    (define cats (map cat rs))
    (define N (length rs))
    (printf "~a 人（预算 ~a，~a 案）\n" n B N)
    (printf "  分类：凶手赢 ~a；结构性必输（E-max ≤ 预算）~a；因拆穿而输 ~a\n"
            (pct (count (λ (x) (eq? x 'win)) cats) N) (pct (count (λ (x) (eq? x 'structural)) cats) N)
            (pct (count (λ (x) (eq? x 'exposure)) cats) N))
    (define lost (filter (λ (r) (and (won-by-det? r) (<= (ap r) B))) rs))
    (define (route r) (hash-ref r 'normal))
    (printf "  侦探在预算内破案的 ~a 案中，破案时成立的路线：排除 ~a，拆穿 ~a，自认 ~a；只有拆穿成立 ~a\n"
            (length lost)
            (pct (count (λ (r) (hash-ref (route r) 'excl)) lost) (length lost))
            (pct (count (λ (r) (hash-ref (route r) 'expo)) lost) (length lost))
            (pct (count (λ (r) (hash-ref (route r) 'adm)) lost) (length lost))
            (pct (count (λ (r) (and (hash-ref (route r) 'expo) (not (hash-ref (route r) 'excl)))) lost) (length lost)))
    (printf "  只用排除的侦探所需 AP：贪心故事中位数 ~a，E-max 中位数 ~a，正常侦探中位数 ~a\n"
            (median (map (λ (r) (hash-ref r 'e-greedy)) rs)) (median (map emax rs)) (median (map ap rs)))
    (define spread (map (λ (r) (- (emax r) (apply min (hash-ref r 'e-greedy) (hash-ref r 'e-samples)))) rs))
    (printf "  同一案件 9 个故事的排除 AP 极差：中位数 ~a，为 0 的案件 ~a\n"
            (median spread) (pct (count zero? spread) N))))

(module+ main
  (define model (make-parameter #f))
  (define shard (make-parameter 0))
  (define of (make-parameter 1))
  (define out (make-parameter #f))
  (define from (make-parameter #f))
  (command-line #:once-each
                ["--model" f "模型采样 JSONL（贪心 + 采样）" (model f)]
                ["--shard" i "分片下标" (shard (string->number i))]
                ["--of" k "分片数" (of (string->number k))]
                ["--out" f "逐案结果 .rktd" (out f)]
                ["--from" f "只汇总" (from f)])
  (cond
    [(from) (summary (read-rows (from)))]
    [else
     (define lines (for/list ([l (in-lines (open-input-file (model)))] [i (in-naturals)]
                              #:when (= (modulo i (of)) (shard)))
                     (string->jsexpr l)))
     (define rows (for/list ([j lines]) (one j)))
     (with-output-to-file (out) #:exists 'replace (λ () (for ([r rows]) (write r) (newline))))
     (summary rows)]))
