#lang racket/base
;; v1 M6：用校验器（verify-v1.rkt）给专用小网络生成的故事打分，并与教师（同一案件的 8 个候选）、规则规划器对比。
;;   racket m6/eval.rkt --model m6/samples.jsonl --teacher runs/sft-v2.rktd [--out m6/eval.rktd]
;;   racket m6/eval.rkt --from 逐案结果.rktd    只汇总已有的逐案结果（不重跑校验器；分片并行评测后合并用）
;; 模型文件每行：{npcs seed greedy samples}，故事为 [[地点 进入 离开] ...]，时间相对 t0（分钟）。
(require racket/list racket/match racket/cmdline racket/format json
         "../map.rkt" "../world.rkt" "../case1.rkt" "../gen-v1.rkt" "../liar-v1.rkt" "../verify-v1.rkt")

(define (read-rows file)
  (with-input-from-file file
    (λ () (let loop ([acc '()])
            (define x (with-handlers ([exn:fail? (λ (e) eof)]) (read)))
            (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(define (rel->path c st)
  (path (case1-culprit c)
        (for/list ([s st]) (match s [(list n a b) (seg (string->symbol n) (+ a (case1-t0 c)) (+ b (case1-t0 c)))]))))

(define (score c st)
  (define r (verify-story c (rel->path c st)))
  (hasheq 'ok (hash-ref r 'ok) 'admits (hash-ref r 'admits) 'key (story-key r)))

(define (best-key ks) (for/fold ([b #f]) ([k ks] #:when (= 1 (car k))) (if (or (not b) (key>? k b)) k b)))
(define (cmp a b) (cond [(and (not a) (not b)) 'same] [(not a) 'worse] [(not b) 'better]
                        [(key>? a b) 'better] [(key>? b a) 'worse] [else 'same]))

(module+ main
  (define model (make-parameter "m6/samples.jsonl"))
  (define teacher (make-parameter "runs/sft-v2.rktd"))
  (define out (make-parameter #f))
  (define from (make-parameter #f))
  (command-line #:once-each
                ["--model" f "模型采样 JSONL" (model f)]
                ["--teacher" f "教师数据 .rktd" (teacher f)]
                ["--out" f "逐案结果 .rktd" (out f)]
                ["--from" f "只汇总已有的逐案结果 .rktd" (from f)])
  (define trows (if (from) (hash) (for/hash ([r (read-rows (teacher))]) (values (cons (hash-ref r 'npcs) (hash-ref r 'seed)) r))))
  (define t0 (current-inexact-milliseconds))
  (define results
    (if (from) (read-rows (from))
    (for/list ([line (in-lines (open-input-file (model)))])
      (define j (string->jsexpr line))
      (define n (hash-ref j 'npcs)) (define s (hash-ref j 'seed))
      (define-values (d a e) (generate-case1 s #:npcs n))
      (define c (datum->case1 d))
      (define tr (hash-ref trows (cons n s) #f))
      (hasheq 'npcs n 'seed s
              'greedy (score c (hash-ref j 'greedy))
              'samples (for/list ([st (hash-ref j 'samples)]) (score c st))
              'teacher (and tr (for/list ([x (hash-ref tr 'candidates)])
                                 (hasheq 'ok (hash-ref x 'ok #f) 'admits (hash-ref x 'admits #f) 'key (hash-ref x 'key))))
              'local (story-key (verify-story c (local-planner c)))
              'omni (story-key (verify-story c (omniscient-planner c)))))))
  (when (out) (with-output-to-file (out) #:exists 'replace (λ () (for ([r results]) (write r) (newline)))))
  (define (pct a b) (if (zero? b) "n/a" (format "~a%" (~r (* 100. (/ a b)) #:precision 0))))
  (define (rates xs)
    (define ok (count (λ (x) (hash-ref x 'ok)) xs))
    (define adm (count (λ (x) (and (hash-ref x 'ok) (hash-ref x 'admits))) xs))
    (define valid (count (λ (x) (= 1 (car (hash-ref x 'key)))) xs))
    (define unexp (count (λ (x) (equal? (take (hash-ref x 'key) 2) '(1 1))) xs))
    (format "可行 ~a 自认 ~a 合法 ~a 合法且未被拆穿 ~a" (pct ok (length xs)) (pct adm ok) (pct valid (length xs)) (pct unexp (length xs))))
  (define (mean xs) (if (null? xs) "n/a" (~r (/ (apply + xs) (exact->inexact (length xs))) #:precision 1)))
  (printf "评测 ~a 案，用时 ~a s\n" (length results) (round (/ (- (current-inexact-milliseconds) t0) 1000)))
  (for ([n (remove-duplicates (map (λ (r) (hash-ref r 'npcs)) results))])
    (define rs (filter (λ (r) (= n (hash-ref r 'npcs))) results))
    (define rt (filter (λ (r) (hash-ref r 'teacher)) rs))
    (printf "npcs ~a：~a 案（其中 ~a 案有教师数据）\n" n (length rs) (length rt))
    (printf "  网络贪心 1 个：~a\n" (rates (map (λ (r) (hash-ref r 'greedy)) rs)))
    (printf "  网络采样：    ~a\n" (rates (append-map (λ (r) (hash-ref r 'samples)) rs)))
    (printf "  教师 8 候选： ~a（只算有教师数据的案件）\n" (rates (append-map (λ (r) (hash-ref r 'teacher)) rt)))
    (define (tally who get-a get-b)
      (define cs (for/list ([r rs] #:when (get-b r)) (cmp (get-a r) (get-b r))))
      (format "~a 更好 ~a 相同 ~a 更差 ~a" who (count (λ (x) (eq? x 'better)) cs) (count (λ (x) (eq? x 'same)) cs) (count (λ (x) (eq? x 'worse)) cs)))
    (define (net-best r) (best-key (map (λ (x) (hash-ref x 'key)) (hash-ref r 'samples))))
    (define (t-best r) (and (hash-ref r 'teacher) (best-key (map (λ (x) (hash-ref x 'key)) (hash-ref r 'teacher)))))
    (define (greedy r) (let ([k (hash-ref (hash-ref r 'greedy) 'key)]) (and (= 1 (car k)) k)))
    (printf "  网络最优 vs：~a；~a；~a\n" (tally "教师最优" net-best t-best)
            (tally "全知" net-best (λ (r) (hash-ref r 'omni))) (tally "局部" net-best (λ (r) (hash-ref r 'local))))
    (printf "  网络贪心 vs：~a；~a\n" (tally "全知" greedy (λ (r) (hash-ref r 'omni))) (tally "局部" greedy (λ (r) (hash-ref r 'local))))
    (printf "  侦探 AP 均值（最优合法候选）：网络 ~a，教师 ~a，全知 ~a，局部 ~a\n"
            (mean (filter-map (λ (r) (let ([k (net-best r)]) (and k (caddr k)))) rs))
            (mean (filter-map (λ (r) (let ([k (t-best r)]) (and k (caddr k)))) rt))
            (mean (map (λ (r) (caddr (hash-ref r 'omni))) rs))
            (mean (map (λ (r) (caddr (hash-ref r 'local))) rs)))))
