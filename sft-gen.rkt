#lang racket/base
;; v1 M5：教师生成凶手故事、校验器打分，产出模仿学习（SFT）数据集。
;;   racket sft-gen.rkt [--seeds 30] [--npcs "4 5 6 8"] [--k 8] [--temp 1.0] [--out runs/sft-v1.rktd]
;;   racket sft-gen.rkt --summary runs/sft-v1.rktd
;;   racket sft-gen.rkt --rescore runs/sft-v1.rktd --out 新文件   用当前校验器重新给已存的故事打分（不调用模型）
;;   racket sft-gen.rkt --resume --seeds 250 --out runs/sft-v2.rktd   续跑：保留 --out 里已完成的案件（丢掉写了一半的最后一行），只跑缺的
;; 每个案件一行（逐行写入，中断后已写的保留）：
;;   (hasheq npcs seed prompt candidates best local omni)
;;   candidates：每个候选 (hasheq text story parse-error key ok v2 admits sources survival ms)
;;   best：key 最大且合法（不自认）的候选下标，没有则 #f；local / omni：两个规则规划器的 key
(require racket/list racket/cmdline racket/format racket/pretty
         "map.rkt" "world.rkt" "case1.rkt" "gen-v1.rkt" "liar-v1.rkt" "verify-v1.rkt" "teacher-v1.rkt")

(define (story->datum p)
  (cons 'story (for/list ([s (path-segs p)]) (list (seg-node s) (min->hhmm (seg-in s)) (min->hhmm (seg-out s))))))

(define (verify->json r)
  (define s (hash-ref r 'survival))
  (hasheq 'ok (hash-ref r 'ok) 'v2 (hash-ref r 'v2) 'admits (hash-ref r 'admits) 'sources (hash-ref r 'sources)
          'survival (and s (hasheq 'status (hash-ref s 'status) 'ap (hash-ref s 'ap) 'exposed (hash-ref s 'exposed)))
          'key (story-key r)))

(define (one-case n seed k temp)
  (define-values (d a errs) (generate-case1 seed #:npcs n))
  (define c (datum->case1 d))
  (define prompt (culprit-prompt c))
  (define cands
    (for/list ([i k])
      (define-values (txt ms toks)
        (with-handlers ([exn:fail? (λ (e) (values (format "调用失败：~a" (exn-message e)) 0 0))])
          (chat-once prompt #:temperature temp)))
      (define s (parse-story c txt))
      (if (string? s)
          (hasheq 'text txt 'parse-error s 'key '(0 0 0 0) 'ms ms)
          (hash-set* (verify->json (verify-story c s)) 'text txt 'story (story->datum s) 'ms ms))))
  (define valid (for/list ([x cands] [i (in-naturals)] #:when (= 1 (car (hash-ref x 'key)))) i))
  (define best (and (pair? valid)
                    (for/fold ([b (car valid)]) ([i (cdr valid)])
                      (if (key>? (hash-ref (list-ref cands i) 'key) (hash-ref (list-ref cands b) 'key)) i b))))
  (hasheq 'npcs n 'seed seed 'prompt prompt 'candidates cands 'best best
          'local (story-key (verify-story c (local-planner c)))
          'omni (story-key (verify-story c (omniscient-planner c)))))

;; 重新打分：重建案件，对每个已解析的候选重新校验，重选最优，重算规则规划器的 key
(define (rescore-row r)
  (define-values (d a errs) (generate-case1 (hash-ref r 'seed) #:npcs (hash-ref r 'npcs)))
  (define c (datum->case1 d))
  (define cands
    (for/list ([x (hash-ref r 'candidates)])
      (cond
        [(hash-ref x 'story #f)
         => (λ (sd) (define s (parse-story c (format "~s" sd)))
              (hash-set* (verify->json (verify-story c s)) 'text (hash-ref x 'text) 'story sd 'ms (hash-ref x 'ms 0)))]
        [else x])))
  (define valid (for/list ([x cands] [i (in-naturals)] #:when (= 1 (car (hash-ref x 'key)))) i))
  (define best (and (pair? valid)
                    (for/fold ([b (car valid)]) ([i (cdr valid)])
                      (if (key>? (hash-ref (list-ref cands i) 'key) (hash-ref (list-ref cands b) 'key)) i b))))
  (hash-set* r 'candidates cands 'best best
             'local (story-key (verify-story c (local-planner c)))
             'omni (story-key (verify-story c (omniscient-planner c)))))

;; ---------------------------------------------------------------- 汇总

(define (summary file)
  (define rows (with-input-from-file file (λ () (for/list ([x (in-port read)]) x))))
  (define (pct a b) (if (zero? b) "n/a" (format "~a%" (~r (* 100. (/ a b)) #:precision 0))))
  (for ([n (remove-duplicates (map (λ (r) (hash-ref r 'npcs)) rows))])
    (define rs (filter (λ (r) (= n (hash-ref r 'npcs))) rows))
    (define cs (append-map (λ (r) (hash-ref r 'candidates)) rs))
    (define parsed (filter (λ (x) (not (hash-ref x 'parse-error #f))) cs))
    (define okv (filter (λ (x) (hash-ref x 'ok #f)) parsed))
    (define adm (filter (λ (x) (hash-ref x 'admits #f)) okv))
    (define valid (filter (λ (x) (= 1 (car (hash-ref x 'key)))) cs))
    (define exposed (filter (λ (x) (= 0 (cadr (hash-ref x 'key)))) valid))
    (define with-best (filter (λ (r) (hash-ref r 'best)) rs))
    (define (best-key r) (hash-ref (list-ref (hash-ref r 'candidates) (hash-ref r 'best)) 'key))
    (define (cmp who) ; 最优候选 vs 规则规划器：严格更好 / 相同 / 更差
      (for/fold ([w 0] [t 0] [l 0]) ([r with-best])
        (define b (best-key r)) (define o (hash-ref r who))
        (cond [(key>? b o) (values (add1 w) t l)] [(key>? o b) (values w t (add1 l))] [else (values w (add1 t) l)])))
    (define-values (wl tl ll) (cmp 'local))
    (define-values (wo to lo) (cmp 'omni))
    (define (mean xs) (if (null? xs) +nan.0 (/ (apply + xs) (exact->inexact (length xs)))))
    (printf "npcs ~a：~a 案 × 候选 ~a\n" n (length rs) (length cs))
    (printf "  可解析 ~a；可行（V2）~a；自认 ~a；合法 ~a；合法中被拆穿定罪 ~a\n"
            (pct (length parsed) (length cs)) (pct (length okv) (length parsed)) (pct (length adm) (length okv))
            (pct (length valid) (length cs)) (pct (length exposed) (length valid)))
    (printf "  有合法候选的案件 ~a/~a；最优候选 vs 局部规划器：更好 ~a 相同 ~a 更差 ~a；vs 全知规划器：更好 ~a 相同 ~a 更差 ~a\n"
            (length with-best) (length rs) wl tl ll wo to lo)
    (printf "  侦探 AP 均值：最优候选 ~a，局部 ~a，全知 ~a；单次调用均值 ~a s\n"
            (~r (mean (map (λ (r) (caddr (best-key r))) with-best)) #:precision 1)
            (~r (mean (map (λ (r) (caddr (hash-ref r 'local))) with-best)) #:precision 1)
            (~r (mean (map (λ (r) (caddr (hash-ref r 'omni))) with-best)) #:precision 1)
            (~r (/ (mean (map (λ (x) (hash-ref x 'ms 0)) cs)) 1000) #:precision 1))))

(module+ main
  (define seeds (make-parameter 30))
  (define npcs (make-parameter "4 5 6 8"))
  (define k (make-parameter 8))
  (define temp (make-parameter 1.0))
  (define out (make-parameter "runs/sft-v1.rktd"))
  (define sum (make-parameter #f))
  (define rescore (make-parameter #f))
  (define resume (make-parameter #f))
  (command-line #:once-each
                ["--seeds" s "每个人数的种子数" (seeds (string->number s))]
                ["--npcs" s "人数列表" (npcs s)]
                ["--k" s "每案候选数" (k (string->number s))]
                ["--temp" s "温度" (temp (string->number s))]
                ["--out" f "输出 .rktd（逐行追加）" (out f)]
                ["--summary" f "只汇总已有文件" (sum f)]
                ["--rescore" f "重新打分已有文件，写到 --out" (rescore f)]
                ["--resume" "续跑：不清空 --out，跳过其中已完成的 (人数, 种子)" (resume #t)])
  (cond
    [(sum) (summary (sum))]
    [(rescore)
     (define rows (with-input-from-file (rescore) (λ () (for/list ([x (in-port read)]) x))))
     (with-output-to-file (out) #:exists 'replace
       (λ () (for ([r rows]) (write (rescore-row r)) (newline))))
     (summary (out))]
    [else
     ;; 续跑：只保留能完整读出的行，重写文件（中断时最后一行可能只写了一半）
     (define done-rows
       (if (and (resume) (file-exists? (out)))
           (with-input-from-file (out)
             (λ () (let loop ([acc '()])
                     (define x (with-handlers ([exn:fail? (λ (e) eof)]) (read)))
                     (if (eof-object? x) (reverse acc) (loop (cons x acc))))))
           '()))
     (with-output-to-file (out) #:exists 'replace (λ () (for ([r done-rows]) (write r) (newline))))
     (define done (for/hash ([r done-rows]) (values (cons (hash-ref r 'npcs) (hash-ref r 'seed)) #t)))
     (when (resume) (printf "续跑：已完成 ~a 案\n" (hash-count done)))
     (for* ([n (map string->number (regexp-split #rx" +" (npcs)))] [s (in-range 1 (add1 (seeds)))]
            #:unless (hash-ref done (cons n s) #f))
       (define t0 (current-inexact-milliseconds))
       (define r (one-case n s (k) (temp)))
       (with-output-to-file (out) #:exists 'append (λ () (write r) (newline)))
       (printf "[~a 人 种子 ~a] 最优 ~a 局部 ~a 全知 ~a（~a s）\n" n s
               (let ([b (hash-ref r 'best)]) (and b (hash-ref (list-ref (hash-ref r 'candidates) b) 'key)))
               (hash-ref r 'local) (hash-ref r 'omni)
               (round (/ (- (current-inexact-milliseconds) t0) 1000)))
       (flush-output))
     (summary (out))]))
