#lang racket/base
;; v1 M6 强化学习的打分进程：stdin 每行一个 JSON {npcs seed story}，stdout 每行一个 JSON {key ok admits}。
;; story 为 [[地点 进入 离开] ...]，时间相对 t0（分钟）；打分与 m6/eval.rkt 相同（verify-v1.rkt，含侦探存活分）。
;; 案件按 (npcs, seed) 缓存。m6/rl.py 启动多个本进程并行打分。
(require racket/match json "../world.rkt" "../case1.rkt" "../gen-v1.rkt" "../verify-v1.rkt")

(define cache (make-hash))
(define (case-of n s)
  (hash-ref! cache (cons n s)
             (λ () (define-values (d a e) (generate-case1 s #:npcs n)) (datum->case1 d))))

(module+ main
  (for ([line (in-lines)])
    (define j (string->jsexpr line))
    (define c (case-of (hash-ref j 'npcs) (hash-ref j 'seed)))
    (define st (path (case1-culprit c)
                     (for/list ([x (hash-ref j 'story)])
                       (match x [(list n a b) (seg (string->symbol n) (+ a (case1-t0 c)) (+ b (case1-t0 c)))]))))
    (define r (with-handlers ([exn:fail? (λ (e) #f)]) (verify-story c st)))
    (write-json (if r
                    (hasheq 'key (story-key r) 'ok (hash-ref r 'ok) 'admits (and (hash-ref r 'admits) #t))
                    (hasheq 'key '(0 0 0 0) 'ok #f 'admits #f 'error #t)))
    (newline)
    (flush-output)))
