#lang racket/base
;; v1 平衡：离线扫描对局预算。读 eval.rkt 的逐案结果（侦探宽预算 400 下所需 AP），对每个候选预算 B 重算胜负。
;;   racket m6/budget-sweep.rkt runs/m6-big-ckpt-rl3.rktd runs/m6-big-ckpt-v2.rktd
;; 第一个文件当"强凶手"（贪心），第二个当"弱凶手"（贪心）；规则规划器取第一个文件里的 local / omni。
;; 凶手赢 = 故事合法且所需 AP > B。故事作用 = 强凶手胜率 − 局部规划器胜率。
;; 局限（推断）：侦探在宽预算下跑，所需 AP 与真实预算下的行为在预算很紧时可能不同。
(require racket/list racket/format "../engine-v1.rkt")
(define (rows f) (with-input-from-file f (λ () (for/list ([x (in-port read)]) x))))
(define strong (rows (vector-ref (current-command-line-arguments) 0)))
(define weak (rows (vector-ref (current-command-line-arguments) 1)))
(define (wins? k B) (and (= 1 (car k)) (> (caddr k) B)))
(define (rate rs get B) (/ (count (λ (r) (wins? (get r) B)) rs) (exact->inexact (length rs))))
(define (g r) (hash-ref (hash-ref r 'greedy) 'key))
(for ([n '(4 5 6 8)])
  (define s (filter (λ (r) (= n (hash-ref r 'npcs))) strong))
  (define w (filter (λ (r) (= n (hash-ref r 'npcs))) weak))
  (define old (ceiling (* 6/5 n n)))   ; 扫描到旧预算 ⌈1.2·n²⌉ 为止
  (printf "~a 人（现预算 ~a，旧预算 ~a，~a 案）\n  B    侦探胜(强)  强凶手  弱凶手  全知  局部  强−局部\n" n (budget-for-npcs n) old (length s))
  (for ([B (in-range (* 2 n) (add1 old) (max 1 (quotient n 2)))])
    (define rs (rate s g B)) (define rw (rate w g B))
    (define ro (rate s (λ (r) (hash-ref r 'omni)) B)) (define rl (rate s (λ (r) (hash-ref r 'local)) B))
    (printf "  ~a  ~a       ~a    ~a    ~a  ~a  ~a\n" (~a B #:min-width 3)
            (~r (- 1 rs) #:precision '(= 2)) (~r rs #:precision '(= 2)) (~r rw #:precision '(= 2))
            (~r ro #:precision '(= 2)) (~r rl #:precision '(= 2)) (~r (- rs rl) #:precision '(= 2)))))
