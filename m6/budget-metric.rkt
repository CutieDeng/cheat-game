#lang racket/base
;; v1 M6：按对局预算算凶手胜率。读 m6/eval.rkt --out 写的逐案结果（不重跑侦探）。
;;   racket m6/budget-metric.rkt runs/m6-fresh-ckpt-v2.rktd runs/m6-fresh-ckpt-rl1.rktd ...
;; 凶手"按预算获胜"：故事合法（key 第 1 位为 1）且公开信息侦探破案所需行动点 > 对局预算（engine-v1.rkt budget-for-npcs；2026-11-26 前为 ⌈1.2·n²⌉，之后为 8n − 14）。
;; 校验器里侦探用宽预算 400 跑，所以所需行动点可以超过对局预算。
(require racket/list racket/format "../engine-v1.rkt")

(define (read-rows file)
  (with-input-from-file file (λ () (for/list ([x (in-port read)]) x))))

(define (wins? k B) (and k (= 1 (car k)) (> (caddr k) B)))
(define (best ks) (for/fold ([b #f]) ([k ks] #:when (= 1 (car k)))
                    (if (or (not b) (> (caddr k) (caddr b))) k b)))   ; 按 AP 取最大

(for ([f (current-command-line-arguments)])
  (define rows (read-rows f))
  (printf "~a\n" f)
  (for ([n (sort (remove-duplicates (map (λ (r) (hash-ref r 'npcs)) rows)) <)])
    (define rs (filter (λ (r) (= n (hash-ref r 'npcs))) rows))
    (define B (budget-for-npcs n))
    (define (pct xs) (format "~a%" (~r (* 100. (/ (count values xs) (max 1 (length xs)))) #:precision 0)))
    (printf "  ~a 人（预算 ~a，~a 案）：贪心 ~a；单个样本 ~a；8 选 1（按 AP）~a；全知 ~a；局部 ~a\n"
            n B (length rs)
            (pct (map (λ (r) (wins? (hash-ref (hash-ref r 'greedy) 'key) B)) rs))
            (pct (append-map (λ (r) (map (λ (x) (wins? (hash-ref x 'key) B)) (hash-ref r 'samples))) rs))
            (pct (map (λ (r) (wins? (best (map (λ (x) (hash-ref x 'key)) (hash-ref r 'samples))) B)) rs))
            (pct (map (λ (r) (wins? (hash-ref r 'omni) B)) rs))
            (pct (map (λ (r) (wins? (hash-ref r 'local) B)) rs)))))
