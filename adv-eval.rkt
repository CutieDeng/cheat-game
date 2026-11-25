#lang racket/base
;; 对抗可解性统计。对每个配置、每个种子，取不加对抗检查时生成器接受的案件，
;; 统计它在对抗 NPC（solve.rkt 的 adversarial-policy，用不可拆穿的编造）下的破案情况：
;;   strict = 检查器只靠证据；press = 卡住后追问凶手案发时段；spam = 从第 1 轮起只追问（不用证据）
;; 然后统计开启 'press 检查后生成器的平均尝试次数。
;; racket adv-eval.rkt [--seeds N] [--out 文件]
(require racket/list racket/cmdline racket/pretty racket/format
         "kb.rkt" "engine.rkt" "solve.rkt" "gen.rkt" "gen-net.rkt")

(define GEN '((A 3 3 2 0) (B 4 4 2 1) (C 5 4 3 1) (D 6 5 3 2) (E 8 5 3 3)))
(define NET '((N4 4 4 2 0) (N5 5 4 2 0) (N6 6 5 3 0) (N8 8 5 3 0) (N5d1 5 4 2 1) (N6d2 6 5 3 2) (N8d2 8 5 3 2)))

(define (spam cd)
  (define g (new-game cd #:seed 1 #:policy adversarial-policy #:ext? #t))
  (define c (case-data-culprit cd))
  (define crime (case-data-crime cd))
  (define slots (slots-overlapping cd (st-from crime) (st-to crime)))
  (let loop ([i 0])
    (when (eq? (game-status g) 'playing)
      (if (convictable? g c) (accuse! g c)
          (begin (ask! g c c (list-ref slots (modulo i (length slots)))) (loop (add1 i))))))
  (game-status g))

(define (won? r) (eq? 'won (hash-ref r 'status)))

(define (row name base checked n)
  (define t0 (current-inexact-milliseconds))
  (define cds (for/list ([s (in-range 1 (add1 n))]) (base s)))
  (define ok (filter values cds))
  (define strict (count (λ (cd) (won? (adversarial-solve cd #:press? #f))) ok))
  (define press (count (λ (cd) (won? (adversarial-solve cd #:press? #t))) ok))
  (define sp (count (λ (cd) (eq? 'won (spam cd))) ok))
  (define atts (for/list ([s (in-range 1 (add1 n))]) (checked s)))
  (define fails (count not atts))
  (define good (filter values atts))
  (define r (hasheq 'config name 'cases (length ok) 'strict strict 'press press 'spam sp
                    'checked-fail fails
                    'checked-mean-attempts (if (null? good) #f (/ (apply + good) (exact->inexact (length good))))
                    'ms (- (current-inexact-milliseconds) t0)))
  (printf "~a：~a 个案件；对抗 NPC 下 strict ~a，press ~a，只追问 ~a；开启 press 检查后平均尝试 ~a 次，生成失败 ~a\n"
          name (length ok) strict press sp
          (and (hash-ref r 'checked-mean-attempts) (~r (hash-ref r 'checked-mean-attempts) #:precision 2)) fails)
  r)

(module+ main
  (define seeds (make-parameter 30))
  (define out (make-parameter #f))
  (command-line #:once-each
                ["--seeds" s "每个配置的种子数" (seeds (string->number s))]
                ["--out" f "输出 .rktd" (out f)])
  (define rows
    (append
     (for/list ([c GEN])
       (define-values (name n S L K) (apply values c))
       (row name
            (λ (s) (let-values ([(d a) (generate-case* s #:npcs n #:slots S #:layers L #:secrets K #:adversarial #f)])
                     (and d (datum->case d))))
            (λ (s) (let-values ([(d a) (generate-case* s #:npcs n #:slots S #:layers L #:secrets K)]) (and d a)))
            (seeds)))
     (for/list ([c NET])
       (define-values (name n S L md) (apply values c))
       (row name
            (λ (s) (let-values ([(d a m) (generate-net-case* s #:npcs n #:slots S #:layers L #:min-depth md #:adversarial #f)])
                     (and d (datum->case d))))
            (λ (s) (let-values ([(d a m) (generate-net-case* s #:npcs n #:slots S #:layers L #:min-depth md)]) (and d a)))
            (seeds)))))
  (when (out) (with-output-to-file (out) #:exists 'replace (λ () (for ([r rows]) (write r) (newline))))))
