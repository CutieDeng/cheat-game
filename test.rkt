#lang racket/base
;; raco test test.rkt
(require rackunit racket/list "kb.rkt" "engine.rkt")

(define cd (load-case "case-manor.rktd"))
(define (P pol a l f t) (prop pol (st a l (hhmm->min f) (hhmm->min t)) 'exact))
(define (props r) (map claim-prop (response-claims r)))
(define (find-prop r pol loc)
  (findf (λ (c) (and (eq? (prop-pol (claim-prop c)) pol) (eq? (st-loc (prop-st (claim-prop c))) loc)))
         (response-claims r)))

;; 第 1 个时间槽是 20:00–20:30
(define S1 1)

(test-case "案件静态校验通过"
  (check-equal? (validate-case cd) '()))

(test-case "静态校验能发现错误"
  (define bad-obs
    (struct-copy case-data cd
                 [knowledge (cons (kitem 'kx 'E2 (P 'pos 'E3 'L2 2000 2010) 'L2 #f)
                                  (case-data-knowledge cd))]))
  (check-true (pair? (validate-case bad-obs)))
  (define no-source
    (struct-copy case-data cd
                 [cover (hash-set (case-data-cover cd) 'E3
                                  (list (list (st 'E3 'L4 (hhmm->min 1930) (hhmm->min 2000)))))]))
  (check-true (pair? (validate-case no-source))))

(test-case "主线：两次拆穿管家后指认成功"
  (define g (new-game cd #:seed 7))
  (define r1 (ask! g 'E1 'E1 S1))
  (check-eq? (response-kind r1) 'cover)
  (check-equal? (props r1) (list (P 'pos 'E1 'L2 2000 2030)))

  (define r2 (ask! g 'E2 'E1 S1))
  (check-eq? (response-kind r2) 'truth)
  (define deny (find-prop r2 'neg 'L2))
  (check-equal? (claim-prop deny) (P 'neg 'E1 'L2 2000 2025))

  (define r3 (present! g 'E1 (claim-id deny)))
  (check-eq? (response-kind r3) 'retract)
  (check-equal? (props r3) (list (P 'pos 'E1 'L3 2000 2025) (P 'pos 'E1 'L2 2025 2030)))

  ;; 园丁说出看见管家会暴露自己在走廊，所以说不知道
  (check-eq? (response-kind (ask! g 'E3 'E1 S1)) 'unknown)
  (define r5 (ask! g 'E3 'E3 S1))
  (check-eq? (response-kind r5) 'cover)
  (check-equal? (props r5) (list (P 'pos 'E3 'L4 2000 2030)))

  (define r6 (present! g 'E3 'EV1))
  (check-eq? (response-kind r6) 'retract)
  (check-equal? (props r6) (list (P 'pos 'E3 'L3 2000 2010) (P 'pos 'E3 'L4 2010 2030)))

  (define r7 (ask! g 'E3 'E1 S1))
  (check-eq? (response-kind r7) 'truth)
  (define study (find-prop r7 'pos 'L1))
  (check-equal? (claim-prop study) (P 'pos 'E1 'L1 2005 2010))

  ;; 凶手最后一层被拆穿后只拒答
  (check-eq? (response-kind (present! g 'E1 (claim-id study))) 'refuse)
  (check-equal? (length (exposed-lies g 'E1)) 2)
  ;; 重复出示同一条
  (check-eq? (response-kind (present! g 'E1 (claim-id study))) 'already)

  (define r9 (accuse! g 'E1))
  (check-eq? (response-kind r9) 'confess)
  (check-eq? (game-status g) 'won))

(test-case "拆穿管家一次后，管家会把园丁在走廊的事说出来"
  (define g (new-game cd #:seed 7))
  (check-eq? (response-kind (ask! g 'E1 'E3 S1)) 'unknown)   ; 第 0 层：说出来会暴露自己
  (ask! g 'E1 'E1 S1)
  (define deny (find-prop (ask! g 'E2 'E1 S1) 'neg 'L2))
  (present! g 'E1 (claim-id deny))
  (define r (ask! g 'E1 'E3 S1))
  (check-eq? (response-kind r) 'truth)
  (check-equal? (props r) (list (P 'pos 'E3 'L3 2000 2005))))

(test-case "备选线：反复追问导致掩护层被遗忘、说漏嘴"
  (define g (new-game cd #:seed 7))
  (ask! g 'E1 'E1 S1)
  (define deny (find-prop (ask! g 'E2 'E1 S1) 'neg 'L2))
  (check-eq? (response-kind (present! g 'E1 (claim-id deny))) 'retract)
  (define kinds (for/list ([i 3]) (response-kind (ask! g 'E1 'E1 S1))))
  (check-equal? kinds '(cover cover slip))
  (check-equal? (map cdr (reverse (game-forget-log g)))
                (list (list 'forget 'E1 '(cover E1 1 1))))
  (check-equal? (length (exposed-lies g 'E1)) 2)
  ;; 遗忘不可恢复：再问一次仍然说漏
  (check-eq? (response-kind (ask! g 'E1 'E1 S1)) 'slip)
  (check-equal? (length (exposed-lies g 'E1)) 2)
  (check-eq? (response-kind (accuse! g 'E1)) 'confess))

(test-case "遗忘保护：critical 观察在被说出前不会退化"
  (define g (new-game cd #:seed 7))
  (hash-set! (game-p-npc g) 'E3 100)
  (hash-set! (game-p-topic g) (list 'E3 'E1 S1) 100)
  (maybe-forget! g 'E3 'E1 S1)
  (check-true (hash-ref (game-degraded g) 'k4 #f))
  (check-false (hash-ref (game-degraded g) 'k5 #f)))

(test-case "退化后的观察以 approx 说出，不产生冲突"
  (define g (new-game cd #:seed 7))
  (hash-set! (game-degraded g) 'k1 #t)
  (define r (ask! g 'E2 'E1 0))
  (check-eq? (response-kind r) 'truth)
  (check-equal? (props r) (list (prop 'pos (st 'E1 'L2 (hhmm->min 1930) (hhmm->min 2000)) 'approx))))

(test-case "凶手自己说出案发时在作案现场，可直接定罪"
  (define (truth-first view actions)
    (or (findf (λ (a) (eq? (action-kind a) 'truth)) actions) (car actions)))
  (define g (new-game cd #:policy truth-first))
  (define r (ask! g 'E1 'E1 S1))
  (check-eq? (response-kind r) 'truth)
  (check-not-false (member (P 'pos 'E1 'L1 2005 2020) (props r)))
  (check-equal? (length (exposed-lies g 'E1)) 0)
  (check-true (admitted? g 'E1))
  (check-eq? (response-kind (accuse! g 'E1)) 'confess)
  (check-eq? (game-status g) 'won))

(test-case "只说出不在案发时段的真话不算承认"
  (define (truth-first view actions)
    (or (findf (λ (a) (eq? (action-kind a) 'truth)) actions) (car actions)))
  (define g (new-game cd #:policy truth-first))
  (ask! g 'E1 'E1 0)                       ; 19:30–20:00 在厨房
  (check-false (admitted? g 'E1))
  (check-eq? (response-kind (accuse! g 'E1)) 'insufficient))

(test-case "作案事实与真相不符时校验报错"
  (define bad (struct-copy case-data cd
                           [crime (st 'E1 'L2 (hhmm->min 2005) (hhmm->min 2020))]))
  (check-true (pair? (validate-case bad))))

(test-case "视图：只有凶手的前缀里有作案事实"
  (define g (new-game cd))
  (define (prefix-keys npc)
    (map car (cdr (assq 'prefix (cdr (assemble-view g npc (list 'ask npc 1)))))))
  (check-not-false (memq 'crime (prefix-keys 'E1)))
  (check-false (memq 'crime (prefix-keys 'E3))))

(test-case "撒谎者的真话只有与掩护说法冲突时才标为暴露"
  (define g (new-game cd))
  (define (truth-action slot)
    (findf (λ (a) (eq? (action-kind a) 'truth)) (legal-answers g 'E1 'E1 slot)))
  (check-false (action-exposing? (truth-action 0)))   ; 19:30–20:00 真话与掩护都是厨房
  (check-true (action-exposing? (truth-action 1))))

(test-case "指认错误或证据不足都失败"
  (define g1 (new-game cd))
  (check-eq? (response-kind (accuse! g1 'E2)) 'wrong)
  (check-eq? (game-status g1) 'lost)
  (define g2 (new-game cd))
  (check-eq? (response-kind (accuse! g2 'E1)) 'insufficient)
  (check-eq? (game-status g2) 'lost))

(test-case "视图长度不随轮次增长（30 轮满负荷）"
  (define g (new-game cd #:seed 3))
  (define npcs (case-data-npcs cd))
  (for ([i (in-range (sub1 TURN-LIMIT))])
    (define npc (list-ref npcs (modulo i 3)))
    (define subj (list-ref npcs (modulo (quotient i 3) 3)))
    (ask! g npc subj (modulo (quotient i 9) 3)))
  (define sizes (reverse (game-view-sizes g)))
  (printf "视图 token 数（按轮）：~a\n" sizes)
  (printf "最大 ~a，前 10 轮均值 ~a，后 10 轮均值 ~a\n"
          (apply max sizes)
          (exact->inexact (/ (apply + (take sizes 10)) 10))
          (exact->inexact (/ (apply + (take-right sizes 10)) 10)))
  (check-true (<= (apply max sizes) 600)))
