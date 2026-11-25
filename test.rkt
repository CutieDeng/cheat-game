#lang racket/base
;; raco test test.rkt
(require rackunit racket/list "kb.rkt" "engine.rkt" "gen.rkt" "solve.rkt" "gen-net.rkt" "net.rkt" "rollout.rkt")

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

(test-case "只靠重复提问不会说漏嘴：第 5 次起压力为负且递增"
  (define g (new-game cd #:seed 7))
  (define ps
    (for/list ([i 8])
      (define k (response-kind (ask! g 'E1 'E1 S1)))
      (check-not-eq? k 'slip)
      (hash-ref (game-p-npc g) 'E1 0)))
  ;; 第 2–4 次各 +5，第 5 次 -5，第 6 次 -10（截到 0）
  (check-equal? (take ps 6) '(0 5 10 15 10 0))
  (check-equal? (game-forget-log g) '()))

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
  (for ([i (in-range (sub1 (case-data-turn-limit cd)))])
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

(test-case "检查器能解开庄园案"
  (check-eq? (hash-ref (oracle-solve cd) 'status) 'won))

(test-case "生成器：同一种子输出相同"
  (check-equal? (generate-case 5 #:npcs 5 #:slots 4 #:layers 3 #:secrets 1)
                (generate-case 5 #:npcs 5 #:slots 4 #:layers 3 #:secrets 1)))

(test-case "生成的案件通过静态校验且检查器能在回合上限内破案"
  (for* ([cfg '((3 3 2 0) (4 4 2 1) (5 4 3 1) (6 5 3 2))] [seed (in-range 1 11)])
    (define c (datum->case (apply (λ (n s l k) (generate-case seed #:npcs n #:slots s #:layers l #:secrets k)) cfg)))
    (check-equal? (validate-case c) '() (format "~a seed ~a" cfg seed))
    (check-eq? (hash-ref (oracle-solve c) 'status) 'won (format "~a seed ~a" cfg seed))))

(test-case "生成的案件：作案地点只有凶手在案发时段出现过"
  (for ([seed (in-range 1 21)])
    (define c (datum->case (generate-case seed #:npcs 6 #:slots 5 #:layers 3 #:secrets 2)))
    (define crime (case-data-crime c))
    (check-equal? (for/list ([s (case-data-truth c)]
                             #:when (eq? (st-loc s) (st-loc crime))
                             #:unless (eq? (st-agent s) 'V)
                             #:when (overlap? s crime))
                    (st-agent s))
                  (list (case-data-culprit c)))))

;; ---------------------------------------------------------------- 扩展动作

(test-case "扩展动作默认关闭"
  (define g (new-game cd))
  (check-equal? (map action-kind (legal-answers g 'E1 'E1 S1)) '(cover truth evade)))

(test-case "扩展动作：撒谎者可编造自己或别人的行踪，诚实者不能"
  (define g (new-game cd #:ext? #t))
  (define kinds (map action-kind (legal-answers g 'E1 'E1 S1)))
  (check-not-false (memq 'fabricate kinds))
  (check-not-false (memq 'frame (map action-kind (legal-answers g 'E1 'E2 S1))))
  (check-false (memq 'fabricate (map action-kind (legal-answers g 'E2 'E2 S1))))
  (check-false (memq 'frame (map action-kind (legal-answers g 'E2 'E1 S1))))
  ;; 编造的都是假话，且与已有承诺一致
  (for ([a (legal-answers g 'E1 'E1 S1)] #:when (eq? (action-kind a) 'fabricate))
    (check-false (prop-true? cd (car (action-props a))))))

(test-case "扩展动作：只说不暴露自己的那部分真话"
  ;; 园丁问管家：k4（走廊）和 k5（书房）都是在走廊看到的，都暴露；问 19:30–20:00 没有观察
  ;; 管家第 0 层问自己 20:00–20:30：真话里 20:00–20:05 在走廊、20:25–20:30 在厨房，后者不与掩护冲突
  (define g (new-game cd #:ext? #t))
  (define partial (findf (λ (a) (eq? (action-kind a) 'partial)) (legal-answers g 'E1 'E1 S1)))
  (check-not-false partial)
  (check-equal? (action-props partial) (list (P 'pos 'E1 'L2 2025 2030))))

(test-case "扩展动作：编造被拆穿后收回，计为被揭穿的谎言，不换掩护层"
  (define (fab-to loc)
    (λ (view actions)
      (or (findf (λ (a) (and (eq? (action-kind a) 'fabricate)
                             (eq? (st-loc (prop-st (car (action-props a)))) loc)))
                 actions)
          (findf (λ (a) (eq? (action-kind a) 'withdraw)) actions)
          (rule-policy view actions))))
  (define g (new-game cd #:ext? #t #:policy (fab-to 'L4)))
  (define r (ask! g 'E1 'E1 S1))             ; 管家编：20:00–20:30 在花园
  (check-eq? (response-kind r) 'fabricate)
  (define r2 (present! g 'E1 'EV1))          ; 烟头只说明园丁在走廊，与管家无关
  (check-eq? (response-kind r2) 'no-effect)
  (define deny (find-prop (ask! g 'E2 'E1 S1) 'neg 'L2))
  (check-eq? (response-kind (present! g 'E1 (claim-id deny))) 'no-effect) ; 否认在厨房与"在花园"不冲突
  (define r3 (ask! g 'E3 'E3 S1))            ; 园丁的掩护：一直在花园 → 与管家编的话无关
  (check-eq? (response-kind r3) 'cover)
  (define r4 (present! g 'E1 (claim-id (car (response-claims r3)))))
  (check-eq? (response-kind r4) 'no-effect)
  ;; 用园丁看见管家在走廊（20:00–20:05）拆穿"在花园"
  (present! g 'E3 'EV1)
  (define seen (find-prop (ask! g 'E3 'E1 S1) 'pos 'L3))
  (check-eq? (response-kind (present! g 'E1 (claim-id seen))) 'withdraw)
  (check-equal? (length (exposed-lies g 'E1)) 1)
  (check-equal? (layer-of g 'E1) 0))

;; ---------------------------------------------------------------- 关系（动机）

;; 庄园案加上关系：管家欠债并否认；园丁与被害人结过仇（不否认）；厨师知道管家欠债；证物：欠条
(define cd-rel
  (struct-copy case-data cd
               [rels (list (rl 'E1 'debt) (rl 'E3 'grudge))]
               [rel-cover (hasheq 'E1 '(debt))]
               [knowledge (cons (kitem 'kr1 'E2 (prop 'pos (rl 'E1 'debt) 'exact) #f #t) (case-data-knowledge cd))]
               [evidence (cons (list 'EV9 "书房抽屉里的欠条" (prop 'pos (rl 'E1 'debt) 'exact)) (case-data-evidence cd))]))

(test-case "关系：校验通过；否认不存在的关系是错误"
  (check-equal? (validate-case cd-rel) '())
  (check-true (pair? (validate-case (struct-copy case-data cd-rel [rel-cover (hasheq 'E2 '(debt))])))))

(test-case "关系：凶手否认，知情人说出，出示后否认被拆穿但不计入定罪"
  (define g (new-game cd-rel))
  (define r1 (ask! g 'E1 'E1 'rel))
  (check-eq? (response-kind r1) 'cover)
  (check-equal? (props r1) (list (prop 'neg (rl 'E1 'debt) 'exact)))
  (define r2 (ask! g 'E2 'E1 'rel))
  (check-eq? (response-kind r2) 'truth)
  (check-eq? (response-kind (present! g 'E1 'EV9)) 'withdraw)
  (check-equal? (length (exposed-lies g 'E1)) 0)
  ;; 否认被拆穿后不再否认，如实说出
  (check-eq? (response-kind (ask! g 'E1 'E1 'rel)) 'truth)
  ;; 园丁不否认自己的过节；厨师没有任何关系
  (check-eq? (response-kind (ask! g 'E3 'E3 'rel)) 'truth)
  (check-eq? (response-kind (ask! g 'E2 'E2 'rel)) 'none))

(test-case "关系：扩展动作下撒谎者可以编造别人的动机"
  (define g (new-game cd-rel #:ext? #t))
  (define frames (filter (λ (a) (eq? (action-kind a) 'frame)) (legal-answers g 'E1 'E2 'rel)))
  (check-equal? (length frames) 4)
  (check-true (null? (filter (λ (a) (eq? (action-kind a) 'frame)) (legal-answers g 'E2 'E1 'rel)))))

(test-case "关系：视图包含自己的关系与打算否认的关系，长度仍有界"
  (define g (new-game cd-rel))
  (define v (assemble-view g 'E1 (list 'ask 'E1 'rel)))
  (define prefix (cdr (assq 'prefix (cdr v))))
  (check-equal? (cdr (assq 'rels prefix)) '(debt))
  (check-equal? (cdr (assq 'hide prefix)) '(debt))
  (check-equal? (cdr (assq 'q (cdr v))) '(ask E1 rel)))

;; ---------------------------------------------------------------- 证据网

(test-case "证据网指标：庄园案链长 1、最小割 1（拆穿管家第 2 层只有园丁一个来源）"
  (check-= (chain-depth cd) 1 0)
  (check-equal? (min-cut cd) 1))

(test-case "网状生成：同一种子输出相同；通过校验、满足筛选条件、检查器能破案"
  (check-equal? (generate-net-case 3 #:npcs 5) (generate-net-case 3 #:npcs 5))
  (for* ([cfg '((4 4 2 0) (5 4 2 1) (6 5 3 2))] [seed (in-range 1 6)])
    (define-values (n S L md) (apply values cfg))
    (define-values (d attempts m) (generate-net-case* seed #:npcs n #:slots S #:layers L #:min-depth md))
    (check-not-false d (format "~a seed ~a" cfg seed))
    (define c (datum->case d))
    (check-equal? (validate-case c) '())
    (check-true (>= (chain-depth c) md))
    (check-true (>= (min-cut c) 2))
    (check-eq? (hash-ref (oracle-solve c) 'status) 'won)))

(test-case "网状生成：凶手有否认的动机，且至少一个来源能拆穿这条否认"
  (for ([seed (in-range 1 11)])
    (define c (datum->case (generate-net-case seed #:npcs 6)))
    (define C (case-data-culprit c))
    (check-not-false (memq C (hash-keys (case-data-rel-cover c))))
    (define r (rl C (car (hash-ref (case-data-rel-cover c) C))))
    (check-true (or (for/or ([k (case-data-knowledge c)]) (equal? (prop-st (kitem-prop k)) r))
                    (for/or ([e (case-data-evidence c)]) (equal? (prop-st (third e)) r))))))

;; ---------------------------------------------------------------- 回放评估

(test-case "回放评估：管家第一次被问案发时段，自认现场的真话价值最低"
  (define opts (legal-answers (new-game cd) 'E1 'E1 S1))
  (define vs (evaluate-options cd 1 #f (list (list 'ask 'E1 'E1 S1)) '() 0 opts 'E1 #:heuristic-seeds 4))
  (define q (for/hasheq ([a opts] [v vs]) (values (action-kind a) (hash-ref v 'q))))
  (printf "管家各选项价值：~s\n" q)
  (check-true (< (hash-ref q 'truth) 0.2))
  (check-true (< (hash-ref q 'truth) (hash-ref q 'cover)))
  (check-true (< (hash-ref q 'truth) (hash-ref q 'evade))))

(test-case "回放评估：重放能复现之前的 NPC 决策；说真话的人价值为 #f"
  (define g (new-game cd))
  (define r (ask! g 'E2 'E1 S1))
  (define a (findf (λ (x) (eq? (action-kind x) 'truth)) (legal-answers (new-game cd) 'E2 'E1 S1)))
  (define vs (evaluate-options cd 1 #f (list (list 'ask 'E2 'E1 S1)) '() 0 (list a) 'E2 #:heuristic-seeds 2))
  (check-false (hash-ref (car vs) 'q))
  (define-values (g2 ps) (replay cd 1 #f (list (list 'ask 'E2 'E1 S1) (list 'ask 'E1 'E1 S1)) (list a) 1
                                 (findf (λ (x) (eq? (action-kind x) 'cover)) (legal-answers (new-game cd) 'E1 'E1 S1))))
  (check-equal? (map claim-prop (game-claims g2)) (cons (P 'pos 'E1 'L2 2000 2030) (reverse (map claim-prop (response-claims r))))))

;; ---------------------------------------------------------------- 回避的代价

(test-case "回避：问自己行踪时说不清楚有压力代价，连续回避第 3 次提问时说漏嘴"
  (define (evader view actions)
    (or (findf (λ (a) (eq? (action-kind a) 'evade)) actions) (car actions)))
  (define g (new-game cd #:policy evader))
  (check-eq? (response-kind (ask! g 'E1 'E1 S1)) 'evade)
  (check-equal? (hash-ref (game-p-topic g) (list 'E1 'E1 S1)) P-EVADE-TOPIC)
  (check-eq? (response-kind (ask! g 'E1 'E1 S1)) 'evade)
  (check-eq? (response-kind (ask! g 'E1 'E1 S1)) 'slip)
  ;; 说漏嘴说出了案发时在书房，可直接定罪
  (check-true (admitted? g 'E1)))

(test-case "回避：问别人时说不清楚没有代价"
  (define g (new-game cd))
  (check-eq? (response-kind (ask! g 'E3 'E1 S1)) 'unknown)
  (check-equal? (hash-ref (game-p-npc g) 'E3 0) 0))

(test-case "关系：有要隐瞒的关系时不能用「没什么瓜葛」模糊否认"
  (define g (new-game cd-rel))
  (check-false (memq 'none (map action-kind (legal-answers g 'E1 'E1 'rel))))
  (check-not-false (memq 'none (map action-kind (legal-answers g 'E2 'E2 'rel)))))

(test-case "对抗可解性：规则 NPC 能破、对抗 NPC 只靠证据破不了的案件，开启检查后被拒绝"
  ;; gen 5/4/3/1 种子 1..50 中，至少有一个案件 strict 失败但 press 成功；generate-case* 第 1 次尝试与 generate-case 相同
  (define hit
    (for/first ([s (in-range 1 51)]
                #:when (let ([cd (datum->case (generate-case s #:npcs 5 #:slots 4 #:layers 3 #:secrets 1))])
                         (and (not (eq? 'won (hash-ref (adversarial-solve cd #:press? #f) 'status)))
                              (eq? 'won (hash-ref (adversarial-solve cd #:press? #t) 'status)))))
      s))
  (check-true (and hit #t))
  (define-values (d1 a1) (generate-case* hit #:npcs 5 #:slots 4 #:layers 3 #:secrets 1 #:adversarial 'press))
  (check-equal? a1 1)
  (check-equal? d1 (generate-case hit #:npcs 5 #:slots 4 #:layers 3 #:secrets 1))
  (define-values (d2 a2) (generate-case* hit #:npcs 5 #:slots 4 #:layers 3 #:secrets 1 #:adversarial 'strict))
  (check-true (> a2 1))
  (check-eq? 'won (hash-ref (adversarial-solve (datum->case d2) #:press? #f) 'status)))

(test-case "行动点：按动作种类计费，付不起的动作报错，只剩指认时仍可指认"
  ;; 预算 4，提问 1、出示 2、指认 0
  (define g (new-game cd #:budget 4))
  (ask! g 'E1 'E1 S1)
  (check-equal? (game-ap g) 1)
  (present! g 'E1 (first (first (case-data-evidence cd))))
  (check-equal? (game-ap g) 3)
  (check-eq? (phase g) 'confront)          ; 对质阶段从 floor(2/3 × 4) = 2 点开始
  (check-exn exn:fail? (λ () (present! g 'E1 (first (first (case-data-evidence cd))))))
  (ask! g 'E1 'E1 S1)
  (check-eq? (game-status g) 'accuse-only)
  (check-exn exn:fail? (λ () (ask! g 'E1 'E1 S1)))
  (accuse! g 'E2)
  (check-eq? (game-status g) 'lost)
  (check-equal? (game-ap g) 4)
  ;; 指认也要付费（按轮计数）且付不起时直接失败
  (define g2 (new-game cd #:costs UNIT-COSTS #:budget 1))
  (ask! g2 'E1 'E1 S1)
  (check-eq? (game-status g2) 'lost))
