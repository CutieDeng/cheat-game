#lang racket/base
;; 引擎：游戏状态、合法动作枚举、规则策略、压力与强度、受控遗忘、视图组装
(require racket/list racket/match "kb.rkt")
(provide (all-defined-out))

;; ---------------------------------------------------------------- 参数

;; 行动点（AP）：每个玩家动作按种类消耗行动点，预算用完结束。
;; 预算缺省为案件回合上限 (case-data-turn-limit cd)，缺省 max(30, 6 × NPC 数)；new-game 可用 #:budget 覆盖。
;; 缺省费用 DEFAULT-COSTS：提问 1、出示 2、指认 0（2026-11-25 按 ap-v1 标定选定，见 design.md「行动点」）。
;; UNIT-COSTS 全为 1：AP 等于轮数，与按轮计数的旧规则完全相同，用于和旧记录对照。
;; 对质阶段从预算的 2/3 开始（30 点时为第 20 点）
(define DEFAULT-COSTS (hasheq 'ask 1 'present 2 'accuse 0))
(define UNIT-COSTS (hasheq 'ask 1 'present 1 'accuse 1))
(define SPEC-COSTS DEFAULT-COSTS)
(define PHASE-BONUS 20)
(define FORGET-THRESHOLD 60)
(define P-CONFLICT-NPC 25)
(define P-CONFLICT-TOPIC 35)
(define P-REPEAT-NPC 5)
(define P-REPEAT-TOPIC 15)
;; 正压力与负压力是两个量，互不抵消（用户决定，2026-11-25）。
;; 正压力（p-npc / p-topic）只增不减，决定强度和说漏嘴。
;; 负压力（neg，按 NPC 累计，无上限、不衰减）不影响强度，只让玩家对这个 NPC 的提问和出示多付行动点：
;;   附加费 = floor(负压力 / NEG-PER-AP)。
;; 同一问题第 2–4 次提问加正压力（保留"连续回避第 3 次说漏嘴"和"被拆穿一次后再问 3 次说漏嘴"）；
;; 第 REPEAT-FREE+2（=5）次起不再加正压力，改加负压力 NEG-REPEAT × (r − REPEAT-FREE)，每多问一次增大。
;; 没有冲突或回避时，只靠重复提问最高到强度 30（对质阶段 50），达不到遗忘阈值。
(define REPEAT-FREE 3)
(define NEG-REPEAT 5)
(define NEG-NO-EFFECT 10)   ; 出示无效（对方没有与之矛盾的说法）
(define NEG-PER-AP 10)
(define (repeat-deltas r)   ; r = 此前已问过的次数（≥1）。返回 (values 正-npc 正-话题 负-npc)
  (if (<= r REPEAT-FREE)
      (values P-REPEAT-NPC P-REPEAT-TOPIC 0)
      (values 0 0 (* NEG-REPEAT (- r REPEAT-FREE)))))
;; 问到自己的行踪却回避（evade）：不形成承诺、无从拆穿，但侦探会更起疑。
;; 同一话题连续回避时，第 3 次提问的强度达到遗忘阈值，掩护说法被遗忘，只能说漏嘴。
(define P-EVADE-NPC 15)
(define P-EVADE-TOPIC 30)
(define VIEW-K 8)
(define VIEW-RECENT 6)
(define LIES-TO-WIN 2)

;; ---------------------------------------------------------------- 结构

;; speaker: NPC 符号。kind: cover truth slip retract confess
(struct claim (id turn speaker prop kind) #:transparent)
;; own: 该 NPC 的承诺（claim）；other: 证物符号或另一条 claim
(struct conflict (npc own other type from to) #:transparent)
;; 合法动作。props 是要说出的命题。
;; exposing?：说出会暴露自己当时的位置（NPC 自己能判断）
;; refutable?：与证物、其他 NPC 的观察或被说到的人的真实行踪矛盾（隐藏信息，只用于评测，不给模型看）
(struct action (kind props exposing? refutable?) #:transparent)
(define (act kind props exposing? [refutable? #f]) (action kind props exposing? refutable?))
;; 返回给 UI 的结果
(struct response (kind claims) #:transparent)

(struct game (cd rng
              [turn #:mutable] [claims #:mutable] [next-id #:mutable]
              [ap #:mutable]   ; 已用行动点
              costs            ; 动作种类 -> 行动点
              budget           ; 行动点预算
              commits          ; npc -> (listof claim)，当前仍坚持的声明
              layer            ; npc -> 当前掩护层下标；= 层数 时表示已回到真相
              p-npc            ; npc -> 压力
              p-topic          ; (list npc 主体 时间槽) -> 压力
              neg              ; npc -> 负压力（无上限，不衰减，不抵消正压力）
              asked            ; (list npc 主体 时间槽) -> 被问次数
              forgotten        ; 掩护命题 id -> #t
              degraded         ; 观察 id -> #t
              processed        ; (list npc 主体 时间槽) -> #t，已做过遗忘判定
              [exposed #:mutable]
              [history #:mutable]     ; 新的在前：(list 动作后已用AP npc q kind props)
              [status #:mutable]      ; playing accuse-only won lost；accuse-only：只剩指认可做
              [view-sizes #:mutable]  ; 新的在前
              [forget-log #:mutable]  ; 新的在前
              policy
              ext?))          ; #t：开启扩展动作（partial / fabricate / frame / withdraw）

(define (new-game cd #:seed [seed 1] #:policy [policy rule-policy] #:ext? [ext? #f]
                  #:costs [costs DEFAULT-COSTS] #:budget [budget (case-data-turn-limit cd)])
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
  (define npcs (case-data-npcs cd))
  (game cd rng 0 '() 1 0 costs budget
        (make-hasheq (for/list ([n npcs]) (cons n '())))
        (make-hasheq (for/list ([n npcs]) (cons n 0)))
        (make-hasheq) (make-hash) (make-hasheq) (make-hash) (make-hash) (make-hash) (make-hash)
        '() '() 'playing '() '() policy ext?))

;; ---------------------------------------------------------------- 压力与强度

(define (hadd! h k delta) (hash-set! h k (max 0 (+ (hash-ref h k 0) delta))))
;; turn-limit / phase-turn 保留原名，单位已是行动点
(define (turn-limit g) (game-budget g))
(define (phase-turn g) (floor (* 2/3 (game-budget g))))
(define (phase g) (if (>= (game-ap g) (phase-turn g)) 'confront 'normal))
;; 负压力附加费：对该 NPC 提问和出示时多付的行动点
(define (surcharge g npc) (floor (/ (hash-ref (game-neg g) npc 0) NEG-PER-AP)))
;; npc 为 #f 时按附加费最低的 NPC 计（"对某个人还付得起"）
(define (cost g kind [npc #f])
  (+ (hash-ref (game-costs g) kind)
     (if (memq kind '(ask present))
         (if npc (surcharge g npc) (apply min (map (λ (n) (surcharge g n)) (case-data-npcs (game-cd g)))))
         0)))
(define (ap-left g) (- (game-budget g) (game-ap g)))
(define (affordable? g kind [npc #f]) (<= (cost g kind npc) (ap-left g)))
(define (active? g) (and (memq (game-status g) '(playing accuse-only)) #t))
(define (intensity g npc subj slot)
  (min 100 (+ (* 1/2 (hash-ref (game-p-npc g) npc 0))
              (* 1/2 (hash-ref (game-p-topic g) (list npc subj slot) 0))
              (if (eq? (phase g) 'confront) PHASE-BONUS 0))))

;; ---------------------------------------------------------------- 掩护层与知识

(define (cover-layers g npc) (hash-ref (case-data-cover (game-cd g)) npc '()))
(define (layer-of g npc) (hash-ref (game-layer g) npc))
(define (at-truth-layer? g npc) (>= (layer-of g npc) (length (cover-layers g npc))))
(define (forgotten? g id) (hash-ref (game-forgotten g) id #f))
(define (self-truth g npc) (truth-of (game-cd g) npc))

;; 当前掩护层：(listof (cons id st))，id = (list 'cover npc 层 序号)
(define (current-cover g npc)
  (define l (layer-of g npc))
  (define layers (cover-layers g npc))
  (if (< l (length layers))
      (for/list ([s (list-ref layers l)] [j (in-naturals)]) (cons (list 'cover npc l j) s))
      '()))

;; NPC 当前对自己行踪的说法（回到真相层时就是真相）
(define (self-story g npc)
  (if (at-truth-layer? g npc) (self-truth g npc) (map cdr (current-cover g npc))))

;; NPC 对 subj 在 [f,t) 的知识：(listof (list prop via))
(define (known-about g npc subj f t)
  (define cd (game-cd g))
  (if (eq? npc subj)
      (for*/list ([s (self-truth g npc)] [c (in-value (clip s f t))] #:when c)
        (list (prop 'pos c 'exact) #f))
      (for*/list ([k (case-data-knowledge cd)]
                  #:when (st? (prop-st (kitem-prop k)))
                  #:when (eq? (kitem-owner k) npc)
                  #:when (eq? (st-agent (prop-st (kitem-prop k))) subj)
                  [c (in-value (clip (prop-st (kitem-prop k)) f t))] #:when c)
        (define pol (prop-pol (kitem-prop k)))
        (if (hash-ref (game-degraded g) (kitem-id k) #f)
            (list (prop pol (widen-to-slots cd c) 'approx) (kitem-via k))
            (list (prop pol c 'exact) (kitem-via k))))))

(define (commit-props g npc) (map claim-prop (hash-ref (game-commits g) npc)))
(define (consistent? g npc props)
  (define cs (commit-props g npc))
  (for*/and ([p props] [c cs]) (not (conflict-kind p c))))

;; 说出这条观察是否会暴露自己当时的位置
(define (exposing? g npc via s)
  (and via
       (let ([me (prop 'pos (st npc via (st-from s) (st-to s)) 'exact)])
         (or (not (consistent? g npc (list me)))
             (for/or ([c (self-story g npc)]) (and (conflict-kind me (prop 'pos c 'exact)) #t))))))

;; 编造的说法能否被拆穿：与证物、其他 NPC 的观察、或被说到的人的真实行踪矛盾
(define (refutable? g npc p)
  (define cd (game-cd g))
  (define a (prop-agent p))
  (or (for/or ([e (case-data-evidence cd)]) (and (conflict-kind p (third e)) #t))
      (for/or ([k (case-data-knowledge cd)] #:unless (eq? (kitem-owner k) npc))
        (and (conflict-kind p (kitem-prop k)) #t))
      (and (not (eq? a npc))
           (for/or ([x (truth-of cd a)]) (and (conflict-kind p (prop 'pos x 'exact)) #t)))))

;; ---------------------------------------------------------------- 合法动作

;; slot 为 'rel 时问的是某人与被害人的关系
(define (legal-answers g npc subj slot)
  (if (eq? slot 'rel) (legal-rel-answers g npc subj) (legal-at-answers g npc subj slot)))

;; 仍在否认的关系：准备否认的，减去否认已被拆穿的
(define (hiding g npc)
  (for/list ([k (hash-ref (case-data-rel-cover (game-cd g)) npc '())]
             #:unless (for/or ([e (game-exposed g)])
                        (and (eq? (conflict-npc e) npc)
                             (equal? (prop-st (claim-prop (conflict-own e))) (rl npc k)))))
    k))

(define (liar? g npc)
  (or (pair? (cover-layers g npc)) (pair? (hash-ref (case-data-rel-cover (game-cd g)) npc '()))))

(define (legal-rel-answers g npc subj)
  (define cd (game-cd g))
  (define self? (eq? npc subj))
  (define hidden (if self? (hiding g npc) '()))
  (define truth-props
    (if self?
        (for/list ([k (rels-of cd npc)]) (prop 'pos (rl npc k) 'exact))
        (remove-duplicates
         (for/list ([k (case-data-knowledge cd)]
                    #:when (rl? (prop-st (kitem-prop k)))
                    #:when (eq? (kitem-owner k) npc)
                    #:when (eq? (rl-agent (prop-st (kitem-prop k))) subj))
           (kitem-prop k)))))
  (define (hidden? p) (and (memq (rl-kind (prop-st p)) hidden) #t))
  (define exposing (ormap hidden? truth-props))
  (define safe (filter (λ (p) (not (hidden? p))) truth-props))
  (define deny (for/list ([k hidden]) (prop 'neg (rl npc k) 'exact)))
  (append
   (if (and (pair? deny) (consistent? g npc deny))
       (list (act 'cover deny #f (ormap (λ (p) (refutable? g npc p)) deny)))
       '())
   (if (and (pair? truth-props) (consistent? g npc truth-props))
       (list (act 'truth truth-props exposing))
       '())
   (if (and (game-ext? g) self? exposing (pair? safe) (consistent? g npc safe))
       (list (act 'partial safe #f))
       '())
   (if (and (game-ext? g) (not self?) (liar? g npc))
       (for*/list ([k REL-KINDS]
                   [p (in-value (prop 'pos (rl subj k) 'exact))]
                   #:unless (prop-true? cd p)
                   #:when (consistent? g npc (list p)))
         (act 'frame (list p) #f (refutable? g npc p)))
       '())
   ;; 问自己：确实没有关系才能说"没什么瓜葛"（不形成承诺）；有要隐瞒的关系就只能否认或说出。
   ;; 问别人：说不清楚
   (if self?
       (if (and (null? safe) (null? hidden)) (list (act 'none '() #f)) '())
       (list (act 'unknown '() #f)))))

(define (legal-at-answers g npc subj slot)
  (define-values (f t) (slot-range (game-cd g) slot))
  (define self? (eq? npc subj))
  (define known (known-about g npc subj f t))
  (define truth-props (map first known))
  (define lying? (and self? (not (at-truth-layer? g npc))))
  (define cover-items
    (if lying?
        (filter (λ (c) (> (overlap-range (cdr c) f t) 0)) (current-cover g npc))
        '()))
  (define cover-props
    (for/list ([c cover-items] #:unless (forgotten? g (car c)))
      (prop 'pos (clip (cdr c) f t) 'exact)))
  (define slip? (ormap (λ (c) (forgotten? g (car c))) cover-items))
  ;; 撒谎者：真话与当前掩护层（含已遗忘部分）在本时间槽内有冲突才算暴露
  (define (prop-exposing? k)
    (if lying?
        (for/or ([c cover-items]) (and (conflict-kind (first k) (prop 'pos (clip (cdr c) f t) 'exact)) #t))
        (exposing? g npc (second k) (prop-st (first k)))))
  (define exposing-flags (map prop-exposing? known))
  (define truth-exposing? (ormap values exposing-flags))
  (define safe-props (for/list ([k known] [e exposing-flags] #:unless e) (first k)))
  (if slip?
      ;; 掩护说法已被遗忘：只能说出实情
      (list (act 'slip truth-props #f))
  (append
   (if (and lying? (pair? cover-props) (consistent? g npc cover-props))
       (list (act 'cover cover-props #f (ormap (λ (p) (refutable? g npc p)) cover-props)))
       '())
   (if (and (pair? truth-props) (consistent? g npc truth-props))
       (list (act 'truth truth-props truth-exposing?))
       '())
   (if (game-ext? g)
       (append
        ;; 只说不暴露自己的那部分真话
        (if (and truth-exposing? (pair? safe-props) (< (length safe-props) (length truth-props))
                 (consistent? g npc safe-props))
            (list (act 'partial safe-props #f))
            '())
        ;; 临场编造：有秘密的人才会编。问自己 → fabricate；问别人 → frame
        (if (pair? (cover-layers g npc))
            (for*/list ([l (sort (hash-keys (case-data-locations (game-cd g))) symbol<?)]
                        [p (in-value (prop 'pos (st subj l f t) 'exact))]
                        #:unless (prop-true? (game-cd g) p)
                        #:unless (equal? (list p) cover-props)
                        #:when (consistent? g npc (list p)))
              (act (if self? 'fabricate 'frame) (list p) #f (refutable? g npc p)))
            '()))
       '())
   ;; 问自己行踪时说不清楚 = 回避，有压力代价；问别人时说不清楚没有代价
   (list (act (if self? 'evade 'unknown) '() #f)))))

(define (legal-reactions g npc found)
  (define cd (game-cd g))
  (define false-owns
    (for/list ([c found] #:unless (prop-true? cd (claim-prop (conflict-own c)))) (conflict-own c)))
  (define own-false? (pair? false-owns))
  ;; 被拆穿的是准备好的说法 → 改口换下一版；只是临场编的 → 收回
  (define layered? (for/or ([o false-owns])
                     (and (memq (claim-kind o) '(cover retract)) (st? (prop-st (claim-prop o))) #t)))
  (define n (length (cover-layers g npc)))
  (define l (layer-of g npc))
  (define can-retract?
    (and layered? (< l n)
         (or (< (add1 l) n) (not (eq? npc (case-data-culprit cd))))))
  (append (if own-false? '() (list (act 'insist '() #f)))
          (if can-retract? (list (act 'retract '() #f)) '())
          (if (and own-false? (not layered?)) (list (act 'withdraw '() #f)) '())
          (list (act 'refuse '() #f))))

;; ---------------------------------------------------------------- 规则策略

;; 策略接口：(policy view actions) -> action，只能从 actions 中选
(define PRIORITY '(slip cover truth none unknown evade insist retract withdraw refuse))
(define (rule-policy view actions)
  (define ok (filter (λ (a) (not (and (eq? (action-kind a) 'truth) (action-exposing? a))))
                     actions))
  (for/or ([k PRIORITY]) (findf (λ (a) (eq? (action-kind a) k)) ok)))

;; ---------------------------------------------------------------- 遗忘

(define (revealed? g npc k)
  (define kp (kitem-prop k))
  (for/or ([c (game-claims g)])
    (define p (claim-prop c))
    (and (eq? (claim-speaker c) npc)
         (st? (prop-st p))
         (eq? (prop-pol p) (prop-pol kp))
         (eq? (st-agent (prop-st p)) (st-agent (prop-st kp)))
         (eq? (st-loc (prop-st p)) (st-loc (prop-st kp)))
         (overlap? (prop-st p) (prop-st kp)))))

;; cands: (listof (cons x 重叠长度))；取最长，并列时用 RNG
(define (pick-max g cands)
  (define best (apply max (map cdr cands)))
  (define ties (filter (λ (c) (= (cdr c) best)) cands))
  (car (list-ref ties (random (length ties) (game-rng g)))))

(define (maybe-forget! g npc subj slot)
  (define key (list npc subj slot))
  (when (and (not (eq? slot 'rel))
             (>= (intensity g npc subj slot) FORGET-THRESHOLD)
             (not (hash-ref (game-processed g) key #f)))
    (hash-set! (game-processed g) key #t)
    (define-values (f t) (slot-range (game-cd g) slot))
    (define cover-cands
      (if (eq? npc subj)
          (for/list ([c (current-cover g npc)]
                     #:unless (forgotten? g (car c))
                     #:when (> (overlap-range (cdr c) f t) 0))
            (cons c (overlap-range (cdr c) f t)))
          '()))
    (define obs-cands
      (for/list ([k (case-data-knowledge (game-cd g))]
                 #:when (st? (prop-st (kitem-prop k)))
                 #:when (eq? (kitem-owner k) npc)
                 #:when (eq? (st-agent (prop-st (kitem-prop k))) subj)
                 #:when (> (overlap-range (prop-st (kitem-prop k)) f t) 0)
                 #:unless (hash-ref (game-degraded g) (kitem-id k) #f)
                 #:when (or (not (kitem-critical? k)) (revealed? g npc k)))
        (cons k (overlap-range (prop-st (kitem-prop k)) f t))))
    (cond
      [(pair? cover-cands)
       (define c (pick-max g cover-cands))
       (hash-set! (game-forgotten g) (car c) #t)
       (set-game-forget-log! g (cons (list (game-ap g) 'forget npc (car c)) (game-forget-log g)))]
      [(pair? obs-cands)
       (define k (pick-max g obs-cands))
       (hash-set! (game-degraded g) (kitem-id k) #t)
       (set-game-forget-log! g (cons (list (game-ap g) 'degrade npc (kitem-id k)) (game-forget-log g)))])))

;; ---------------------------------------------------------------- 内部操作

(define (new-claim! g speaker p kind)
  (define c (claim (game-next-id g) (game-turn g) speaker p kind))
  (set-game-next-id! g (add1 (game-next-id g)))
  (set-game-claims! g (cons c (game-claims g)))
  c)
;; 命题相同的声明只保留最早那条
(define (commit! g npc c)
  (hash-update! (game-commits g) npc
                (λ (cs) (if (for/or ([o cs]) (equal? (claim-prop o) (claim-prop c)))
                            cs
                            (append cs (list c))))))
(define (find-claim g id) (findf (λ (c) (= (claim-id c) id)) (game-claims g)))

(define (item-props g item)
  (if (symbol? item)
      (match (assq item (case-data-evidence (game-cd g)))
        [(list _ _ p) (list p)]
        [#f (error 'item-props "未知证物 ~a" item)])
      (list (claim-prop (find-claim g item)))))

(define (push-history! g npc q kind props)
  (set-game-history! g (cons (list (game-ap g) npc q kind props) (game-history g))))

(define (start-turn! g kind npc)
  (unless (or (eq? (game-status g) 'playing)
              (and (eq? kind 'accuse) (eq? (game-status g) 'accuse-only)))
    (error 'engine "游戏已结束"))
  (unless (affordable? g kind npc)
    (error 'engine "行动点不足：~a ~a 需要 ~a，剩 ~a" kind npc (cost g kind npc) (ap-left g)))
  (set-game-turn! g (add1 (game-turn g)))
  (set-game-ap! g (+ (game-ap g) (cost g kind npc))))
;; 提问和出示都付不起时结束：指认仍付得起则只剩指认（accuse-only），否则失败
(define (end-turn! g)
  (when (and (eq? (game-status g) 'playing)
             (not (affordable? g 'ask)) (not (affordable? g 'present)))
    (set-game-status! g (if (affordable? g 'accuse) 'accuse-only 'lost))))

;; 冲突带来的话题压力，并做遗忘判定
(define (press-conflicts! g npc found)
  (hadd! (game-p-npc g) npc P-CONFLICT-NPC)
  (define topics
    (remove-duplicates
     (for*/list ([c found]
                 [slot (if (conflict-from c)
                           (slots-overlapping (game-cd g) (conflict-from c) (conflict-to c))
                           '(rel))])
       (list npc (prop-agent (claim-prop (conflict-own c))) slot))))
  (for ([k topics]) (hadd! (game-p-topic g) k P-CONFLICT-TOPIC))
  (for ([k topics]) (apply maybe-forget! g k)))

(define (record-view! g npc q)
  (define v (assemble-view g npc q))
  (set-game-view-sizes! g (cons (token-count v) (game-view-sizes g)))
  v)

;; ---------------------------------------------------------------- 玩家动作

(define (ask! g npc subj slot)
  (start-turn! g 'ask npc)
  (define key (list npc subj slot))
  (define r (hash-ref (game-asked g) key 0))
  (when (> r 0)
    (define-values (dn dt dneg) (repeat-deltas r))
    (hadd! (game-p-npc g) npc dn)
    (hadd! (game-p-topic g) key dt)
    (hadd! (game-neg g) npc dneg))
  (hash-update! (game-asked g) key add1 0)
  (maybe-forget! g npc subj slot)
  (define q (list 'ask subj slot))
  (define view (record-view! g npc q))
  (define a ((game-policy g) view (legal-answers g npc subj slot)))
  (define cs (for/list ([p (action-props a)]) (new-claim! g npc p (action-kind a))))
  (case (action-kind a)
    [(cover truth partial fabricate frame) (for ([c cs]) (commit! g npc c))]
    [(slip) (register-slip! g npc cs)]
    [(evade) (hadd! (game-p-npc g) npc P-EVADE-NPC)
             (hadd! (game-p-topic g) key P-EVADE-TOPIC)])
  (push-history! g npc q (action-kind a) (action-props a))
  (end-turn! g)
  (response (action-kind a) cs))

;; 说漏嘴：与自身承诺冲突的部分自动计为被揭穿
(define (register-slip! g npc cs)
  (define found
    (for*/list ([o (hash-ref (game-commits g) npc)]
                [c cs]
                [k (in-value (conflict-kind (claim-prop o) (claim-prop c)))] #:when k
                #:unless (for/or ([e (game-exposed g)])
                           (and (eq? (conflict-own e) o) (claim? (conflict-other e))
                                (eq? (claim-kind (conflict-other e)) 'slip))))
      (define-values (f t) (overlap-bounds (prop-st (claim-prop o)) (prop-st (claim-prop c))))
      (conflict npc o c k f t)))
  ;; 同一条承诺只记一次
  (define dedup (remove-duplicates found (λ (a b) (eq? (conflict-own a) (conflict-own b)))))
  (when (pair? dedup)
    (set-game-exposed! g (append (game-exposed g) dedup))
    (press-conflicts! g npc dedup)))

(define (present! g npc item)
  (start-turn! g 'present npc)
  (define props (item-props g item))
  (define raw
    (for*/list ([o (hash-ref (game-commits g) npc)] [p props]
                [k (in-value (conflict-kind (claim-prop o) p))] #:when k)
      (define-values (f t)
        (if (rl? (prop-st p)) (values #f #f) (overlap-bounds (prop-st (claim-prop o)) (prop-st p))))
      (conflict npc o item k f t)))
  (define found
    (filter (λ (c) (not (for/or ([e (game-exposed g)])
                          (and (eq? (conflict-own e) (conflict-own c))
                               (equal? (conflict-other e) item)))))
            raw))
  (define q (list 'present item))
  (define result
    (cond
      [(and (null? found) (pair? raw)) (response 'already '())]
      [(null? found)
       (hadd! (game-neg g) npc NEG-NO-EFFECT)
       (response 'no-effect '())]
      [else
       (set-game-exposed! g (append (game-exposed g) found))
       (press-conflicts! g npc found)
       (define view (record-view! g npc q))
       (define a ((game-policy g) view (legal-reactions g npc found)))
       (case (action-kind a)
         [(retract) (response 'retract (do-retract! g npc found))]
         [(withdraw) (do-withdraw! g npc found) (response 'withdraw '())]
         [else (response (action-kind a) '())])]))
  (push-history! g npc q (response-kind result) (map claim-prop (response-claims result)))
  (end-turn! g)
  result)

;; 改口：进入下一层，撤回与新说法冲突的承诺，说出新说法在冲突时间槽内的部分
(define (do-retract! g npc found)
  (define cd (game-cd g))
  (hash-update! (game-layer g) npc add1)
  (define story (for/list ([s (self-story g npc)]) (prop 'pos s 'exact)))
  (hash-update! (game-commits g) npc
                (λ (cs) (filter (λ (c) (not (for/or ([p story]) (conflict-kind (claim-prop c) p))))
                                cs)))
  (define slots
    (remove-duplicates
     (append-map (λ (c) (slots-overlapping cd (conflict-from c) (conflict-to c)))
                 (filter conflict-from found))))
  (define-values (f t)
    (values (apply min (for/list ([i slots]) (let-values ([(a b) (slot-range cd i)]) a)))
            (apply max (for/list ([i slots]) (let-values ([(a b) (slot-range cd i)]) b)))))
  (for/list ([p story]
             #:when (> (overlap-range (prop-st p) f t) 0))
    (define c (new-claim! g npc (prop 'pos (clip (prop-st p) f t) 'exact) 'retract))
    (commit! g npc c)
    c))

;; 收回临场编的说法：撤掉被拆穿的那几条承诺，不换掩护层
(define (do-withdraw! g npc found)
  (define bad (for/list ([c found] #:unless (prop-true? (game-cd g) (claim-prop (conflict-own c)))) (conflict-own c)))
  (hash-update! (game-commits g) npc (λ (cs) (filter (λ (c) (not (memq c bad))) cs))))

;; 同一条说法被多件东西拆穿只算一次
(define (exposed-lies g npc)
  (remove-duplicates
   ;; 只算行踪上的谎言：关系（动机）只作线索，不进定罪条件
   (for/list ([c (game-exposed g)]
              #:when (eq? (conflict-npc c) npc)
              #:when (st? (prop-st (claim-prop (conflict-own c))))
              #:unless (prop-true? (game-cd g) (claim-prop (conflict-own c))))
     c)
   (λ (a b) (eq? (conflict-own a) (conflict-own b)))))

;; 凶手自己说过：案发时段在作案地点（含说漏嘴；不含认罪本身）
(define (admitted? g npc)
  (define c (case-data-crime (game-cd g)))
  (and (eq? npc (st-agent c))
       (for/or ([cl (game-claims g)])
         (define p (claim-prop cl))
         (and (eq? (claim-speaker cl) npc)
              (st? (prop-st p))
              (not (eq? (claim-kind cl) 'confess))
              (eq? (prop-pol p) 'pos) (eq? (prop-mode p) 'exact)
              (eq? (st-agent (prop-st p)) npc)
              (eq? (st-loc (prop-st p)) (st-loc c))
              (overlap? (prop-st p) c)))))

;; 定罪条件：被揭穿的谎言达到 LIES-TO-WIN 条，或凶手自己承认案发时在作案地点
(define (convictable? g npc)
  (or (>= (length (exposed-lies g npc)) LIES-TO-WIN)
      (admitted? g npc)))

(define (accuse! g npc)
  (start-turn! g 'accuse npc)
  (define cd (game-cd g))
  (define culprit (case-data-culprit cd))
  (define result
    (cond
      [(and (eq? npc culprit) (convictable? g npc))
       (set-game-status! g 'won)
       (response 'confess
                 (for/list ([s (self-truth g npc)])
                   (new-claim! g npc (prop 'pos s 'exact) 'confess)))]
      [(eq? npc culprit)
       (set-game-status! g 'lost)
       (response 'insufficient '())]
      [else
       (set-game-status! g 'lost)
       (response 'wrong '())]))
  (push-history! g npc (list 'accuse) (response-kind result) '())
  result)

;; ---------------------------------------------------------------- 视图组装

(define (prop->sexp p)
  (define s (prop-st p))
  (if (rl? s)
      `(,(prop-pol p) rel ,(rl-agent s) ,(rl-kind s) ,(prop-mode p))
      (at->sexp p)))

(define (at->sexp p)
  (define s (prop-st p))
  `(,(prop-pol p) ,(st-agent s) ,(st-loc s)
    ,(quotient (st-from s) 60) ,(remainder (st-from s) 60)
    ,(quotient (st-to s) 60) ,(remainder (st-to s) 60)
    ,(prop-mode p)))

(define (take-up-to lst k) (take lst (min k (length lst))))

(define (assemble-view g npc q)
  (define cd (game-cd g))
  (define-values (subj slot)
    (match q
      [(list 'ask s sl) (values s sl)]
      [(list 'present item)
       (define s (prop-st (car (item-props g item))))
       (if (rl? s)
           (values (rl-agent s) 'rel)
           (values (st-agent s) (car (slots-overlapping cd (st-from s) (st-to s)))))]))
  (define-values (f t) (if (eq? slot 'rel) (values (car (case-data-window cd)) (cdr (case-data-window cd))) (slot-range cd slot)))
  (define commits (hash-ref (game-commits g) npc))
  (define (on-topic? c)
    (define s (prop-st (claim-prop c)))
    (if (eq? slot 'rel)
        (and (rl? s) (eq? (rl-agent s) subj))
        (and (st? s) (eq? (st-agent s) subj) (> (overlap-range s f t) 0))))
  (define (score c)
    (define s (prop-st (claim-prop c)))
    (+ (if (eq? (prop-agent (claim-prop c)) subj) 2 0)
       (if (and (st? s) (> (overlap-range s f t) 0)) 1 0)
       (/ (claim-turn c) 1000)))
  (define ranked
    (take-up-to (sort (filter (λ (c) (not (on-topic? c))) commits) > #:key score) VIEW-K))
  (define presented
    (remove-duplicates
     (for*/list ([h (game-history g)]
                 #:when (eq? (second h) npc)
                 #:when (eq? (car (third h)) 'present)
                 [p (item-props g (cadr (third h)))])
       (prop->sexp p))))
  (define recent
    (take-up-to (filter (λ (h) (eq? (second h) npc)) (game-history g)) VIEW-RECENT))
  (define role
    (cond [(eq? npc (case-data-culprit cd)) 'culprit]
          [(liar? g npc) 'secret]
          [else 'honest]))
  `(view
    (self ,npc) (role ,role) (layer ,(layer-of g npc))
    (prefix (truth ,@(map (λ (s) (prop->sexp (prop 'pos s 'exact))) (self-truth g npc)))
            (cover ,@(for/list ([layer (cover-layers g npc)])
                       (map (λ (s) (prop->sexp (prop 'pos s 'exact))) layer)))
            (rels ,@(rels-of cd npc))
            (hide ,@(hash-ref (case-data-rel-cover cd) npc '()))
            ;; 自己掌握的观察：(命题 观察时所在地点)；已模糊的记忆显示为 approx
            (know ,@(for/list ([k (case-data-knowledge cd)] #:when (eq? (kitem-owner k) npc))
                      (define p (kitem-prop k))
                      (define p* (if (and (st? (prop-st p)) (hash-ref (game-degraded g) (kitem-id k) #f))
                                     (prop (prop-pol p) (widen-to-slots cd (prop-st p)) 'approx)
                                     p))
                      (list (prop->sexp p*) (kitem-via k))))
            ;; 视线（单向），同一地点总是互相看得见
            (sight ,@(for/list ([v (case-data-views cd)]) (list (car v) (cdr v))))
            ,@(if (eq? role 'culprit)
                  `((crime ,(prop->sexp (prop 'pos (case-data-crime cd) 'exact))))
                  '()))
    (flags (intensity ,(round (intensity g npc subj slot)))
           (repeat ,(hash-ref (game-asked g) (list npc subj slot) 0))
           (phase ,(phase g)))
    (pinned
     (conflicts ,@(for/list ([c (game-exposed g)] #:when (eq? (conflict-npc c) npc))
                    `(,(conflict-type c) ,(prop->sexp (claim-prop (conflict-own c))))))
     (presented ,@presented)
     (commits ,@(map (λ (c) (prop->sexp (claim-prop c))) (filter on-topic? commits))))
    (ranked ,@(map (λ (c) (prop->sexp (claim-prop c))) ranked))
    (recent ,@(for/list ([h recent]) `(,(car (third h)) ,(fourth h) ,@(map prop->sexp (fifth h)))))
    (q ,@(match q
           [(list 'present item) `(present ,@(map prop->sexp (item-props g item)))]
           [_ q]))))

(define (token-count v)
  (cond [(pair? v) (+ (token-count (car v)) (token-count (cdr v)))]
        [(null? v) 0]
        [else 1]))
