#lang racket/base
;; v1 M1 测试：地图、路径、观察推导、带容限的冲突。raco test test-v1.rkt
(require rackunit racket/list "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt" "exclude.rkt" "gen-v1.rkt"
         "engine-v1.rkt" "liar-v1.rkt" "detective-v1.rkt" "text-v1.rkt"
         "api-v1.rkt" (only-in "http-error.rkt" http-error? http-error-code) json)

(define c (load-case1 "cases/manor-v1.rktd"))
(define m (case1-map c))
(define t0 (case1-t0 c))
(define t1 (case1-t1 c))
(define H hhmm->min)
(define (truth who) (findf (λ (p) (eq? (path-who p) who)) (case1-paths c)))
(define obs-sexps (map obs->sexp (case1-observations c)))
(define (C speaker d #:source [src 'testimony]) (datum->claim 'x speaker d #:source src))
(define (conf who a b) (conflict-type m t0 t1 who a b))

;; ---------------------------------------------------------------- 地图

(test-case "最短路与视线"
  (check-equal? (dist m 'kitchen 'garden) 3)
  (check-equal? (dist m 'study 'garden) 3)
  (check-equal? (dist m 'kitchen 'kitchen) 0)
  (check-true (door-visible? m 'hall 'study))
  (check-false (door-visible? m 'hall 'garden))      ; 花园是 outdoor，没有门
  (check-false (door-visible? m 'kitchen 'hall))     ; 只有走廊能看门口
  (check-true (sees-inside? m 'hall 'hall))
  (check-false (sees-inside? m 'hall 'study))
  (define m2 (datum->map '(map (node a room "甲") (node b outdoor "乙") (edge a b 2) (window b a))))
  (check-true (sees-inside? m2 'b 'a))
  (check-false (sees-inside? m2 'a 'b)))

(test-case "地图校验：未知节点、不连通"
  (check-exn exn:fail? (λ () (datum->map '(map (node a room "甲") (edge a z 1)))))
  (check-exn exn:fail? (λ () (datum->map '(map (node a room "甲") (node b room "乙"))))))

;; ---------------------------------------------------------------- 路径与案件

(test-case "庄园地图版静态校验通过"
  (check-equal? (case1-errors c) '()))

(test-case "路径校验：不相邻、通行时间不符、没覆盖时间窗"
  (check-pred pair? (path-errors m (datum->path '(path X (kitchen 1930 2000) (study 2001 2100))) t0 t1))
  (check-pred pair? (path-errors m (datum->path '(path X (kitchen 1930 2000) (hall 2005 2100))) t0 t1))
  (check-pred pair? (path-errors m (datum->path '(path X (kitchen 1940 2100))) t0 t1))
  (check-equal? (path-errors m (truth 'E1) t0 t1) '()))

(test-case "位置：在路上的分钟不在任何节点"
  (check-eq? (at-node (truth 'E3) (H 1957)) 'garden)
  (check-false (at-node (truth 'E3) (H 1958)))       ; 花园 → 走廊 2 分钟，1958 在路上
  (check-eq? (at-node (truth 'E3) (H 1959)) 'hall))

;; ---------------------------------------------------------------- 观察推导

(test-case "观察：同处、门口、负观察"
  (for ([o '((saw E3 E1 hall 2000 2003)
             (saw E1 E3 hall 2000 2003)
             (saw-door E3 E1 kitchen out 1959)
             (saw-door E3 E1 study in 2004)
             (saw E2 E1 kitchen 1930 1959)
             (saw E2 E1 kitchen 2024 2100)
             (saw-only E2 kitchen 2000 2023 ())
             (saw-only E3 hall 2000 2003 (E1))
             (saw-only E3 hall 2004 2009 ())
             (doors-only E3 kitchen 1959 2009 (E1))
             (saw E3 E2 kitchen 1930 1957))])
    (check-not-false (member o obs-sexps) (format "缺少观察 ~s" o)))
  ;; 管家 20:18 出书房时园丁已在花园，没人看见
  (check-false (member '(saw-door E3 E1 study out 2018) obs-sexps))
  ;; 走廊看不见书房内部
  (check-false (for/or ([o obs-sexps]) (and (eq? (car o) 'saw) (eq? (fourth o) 'study) (eq? (second o) 'E3)))))

(test-case "真实路径满足全部观察和物证（精确与按默认容限两种）"
  (define obs (case1-observations c))
  (for* ([tol (list 0 #f)]
         [who (person-ids c)])
    (define cls (append (for/list ([o obs]) (obs->claim 'o o #:tol tol))
                        (map third (case1-evidence c))))
    (define cs (append-map (λ (cl) (constraints-on m cl who)) cls))
    (check-true (path-satisfies? m (truth who) cs) (format "~a 的真实路径不满足（tol ~a）" who tol))
    (check-true (satisfiable? m t0 t1 cs) (format "~a 的约束不可满足（tol ~a）" who tol))))

;; ---------------------------------------------------------------- 冲突（各有正反例）

(test-case "冲突：位置互斥"
  ;; 管家说一直在厨房；园丁说 20:00–20:03 在走廊看见他（关于别人，容限 5，短暂停留变成"附近到过"）
  (define cover (C 'E1 '(at E1 kitchen 1930 2100)))
  (define seen (C 'E3 '(saw E1 hall 2000 2003)))
  (check-eq? (conf 'E1 cover seen) 'location)
  ;; 反例：管家如实说 20:00–20:03 在走廊
  (check-false (conf 'E1 (C 'E1 '(at E1 hall 2000 2003)) seen)))

(test-case "冲突：来不及走到（通行时间）"
  (define m2 (datum->map '(map (node a room "甲") (node b room "乙") (edge a b 10))))
  (define (conf2 x y) (conflict-type m2 (H 1900) (H 2100) 'P x y))
  ;; 自己说 19:30–20:00 在甲（核心 19:33–19:57）；别人说 20:05–20:30 看见他在乙（容限 5，核心 20:10–20:25）：间隔 13 分钟 ≥ 10，可行
  (check-false (conf2 (C 'P '(at P a 1930 2000)) (C 'Q '(saw P b 2005 2030))))
  ;; 别人说 19:55–20:30 在乙（核心 20:00–20:25）：间隔 3 分钟 < 10，来不及
  (check-eq? (conf2 (C 'P '(at P a 1930 2000)) (C 'Q '(saw P b 1955 2030))) 'travel))

(test-case "冲突：视线（负观察）"
  ;; 厨师说 20:00–20:23 厨房里除了她没别人；管家说一直在厨房
  (define none (C 'E2 '(saw-none kitchen 2000 2023)))   ; 等同 (saw-only kitchen 2000 2023 ())
  (check-eq? (conf 'E1 (C 'E1 '(at E1 kitchen 1930 2100)) none) 'sight)
  ;; 反例：管家说 19:30–20:00 在厨房（核心到 19:57，不与 20:08 起的负观察重叠）
  (check-false (conf 'E1 (C 'E1 '(at E1 kitchen 1930 2000)) none))
  ;; 负观察不约束说话人自己
  (check-false (conf 'E2 (C 'E2 '(at E2 kitchen 1930 2100)) none)))

(test-case "冲突：视线（只看见某些人、门口进出名单）"
  ;; 负观察按容限两端各收缩 5 分钟（关于别人的容限），所以用较长的时段（与真相无关，只测规则）
  ;; 园丁说 19:40–20:30 在走廊只看见管家；厨师说自己 20:00–20:10 在走廊 → 冲突
  (define only (C 'E3 '(saw-only hall 1940 2030 (E1))))
  (check-eq? (conf 'E2 (C 'E2 '(at E2 hall 2000 2010)) only) 'sight)
  (check-false (conf 'E1 (C 'E1 '(at E1 hall 2000 2003)) only))      ; 名单里的人不受约束
  ;; 园丁说 19:40–20:30 看着厨房门口，只有管家进出；厨师说 20:00 前在厨房、20:05 后在书房 → 必须出厨房门
  (define doors (C 'E3 '(doors-only kitchen 1940 2030 (E1))))   ; 收缩后 19:48–20:22，覆盖她最早能出门的 19:57
  (define kit (C 'E2 '(at E2 kitchen 1930 2000)))
  (define stu (C 'E2 '(at E2 study 2005 2020)))
  (check-true (person-satisfiable? m t0 t1 'E2 (list kit stu)))
  (check-false (person-satisfiable? m t0 t1 'E2 (list kit stu doors))))

(test-case "冲突：物证"
  (define ev (third (assq 'EV1 (case1-evidence c))))
  (check-eq? (conf 'E3 (C 'E3 '(at E3 garden 1930 2100)) ev) 'evidence)
  (check-false (conf 'E3 (C 'E3 '(at E3 hall 1959 2009)) ev)))

(test-case "冲突：经过必经点被看见（路径推理）"
  ;; 管家说 20:00 前在厨房、20:05 后在书房：从厨房到书房必经走廊。
  ;; 园丁说 19:59–20:09 走廊里一直没人（与真相不符，只为测试）→ 他不可能从厨房走到书房
  (define kit (C 'E1 '(at E1 kitchen 1930 2000)))
  (define stu (C 'E1 '(at E1 study 2005 2020)))
  (define none (claim 'n 'E3 (list 'saw-only 'hall (H 1955) (H 2012) '()) 0 'testimony))
  (check-true (person-satisfiable? m t0 t1 'E1 (list kit stu)))
  (check-false (person-satisfiable? m t0 t1 'E1 (list kit stu none))))

;; ---------------------------------------------------------------- 作案机会

(test-case "排除法（精确观察）：只用第三方观察和物证，厨师和园丁被排除，管家没有"
  ;; 对 Y：用说话人既不是 Y 也不是凶手的观察（精确，容限 0）和物证
  (define obs (case1-observations c))
  (define culprit (case1-culprit c))
  (define (third-party Y)
    (append (for/list ([o obs] #:unless (memq (obs-observer o) (list Y culprit))) (obs->claim 'o o #:tol 0))
            (map third (case1-evidence c))))
  (define (excluded? Y)
    (and (person-satisfiable? m t0 t1 Y (third-party Y))
         (not (person-satisfiable? m t0 t1 Y (third-party Y) #:opportunity (case1-crime c)))))
  (check-true (excluded? 'E2))
  (check-true (excluded? 'E3))
  (check-false (excluded? 'E1)))

(test-case "作案机会：书房内连续停留 ≥ 5 分钟"
  (define crime (case1-crime c))
  ;; 只有物证时，园丁有机会
  (check-true (person-satisfiable? m t0 t1 'E3 (map third (case1-evidence c)) #:opportunity crime))
  ;; 加上"书房 20:04–20:18 除自己外没人"（管家在书房里看到的，精确），园丁没有机会：书房只剩 20:19–20:20
  (define none (claim 'n 'E1 (list 'saw-only 'study (H 2004) (H 2018) '()) 0 'testimony))
  (check-false (person-satisfiable? m t0 t1 'E3 (cons none (map third (case1-evidence c))) #:opportunity crime))
  ;; 管家的真实路径有机会
  (check-true (person-satisfiable? m t0 t1 'E1
                                   (for/list ([s (path-segs (truth 'E1))])
                                     (claim 's 'E1 (list 'at 'E1 (seg-node s) (seg-in s) (seg-out s)) 0 'evidence))
                                   #:opportunity crime)))

;; ---------------------------------------------------------------- M2：记忆、排除、生成

(test-case "记忆：只有时间偏移（不记混、不漏人）时，所有诚实记忆都与真相一致"
  (define noise (hash-set* DEFAULT-NOISE 'p-confuse 0 'p-miss 0))
  (for ([seed (in-range 1 21)])
    (define rng (make-pseudo-random-generator))
    (parameterize ([current-pseudo-random-generator rng]) (random-seed seed))
    (define mem (sample-memory m (case1-paths c) (case1-observations c) rng #:noise noise))
    (for* ([cl (memory->claims mem)] [y (claim-subjects cl (person-ids c))])
      (check-true (path-satisfies? m (truth y) (constraints-on m cl y))
                  (format "种子 ~a：~s 与 ~a 的真实路径不符" seed (claim-clause cl) y)))))

(test-case "排除：一条错记与诚实证词冲突时，去掉它仍能排除；只有错记时不能排除"
  (define culprit (case1-culprit c))
  ;; 精确（容限 0）的园丁观察：庄园地图版是为 M1 手写的，按容限 5 时园丁 10 分钟的看门不产生约束，厨师排除不了
  (define good (append (for/list ([o (case1-observations c)] #:when (eq? (obs-observer o) 'E3)) (obs->claim 'o o #:tol 0))
                       (map third (case1-evidence c))))
  ;; 某人错记"20:08–20:18 在书房看见厨师"
  (define bad (claim 'bad 'X (list 'saw 'E2 'study (H 2008) (H 2018)) TOL-OTHER 'testimony))
  (check-true (excluded? m t0 t1 'E2 good (case1-crime c)))
  (check-pred pair? (conflict-cores m t0 t1 'E2 (cons bad good)))
  (check-true (excluded? m t0 t1 'E2 (cons bad good) (case1-crime c)))
  (check-false (excluded? m t0 t1 'E2 (list bad) (case1-crime c))))

(test-case "生成器：确定性、通过检查、可读回"
  (for ([seed '(1 2 3)] [n '(4 5 6)])
    (define-values (d a errs) (generate-case1 seed #:npcs n))
    (check-not-false d (format "种子 ~a 生成失败：~a" seed errs))
    (define-values (d2 a2 _) (generate-case1 seed #:npcs n))
    (check-equal? d d2)
    (define cc (datum->case1 d))
    (check-equal? (case-checks cc) '())
    (check-equal? (length (person-ids cc)) n)
    (check-pred hash? (case1-memory cc))))

;; ---------------------------------------------------------------- M3：对局、规划器、侦探

(define (gen-case seed n) (let-values ([(d a _) (generate-case1 seed #:npcs n)]) (datum->case1 d)))

(test-case "对局：提问计费、重复提问答案不变、指认错人或证据不足都失败"
  (define cc (gen-case 2 5))
  (define g (new-game1 cc))
  (define p (car (person-ids cc)))
  (define a1 (ask1! g (list 'self p 1)))
  (define a2 (ask1! g (list 'self p 1)))
  (check-equal? a1 a2)
  (check-equal? (game1-ap g) 2)
  (check-equal? (length (game1-heard g)) (length a1))      ; 重复听到的不再记一次
  (define culprit (case1-culprit cc))
  (accuse1! g culprit)                                      ; 什么都没问清就指认
  (check-eq? (game1-status g) 'lost)
  (define g2 (new-game1 cc))
  (accuse1! g2 (car (remq culprit (person-ids cc))))
  (check-eq? (game1-status g2) 'lost))

(test-case "全知检查器：按它的问题问完就能指认成功，所需行动点不超过预算"
  (for* ([n '(4 5 6)] [seed '(1 2 3)])
    (define cc (gen-case seed n))
    (define qs (oracle-questions (new-game1 cc)))
    (define g (new-game1 cc))
    (for ([q qs]) (ask1! g q))
    (check-true (accuse1! g (case1-culprit cc)) (format "~a 人 种子 ~a" n seed))
    (check-true (<= (length qs) (default-budget1 cc)))))

(test-case "规划器：候选故事是合法路径；假记忆里自己的行踪与故事一致"
  (for ([seed '(1 2 3 4)])
    (define cc (gen-case seed 5))
    (define m (case1-map cc))
    (for ([q (story-candidates cc)])
      (check-equal? (path-errors m q (case1-t0 cc) (case1-t1 cc)) '()))
    (for ([planner (list local-planner omniscient-planner)])
      (define story (planner cc))
      (define who (case1-culprit cc))
      (for ([cl (fake-memory cc story)] #:when (and (eq? (car cl) 'at) (eq? (second cl) who)))
        (check-true (path-satisfies? m story (constraints-on m (claim 'f who cl 0 'testimony) who)))))))

(test-case "公开信息侦探：宽预算下破案，且从不指认错人"
  (for* ([n '(4 5)] [seed '(1 2 3 4 5)] [planner (list local-planner omniscient-planner)])
    (define cc (gen-case seed n))
    (define g (new-game1 cc #:planner planner #:budget 400))
    (check-eq? (detective-run! g) 'won (format "~a 人 种子 ~a" n seed))))

(test-case "文本：每种子句都能转成中文"
  (define cc (gen-case 1 5))
  (define g (new-game1 cc))
  (for ([cls (hash-values (game1-mem g))])
    (for ([cl cls]) (check-pred string? (claim->text cc cl)))))

(test-case "拆穿路线：凶手的行踪说法被两个不同来源拆穿即可定罪，一个来源不够"
  ;; 庄园地图版，给管家一个很差的故事：一整晚都在厨房
  (define (play n-sources)
    (define g (new-game1 c #:planner (λ (cc) (path 'E1 (list (seg 'kitchen t0 t1))))))
    (define alibi (car (ask1! g (list 'self 'E1 1))))              ; 「我 19:30–21:00 在厨房」
    (check-equal? (claim-clause alibi) (list 'at 'E1 'kitchen t0 t1))
    (define cook (findf (λ (cl) (eq? (car (claim-clause cl)) 'saw-only)) (ask1! g (list 'place 'E2 'kitchen 1))))
    (define gardener (findf (λ (cl) (eq? (car (claim-clause cl)) 'saw)) (ask1! g (list 'about 'E3 'E1 1))))
    (check-pred pair? (present1! g 'E1 (claim-id cook)))           ; 厨师：那时厨房里没别人
    (when (= n-sources 2)
      (check-pred pair? (present1! g 'E1 (claim-id gardener))))     ; 园丁：在走廊看见了他
    (check-equal? (length (exposure-sources g 'E1)) n-sources)
    (accuse1! g 'E1))
  (check-true (play 2))
  (check-false (play 1)))

(test-case "预算：⌈1.2 · 人数²⌉"
  (check-equal? (map budget-for-npcs '(4 5 6 8)) '(20 30 44 77))
  (check-equal? (game1-budget (new-game1 (gen-case 1 6))) 44))

(test-case "web API：建局、提问、出示、指认，响应都能转成 JSON；非法参数报 400，结束后再动作报 409"
  (define-values (c0 st) (handle-api-v1 "POST" '("games") (hasheq 'npcs 5 'seed 3)))
  (check-equal? c0 201)
  (check-false (hash-has-key? st 'excluded))            ; 不显示排除状态
  (define id (hash-ref st 'id))
  (define p1 (hash-ref (car (hash-ref st 'persons)) 'id))
  (define-values (c1 r1) (handle-api-v1 "POST" (list "games" id "ask") (hasheq 'kind "self" 'npc p1 'slot 1)))
  (check-equal? (hash-ref (hash-ref r1 'state) 'ap_used) 1)
  (define ev (hash-ref (car (hash-ref st 'evidence)) 'id))
  (define-values (c2 r2) (handle-api-v1 "POST" (list "games" id "present") (hasheq 'npc p1 'item ev)))
  (check-equal? (hash-ref (hash-ref r2 'state) 'ap_used) 3)
  (define (code-of thunk) (with-handlers ([http-error? http-error-code]) (thunk) 'ok))
  (check-equal? (code-of (λ () (handle-api-v1 "POST" (list "games" id "ask") (hasheq 'kind "self" 'npc "nobody" 'slot 1)))) 400)
  (check-equal? (code-of (λ () (handle-api-v1 "POST" (list "games" id "ask") (hasheq 'kind "self" 'npc p1 'slot 99)))) 400)
  (define-values (c3 r3) (handle-api-v1 "POST" (list "games" id "accuse") (hasheq 'npc p1)))
  (check-pred string? (jsexpr->string r3))
  (check-pred hash? (hash-ref (hash-ref r3 'state) 'reveal))
  (check-equal? (code-of (λ () (handle-api-v1 "POST" (list "games" id "ask") (hasheq 'kind "self" 'npc p1 'slot 1)))) 409)
  (check-equal? (code-of (λ () (handle-api-v1 "GET" (list "games" "nope") #f))) 404))
