#lang racket/base
;; 网状案件生成：多个目击来源、撒谎者之间的随机依赖、关系（动机）与真假动机。
;;
;; 与 gen.rkt 的区别：
;;   - 单向视线：走廊 → 作案地点；一个"窗户房间" → 作案地点；其余房间随机看向走廊或别的房间
;;   - 凶手的行踪至少有 2–3 个潜在目击者（走廊、窗户房间、返回时的走廊），多数是撒谎者
;;   - 其余 NPC 在案发前后随机外出，随机决定是否撒谎；观察由共处和视线推出，依赖关系随之形成
;;   - 关系：凶手有 1 条动机并否认；另有 1–2 人有动机（假动机），一半否认；知情人和文书证物提供来源
;; 生成后用 net.rkt 的静态分析筛选：最小割 ≥ min-cut、深度 ≥ min-depth，且检查器能破案；
;; 默认还要求对抗 NPC（会用不可拆穿的编造）下检查器能破案（#:adversarial 'press | 'strict | #f）。
;;
;; racket gen-net.rkt --seed N [--npcs 5] [--slots 4] [--layers 2] [--min-cut 2] [--min-depth 2] [--out 文件]
(require racket/list racket/match racket/cmdline racket/pretty racket/string
         "kb.rkt" "gen.rkt" "net.rkt" "solve.rkt")
(provide generate-net-case generate-net-case*)

(define DOCS (hasheq 'debt "欠条" 'heir "遗嘱副本" 'blackmail "恐吓信" 'grudge "被划花的合影"))
(define REL-NAMES (hasheq 'debt "欠债" 'heir "遗产" 'blackmail "勒索" 'grudge "旧怨"))

(define (generate-net-case seed #:npcs [n 5] #:slots [S 4] #:layers [n-layers 2] #:p-liar [p-liar 0.7]
                           #:chain [chain 0])
  (unless (<= 4 n (length PERSON-NAMES)) (error 'generate-net-case "npcs 需在 4..~a" (length PERSON-NAMES)))
  (unless (>= S 3) (error 'generate-net-case "slots 至少 3"))
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (define (pick xs) (list-ref xs (random (length xs))))
    (define (rnd a b) (+ a (random (add1 (- b a)))))
    (define (chance p) (< (random) p))
    (define (grid x) (* 5 (round (/ x 5))))

    ;; ------------------------------------------------ 人物、地点、视线
    (define ids (for/list ([i n]) (string->symbol (format "E~a" (add1 i)))))
    (define names (for/hasheq ([e ids] [nm (take (shuffle PERSON-NAMES) n)]) (values e nm)))
    (define roles (shuffle ids))
    (define C (first roles))
    (define H (second roles))
    (define others (cddr roles))

    (define n-rooms (max 3 (min (length ROOM-NAMES) (sub1 n))))
    (define loc-names (list* (pick CRIME-LOCS) HUB-NAME (take (shuffle ROOM-NAMES) n-rooms)))
    (define loc-ids (shuffle (for/list ([i (length loc-names)]) (string->symbol (format "L~a" (add1 i))))))
    (define lnames (for/hasheq ([l loc-ids] [nm loc-names]) (values l nm)))
    (define Lc (first loc-ids))
    (define Lh (second loc-ids))
    (define rooms (cddr loc-ids))
    (define Rw (pick rooms))
    (define views (list (cons Lh Lc) (cons Rw Lc)))
    (define (add-view! a b) (unless (member (cons a b) views) (set! views (append views (list (cons a b))))))
    (for ([r rooms] #:unless (eq? r Rw) #:when (chance (if (> chain 0) 0.3 0.5)))
      (add-view! r (pick (cons Lh (remq r rooms)))))
    (define (visible? from to) (or (eq? from to) (and (member (cons from to) views) #t)))
    (define (pick-from xs fallback) (pick (if (pair? xs) xs fallback)))

    (define base (make-hasheq))
    (define bC (pick (remq Rw rooms)))
    (hash-set! base C bC)
    (hash-set! base H bC)

    ;; ------------------------------------------------ 时间与行踪
    (define t0 (+ (* 19 60) 30))
    (define t1 (+ t0 (* 30 S)))
    (define k (rnd 1 (- S 2)))
    (define c1 (+ t0 (* 30 k) (* 5 (rnd 1 3))))
    (define c2 (+ c1 (* 5 (rnd 2 4))))
    (define away-f (- c1 5))
    (define away-t (+ c2 5))

    (define exs (make-hasheq))
    (hash-set! exs C (list (list Lh away-f c1) (list Lc c1 c2) (list Lh c2 away-t)))
    (hash-set! exs H '())
    ;; 潜在目击者：走廊（进入时）、窗户房间、走廊（离开时，6 人以上）
    (define witness-spots
      (take (list (list Lh (- c1 (* 5 (rnd 1 2))) (min c2 (+ c1 (* 5 (rnd 1 2)))))
                  (list Rw (- c1 (* 5 (rnd 0 2))) (min t1 (+ c1 (* 5 (rnd 2 4)))))
                  (list Lh (max c1 (- c2 (* 5 (rnd 1 2)))) (min t1 (+ c2 (* 5 (rnd 1 2))))))
            ;; 要求链长时，目击者数 × 链长 不超过可用人数（每个目击者需要 chain-1 个帮手）
            (max 1 (min (length others) (if (>= n 6) 3 2)
                        (if (> chain 0) (quotient (length others) chain) 99)))))
    (define witnesses (take others (length witness-spots)))
    (for ([w witnesses] [spot witness-spots])
      (hash-set! base w (pick-from (remove* (list (first spot) bC Rw) rooms) (remq (first spot) rooms)))
      (hash-set! exs w (list spot)))
    ;; 帮手链：第 j 个帮手从另一个房间的窗户看见前一个人的秘密行踪，自己的行踪也是秘密
    (define pool (drop others (length witnesses)))
    (define chain-members '())
    (define chain-heads '())
    (for ([w witnesses] [spot witness-spots])
      (define head
        (for/fold ([observed (cons w spot)]) ([j (in-range (max 0 (sub1 chain)))])
          #:break (null? pool)
          (match-define (cons who (list loc f t)) observed)
          (define cands (remove* (list loc Rw bC) rooms))
          (cond
            [(null? cands) observed]
            [else
             (define z (car pool))
             (set! pool (cdr pool))
             (define rz (pick cands))
             (hash-set! base z (pick-from (remove* (list rz bC) rooms) (remq rz rooms)))
             (add-view! rz loc)
             ;; 帮手只看见被观察者，看不见凶手：观察时段扣掉凶手在该地点的时段，取最长的一段
             (define c-at-loc (for/list ([x (hash-ref exs C)] #:when (eq? (first x) loc)) (cons (second x) (third x))))
             (define pieces
               (let loop ([segs (list (cons f t))] [cuts c-at-loc])
                 (if (null? cuts) segs
                     (loop (append* (for/list ([g segs])
                                      (define a (car g)) (define b (cdr g))
                                      (define ca (car (car cuts))) (define cb (cdr (car cuts)))
                                      (filter (λ (x) (< (car x) (cdr x)))
                                              (if (or (<= cb a) (<= b ca)) (list g)
                                                  (list (cons a (min b ca)) (cons (max a cb) b))))))
                           (cdr cuts)))))
             (define best (argmax (λ (x) (- (cdr x) (car x))) (if (pair? pieces) pieces (list (cons f t)))))
             (define zspot (list rz (car best) (cdr best)))
             (hash-set! exs z (list zspot))
             (set! chain-members (cons z chain-members))
             (cons z zspot)])))
      (set! chain-heads (cons (car head) chain-heads)))
    ;; 其余人：案发前后随机外出（不进作案地点）
    (for ([e pool])
      (hash-set! base e (pick rooms))
      (define dests (remq (hash-ref base e) (cons Lh rooms)))
      (hash-set! exs e
                 (if (chance 0.8)
                     (let* ([d (* 5 (rnd 2 4))]
                            [lo (max t0 (- c1 30))]
                            [hi (min (- t1 d) (+ c2 20))]
                            [f (grid (rnd lo (max lo hi)))])
                       (list (list (pick dests) f (+ f d))))
                     '())))

    (define segs (for/hasheq ([e ids]) (values e (route e (hash-ref base e) t0 t1 (hash-ref exs e)))))
    (define truth (cons (st 'V Lc t0 t1) (append* (for/list ([e ids]) (hash-ref segs e)))))

    ;; ------------------------------------------------ 撒谎者与掩护层
    (define forced (if (> chain 0) (append witnesses chain-members) '()))
    (define liars
      (cons C (for/list ([e others]
                         #:when (pair? (hash-ref exs e))
                         #:when (or (memq e forced) (chance p-liar)))
                e)))
    (define (whole e) (list (st e (hash-ref base e) t0 t1)))
    (define (story e loc) (route e (hash-ref base e) t0 t1 (list (list loc away-f away-t))))
    (define cover
      (cons (cons C (if (= n-layers 2)
                        (list (whole C) (story C Lh))
                        (list (whole C) (story C (pick (remq bC rooms))) (story C Lh))))
            (for/list ([e (cdr liars)]) (cons e (list (whole e))))))

    ;; ------------------------------------------------ 位置观察
    (define pos-obs
      (for*/list ([a ids] [b ids] #:unless (eq? a b)
                  [sa (hash-ref segs a)] [sb (hash-ref segs b)]
                  #:when (visible? (st-loc sa) (st-loc sb))
                  #:when (overlap? sa sb))
        (define-values (f t) (overlap-bounds sa sb))
        (list a 'pos (st b (st-loc sb) f t) (st-loc sa))))
    (define neg-obs
      (for*/list ([a ids] [b ids] #:unless (eq? a b)
                  [sa (hash-ref segs a)] #:when (eq? (st-loc sa) (hash-ref base b))
                  [sb (hash-ref segs b)] #:unless (eq? (st-loc sb) (hash-ref base b))
                  #:when (overlap? sa sb))
        (define-values (f t) (overlap-bounds sa sb))
        (list a 'neg (st b (hash-ref base b) f t) (st-loc sa))))
    (define lie-neg-obs
      (for*/list ([c cover] [b (in-value (car c))] [layer (cdr c)] [s layer]
                  [a ids] #:unless (eq? a b)
                  [sa (hash-ref segs a)] #:when (eq? (st-loc sa) (st-loc s)) #:when (overlap? sa s)
                  [sb (hash-ref segs b)] #:unless (eq? (st-loc sb) (st-loc s))
                  #:when (overlap? sb sa) #:when (overlap? sb s))
        (list a 'neg (st b (st-loc s) (max (st-from sa) (st-from s) (st-from sb))
                         (min (st-to sa) (st-to s) (st-to sb)))
              (st-loc sa))))
    (define obs (merge-obs (filter (λ (o) (< (st-from (third o)) (st-to (third o))))
                                   (append pos-obs neg-obs lie-neg-obs))))
    (define (obs->prop o) (prop (second o) (third o) 'exact))
    (define (conflicts-layer? p layer) (for/or ([s layer]) (conflict-kind (prop 'pos s 'exact) p)))

    ;; ------------------------------------------------ 关系（动机）
    (define rels
      (cons (rl C (pick REL-KINDS))
            (for/list ([e (take (shuffle (remq C ids)) (rnd 1 2))]) (rl e (pick REL-KINDS)))))
    (define rel-cover
      (for/list ([r rels] #:when (or (eq? (rl-agent r) C) (chance 0.5)))
        (list (rl-agent r) (rl-kind r))))
    (define rel-obs
      (append*
       (for/list ([r rels])
         (for/list ([h (take (shuffle (remq (rl-agent r) ids)) (rnd 1 2))])
           (list h 'pos r #f)))))

    ;; ------------------------------------------------ 证物
    (define traces (shuffle TRACES))
    (define evidence '())
    (define (add-evidence! nm p)
      (set! evidence (append evidence (list (list (string->symbol (format "EV~a" (add1 (length evidence)))) nm p)))))
    (define (add-trace! s)
      (add-evidence! (format "~a里的~a（看得出是~a的）" (hash-ref lnames (st-loc s))
                             (list-ref traces (modulo (length evidence) (length traces)))
                             (hash-ref names (st-agent s)))
                     (prop 'pos s 'exact)))
    (for ([c (cdr cover)])
      (define e (car c))
      (define away (findf (λ (s) (not (eq? (st-loc s) (hash-ref base e)))) (hash-ref segs e)))
      (define has-source? (for/or ([o obs]) (conflicts-layer? (obs->prop o) (cadr c))))
      ;; 链尾必须有证物；链中间的人不补证物（否则链被截短）
      (when (cond [(> chain 0) (or (memq e chain-heads) (and (not (memq e forced)) (or (not has-source?) (chance 0.3))))]
                  [else (or (not has-source?) (chance 0.3))])
        (add-trace! away)))
    (for ([r rels] #:when (chance (if (eq? (rl-agent r) C) 0.5 0.3)))
      (add-evidence! (format "~a里的~a（涉及~a）" (hash-ref lnames Lc) (hash-ref DOCS (rl-kind r)) (hash-ref names (rl-agent r)))
                     (prop 'pos r 'exact)))

    (define hidden-rels (map (λ (x) (rl (first x) (second x))) rel-cover))
    (define (critical? o)
      (define p (obs->prop o))
      (if (rl? (third o))
          (and (member (third o) hidden-rels) #t)
          (for/or ([c cover] #:when (eq? (car c) (st-agent (third o))))
            (for/or ([layer (cdr c)]) (conflicts-layer? p layer)))))

    ;; ------------------------------------------------ 输出
    (define all-obs
      (sort (append obs rel-obs)
            (λ (a b) (or (symbol<? (first a) (first b))
                         (and (eq? (first a) (first b))
                              (< (if (st? (third a)) (st-from (third a)) 9999)
                                 (if (st? (third b)) (st-from (third b)) 9999)))))))
    (define (sdatum s) (if (rl? s) `(rel ,(rl-agent s) ,(rl-kind s)) (st->datum s)))
    (define mid (* 5 (round (/ (+ c1 c2) 10))))
    `((title ,(format "生成案件 #~a：~a案" seed (hash-ref lnames Lc)))
      (briefing ,(string-append
                  (format "被害人约 ~a 在~a遇害。" (min->str mid) (hash-ref lnames Lc))
                  (string-join (for/list ([v views])
                                 (format "从~a能看见~a。" (hash-ref lnames (car v)) (hash-ref lnames (cdr v))))
                               "")
                  (apply string-append
                         (for/list ([ev evidence])
                           (format "现场发现：~a。" (second ev))))
                  (format "在场者：~a。找出凶手。" (string-join (for/list ([e ids]) (hash-ref names e)) "、"))))
      (window ,(hhmm t0) ,(hhmm t1))
      (slots ,@(for/list ([i S]) (list (hhmm (+ t0 (* 30 i))) (hhmm (+ t0 (* 30 (add1 i)))))))
      (persons (V "被害人") ,@(for/list ([e ids]) (list e (hash-ref names e))))
      (locations ,@(for/list ([l (sort loc-ids symbol<?)]) (list l (hash-ref lnames l))))
      (npcs ,@ids)
      (culprit ,C)
      (crime ,Lc ,(hhmm c1) ,(hhmm c2))
      (truth ,@(map st->datum truth))
      (knowledge ,@(for/list ([o all-obs] [i (in-naturals 1)])
                     `(,(string->symbol (format "k~a" i)) ,(first o) ,(second o) ,(sdatum (third o)) ,(fourth o)
                       ,@(if (critical? o) '(critical) '()))))
      (cover ,@(for/list ([c cover]) (cons (car c) (for/list ([layer (cdr c)]) (map st->datum layer)))))
      (evidence ,@(for/list ([ev evidence]) (list (first ev) (second ev) (sdatum (prop-st (third ev))))))
      (rels ,@(for/list ([r rels]) (list (rl-agent r) (rl-kind r))))
      (rel-cover ,@(for/list ([g (group-by car rel-cover)]) (cons (caar g) (map second g))))
      (views ,@(for/list ([v views]) (list (car v) (cdr v))))
      (meta (seed ,seed) (npcs ,n) (slots ,S) (layers ,n-layers)
            (views ,@(for/list ([v views]) (list (car v) (cdr v))))
            (roles (culprit ,C) (coworker ,H) (witnesses ,@witnesses) (chain ,@chain-members) (liars ,@liars))))))

;; 按目标筛选：返回 (values datum 尝试次数 指标) 或在 attempts 次内失败返回 (values #f attempts #f)
(define (generate-net-case* seed #:npcs [n 5] #:slots [S 4] #:layers [L 2]
                            #:min-cut [mc 2] #:min-depth [md 0] #:attempts [attempts 200]
                            #:adversarial [adv 'press])
  (let loop ([a 0])
    (cond
      [(>= a attempts) (values #f attempts #f)]
      [else
       (define d (generate-net-case (+ (* seed 1000) a) #:npcs n #:slots S #:layers L #:chain md))
       (define cd (datum->case d))
       (define-values (ok _) (static-reach cd))
       (define depth (chain-depth cd))
       (define cut (and ok (null? (validate-case cd)) (>= depth md) (min-cut cd)))
       (define o (and cut (>= cut mc) (oracle-solve cd)))
       ;; 对抗可解性：见 gen.rkt 的 generate-case*
       (define av (and o (eq? (hash-ref o 'status) 'won) adv
                       (adversarial-solve cd #:press? (eq? adv 'press))))
       (if (and o (eq? (hash-ref o 'status) 'won) (or (not adv) (eq? 'won (hash-ref av 'status))))
           (values d (add1 a) (hasheq 'depth depth 'min-cut cut 'oracle-turns (hash-ref o 'turns)
                                      'adversarial-turns (and av (hash-ref av 'turns))
                                      'sources (length (relevant-sources cd))))
           (loop (add1 a)))])))

(module+ main
  (define seed (make-parameter 1))
  (define npcs (make-parameter 5))
  (define slots (make-parameter 4))
  (define layers (make-parameter 2))
  (define mc (make-parameter 2))
  (define md (make-parameter 0))
  (define out (make-parameter #f))
  (command-line
   #:once-each
   ["--seed" s "随机种子" (seed (string->number s))]
   ["--npcs" s "NPC 数（4..10）" (npcs (string->number s))]
   ["--slots" s "时间槽数（>=3）" (slots (string->number s))]
   ["--layers" s "凶手掩护层数（2|3）" (layers (string->number s))]
   ["--min-cut" s "最小割下限" (mc (string->number s))]
   ["--min-depth" s "链长下限：拆穿凶手前至少要先依次拆穿几个撒谎者" (md (string->number s))]
   ["--out" f "输出文件（默认打印）" (out f)])
  (define-values (d attempts m)
    (generate-net-case* (seed) #:npcs (npcs) #:slots (slots) #:layers (layers) #:min-cut (mc) #:min-depth (md)))
  (unless d (eprintf "~a 次尝试内没有生成满足条件的案件\n" attempts) (exit 1))
  (eprintf "尝试 ~a 次；指标 ~s\n" attempts m)
  (if (out)
      (call-with-output-file (out) #:exists 'replace (λ (o) (pretty-write d o)))
      (pretty-write d)))
