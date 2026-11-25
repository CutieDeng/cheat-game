#lang racket/base
;; 程序化案件生成。输出与 case-manor.rktd 相同格式的 datum（多一个 meta 字段，加载时忽略）。
;;
;; 结构（与庄园书房案同构，规模和时间随机）：
;;   凶手 C：平时在房间 bC，经走廊进入作案地点，作案后经走廊返回
;;   目击者 W：案发时在走廊（能看见作案地点门口），可选撒谎称一直在自己的房间
;;   同事 H：一直在 bC，知道 C 中途离开
;;   其余 NPC：各有自己的房间，可能离开去别的房间；--secrets 个人对此撒谎（干扰项）
;; 观察由共处关系自动推出：同一地点互相看见；走廊能看见作案地点；
;; 某人的固定房间里有人时，能知道他不在。
;;
;; racket gen.rkt --seed N [--npcs 3..8] [--slots 3..5] [--layers 2|3] [--secrets K] [--honest-witness] [--out 文件]
(require racket/list racket/match racket/cmdline racket/pretty racket/string "kb.rkt")
(provide generate-case hhmm st->datum route merge-obs PERSON-NAMES CRIME-LOCS HUB-NAME ROOM-NAMES TRACES)

(define PERSON-NAMES '("管家" "厨师" "园丁" "女仆" "司机" "秘书" "医生" "侄子" "律师" "保姆"))
(define CRIME-LOCS '("书房" "卧室" "琴房" "阁楼"))
(define HUB-NAME "走廊")
(define ROOM-NAMES '("厨房" "花园" "客厅" "车库" "酒窖" "餐厅" "温室" "洗衣房"))
(define TRACES '("烟头" "手帕" "纽扣" "泥脚印" "怀表链" "发夹" "火柴盒"))

(define (hhmm m) (+ (* 100 (quotient m 60)) (remainder m 60)))
(define (st->datum s) `(at ,(st-agent s) ,(st-loc s) ,(hhmm (st-from s)) ,(hhmm (st-to s))))

;; 固定房间 base，外加若干次外出 (list 地点 起 止)
(define (route who base f0 t0 exs)
  (let loop ([cur f0] [exs (sort exs < #:key second)] [acc '()])
    (match exs
      ['() (reverse (if (< cur t0) (cons (st who base cur t0) acc) acc))]
      [(cons (list l f t) more)
       (loop t more (cons (st who l f t) (if (< cur f) (cons (st who base cur f) acc) acc)))])))

;; 同一 (持有者 极性 主体 地点 via) 的相邻区间合并
(define (merge-obs obs)
  (define groups (group-by (λ (o) (list (first o) (second o) (st-agent (third o)) (st-loc (third o)) (fourth o))) obs))
  (append*
   (for/list ([g groups])
     (define sorted (sort g < #:key (λ (o) (st-from (third o)))))
     (let loop ([xs (cdr sorted)] [cur (car sorted)] [acc '()])
       (cond
         [(null? xs) (reverse (cons cur acc))]
         [(>= (st-to (third cur)) (st-from (third (car xs))))
          (define s (third cur))
          (loop (cdr xs) (list (first cur) (second cur)
                               (st (st-agent s) (st-loc s) (st-from s) (max (st-to s) (st-to (third (car xs)))))
                               (fourth cur))
                acc)]
         [else (loop (cdr xs) (car xs) (cons cur acc))])))))

(define (generate-case seed #:npcs [n 3] #:slots [S 3] #:layers [n-layers 2]
                       #:secrets [n-secret 0] #:witness-lies [witness-lies? #t])
  (unless (<= 3 n (length PERSON-NAMES)) (error 'generate-case "npcs 需在 3..~a" (length PERSON-NAMES)))
  (unless (>= S 3) (error 'generate-case "slots 至少 3"))
  (unless (memv n-layers '(2 3)) (error 'generate-case "layers 只能是 2 或 3"))
  (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
    (random-seed seed)
    (define (pick xs) (list-ref xs (random (length xs))))
    (define (rnd a b) (+ a (random (add1 (- b a)))))

    ;; ------------------------------------------------ 人物与地点
    (define ids (for/list ([i n]) (string->symbol (format "E~a" (add1 i)))))
    (define names (for/hasheq ([e ids] [nm (take (shuffle PERSON-NAMES) n)]) (values e nm)))
    (define roles (shuffle ids))
    (define C (first roles))
    (define W (second roles))
    (define H (third roles))
    (define extras (drop roles 3))
    (define secret-extras (take extras (min n-secret (length extras))))

    (define n-rooms (max 2 (min (length ROOM-NAMES) (sub1 n))))
    (define loc-names (list* (pick CRIME-LOCS) HUB-NAME (take (shuffle ROOM-NAMES) n-rooms)))
    (define loc-ids (shuffle (for/list ([i (length loc-names)]) (string->symbol (format "L~a" (add1 i))))))
    (define lnames (for/hasheq ([l loc-ids] [nm loc-names]) (values l nm)))
    (define Lc (first loc-ids))
    (define Lh (second loc-ids))
    (define rooms (cddr loc-ids))

    (define bC (pick rooms))
    (define bW (pick (or (let ([r (remq bC rooms)]) (and (pair? r) r)) rooms)))
    (define base (make-hasheq))
    (hash-set! base C bC) (hash-set! base H bC) (hash-set! base W bW)
    (for ([e extras]) (hash-set! base e (pick rooms)))

    ;; ------------------------------------------------ 时间
    (define t0 (+ (* 19 60) 30))
    (define t1 (+ t0 (* 30 S)))
    (define k (rnd 1 (- S 2)))
    (define c1 (+ t0 (* 30 k) (* 5 (rnd 1 3))))
    (define c2 (+ c1 (* 5 (rnd 2 4))))
    (define away-f (- c1 5))
    (define away-t (+ c2 5))
    (define w1 (- c1 (* 5 (rnd 1 2))))
    (define w2 (min c2 (+ c1 (* 5 (rnd 1 2)))))

    (define exs (make-hasheq))
    (hash-set! exs C (list (list Lh away-f c1) (list Lc c1 c2) (list Lh c2 away-t)))
    (hash-set! exs W (list (list Lh w1 w2)))
    (hash-set! exs H '())
    (for ([e extras])
      (define others (remq (hash-ref base e) rooms))
      (hash-set! exs e
                 (if (and (pair? others) (or (memq e secret-extras) (< (random) 0.6)))
                     (let* ([d (* 5 (rnd 2 5))]
                            [f (+ t0 (* 5 (rnd 0 (quotient (- (* 30 S) d) 5))))])
                       (list (list (pick others) f (+ f d))))
                     '())))

    (define segs (for/hasheq ([e ids]) (values e (route e (hash-ref base e) t0 t1 (hash-ref exs e)))))
    (define truth (cons (st 'V Lc t0 t1) (append* (for/list ([e ids]) (hash-ref segs e)))))

    ;; ------------------------------------------------ 观察：(持有者 极性 st via)
    (define (visible? from to) (or (eq? from to) (and (eq? from Lh) (eq? to Lc))))
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
    ;; ------------------------------------------------ 掩护层
    (define (whole e) (list (st e (hash-ref base e) t0 t1)))
    (define (story e loc) (route e (hash-ref base e) t0 t1 (list (list loc away-f away-t))))
    (define cover
      (append
       (list (cons C (if (= n-layers 2)
                         (list (whole C) (story C Lh))
                         (list (whole C) (story C (pick (remq bC rooms))) (story C Lh)))))
       (if witness-lies? (list (cons W (list (whole W)))) '())
       (for/list ([e secret-extras] #:when (pair? (hash-ref exs e))) (cons e (list (whole e))))))

    ;; 掩护说法声称 B 在 L，而当时有人在 L：此人知道 B 不在
    (define lie-neg-obs
      (for*/list ([c cover] [b (in-value (car c))] [layer (cdr c)] [s layer]
                  [a ids] #:unless (eq? a b)
                  [sa (hash-ref segs a)] #:when (eq? (st-loc sa) (st-loc s)) #:when (overlap? sa s)
                  [sb (hash-ref segs b)] #:unless (eq? (st-loc sb) (st-loc s))
                  #:when (overlap? sb sa) #:when (overlap? sb s))
        (define f (max (st-from sa) (st-from s) (st-from sb)))
        (define t (min (st-to sa) (st-to s) (st-to sb)))
        (list a 'neg (st b (st-loc s) f t) (st-loc sa))))
    (define obs (merge-obs (append pos-obs neg-obs (filter (λ (o) (< (st-from (third o)) (st-to (third o)))) lie-neg-obs))))

    (define (obs->prop o) (prop (second o) (third o) 'exact))

    (define (conflicts-layer? p layer) (for/or ([s layer]) (conflict-kind (prop 'pos s 'exact) p)))

    ;; ------------------------------------------------ 证物：目击者大概率留下痕迹；没有观察能拆穿的掩护层必须补证物
    (define traces (shuffle TRACES))
    (define evidence '())
    (define (add-evidence! s)
      (define i (add1 (length evidence)))
      (define nm (format "~a里的~a（看得出是~a的）" (hash-ref lnames (st-loc s)) (list-ref traces (sub1 i))
                         (hash-ref names (st-agent s))))
      (set! evidence (append evidence (list (list (string->symbol (format "EV~a" i)) nm s)))))
    (for ([c cover])
      (define e (car c))
      (for ([layer (cdr c)])
        (define has-source?
          (or (for/or ([o obs] #:unless (assq (first o) cover)) (conflicts-layer? (obs->prop o) layer))
              (for/or ([ev evidence]) (conflicts-layer? (prop 'pos (third ev) 'exact) layer))))
        (define away (findf (λ (s) (not (eq? (st-loc s) (hash-ref base e)))) (hash-ref segs e)))
        (when (or (not has-source?) (and (eq? e W) (< (random) 0.7)))
          (unless (eq? e C) (add-evidence! away)))))

    (define critical?
      (λ (o) (for/or ([c cover] #:when (eq? (car c) (st-agent (third o))))
               (for/or ([layer (cdr c)]) (conflicts-layer? (obs->prop o) layer)))))

    ;; ------------------------------------------------ 输出
    (define (name-list xs) (string-join (for/list ([e xs]) (hash-ref names e)) "、"))
    (define mid (* 5 (round (/ (+ c1 c2) 10))))
    `((title ,(format "生成案件 #~a：~a案" seed (hash-ref lnames Lc)))
      (briefing ,(string-append
                  (format "被害人约 ~a 在~a遇害。" (min->str mid) (hash-ref lnames Lc))
                  (apply string-append (for/list ([ev evidence])
                                        (match (regexp-match #px"^(.*?)里的(.*?)（" (second ev))
                                          [(list _ l t) (format "~a里发现了一个~a。" l t)])))
                  (format "在场者：~a。找出凶手。" (name-list ids))))
      (window ,(hhmm t0) ,(hhmm t1))
      (slots ,@(for/list ([i S]) (list (hhmm (+ t0 (* 30 i))) (hhmm (+ t0 (* 30 (add1 i)))))))
      (persons (V "被害人") ,@(for/list ([e ids]) (list e (hash-ref names e))))
      (locations ,@(for/list ([l (sort loc-ids symbol<?)]) (list l (hash-ref lnames l))))
      (npcs ,@ids)
      (culprit ,C)
      (crime ,Lc ,(hhmm c1) ,(hhmm c2))
      (truth ,@(map st->datum truth))
      (knowledge ,@(for/list ([o (sort obs (λ (a b) (or (symbol<? (first a) (first b))
                                                         (and (eq? (first a) (first b))
                                                              (< (st-from (third a)) (st-from (third b)))))))]
                             [i (in-naturals 1)])
                     `(,(string->symbol (format "k~a" i)) ,(first o) ,(second o) ,(st->datum (third o)) ,(fourth o)
                       ,@(if (critical? o) '(critical) '()))))
      (cover ,@(for/list ([c cover]) (cons (car c) (for/list ([layer (cdr c)]) (map st->datum layer)))))
      (evidence ,@(for/list ([ev evidence]) (list (first ev) (second ev) (st->datum (third ev)))))
      (meta (seed ,seed) (npcs ,n) (slots ,S) (layers ,n-layers) (secrets ,(length secret-extras))
            (witness-lies ,witness-lies?)
            (roles (culprit ,C) (witness ,W) (coworker ,H) (secret ,@(map car (cdr cover))))))))

(module+ main
  (define seed (make-parameter 1))
  (define npcs (make-parameter 3))
  (define slots (make-parameter 3))
  (define layers (make-parameter 2))
  (define secrets (make-parameter 0))
  (define wl (make-parameter #t))
  (define out (make-parameter #f))
  (command-line
   #:once-each
   ["--seed" s "随机种子" (seed (string->number s))]
   ["--npcs" s "NPC 数（3..10）" (npcs (string->number s))]
   ["--slots" s "30 分钟时间槽数（>=3）" (slots (string->number s))]
   ["--layers" s "凶手掩护层数（2|3）" (layers (string->number s))]
   ["--secrets" s "撒谎的干扰 NPC 数" (secrets (string->number s))]
   ["--honest-witness" "目击者不撒谎" (wl #f)]
   ["--out" f "输出文件（默认打印）" (out f)])
  (define d (generate-case (seed) #:npcs (npcs) #:slots (slots) #:layers (layers)
                           #:secrets (secrets) #:witness-lies (wl)))
  (define errs (validate-case (datum->case d)))
  (unless (null? errs) (eprintf "校验失败：~s\n" errs))
  (if (out)
      (call-with-output-file (out) #:exists 'replace (λ (o) (pretty-write d o)))
      (pretty-write d)))
