#lang racket/base
;; 知识库：命题表示、区间运算、冲突规则、案件加载与静态校验
(require racket/list racket/match racket/format)
(provide (struct-out st) (struct-out rl) (struct-out prop) (struct-out kitem) (struct-out case-data)
         REL-KINDS rels-of prop-agent rel-text default-turn-limit
         hhmm->min min->str st->string
         overlap? overlap-range clip conflict-kind overlap-bounds
         load-case datum->case validate-case prop-true? truth-of
         slot-range slots-overlapping widen-to-slots
         person-name loc-name)

;; 命题：agent 在 [from, to) 期间位于 loc（时间单位：分钟）
(struct st (agent loc from to) #:transparent)
;; 关系命题：agent 与被害人之间存在 kind 关系（不带时间）。动机只作线索，不进定罪条件
(struct rl (agent kind) #:transparent)
(define REL-KINDS '(debt heir blackmail grudge))
;; pol: 'pos | 'neg    mode: 'exact | 'approx
(struct prop (pol st mode) #:transparent)
;; NPC 的观察。via = 观察时观察者所在的地点
(struct kitem (id owner prop via critical?) #:transparent)
(struct case-data (title briefing window slots persons locations npcs culprit
                   truth knowledge cover evidence crime
                   rels rel-cover turn-limit views)
  #:transparent)
;; views     : (listof (cons 从 到))，从"从"能看见"到"（单向）；同一地点总是互相看得见。
;;             案件文件可用 (views (L3 L1) ...) 指定，缺省为空。只进入 NPC 视图，不参与规则判定。
;; turn-limit：回合上限。案件文件可用 (turn-limit N) 指定；缺省为 max(30, 6 × NPC 数)
;; rels      : (listof rl)，真实存在的关系
;; rel-cover : hash npc -> (listof kind)，该 NPC 会否认的、自己与被害人的关系
;; crime : st，agent 为凶手，表示作案地点与时段
;; persons / locations : hash 符号 -> 名称
;; slots    : vector of (cons from to)
;; cover    : hash npc -> (listof (listof st))，按层排列
;; evidence : (listof (list id 名称 prop))

;; ---------------------------------------------------------------- 时间与区间

(define (hhmm->min n) (+ (* 60 (quotient n 100)) (remainder n 100)))
(define (min->str m)
  (~a (quotient m 60) ":" (~r (remainder m 60) #:min-width 2 #:pad-string "0")))
(define (st->string s)
  (format "(at ~a ~a ~a-~a)" (st-agent s) (st-loc s) (min->str (st-from s)) (min->str (st-to s))))

(define (prop-agent p)
  (define s (prop-st p))
  (if (rl? s) (rl-agent s) (st-agent s)))

(define (rel-text kind pol)
  (case kind
    [(debt) (if (eq? pol 'pos) "欠被害人一大笔钱" "不欠被害人钱")]
    [(heir) (if (eq? pol 'pos) "是被害人遗嘱的受益人" "不在被害人的遗嘱里")]
    [(blackmail) (if (eq? pol 'pos) "正被被害人勒索" "没有被被害人勒索")]
    [(grudge) (if (eq? pol 'pos) "和被害人结过仇" "和被害人没有过节")]))

(define (overlap? a b)
  (and (< (st-from a) (st-to b)) (< (st-from b) (st-to a))))
(define (overlap-range s f t)
  (max 0 (- (min (st-to s) t) (max (st-from s) f))))
(define (overlap-bounds a b)
  (values (max (st-from a) (st-from b)) (min (st-to a) (st-to b))))
(define (clip s f t)
  (define f* (max (st-from s) f))
  (define t* (min (st-to s) t))
  (and (< f* t*) (st (st-agent s) (st-loc s) f* t*)))

(define (slot-range cd i)
  (define p (vector-ref (case-data-slots cd) i))
  (values (car p) (cdr p)))
(define (slots-overlapping cd f t)
  (for/list ([p (in-vector (case-data-slots cd))] [i (in-naturals)]
             #:when (and (< (car p) t) (< f (cdr p))))
    i))
;; 精度退化：区间扩到所覆盖时间槽的边界
(define (widen-to-slots cd s)
  (define idx (slots-overlapping cd (st-from s) (st-to s)))
  (define ps (for/list ([i idx]) (vector-ref (case-data-slots cd) i)))
  (st (st-agent s) (st-loc s) (apply min (map car ps)) (apply max (map cdr ps))))

;; ---------------------------------------------------------------- 冲突规则

;; 返回冲突类型或 #f。approx 命题不参与冲突检测。
;; 关系命题只与同一 (主体, 关系) 的反极性命题冲突。
(define (conflict-kind p q)
  (define a (prop-st p))
  (define b (prop-st q))
  (cond
    [(or (eq? (prop-mode p) 'approx) (eq? (prop-mode q) 'approx)) #f]
    [(or (rl? a) (rl? b))
     (and (rl? a) (rl? b) (equal? a b) (not (eq? (prop-pol p) (prop-pol q))) 'assert-deny)]
    [(not (eq? (st-agent a) (st-agent b))) #f]
    [(not (overlap? a b)) #f]
    [(and (eq? (prop-pol p) 'pos) (eq? (prop-pol q) 'pos)
          (not (eq? (st-loc a) (st-loc b))))
     'location-exclusive]
    [(and (not (eq? (prop-pol p) (prop-pol q))) (eq? (st-loc a) (st-loc b)))
     'assert-deny]
    [else #f]))

;; ---------------------------------------------------------------- 真值

(define (truth-of cd agent)
  (filter (λ (s) (eq? (st-agent s) agent)) (case-data-truth cd)))

(define (covered? s segs)
  (let loop ([cur (st-from s)] [xs (sort segs < #:key st-from)])
    (cond [(>= cur (st-to s)) #t]
          [(null? xs) #f]
          [(<= (st-from (car xs)) cur) (loop (max cur (st-to (car xs))) (cdr xs))]
          [else #f])))

(define (rels-of cd agent)
  (for/list ([r (case-data-rels cd)] #:when (eq? (rl-agent r) agent)) (rl-kind r)))

(define (prop-true? cd p)
  (define s (prop-st p))
  (if (rl? s)
      (let ([has? (and (member s (case-data-rels cd)) #t)])
        (if (eq? (prop-pol p) 'pos) has? (not has?)))
      (prop-true/at? cd p)))

(define (prop-true/at? cd p)
  (define s (prop-st p))
  (define segs (filter (λ (x) (eq? (st-loc x) (st-loc s))) (truth-of cd (st-agent s))))
  (case (prop-pol p)
    [(pos) (if (eq? (prop-mode p) 'approx)
               (ormap (λ (x) (overlap? x s)) segs)
               (covered? s segs))]
    [(neg) (if (eq? (prop-mode p) 'approx)
               (not (covered? s segs))
               (not (ormap (λ (x) (overlap? x s)) segs)))]))

;; ---------------------------------------------------------------- 加载

(define (parse-st d)
  (match d
    [(list 'at a l f t) (st a l (hhmm->min f) (hhmm->min t))]
    [(list 'rel a k) (rl a k)]
    [_ (error 'load-case "无法解析命题: ~s" d)]))

(define (load-case path) (datum->case (call-with-input-file path read)))

(define (datum->case d)
  (define (field k)
    (cond [(assq k d) => cdr]
          [else (error 'load-case "缺少字段 ~a" k)]))
  (define (opt-field k) (cond [(assq k d) => cdr] [else '()]))
  (case-data
   (car (field 'title))
   (car (field 'briefing))
   (let ([w (field 'window)]) (cons (hhmm->min (first w)) (hhmm->min (second w))))
   (for/vector ([s (field 'slots)]) (cons (hhmm->min (first s)) (hhmm->min (second s))))
   (for/hasheq ([p (field 'persons)]) (values (first p) (second p)))
   (for/hasheq ([p (field 'locations)]) (values (first p) (second p)))
   (field 'npcs)
   (car (field 'culprit))
   (map parse-st (field 'truth))
   (for/list ([k (field 'knowledge)])
     (match k
       [(list id owner pol s via rest ...)
        (kitem id owner (prop pol (parse-st s) 'exact) via (and (memq 'critical rest) #t))]))
   (for/hasheq ([c (field 'cover)])
     (values (first c) (for/list ([layer (rest c)]) (map parse-st layer))))
   (for/list ([e (field 'evidence)])
     (list (first e) (second e) (prop 'pos (parse-st (third e)) 'exact)))
   (match (field 'crime)
     [(list l f t) (parse-st (list 'at (car (field 'culprit)) l f t))])
   (for/list ([r (opt-field 'rels)]) (rl (first r) (second r)))
   (for/hasheq ([c (opt-field 'rel-cover)]) (values (first c) (rest c)))
   (match (opt-field 'turn-limit)
     [(list n) n]
     ['() (default-turn-limit (length (field 'npcs)))])
   ;; 旧的网状案件文件只在 meta 里有 views
   (for/list ([v (let ([top (opt-field 'views)])
                   (if (pair? top) top
                       (cond [(assq 'views (opt-field 'meta)) => cdr] [else '()])))])
     (cons (first v) (second v)))))

(define (default-turn-limit n-npcs) (max 30 (* 6 n-npcs)))

(define (person-name cd sym) (hash-ref (case-data-persons cd) sym (λ () (symbol->string sym))))
(define (loc-name cd sym) (hash-ref (case-data-locations cd) sym (λ () (symbol->string sym))))

;; ---------------------------------------------------------------- 静态校验

;; 返回错误信息列表；空列表表示通过。
(define (validate-case cd #:lies-to-win [lies-to-win 2])
  (define errs '())
  (define (err! fmt . args) (set! errs (cons (apply format fmt args) errs)))
  (define (check-st s where)
    (unless (hash-has-key? (case-data-persons cd) (st-agent s))
      (err! "~a: 未定义的人物 ~a" where (st-agent s)))
    (unless (hash-has-key? (case-data-locations cd) (st-loc s))
      (err! "~a: 未定义的地点 ~a" where (st-loc s)))
    (unless (< (st-from s) (st-to s))
      (err! "~a: 空区间 ~a" where (st->string s))))
  (define (pos s) (prop 'pos s 'exact))

  ;; 1. 真相内部无位置冲突
  (for ([s (case-data-truth cd)]) (check-st s "truth"))
  (define tv (list->vector (case-data-truth cd)))
  (for* ([i (vector-length tv)] [j (in-range (add1 i) (vector-length tv))])
    (define a (vector-ref tv i))
    (define b (vector-ref tv j))
    (when (conflict-kind (pos a) (pos b))
      (err! "真相自相矛盾: ~a / ~a" (st->string a) (st->string b))))

  ;; 2. 观察与真相一致；观察者当时确实在 via
  (define (check-rl r where)
    (unless (hash-has-key? (case-data-persons cd) (rl-agent r)) (err! "~a: 未定义的人物 ~a" where (rl-agent r)))
    (unless (memq (rl-kind r) REL-KINDS) (err! "~a: 未知关系 ~a" where (rl-kind r))))
  (for ([r (case-data-rels cd)]) (check-rl r "rels"))
  (for* ([(npc kinds) (in-hash (case-data-rel-cover cd))] [k kinds])
    (unless (member (rl npc k) (case-data-rels cd))
      (err! "~a 否认的关系 ~a 并不存在，否认不是谎言" npc k)))
  (for ([k (case-data-knowledge cd)] #:when (rl? (prop-st (kitem-prop k))))
    (check-rl (prop-st (kitem-prop k)) (kitem-id k))
    (unless (prop-true? cd (kitem-prop k)) (err! "观察 ~a 与真相不符" (kitem-id k))))
  (for ([e (case-data-evidence cd)] #:when (rl? (prop-st (third e))))
    (unless (prop-true? cd (third e)) (err! "证物 ~a 与真相不符" (first e))))
  (for ([k (case-data-knowledge cd)] #:unless (rl? (prop-st (kitem-prop k))))
    (define s (prop-st (kitem-prop k)))
    (check-st s (kitem-id k))
    (unless (memq (kitem-owner k) (case-data-npcs cd))
      (err! "观察 ~a: 持有者 ~a 不是 NPC" (kitem-id k) (kitem-owner k)))
    (unless (prop-true? cd (kitem-prop k))
      (err! "观察 ~a 与真相不符: ~a" (kitem-id k) (st->string s)))
    (unless (prop-true? cd (pos (st (kitem-owner k) (kitem-via k) (st-from s) (st-to s))))
      (err! "观察 ~a: ~a 在该时段不在 ~a" (kitem-id k) (kitem-owner k) (kitem-via k))))

  ;; 3. 掩护层内部一致；4. 每层至少有一个矛盾来源（critical 观察或证物）
  (define sources
    (append (for/list ([k (case-data-knowledge cd)] #:when (kitem-critical? k)) (kitem-prop k))
            (map third (case-data-evidence cd))))
  (for ([(npc layers) (in-hash (case-data-cover cd))])
    (for ([layer layers] [i (in-naturals)])
      (for ([s layer]) (check-st s (format "cover ~a/~a" npc i)))
      (for* ([a layer] [b layer] #:unless (eq? a b))
        (when (conflict-kind (pos a) (pos b))
          (err! "~a 第 ~a 层掩护故事自相矛盾" npc i)))
      (unless (for*/or ([s layer] [p sources]) (conflict-kind (pos s) p))
        (err! "~a 第 ~a 层掩护故事没有矛盾来源（critical 观察或证物），案件不可解" npc i))))

  ;; 5. 凶手
  (define culprit (case-data-culprit cd))
  (unless (memq culprit (case-data-npcs cd)) (err! "凶手 ~a 不是 NPC" culprit))
  (unless (>= (length (hash-ref (case-data-cover cd) culprit '())) lies-to-win)
    (err! "凶手的掩护层数少于胜利所需的 ~a 次揭穿" lies-to-win))
  (define crime (case-data-crime cd))
  (check-st crime "crime")
  (unless (prop-true? cd (pos crime))
    (err! "作案时段内凶手不在作案地点：~a" (st->string crime)))
  (reverse errs))
