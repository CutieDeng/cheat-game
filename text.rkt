#lang racket/base
;; 表层文本：把命题与响应按模板转成中文。含义完全由命题决定，模板只负责措辞。
(require racket/match racket/string "kb.rkt" "engine.rkt")
(provide prop->text response->text claim->note evidence->note conflict->note)

;; speaker 不为 #f 时，主体等于说话人则用"我"
(define (prop->text cd p [speaker #f])
  (if (rl? (prop-st p))
      (let ([a (rl-agent (prop-st p))])
        (string-append (if (eq? a speaker) "我" (person-name cd a)) (rel-text (rl-kind (prop-st p)) (prop-pol p))))
      (at->text cd p speaker)))

(define (at->text cd p speaker)
  (define s (prop-st p))
  (define who (if (eq? (st-agent s) speaker) "我" (person-name cd (st-agent s))))
  (define span (format "~a–~a" (min->str (st-from s)) (min->str (st-to s))))
  (define where (loc-name cd (st-loc s)))
  (match* ((prop-pol p) (prop-mode p))
    [('pos 'exact) (format "~a ~a 在~a" who span where)]
    [('neg 'exact) (format "~a ~a 不在~a" who span where)]
    [('pos 'approx) (format "~a 大概在 ~a 之间某个时候在~a，具体时间记不清了" who span where)]
    [('neg 'approx) (format "~a 在 ~a 之间有一阵子不在~a，具体时间记不清了" who span where)]))

(define (response->text cd npc r)
  (define name (person-name cd npc))
  (define body
    (string-join (for/list ([c (response-claims r)]) (prop->text cd (claim-prop c) npc)) "；"))
  (case (response-kind r)
    [(cover truth partial fabricate frame) (format "~a：「~a。」" name body)]
    [(unknown) (format "~a：「这个我不清楚。」" name)]
    [(evade) (format "~a：「我……记不太清了。」\n（~a避开了你的目光。）" name name)]
    [(none) (format "~a：「我和他没什么瓜葛。」" name)]
    [(slip) (format "~a：「~a——」\n（~a突然停住，脸色变了。）" name body name)]
    [(retract) (format "~a：「……好吧，刚才我没说实话。~a。」" name body)]
    [(refuse) (format "~a：「我没什么要补充的。」" name)]
    [(insist) (format "~a：「我说的都是实话。」" name)]
    [(withdraw) (format "~a：「……那句话是我记错了，当我没说。」" name)]
    [(no-effect) (format "~a：「这能说明什么？」" name)]
    [(already) (format "~a：「这件事你刚才已经拿出来过了。」" name)]
    [(confess) (format "~a：「……是我。~a。」" name body)]
    [(insufficient) (format "~a：「证据呢？你拿不出证据。」\n指控被驳回。" name)]
    [(wrong) (format "~a：「不是我！」\n你指认错了人。" name)]
    [else (format "~a：（~a）" name (response-kind r))]))

(define (claim->note cd c)
  (format "#~a ~a 说：~a~a"
          (claim-id c)
          (person-name cd (claim-speaker c))
          (prop->text cd (claim-prop c))
          (case (claim-kind c)
            [(retract) "（改口）"]
            [(slip) "（说漏嘴）"]
            [else ""])))

(define (evidence->note cd e)
  (match e [(list id name p) (format "~a ~a（表明：~a）" id name (prop->text cd p))]))

(define (conflict->note cd c)
  (define other (conflict-other c))
  (format "~a 的说法「~a」与 ~a 矛盾"
          (person-name cd (conflict-npc c))
          (prop->text cd (claim-prop (conflict-own c)))
          (cond [(claim? other) (format "#~a" (claim-id other))]
                [(number? other) (format "#~a" other)]
                [else other])))
