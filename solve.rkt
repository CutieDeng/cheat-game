#lang racket/base
;; 可解性检查：知道全部案情的检查器在规则策略下对局，确认案件能在回合上限内破案。
;; 检查器读取案件文件来决定问什么、出示什么，但每个回答都由引擎和 NPC 策略产生，
;; 所以通过说明"存在一条可行的提问路径"，轮数是该路径的长度（近似下界，不保证最短）。
(require racket/list racket/match "kb.rkt" "engine.rkt")
(provide oracle-solve)

;; 有用的提问：
;;   撒谎者本人、在其掩护层与某个矛盾来源重叠的时间槽（取得承诺）
;;   critical 观察的持有者、关于该观察主体、在该观察所在的时间槽（取得矛盾来源）
(define (useful-topics cd)
  (define sources
    (append (for/list ([k (case-data-knowledge cd)]) (kitem-prop k))
            (map third (case-data-evidence cd))))
  (define self-topics
    (remove-duplicates
     (for*/list ([(e layers) (in-hash (case-data-cover cd))]
                 [layer layers] [s layer] [p sources]
                 #:when (conflict-kind (prop 'pos s 'exact) p)
                 [slot (let-values ([(f t) (overlap-bounds s (prop-st p))]) (slots-overlapping cd f t))])
       (list e e slot))))
  (define obs-topics
    (remove-duplicates
     (for*/list ([k (case-data-knowledge cd)] #:when (kitem-critical? k)
                 [s (in-value (prop-st (kitem-prop k)))]
                 [slot (if (rl? s) '(rel) (slots-overlapping cd (st-from s) (st-to s)))])
       (list (kitem-owner k) (prop-agent (kitem-prop k)) slot))))
  (values (sort self-topics < #:key third) obs-topics))

(define (presentable g ps)
  (define cd (game-cd g))
  (define items (append (map first (case-data-evidence cd))
                        (map claim-id (reverse (game-claims g)))))
  ;; 先拆穿非凶手（解开目击者），凶手放最后
  (define order (append (remq (case-data-culprit cd) (case-data-npcs cd)) (list (case-data-culprit cd))))
  (for*/first ([npc order] [it items]
               #:unless (hash-ref ps (cons npc it) #f)
               #:unless (and (number? it) (eq? (claim-speaker (find-claim g it)) npc))
               #:when (for*/or ([o (hash-ref (game-commits g) npc)] [p (item-props g it)])
                        ;; 只出示能拆穿谎言的：被冲突的承诺本身为假
                        (and (conflict-kind (claim-prop o) p)
                             (not (prop-true? cd (claim-prop o)))
                             (not (for/or ([e (game-exposed g)])
                                    (and (eq? (conflict-own e) o) (equal? (conflict-other e) it)))))))
    (cons npc it)))

;; 返回 hasheq：'status won|lost|stuck 'turns 'lies 'admitted 'moves
(define (oracle-solve cd #:seed [seed 1] #:policy [policy rule-policy])
  (define g (new-game cd #:seed seed #:policy policy))
  (define culprit (case-data-culprit cd))
  (define-values (self-topics obs-topics) (useful-topics cd))
  (define presented (make-hash))
  (define asked (make-hash))
  (define moves '())
  (define (move! m r) (set! moves (cons (list m (response-kind r)) moves)))
  (define status
    (let loop ()
      (cond
        [(not (eq? (game-status g) 'playing)) (game-status g)]
        [(convictable? g culprit) (move! (list 'accuse culprit) (accuse! g culprit)) (loop)]
        [(presentable g presented)
         => (λ (c)
              (hash-set! presented c #t)
              (move! (list 'present (car c) (cdr c)) (present! g (car c) (cdr c)))
              (loop))]
        [(or (findf (λ (t) (not (hash-ref asked t #f))) self-topics)
             ;; 观察持有者的回答取决于他当前的掩护层，层变了可以再问
             (for/first ([t obs-topics]
                         #:unless (hash-ref asked (cons t (layer-of g (first t))) #f))
               (cons t (layer-of g (first t)))))
         => (λ (key)
              (hash-set! asked key #t)
              (define t (if (symbol? (car key)) key (car key)))
              (move! (cons 'ask t) (apply ask! g t))
              (loop))]
        [else 'stuck])))
  (hasheq 'status status 'turns (game-turn g)
          'lies (length (exposed-lies g culprit)) 'admitted (admitted? g culprit)
          'view-max (apply max 0 (game-view-sizes g))
          'moves (reverse moves)))

(module+ main
  (require racket/cmdline racket/pretty)
  (define file (command-line #:args (case-file) case-file))
  (define r (oracle-solve (load-case file)))
  (pretty-write r))
