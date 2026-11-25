#lang racket/base
;; 策略评测：启发式玩家（只用公开信息）对局，比较 NPC 策略。
;; racket eval.rkt --policy rule|llm --games N [--temp T] [--eps E] [--out 文件]
(require racket/list racket/match racket/cmdline racket/format
         "kb.rkt" "engine.rkt" "llm.rkt")
(provide (struct-out pstate) player-move apply-move! heard run-game)

;; ---------------------------------------------------------------- 启发式玩家
;; 只读取：证物、听到的声明（game-claims）、自己动作的响应结果。

(struct pstate (presented score rng eps) #:transparent)  ; presented: hash (npc . item); score: hash npc -> n

(define (heard g) (reverse (game-claims g)))

;; npc 仍在坚持的说法：之后没有被自己的另一条说法推翻
(define (standing-claims g npc)
  (define cs (filter (λ (c) (eq? (claim-speaker c) npc)) (heard g)))
  (for/list ([c cs]
             #:unless (for/or ([d cs])
                        (and (> (claim-id d) (claim-id c))
                             (conflict-kind (claim-prop c) (claim-prop d)))))
    c))

(define (items g)
  (append (for/list ([e (case-data-evidence (game-cd g))]) (cons (first e) (third e)))
          (for/list ([c (heard g)]) (cons (claim-id c) (claim-prop c)))))

(define (present-candidates g ps)
  (for*/list ([npc (case-data-npcs (game-cd g))]
              [it (items g)]
              #:unless (hash-ref (pstate-presented ps) (cons npc (car it)) #f)
              #:unless (and (number? (car it))
                            (eq? (claim-speaker (findf (λ (c) (= (claim-id c) (car it))) (game-claims g))) npc))
              #:when (for/or ([c (standing-claims g npc)])
                       (conflict-kind (claim-prop c) (cdr it))))
    (cons npc (car it))))

(define (all-topics g)
  (define cd (game-cd g))
  (define npcs (case-data-npcs cd))
  (define slots (range (vector-length (case-data-slots cd))))
  (append (for*/list ([n npcs] [s slots]) (list n n s))
          (for*/list ([n npcs] [o npcs] #:unless (eq? n o) [s slots]) (list n o s))
          (if (null? (case-data-rels cd)) '() (for*/list ([n npcs] [o npcs]) (list n o 'rel)))))

(define (pick ps xs) (list-ref xs (random (length xs) (pstate-rng ps))))

(define (best-suspect ps npcs)
  (argmax (λ (n) (hash-ref (pstate-score ps) n 0)) npcs))

;; 返回 (list 'ask npc subj slot) | (list 'present npc item) | (list 'accuse npc)
(define (player-move g ps)
  (define cd (game-cd g))
  (define npcs (case-data-npcs cd))
  (define top (best-suspect ps npcs))
  (define unasked (filter (λ (k) (zero? (hash-ref (game-asked g) k 0))) (all-topics g)))
  (define cands (present-candidates g ps))
  ;; 作案地点与时段是公开信息：有人自己说案发时在作案地点，直接指认
  (define crime (case-data-crime cd))
  (define self-placed
    (for/first ([c (heard g)]
                #:when (let ([p (claim-prop c)])
                         (and (st? (prop-st p)) (eq? (prop-pol p) 'pos) (eq? (prop-mode p) 'exact)
                              (eq? (st-agent (prop-st p)) (claim-speaker c))
                              (eq? (st-loc (prop-st p)) (st-loc crime))
                              (overlap? (prop-st p) crime))))
      (claim-speaker c)))
  (cond
    [self-placed (list 'accuse self-placed)]
    [(>= (hash-ref (pstate-score ps) top 0) LIES-TO-WIN) (list 'accuse top)]
    [(>= (add1 (game-turn g)) (turn-limit g)) (list 'accuse top)]
    [(< (random (pstate-rng ps)) (pstate-eps ps))
     ;; 探索：随机提问或随机出示
     (if (or (null? (items g)) (zero? (random 2 (pstate-rng ps))))
         (cons 'ask (pick ps (all-topics g)))
         (list 'present (pick ps npcs) (car (pick ps (items g)))))]
    [(pair? cands) (cons 'present (let ([c (pick ps cands)]) (list (car c) (cdr c))))]
    [(pair? unasked)
     (define self-first (filter (λ (k) (eq? (first k) (second k))) unasked))
     (cons 'ask (pick ps (if (pair? self-first) self-first unasked)))]
    [else
     ;; 追问嫌疑最大者自己的行踪，制造压力
     (list 'ask top top (pick ps (range (vector-length (case-data-slots cd)))))]))

(define (apply-move! g ps mv)
  (match mv
    [(list 'ask npc subj slot)
     (define r (ask! g npc subj slot))
     (when (eq? (response-kind r) 'slip) (hash-update! (pstate-score ps) npc add1 0))
     r]
    [(list 'present npc item)
     (hash-set! (pstate-presented ps) (cons npc item) #t)
     (define r (present! g npc item))
     (when (memq (response-kind r) '(retract refuse withdraw)) (hash-update! (pstate-score ps) npc add1 0))
     r]
    [(list 'accuse npc) (accuse! g npc)]))

;; ---------------------------------------------------------------- 对局

;; 包一层：记录每个决策点的选择、规则策略的选择、是否存在 / 是否选了暴露自己的选项
(define (recording-policy inner decisions)
  (λ (view actions)
    (define a (inner view actions))
    (define r (rule-policy view actions))
    (set-box! decisions
              (cons (hasheq 'npc (car (cdr (assq 'self (cdr view))))
                            'q (car (cdr (assq 'q (cdr view))))
                            'n (length actions)
                            'kinds (map action-kind actions)
                            'chosen (action-kind a)
                            'rule (action-kind r)
                            'exposing-available (and (findf action-exposing? actions) #t)
                            'exposing-chosen (action-exposing? a)
                            'refutable (map action-refutable? actions)
                            'refutable-chosen (action-refutable? a))
                    (unbox decisions)))
    a))

(define (run-game cd make-policy seed eps #:ext? [ext? #f])
  (define decisions (box '()))
  (define g (new-game cd #:seed seed #:ext? ext? #:policy (recording-policy (make-policy seed) decisions)))
  (define ps (pstate (make-hash) (make-hasheq) (let ([r (make-pseudo-random-generator)])
                                                 (parameterize ([current-pseudo-random-generator r])
                                                   (random-seed seed))
                                                 r)
                     eps))
  (define moves
    (let loop ([acc '()])
      (if (eq? (game-status g) 'playing)
          (let* ([mv (player-move g ps)] [r (apply-move! g ps mv)])
            (loop (cons (list mv (response-kind r)) acc)))
          (reverse acc))))
  (define culprit (case-data-culprit cd))
  (hasheq 'seed seed 'status (game-status g) 'turns (game-turn g)
          'lies (length (exposed-lies g culprit))
          'admitted (admitted? g culprit)
          'first-exposed (let ([cs (exposed-lies g culprit)])
                           (and (pair? cs)
                                (for/first ([h (reverse (game-history g))]
                                            #:when (and (eq? (second h) culprit)
                                                        (memq (fourth h) '(retract refuse slip))))
                                  (first h))))
          'forget (reverse (game-forget-log g))
          'moves moves
          'decisions (reverse (unbox decisions))))

;; ---------------------------------------------------------------- 汇总

(define (summarize label results)
  (define n (length results))
  (define ds (append-map (λ (r) (hash-ref r 'decisions)) results))
  (define free (filter (λ (d) (> (hash-ref d 'n) 1)) ds))
  (define exp-av (filter (λ (d) (hash-ref d 'exposing-available)) free))
  (define (pct a b) (if (zero? b) "n/a" (~r (* 100. (/ a b)) #:precision 1)))
  (define wins (filter (λ (r) (eq? (hash-ref r 'status) 'won)) results))
  (printf "\n== ~a：~a 局 ==\n" label n)
  (printf "玩家破案 ~a/~a；破案局平均轮数 ~a\n" (length wins) n
          (if (null? wins) "n/a" (~r (exact->inexact (/ (apply + (map (λ (r) (hash-ref r 'turns)) wins)) (length wins))) #:precision 1)))
  (printf "凶手自己说出在作案现场：~a/~a 局\n" (count (λ (r) (hash-ref r 'admitted)) results) n)
  (printf "凶手首次被拆穿的平均轮数 ~a\n"
          (let ([fs (filter values (map (λ (r) (hash-ref r 'first-exposed)) results))])
            (if (null? fs) "n/a" (~r (exact->inexact (/ (apply + fs) (length fs))) #:precision 1))))
  (printf "决策点 ~a（有选择的 ~a）\n" (length ds) (length free))
  (printf "与规则策略一致 ~a%\n" (pct (count (λ (d) (eq? (hash-ref d 'chosen) (hash-ref d 'rule))) free) (length free)))
  (printf "存在暴露选项的决策 ~a，其中选了暴露选项 ~a%\n" (length exp-av)
          (pct (count (λ (d) (hash-ref d 'exposing-chosen)) exp-av) (length exp-av)))
  (printf "选择分布：~s\n"
          (sort (hash->list (for/fold ([h (hasheq)]) ([d free]) (hash-update h (hash-ref d 'chosen) add1 0)))
                > #:key cdr))
  (printf "不一致的决策（选择 / 规则 / 可选项）：~s\n"
          (remove-duplicates
           (for/list ([d free] #:unless (eq? (hash-ref d 'chosen) (hash-ref d 'rule)))
             (list (hash-ref d 'npc) (hash-ref d 'chosen) (hash-ref d 'rule) (hash-ref d 'kinds))))))

(module+ main
  (define policy-name (make-parameter "rule"))
  (define games (make-parameter 10))
  (define temp (make-parameter 0))
  (define eps (make-parameter 0.2))
  (define out (make-parameter #f))
  (define case-file (make-parameter "case-manor.rktd"))
  (command-line
   #:once-each
   ["--policy" p "rule | llm" (policy-name p)]
   ["--games" n "局数" (games (string->number n))]
   ["--temp" t "LLM 温度" (temp (string->number t))]
   ["--eps" e "玩家随机探索概率" (eps (string->number e))]
   ["--out" f "逐局结果输出（.rktd，每行一局）" (out f)]
   ["--case" f "案件文件" (case-file f)])
  (define cd (load-case (case-file)))
  (define llm-log (and (out) (open-output-file (string-append (out) ".prompts") #:exists 'replace)))
  (define (make-policy seed)
    (case (policy-name)
      [("rule") rule-policy]
      [("llm") (make-llm-policy cd #:temperature (temp) #:seed seed
                                #:log! (λ (rec) (when llm-log (write rec llm-log) (newline llm-log) (flush-output llm-log))))]
      [else (error 'eval "未知策略 ~a" (policy-name))]))
  (define o (and (out) (open-output-file (out) #:exists 'replace)))
  (define results
    (for/list ([seed (in-range 1 (add1 (games)))])
      (define r (run-game cd make-policy seed (eps)))
      (printf "局 ~a：~a，~a 轮，凶手被拆穿 ~a 次~a\n" seed (hash-ref r 'status) (hash-ref r 'turns) (hash-ref r 'lies)
              (if (hash-ref r 'admitted) "，凶手自己说出了在作案现场" ""))
      (flush-output)
      (when o (write r o) (newline o) (flush-output o))
      r))
  (summarize (format "~a 策略" (policy-name)) results)
  (when (equal? (policy-name) "llm")
    (define s (llm-stats-snapshot))
    (define q (hash-ref s 'queries 0))
    (printf "LLM 调用 ~a 次，非法输出 ~a 次，单一选项跳过 ~a 次，平均延迟 ~a ms\n"
            q (hash-ref s 'invalid 0) (hash-ref s 'forced 0)
            (if (zero? q) "n/a" (round (/ (hash-ref s 'latency-ms 0) q))))))
