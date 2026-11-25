#lang racket/base
;; 回放评估：在某个 NPC 决策点，对每个合法选项分别把对局继续下完，估计该选项对这个 NPC 的价值。
;;
;; 复现方式：同一案件、同一种子、同一扩展开关。先按记录重放玩家着法，以及该决策点之前的 NPC 选择（脚本策略）；
;; 在决策点强制选某个选项；之后 NPC 一律用规则策略，玩家用两种对手继续：
;;   (a) 启发式玩家，若干随机种子取平均（弱对手，只用公开信息）
;;   (b) 检查器（强对手，读取完整案情决定问什么）
;;
;; 价值只对撒谎者（凶手、有秘密的人）定义，取值 0..1，越大越好：
;;   t-expose  = 该 NPC 第一次被拆穿（retract / refuse / withdraw / slip）或被定罪的轮数，未发生记为回合上限
;;   t-convict = 凶手被定罪的轮数，未发生记为回合上限
;;   凶手：(t-expose + t-convict) / (2 × 上限)；有秘密的人：t-expose / 上限
;; 说真话的人返回 #f。
(require racket/list "kb.rkt" "engine.rkt" "eval.rkt" "solve.rkt")
(provide evaluate-options npc-value replay)

(define (npc-value g npc)
  (define cd (game-cd g))
  (define L (turn-limit g))
  (define culprit? (eq? npc (case-data-culprit cd)))
  (and (or culprit? (liar? g npc))
       (let* ([t-conv (if (and culprit? (eq? (game-status g) 'won)) (game-ap g) L)]
              ;; 被定罪也算被识破：t-expose 不晚于 t-convict（凶手自认在现场时两者相同）
              [t-exp (min t-conv
                          (or (for/first ([h (reverse (game-history g))]
                                          #:when (eq? (second h) npc)
                                          #:when (memq (fourth h) '(retract refuse withdraw slip)))
                                (first h))
                              L))])
         (if culprit?
             (/ (+ t-exp t-conv) (* 2.0 L))
             (/ t-exp (exact->inexact L))))))

;; moves：到决策点为止的玩家着法（含正在进行的那一步）；npc-log：之前每次 NPC 决策选中的动作
;; 返回 (values 对局 玩家状态)，对局停在 moves 全部执行完之后
(define (replay cd seed ext? moves npc-log k forced #:pseed [pseed 0] #:eps [eps 0.2])
  (define calls 0)
  (define (policy view actions)
    (define i calls)
    (set! calls (add1 calls))
    (define want (cond [(< i k) (list-ref npc-log i)] [(= i k) forced] [else #f]))
    (if want
        (or (findf (λ (a) (equal? a want)) actions)
            (error 'replay "第 ~a 次 NPC 决策无法重放：~s 不在合法动作中" i want))
        (rule-policy view actions)))
  (define g (new-game cd #:seed seed #:policy policy #:ext? ext?))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng]) (random-seed (+ 1 pseed)))
  (define ps (pstate (make-hash) (make-hasheq) rng eps))
  (for ([mv moves]) (apply-move! g ps mv))
  (unless (> calls k) (error 'replay "重放结束时决策点 ~a 尚未出现（共 ~a 次决策）" k calls))
  (values g ps))

(define (mean xs) (/ (apply + xs) (exact->inexact (length xs))))

;; 返回与 options 对齐的列表：每项 (hasheq 'heuristic 'oracle 'q)，说真话的人各项为 #f
(define (evaluate-options cd seed ext? moves npc-log k options npc
                          #:heuristic-seeds [hs 8] #:eps [eps 0.2])
  (for/list ([a options])
    (define hv
      (for/list ([r (in-range hs)])
        (define-values (g ps) (replay cd seed ext? moves npc-log k a #:pseed (+ 100 r) #:eps eps))
        (let loop ()
          (when (active? g)
            (apply-move! g ps (player-move g ps))
            (loop)))
        (npc-value g npc)))
    (define-values (g2 _) (replay cd seed ext? moves npc-log k a))
    (oracle-run! g2)
    (define ov (npc-value g2 npc))
    (if (and ov (andmap values hv))
        (let ([h (mean hv)]) (hasheq 'heuristic h 'oracle ov 'q (/ (+ h ov) 2)))
        (hasheq 'heuristic #f 'oracle #f 'q #f))))
