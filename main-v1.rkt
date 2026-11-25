#lang racket/base
;; v1 终端菜单（地图版）。
;;   racket main-v1.rkt --seed 3 --npcs 5          生成一局
;;   racket main-v1.rkt --case cases/xxx.rktd       读案件文件
;;   --planner local|omni  凶手用哪个规则规划器编故事（缺省 local）
;;   --debug               显示真相与凶手的故事
(require racket/list racket/match racket/string racket/cmdline racket/format
         "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "engine-v1.rkt" "liar-v1.rkt" "text-v1.rkt" "gen-v1.rkt")

(define (choose prompt options #:back [back "返回"])
  (printf "\n~a\n" prompt)
  (for ([o options] [i (in-naturals 1)]) (printf "  ~a. ~a\n" i (car o)))
  (printf "  0. ~a\n> " back)
  (flush-output)
  (define line (read-line))
  (cond
    [(eof-object? line) (exit 0)]
    [(string->number (string-trim line))
     => (λ (k) (cond [(= k 0) #f]
                     [(<= 1 k (length options)) (cdr (list-ref options (sub1 k)))]
                     [else (choose prompt options #:back back)]))]
    [else (choose prompt options #:back back)]))

(define (play c planner debug?)
  (define g (new-game1 c #:planner planner))
  (define ids (person-ids c))
  (define m (case1-map c))
  (define (slot-label s) (match (slot-range g s) [(cons a b) (format "~a–~a" (fmt-time a) (fmt-time b))]))
  (define people (for/list ([p ids]) (cons (person-name c p) p)))
  (define slots (for/list ([s (length (game1-slots g))]) (cons (slot-label s) s)))
  (define places (for/list ([n (node-ids m)]) (cons (place-name c n) n)))
  (printf "《~a》\n~a\n\n~a\n" (case1-title c) (case1-briefing c) (map->text c))
  (match-define (list cn clo chi cd) (case1-crime c))
  (printf "作案需要在~a连续停留至少 ~a 分钟。\n" (place-name c cn) cd)
  (printf "胜利条件：指认凶手，并且满足其一——\n  （1）其他每个人都已被排除：只用别人的证词和物证（被排除者自己的话、被指认者的话都不算），他在案发时段不可能在~a待够 ~a 分钟；\n  （2）凶手关于自己行踪的说法，已被至少 ~a 个不同来源（不同的人的证词或不同的物证）出示后证明矛盾。\n"
          (place-name c cn) cd EXPOSE-TO-WIN)
  (printf "行动点：提问 ~a，出示 ~a，指认 ~a；预算 ~a。\n"
          (hash-ref COSTS1 'ask) (hash-ref COSTS1 'present) (hash-ref COSTS1 'accuse) (game1-budget g))
  (when debug?
    (printf "\n[debug] 凶手 ~a；真实路径：\n" (person-name c (case1-culprit c)))
    (for ([p (case1-paths c)])
      (printf "  ~a：~a\n" (person-name c (path-who p))
              (string-join (for/list ([s (path-segs p)]) (format "~a ~a–~a" (place-name c (seg-node s)) (fmt-time (seg-in s)) (fmt-time (seg-out s)))) " → ")))
    (printf "[debug] 凶手的故事：~a\n"
            (string-join (for/list ([s (path-segs (game1-story g))]) (format "~a ~a–~a" (place-name c (seg-node s)) (fmt-time (seg-in s)) (fmt-time (seg-out s)))) " → ")))
  (define (show-claims cls)
    (if (null? cls)
        (printf "（不清楚。）\n")
        (for ([cl cls]) (printf "  #~a ~a：「~a」\n" (claim-id cl) (person-name c (claim-speaker cl)) (claim->text c cl)))))
  (define (notes)
    (printf "\n—— 物证 ——\n")
    (for ([e (case1-evidence c)]) (printf "  ~a ~a：~a\n" (first e) (second e)
                                          (match (claim-clause (third e))
                                            [(list 'at x n a b) (format "~a ~a–~a 在~a" (person-name c x) (fmt-time a) (fmt-time b) (place-name c n))])))
    (printf "—— 听到的证词（~a 条）——\n" (length (game1-heard g)))
    ;; 不显示排除状态（用户决定 2026-11-25）：谁已被排除由玩家自己判断，指认时引擎按定义检查
    (show-claims (game1-heard g))
    (printf "—— 已拆穿（出示后对方关于自己行踪的说法被证明矛盾）——\n")
    (if (null? (game1-exposed g))
        (printf "  （无）\n")
        (for ([e (game1-exposed g)])
          (printf "  ~a 的「~a」被 ~a 拆穿\n" (person-name c (first e)) (claim->text c (second e))
                  (let ([src (third e)]) (if (assq src (case1-persons c)) (person-name c src) src))))))
  (let loop ()
    (when (active1? g)
      (printf "\n==== 行动点 已用 ~a / ~a~a ====\n" (game1-ap g) (game1-budget g)
              (if (eq? (game1-status g) 'accuse-only) "（只能指认）" ""))
      (define cmd (choose "行动：" (append (if (affordable1? g 'ask) '(("询问" . ask)) '())
                                           (if (affordable1? g 'present) '(("出示" . present)) '())
                                           '(("笔记" . notes) ("指认凶手" . accuse)))
                          #:back "退出"))
      (case cmd
        [(#f) (exit 0)]
        [(notes) (notes)]
        [(ask)
         (define p (choose "问谁？" people))
         (define kind (and p (choose "问什么？" '(("他自己在哪" . self) ("有没有见过某人" . about) ("在某地见过谁" . place)))))
         (define arg (case kind
                       [(about) (choose "关于谁？" (filter (λ (x) (not (eq? (cdr x) p))) people))]
                       [(place) (choose "哪个地方？" places)]
                       [else (and kind 'none)]))
         (define s (and arg (choose "哪个时段？" slots)))
         (when s
           (define q (if (eq? kind 'self) (list 'self p s) (list kind p arg s)))
           (printf "\n~a\n" (question->text c slot-label q))
           (show-claims (ask1! g q)))]
        [(present)
         (define items (append (for/list ([e (case1-evidence c)]) (cons (format "~a ~a" (first e) (second e)) (first e)))
                               (for/list ([cl (game1-heard g)]) (cons (format "#~a ~a：~a" (claim-id cl) (person-name c (claim-speaker cl)) (claim->text c cl)) (claim-id cl)))))
         (define it (choose "出示什么？" items))
         (define p (and it (choose "给谁看？" people)))
         (when p
           (define found (present1! g p it))
           (if (null? found)
               (printf "\n~a：「这和我说的不矛盾。」\n" (person-name c p))
               (for ([f found])
                 (printf "\n与 #~a「~a」矛盾（~a）。~a：「……可能是我记错了。」\n"
                         (claim-id (first f)) (claim->text c (first f))
                         (case (second f) [(evidence) "物证"] [(sight) "视线"] [(location) "位置"] [(travel) "来不及走到"])
                         (person-name c p)))))]
        [(accuse)
         (define p (choose "指认谁？" people))
         (when p
           (accuse1! g p)
           (cond
             [(eq? (game1-status g) 'won) (printf "\n破案。\n")]
             [(eq? p (case1-culprit c))
              (printf "\n证据不足。\n")
              (for ([x (accusation-gaps g p)])
                (printf "  · ~a没有被排除：~a\n" (person-name c (car x))
                        (if (eq? (cdr x) 'self-only)
                            "你没有听到别人关于他去向的证词（他本人的话不算）。"
                            "有别人的证词提到他，但还不足以排除。")))
              (printf "  · 拆穿：~a的行踪说法被 ~a 个不同来源拆穿，需要 ~a 个。\n"
                      (person-name c p) (length (exposure-sources g p)) EXPOSE-TO-WIN)]
             [else (printf "\n指认错误：~a不是凶手。\n" (person-name c p))]))])
      (loop)))
  (printf "\n==== 结局：~a（用了 ~a 行动点）====\n" (if (eq? (game1-status g) 'won) "破案" "失败") (game1-ap g))
  (unless (eq? (game1-status g) 'won)
    (printf "凶手是~a。\n" (person-name c (case1-culprit c)))))

(module+ main
  (define file (make-parameter #f))
  (define seed (make-parameter 1))
  (define npcs (make-parameter 5))
  (define planner (make-parameter "local"))
  (define debug (make-parameter #f))
  (command-line #:once-each
                ["--case" f "案件文件" (file f)]
                ["--seed" s "生成种子" (seed (string->number s))]
                ["--npcs" s "人数" (npcs (string->number s))]
                ["--planner" p "local | omni" (planner p)]
                ["--debug" "显示真相" (debug #t)])
  (define c
    (if (file)
        (load-case1 (file))
        (let-values ([(d a errs) (generate-case1 (seed) #:npcs (npcs))])
          (unless d (eprintf "生成失败：~a\n" errs) (exit 1))
          (datum->case1 d))))
  (play c (if (equal? (planner) "omni") omniscient-planner local-planner) (debug)))
