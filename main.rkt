#lang racket/base
;; 终端编号菜单。用法：racket main.rkt [--debug] [--seed N] [--case 文件]
(require racket/list racket/string racket/cmdline
         "kb.rkt" "engine.rkt" "text.rkt")

(define debug? (make-parameter #f))
(define seed (make-parameter 1))
(define case-file (make-parameter "case-manor.rktd"))

(define (choose prompt options #:back [back "返回"])
  (printf "\n~a\n" prompt)
  (for ([o options] [i (in-naturals 1)]) (printf "  ~a. ~a\n" i (car o)))
  (printf "  0. ~a\n> " back)
  (flush-output)
  (define line (read-line))
  (when (eof-object? line) (newline) (exit 0))
  (define n (string->number (string-trim line)))
  (cond [(not (exact-nonnegative-integer? n)) (printf "无效输入\n") (choose prompt options)]
        [(= n 0) #f]
        [(<= n (length options)) (cdr (list-ref options (sub1 n)))]
        [else (printf "无效输入\n") (choose prompt options #:back back)]))

(define (slot-label cd i)
  (define-values (f t) (slot-range cd i))
  (format "~a–~a" (min->str f) (min->str t)))

(define (show-notes g)
  (define cd (game-cd g))
  (printf "\n—— 笔记 ——\n证物：\n")
  (for ([e (case-data-evidence cd)]) (printf "  ~a\n" (evidence->note cd e)))
  (printf "听到的说法：\n")
  (for ([c (reverse (game-claims g))]) (printf "  ~a\n" (claim->note cd c)))
  (printf "回避过的问题：\n")
  (for ([h (reverse (game-history g))] #:when (eq? (fourth h) 'evade))
    (printf "  第 ~a 轮 ~a 说不清自己 ~a 在哪\n" (first h) (person-name cd (second h)) (slot-label cd (third (third h)))))
  (printf "已揭穿的矛盾：\n")
  (for ([c (game-exposed g)]) (printf "  ~a\n" (conflict->note cd c))))

(define (show-debug g npc)
  (when (debug?)
    (printf "  [debug] 压力 ~a=~a  视图 token=~a  遗忘记录=~s\n"
            npc (hash-ref (game-p-npc g) npc 0)
            (if (pair? (game-view-sizes g)) (car (game-view-sizes g)) 0)
            (reverse (game-forget-log g)))))

(define (main)
  (define cd (load-case (case-file)))
  (define errs (validate-case cd #:lies-to-win LIES-TO-WIN))
  (unless (null? errs)
    (for ([e errs]) (eprintf "案件校验失败：~a\n" e))
    (exit 1))
  (define g (new-game cd #:seed (seed)))
  (define npcs (for/list ([n (case-data-npcs cd)]) (cons (person-name cd n) n)))
  ;; 提问 / 出示的对象：标出负压力附加费，只列付得起的人
  (define (choose-target g prompt kind)
    (choose prompt
            (for/list ([n (case-data-npcs cd)] #:when (affordable? g kind n))
              (cons (if (> (surcharge g n) 0)
                        (format "~a（~a 点，含抵触附加 ~a 点）" (person-name cd n) (cost g kind n) (surcharge g n))
                        (person-name cd n))
                    n))))
  (define slots (for/list ([i (vector-length (case-data-slots cd))]) (cons (slot-label cd i) i)))
  (printf "《~a》\n~a\n" (case-data-title cd) (case-data-briefing cd))
  (let loop ()
    (when (active? g)
      (printf "\n==== 行动点 已用 ~a / ~a~a ====\n" (game-ap g) (turn-limit g)
              (cond [(eq? (game-status g) 'accuse-only) "（行动点用尽，只能指认）"]
                    [(eq? (phase g) 'confront) "（对质阶段）"]
                    [else ""]))
      (define cmd
        (choose "行动："
                (append (if (affordable? g 'ask) (list (cons (format "询问（~a 点）" (cost g 'ask)) 'ask)) '())
                        (if (affordable? g 'present) (list (cons (format "出示（~a 点）" (cost g 'present)) 'present)) '())
                        (list (cons "查看笔记" 'notes) (cons (format "指认凶手（~a 点）" (cost g 'accuse)) 'accuse)))
                #:back "退出游戏"))
      (case cmd
        [(#f) (exit 0)]
        [(notes) (show-notes g)]
        [(ask)
         (define npc (choose-target g "询问谁？" 'ask))
         (define subj (and npc (choose "关于谁？" npcs)))
         (define slot (and subj (choose "问什么？" (append (for/list ([s slots]) (cons (format "~a 在哪里" (car s)) (cdr s)))
                                                          (list (cons "和被害人的关系" 'rel))))))
         (when slot
           (printf "\n~a\n" (response->text cd npc (ask! g npc subj slot)))
           (show-debug g npc))]
        [(present)
         (define npc (choose-target g "向谁出示？" 'present))
         (define items
           (append (for/list ([e (case-data-evidence cd)]) (cons (evidence->note cd e) (car e)))
                   (for/list ([c (reverse (game-claims g))]) (cons (claim->note cd c) (claim-id c)))))
         (define item (and npc (choose "出示什么？" items)))
         (when item
           (printf "\n~a\n" (response->text cd npc (present! g npc item)))
           (show-debug g npc))]
        [(accuse)
         (define npc (choose "指认谁是凶手？" npcs))
         (when npc (printf "\n~a\n" (response->text cd npc (accuse! g npc))))])
      (loop)))
  (printf "\n==== 结局：~a（用了 ~a 行动点）====\n"
          (case (game-status g) [(won) "破案"] [(lost) "失败"] [else "中止"])
          (game-ap g)))

(module+ main
  (command-line
   #:once-each
   ["--debug" "显示压力、视图长度、遗忘记录" (debug? #t)]
   ["--seed" n "随机种子" (seed (string->number n))]
   ["--case" f "案件文件" (case-file f)])
  (main))
