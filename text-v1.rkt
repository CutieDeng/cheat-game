#lang racket/base
;; v1 声明 → 中文。时间对玩家显示时取整到 5 分钟（spec-v1.md §1）。
(require racket/list racket/match racket/string "map.rkt" "claims.rkt" "case1.rkt")
(provide fmt-time person-name place-name claim->text question->text map->text)

(define (fmt-time m)
  (define r (* 5 (round (/ m 5))))
  (format "~a:~a" (quotient r 60) (~2 (remainder r 60))))
(define (~2 n) (if (< n 10) (format "0~a" n) (format "~a" n)))

(define (person-name c who) (cond [(assq who (case1-persons c)) => cadr] [else (symbol->string who)]))
(define (place-name c n) (name-of (case1-map c) n))
(define (entry-who e) (if (symbol? e) e (car e)))

(define (claim->text c cl)
  (define sp (claim-speaker cl))
  (define (who x) (if (eq? x sp) "我" (person-name c x)))
  (match (claim-clause cl)
    [(list 'at x n a b) (format "~a ~a–~a 在~a。" (who x) (fmt-time a) (fmt-time b) (place-name c n))]
    [(list 'saw x n a b) (format "我 ~a–~a 在~a看见了~a。" (fmt-time a) (fmt-time b) (place-name c n) (who x))]
    [(list 'saw-door x r d t)
     (format "大约 ~a，我看见~a~a了~a。" (fmt-time t) (who x) (if (eq? d 'in) "进" "出") (place-name c r))]
    [(list 'saw-only n a b ps)
     (format "~a–~a 我一直看得见~a：~a" (fmt-time a) (fmt-time b) (place-name c n)
             (if (null? ps)
                 "除了我没有别人。"
                 (string-append
                  (string-join
                   (for/list ([g (group-by entry-who ps)])
                     (format "~a ~a 在" (person-name c (entry-who (car g)))
                             (string-join (for/list ([e g])
                                            (if (symbol? e) "一直"
                                                (format "~a–~a" (fmt-time (second e)) (fmt-time (third e)))))
                                          "、")))
                   "；")
                  "；没有别人。")))]
    [(list 'doors-only r a b ps)
     (format "~a–~a 我一直看着~a的门口：~a" (fmt-time a) (fmt-time b) (place-name c r)
             (if (null? ps)
                 "没人进出。"
                 (string-append
                  (string-join (for/list ([e ps] #:unless (symbol? e))
                                 (format "~a ~a~a" (person-name c (first e)) (fmt-time (third e)) (if (eq? (second e) 'in) "进" "出")))
                               "，")
                  "；没有别人进出。")))]
    [(list 'move x from to t)
     (format "大约 ~a，~a从~a去了~a。" (fmt-time t) (who x) (place-name c from) (place-name c to))]))

(define (question->text c slot-label q)
  (match q
    [(list 'self p s) (format "问~a：~a 你在哪？" (person-name c p) (slot-label s))]
    [(list 'about p y s) (format "问~a：~a 你见过~a吗？" (person-name c p) (slot-label s) (person-name c y))]
    [(list 'place p n s) (format "问~a：~a 你在~a见过谁？" (person-name c p) (slot-label s) (place-name c n))]))

(define (map->text c)
  (define m (case1-map c))
  (define (kind k) (case k [(room) "房间"] [(corridor) "走廊"] [(outdoor) "户外"]))
  (string-append
   "地点：" (string-join (for/list ([n (node-ids m)]) (format "~a（~a）" (place-name c n) (kind (kind-of m n)))) "，") "\n"
   "通道：" (string-join
             (remove-duplicates
              (for*/list ([a (node-ids m)] [e (neighbors m a)] #:when (symbol<? a (car e)))
                (format "~a—~a ~a 分钟" (place-name c a) (place-name c (car e)) (cdr e))))
             "，")
   (let ([ws (gmap-windows m)])
     (if (null? ws) ""
         (string-append "\n窗户：" (string-join (for/list ([w ws]) (format "从~a能看见~a" (place-name c (car w)) (place-name c (cdr w)))) "，"))))
   "\n视线：同一地点互相看得见；站在走廊上能看见相邻房间的门口（看得见谁进出，看不见屋里）。"))
