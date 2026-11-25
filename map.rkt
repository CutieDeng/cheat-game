#lang racket/base
;; v1 地图：节点、带通行分钟数的边、单向窗户；最短路与视线（spec-v1.md §1）
;;
;; (map (node id kind "名称") ... (edge a b 分钟) ... (window 从 到) ...)
;;   kind：room | corridor | outdoor
;; 视线：
;;   同一节点内互相看见；
;;   window：从 A 能看见 B 内部（单向）；
;;   站在 corridor 上能看见与之相邻的 room 的门口（看见有人进出，看不见房间内部）。
(require racket/list racket/match)
(provide (struct-out node) (struct-out gmap)
         datum->map node-ids kind-of name-of neighbors edge-w dist
         sees-inside? door-visible? rooms-with-door-from
         hhmm->min min->hhmm)

(define (hhmm->min n) (+ (* 60 (quotient n 100)) (remainder n 100)))
(define (min->hhmm m) (+ (* 100 (quotient m 60)) (remainder m 60)))

(struct node (id kind name) #:transparent)
;; nodes: id -> node（保持声明顺序的 ids 列表另存）；adj: id -> (listof (cons 邻居 分钟))
;; windows: (listof (cons 从 到))；dist: (cons a b) -> 最短分钟数
(struct gmap (ids nodes adj windows dist) #:transparent)

(define KINDS '(room corridor outdoor))

(define (datum->map d)
  (match d
    [(cons 'map items)
     (define nodes
       (for/list ([it items] #:when (eq? (car it) 'node))
         (match it
           [(list 'node id kind name)
            (unless (memq kind KINDS) (error 'datum->map "节点 ~a 的类型 ~a 不在 ~a 中" id kind KINDS))
            (node id kind name)])))
     (define ids (map node-id nodes))
     (unless (= (length ids) (length (remove-duplicates ids))) (error 'datum->map "节点 id 重复"))
     (define (check-id x) (unless (memq x ids) (error 'datum->map "未知节点 ~a" x)))
     (define adj (make-hasheq (for/list ([i ids]) (cons i '()))))
     (for ([it items] #:when (eq? (car it) 'edge))
       (match it
         [(list 'edge a b w)
          (check-id a) (check-id b)
          (unless (and (exact-positive-integer? w)) (error 'datum->map "边 ~a–~a 的分钟数须为正整数" a b))
          (when (eq? a b) (error 'datum->map "自环 ~a" a))
          (hash-update! adj a (λ (l) (cons (cons b w) l)))
          (hash-update! adj b (λ (l) (cons (cons a w) l)))]))
     (define windows
       (for/list ([it items] #:when (eq? (car it) 'window))
         (match it [(list 'window a b) (check-id a) (check-id b) (cons a b)])))
     (define dist (floyd ids adj))
     (for* ([a ids] [b ids])
       (unless (hash-ref dist (cons a b) #f) (error 'datum->map "地图不连通：~a 到 ~a" a b)))
     (gmap ids (for/hasheq ([n nodes]) (values (node-id n) n)) adj windows dist)]))

(define (floyd ids adj)
  (define d (make-hash))
  (for ([a ids]) (hash-set! d (cons a a) 0))
  (for* ([a ids] [e (hash-ref adj a)])
    (define k (cons a (car e)))
    (define cur (hash-ref d k #f))
    (hash-set! d k (if cur (min cur (cdr e)) (cdr e))))
  (for* ([k ids] [i ids] [j ids])
    (define ik (hash-ref d (cons i k) #f))
    (define kj (hash-ref d (cons k j) #f))
    (define ij (hash-ref d (cons i j) #f))
    (when (and ik kj (or (not ij) (< (+ ik kj) ij)))
      (hash-set! d (cons i j) (+ ik kj))))
  d)

(define (node-ids m) (gmap-ids m))
(define (kind-of m id) (node-kind (hash-ref (gmap-nodes m) id)))
(define (name-of m id) (node-name (hash-ref (gmap-nodes m) id)))
(define (neighbors m a) (hash-ref (gmap-adj m) a))
(define (edge-w m a b) (cond [(assq b (neighbors m a)) => cdr] [else #f]))
(define (dist m a b) (hash-ref (gmap-dist m) (cons a b)))

;; 站在 from 能看见 to 内部
(define (sees-inside? m from to)
  (or (eq? from to) (and (member (cons from to) (gmap-windows m)) #t)))

;; 站在 from 能看见 room 的门口：from 是走廊且与 room 相邻，room 是房间
(define (door-visible? m from room)
  (and (eq? (kind-of m from) 'corridor)
       (eq? (kind-of m room) 'room)
       (edge-w m from room)
       #t))

(define (rooms-with-door-from m from)
  (for/list ([e (neighbors m from)] #:when (door-visible? m from (car e))) (car e)))
