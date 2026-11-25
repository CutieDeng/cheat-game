#lang racket/base
;; v1 真实行踪与观察（spec-v1.md §2、§3.1）
;;
;; 路径：(path 谁 (节点 进入 离开) ...)，时间写作 HHMM，加载后为分钟。
;;   区间是闭区间：在 [进入, 离开] 的每一分钟位于该节点；
;;   相邻两段节点在图上相邻，后段进入 = 前段离开 + 边权；中间的分钟在路上（不在任何节点，谁也看不见）。
;;   停留 0 分钟（进入 = 离开）表示经过。
;; 观察（精确，由路径推出）：
;;   (saw 观察者 某人 节点 起 止)      同一节点或经窗户看见某人在该节点
;;   (saw-door 观察者 某人 房间 in|out 时刻)  在走廊看见某人进 / 出相邻房间
;;   (saw-only 观察者 节点 起 止 ((人 起 止) ...))   一段连续的看守（能看见该节点内部）：期间除自己外，在那里的人和各自在场的时段；
;;                                                  名单外的人都不在，名单里的人在列出的时段以外不在
;;   (doors-only 观察者 房间 起 止 ((人 in|out 时刻) ...))  一段连续的看守（站在能看见该房间门口的走廊上）：期间进出这扇门的全部事件
(require racket/list racket/match "map.rkt")
(provide (struct-out seg) (struct-out path) (struct-out obs)
         datum->path path-errors at-node door-events derive-observations obs->sexp route build-path)

(struct seg (node in out) #:transparent)
(struct path (who segs) #:transparent)
;; kind: saw | saw-door | saw-only | doors-only。
;; saw-only 的 subject 为 ((人 起 止) ...)，doors-only 的 subject 为 ((人 in|out 时刻) ...)；saw-door 的 dir 为 in/out，from = to = 时刻
;; 2026-11-26 起负观察按"一段连续看守一条"记录：此前按"在场者集合不变"分段，过半片段短于 2 × 容限、不产生约束
(struct obs (kind observer subject node from to dir) #:transparent)

(define (datum->path d)
  (match d
    [(list 'path who segs ...)
     (path who (for/list ([s segs])
                 (match s [(list n a b) (seg n (hhmm->min a) (hhmm->min b))])))]))

;; 返回错误字符串列表（空表示合法）。t0 t1：路径必须覆盖的时间窗（分钟）
(define (path-errors m p t0 t1)
  (define ss (path-segs p))
  (define who (path-who p))
  (append
   (if (null? ss) (list (format "~a 的路径为空" who)) '())
   (for/list ([s ss] #:unless (memq (seg-node s) (node-ids m)))
     (format "~a 的路径含未知节点 ~a" who (seg-node s)))
   (for/list ([s ss] #:when (> (seg-in s) (seg-out s)))
     (format "~a 在 ~a 的进入时刻晚于离开时刻" who (seg-node s)))
   (for/list ([a ss] [b (if (pair? ss) (cdr ss) '())]
              #:unless (let ([w (and (memq (seg-node a) (node-ids m)) (edge-w m (seg-node a) (seg-node b)))])
                         (and w (= (seg-in b) (+ (seg-out a) w)))))
     (format "~a 从 ~a 到 ~a 不相邻或时间不等于通行分钟数" who (seg-node a) (seg-node b)))
   (if (and (pair? ss) (or (> (seg-in (first ss)) t0) (< (seg-out (last ss)) t1)))
       (list (format "~a 的路径没有覆盖时间窗" who))
       '())))

;; 时刻 t 所在节点；在路上或在路径之外返回 #f
(define (at-node p t)
  (for/first ([s (path-segs p)] #:when (<= (seg-in s) t (seg-out s))) (seg-node s)))

;; 进出房间的事件：(list 房间 in|out 时刻)。只对 room 类型节点记录
(define (door-events m p)
  (define ss (path-segs p))
  (append*
   (for/list ([a ss] [b (if (pair? ss) (cdr ss) '())])
     (append (if (eq? (kind-of m (seg-node a)) 'room) (list (list (seg-node a) 'out (seg-out a))) '())
             (if (eq? (kind-of m (seg-node b)) 'room) (list (list (seg-node b) 'in (seg-in b))) '())))))

;; 把逐分钟的记录按 (键 . 时刻) 合并成连续区间
(define (merge-minutes keyed)   ; keyed: (listof (cons 键 时刻))
  (define by (make-hash))
  (for ([kt keyed]) (hash-update! by (car kt) (λ (l) (cons (cdr kt) l)) '()))
  (append*
   (for/list ([(k ts) (in-hash by)])
     (let loop ([ts (sort (remove-duplicates ts) <)] [acc '()])
       (cond
         [(null? ts) (reverse acc)]
         [else
          (define start (car ts))
          (let run ([end start] [rest (cdr ts)])
            (if (and (pair? rest) (= (car rest) (add1 end)))
                (run (car rest) (cdr rest))
                (loop rest (cons (list k start end) acc))))])))))

;; paths：所有在场者的路径（观察者与被观察者都在其中）。t0 t1：时间窗
(define (derive-observations m paths t0 t1)
  (define (pos p t) (at-node p t))
  ;; saw：逐分钟
  (define saw-minutes
    (for*/list ([o paths] [t (in-range t0 (add1 t1))]
                [a (in-value (pos o t))] #:when a
                [s paths] #:unless (eq? s o)
                [b (in-value (pos s t))] #:when (and b (sees-inside? m a b)))
      (cons (list (path-who o) (path-who s) b) t)))
  (define saws
    (for/list ([r (merge-minutes saw-minutes)])
      (match r [(list (list o s b) f t) (obs 'saw o s b f t #f)])))
  (define (entry<? x y) (or (symbol<? (car x) (car y)) (and (eq? (car x) (car y)) (< (cadr* x) (cadr* y)))))
  (define (cadr* e) (if (number? (cadr e)) (cadr e) (caddr e)))
  ;; saw-only：观察者能看见某节点内部的每一段连续时间为一条；记下这段时间里别人在该节点的时段
  (define watch-minutes
    (for*/list ([o paths] [t (in-range t0 (add1 t1))]
                [a (in-value (pos o t))] #:when a
                [b (node-ids m)] #:when (sees-inside? m a b))
      (cons (list (path-who o) b) t)))
  (define onlys
    (for/list ([w (merge-minutes watch-minutes)])
      (match-define (list (list o b) f t) w)
      (define present
        (for*/list ([s paths] #:unless (eq? (path-who s) o)
                    [tt (in-range f (add1 t))] #:when (eq? (pos s tt) b))
          (cons (list (path-who s)) tt)))
      (obs 'saw-only o
           (sort (for/list ([r (merge-minutes present)]) (match r [(list (list s) c d) (list s c d)])) entry<?)
           b f t #f)))
  ;; doors-only：观察者在能看见某房间门口的走廊上的每一段时间，期间进出该门的人
  (define door-watch
    (for*/list ([o paths] [t (in-range t0 (add1 t1))]
                [a (in-value (pos o t))] #:when a
                [r (rooms-with-door-from m a)])
      (cons (list (path-who o) r) t)))
  (define door-onlys
    (for/list ([w (merge-minutes door-watch)])
      (match-define (list (list o r) f t) w)
      (obs 'doors-only o
           (sort (for*/list ([s paths] #:unless (eq? (path-who s) o)
                             [ev (door-events m s)]
                             #:when (and (eq? (first ev) r) (<= f (third ev) t)))
                   (list (path-who s) (second ev) (third ev)))
                 entry<?)
           r f t #f)))
  ;; saw-door：事件发生时观察者站在能看见该门口的走廊上
  (define doors
    (for*/list ([s paths] [ev (door-events m s)]
                [o paths] #:unless (eq? o s)
                [a (in-value (pos o (third ev)))]
                #:when (and a (door-visible? m a (first ev))))
      (obs 'saw-door (path-who o) (path-who s) (first ev) (third ev) (third ev) (second ev))))
  ;; 稳定顺序：按开始时刻，再按文本
  (sort (append saws doors onlys door-onlys)
        (λ (x y) (or (< (obs-from x) (obs-from y))
                     (and (= (obs-from x) (obs-from y))
                          (string<? (format "~s" (obs->sexp x)) (format "~s" (obs->sexp y))))))))

(define (obs->sexp o)
  (match o
    [(obs 'saw o s n f t _) `(saw ,o ,s ,n ,(min->hhmm f) ,(min->hhmm t))]
    [(obs 'saw-door o s n f _ d) `(saw-door ,o ,s ,n ,d ,(min->hhmm f))]
    [(obs 'saw-only o ps n f t _)
     `(saw-only ,o ,n ,(min->hhmm f) ,(min->hhmm t) ,(for/list ([e ps]) (list (first e) (min->hhmm (second e)) (min->hhmm (third e)))))]
    [(obs 'doors-only o ps n f t _)
     `(doors-only ,o ,n ,(min->hhmm f) ,(min->hhmm t) ,(for/list ([e ps]) (list (first e) (second e) (min->hhmm (third e)))))]))

;; 最短路的节点序列（含两端）
(define (route m a b)
  (if (eq? a b) (list a)
      (let ([nx (for/first ([e (sort (neighbors m a) symbol<? #:key car)]
                            #:when (= (+ (cdr e) (dist m (car e) b)) (dist m a b)))
                  (car e))])
        (cons a (route m nx b)))))

;; 从基地出发的若干次往返构成的路径。excursions：(listof (list 目的地 出发时刻 停留分钟))，按出发时刻排序；放不下的往返被丢弃
(define (build-path m who base excursions t0 t1)
  (let loop ([t t0] [exs excursions] [acc '()])
    (define (finish) (path who (reverse (cons (seg base t t1) acc))))
    (cond
      [(null? exs) (finish)]
      [else
       (match-define (list dest dep stay) (car exs))
       (define go (route m base dest))
       (define back (route m dest base))
       (define arrive (+ dep (dist m base dest)))
       (define leave (+ arrive stay))
       (define home (+ leave (dist m dest base)))
       (if (or (< dep t) (> home t1) (eq? dest base))
           (loop t (cdr exs) acc)
           (let* ([acc (cons (seg base t dep) acc)]
                  [acc (let walk ([ns (cdr go)] [prev base] [time dep] [acc acc])
                         (define nt (+ time (edge-w m prev (car ns))))
                         (if (null? (cdr ns))
                             (cons (seg (car ns) nt leave) acc)
                             (walk (cdr ns) (car ns) nt (cons (seg (car ns) nt nt) acc))))]
                  [acc (let walk ([ns (cdr back)] [prev dest] [time leave] [acc acc])
                         (define nt (+ time (edge-w m prev (car ns))))
                         (if (null? (cdr ns))
                             acc   ; 回到基地那一段由下一轮的 (seg base home ...) 补上
                             (walk (cdr ns) (car ns) nt (cons (seg (car ns) nt nt) acc))))])
             (loop home (cdr exs) acc)))])))

