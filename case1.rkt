#lang racket/base
;; v1 案件文件的加载与静态校验（spec-v1.md §1、§2）
(require racket/list racket/match "map.rkt" "world.rkt" "claims.rkt")
(provide (struct-out case1) load-case1 datum->case1 case1-errors case1-observations person-ids
         case1-memory* exact-memory)

;; crime：(list 节点 lo hi d)，时间为分钟；evidence：(listof (list id 名称 claim))
;; memory：#f 或 hasheq 人 -> 子句列表（分钟），见 memory.rkt
(struct case1 (title briefing t0 t1 persons culprit crime map paths evidence memory) #:transparent)

(define (field d k) (cond [(assq k d) => cdr] [else (error 'case1 "缺少字段 ~a" k)]))

(define (datum->case1 d)
  (define m (datum->map (cons 'map (field d 'map))))
  (match-define (list w0 w1) (field d 'window))
  (match-define (list cn ca cb cd) (field d 'crime))
  (case1 (car (field d 'title)) (car (field d 'briefing))
         (hhmm->min w0) (hhmm->min w1)
         (field d 'persons) (car (field d 'culprit))
         (list cn (hhmm->min ca) (hhmm->min cb) cd)
         m
         (map datum->path (field d 'paths))
         (for/list ([e (field d 'evidence)])
           (match e [(list id name cl) (list id name (datum->claim id #f cl #:source 'evidence))]))
         (cond [(assq 'memory d)
                => (λ (x) (for/hasheq ([row (cdr x)])
                            (values (car row) (for/list ([cl (cdr row)]) (claim-clause (datum->claim 'x (car row) cl))))))]
               [else #f])))

;; 无误差的记忆：自己的真实路径 + 自己的精确观察
(define (exact-memory c)
  (define obs (case1-observations c))
  (for/hasheq ([p (case1-paths c)])
    (define who (path-who p))
    (values who
            (append (for/list ([s (path-segs p)]) (list 'at who (seg-node s) (seg-in s) (seg-out s)))
                    (for/list ([o obs] #:when (eq? (obs-observer o) who)) (claim-clause (obs->claim 'x o)))))))

;; 案件文件里有 memory 就用它，否则用无误差的记忆
(define (case1-memory* c) (or (case1-memory c) (exact-memory c)))

(define (load-case1 file) (datum->case1 (with-input-from-file file read)))

(define (person-ids c) (map car (case1-persons c)))

(define (case1-observations c)
  (derive-observations (case1-map c) (case1-paths c) (case1-t0 c) (case1-t1 c)))

;; 静态校验：路径合法且覆盖时间窗；每人恰有一条路径；凶手路径给出作案机会；物证与真实路径一致
(define (case1-errors c)
  (define m (case1-map c))
  (define ids (person-ids c))
  (define whos (map path-who (case1-paths c)))
  (define truth-of (for/hasheq ([p (case1-paths c)]) (values (path-who p) p)))
  (append
   (for*/list ([p (case1-paths c)] [e (path-errors m p (case1-t0 c) (case1-t1 c))]) e)
   (if (equal? (sort ids symbol<?) (sort whos symbol<?)) '() (list "persons 与 paths 不一致"))
   (if (memq (case1-culprit c) ids) '() (list "culprit 不在 persons 中"))
   (match (case1-crime c)
     [(list n lo hi d)
      (define p (hash-ref truth-of (case1-culprit c) #f))
      (if (and p (satisfiable? m (case1-t0 c) (case1-t1 c)
                               (for/list ([s (path-segs p)]) (in-c (seg-node s) (seg-in s) (seg-out s)))
                               #:opportunity (case1-crime c)))
          '()
          (list "凶手的真实路径没有作案机会"))])
   (for*/list ([e (case1-evidence c)]
               [cl (in-value (third e))]
               [who (claim-subjects cl ids)]
               #:unless (path-satisfies? m (hash-ref truth-of who) (constraints-on m cl who)))
     (format "物证 ~a 与 ~a 的真实路径不一致" (first e) who))))
