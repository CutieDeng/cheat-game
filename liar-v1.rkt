#lang racket/base
;; v1 凶手的谎（spec-v1.md §5、§7.2）：规则规划器。模型版规划器在 M5 之后替换这里。
;;
;; 故事 = 一条假路径。凶手回答问题时用"假记忆"：
;;   自己的行踪：故事路径的每一段（精确）；
;;   对别人的观察：假设他真按故事走、其他人按真实路径走，从他的位置推出的观察（精确）。
;;   所以他说的关于别人的话在"他若真在那里"时成立；与别人的诚实证词是否冲突，取决于故事选得好不好。
;;
;; 候选故事：真实路径中去作案现场的那次往返，换成"一直待在基地"，或"同样的时间去另一个地方"。
;; 局部规划器：只用凶手自己知道的约束——他看见过的人也看见了他（同一地点互相看得见），以及公开物证；
;;             选第一个满足这些约束的候选（"一直待在基地"优先）。
;; 全知规划器：知道全部诚实记忆；选与之冲突条数最少的候选（同分取靠前的）。
(require racket/list racket/match "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt")
(provide story-candidates local-planner omniscient-planner fake-memory)

(define (truth-of c who) (findf (λ (p) (eq? (path-who p) who)) (case1-paths c)))

(define (story-candidates c)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (define t0 (case1-t0 c))
  (define t1 (case1-t1 c))
  (define p (truth-of c who))
  (define crime-node (first (case1-crime c)))
  (define base (seg-node (first (path-segs p))))
  (define dep (seg-out (first (path-segs p))))
  (define at-crime (findf (λ (s) (eq? (seg-node s) crime-node)) (path-segs p)))
  (define stay (if at-crime (- (seg-out at-crime) (seg-in at-crime)) 0))
  (cons (path who (list (seg base t0 t1)))
        (for*/list ([x (sort (node-ids m) symbol<?)]
                    #:unless (memq x (list base crime-node))
                    [q (in-value (build-path m who base (list (list x dep stay)) t0 t1))]
                    #:when (> (length (path-segs q)) 1))
          q)))

;; 凶手确知的约束：他看见过的人也看见了他（同一地点），以及关于他的公开物证
(define (known-constraints c)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (append
   (for/list ([o (case1-observations c)]
              #:when (and (eq? (obs-kind o) 'saw) (eq? (obs-observer o) who)
                          (eq? (at-node (truth-of c (obs-subject o)) (obs-from o)) (obs-node o))))
     (claim 'k (obs-subject o) (list 'at who (obs-node o) (obs-from o) (obs-to o)) 0 'testimony))
   (for/list ([e (case1-evidence c)] #:when (memq who (claim-subjects (third e) (person-ids c)))) (third e))))

(define (violations c q claims)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (for/sum ([cl claims]) (if (path-satisfies? m q (constraints-on m cl who)) 0 1)))

(define (local-planner c)
  (define cands (story-candidates c))
  (define known (known-constraints c))
  (or (findf (λ (q) (zero? (violations c q known))) cands)
      (argmin (λ (q) (violations c q known)) cands)))

(define (omniscient-planner c)
  (define who (case1-culprit c))
  (define honest (append (memory->claims (case1-memory* c) #:only (remq who (person-ids c)))
                         (map third (case1-evidence c))))
  (argmin (λ (q) (violations c q honest)) (story-candidates c)))

;; 凶手的假记忆：故事路径每一段 + 按故事位置推出的对别人的精确观察
(define (fake-memory c story)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (define paths (for/list ([p (case1-paths c)]) (if (eq? (path-who p) who) story p)))
  (define obs (derive-observations m paths (case1-t0 c) (case1-t1 c)))
  (append (for/list ([s (path-segs story)]) (list 'at who (seg-node s) (seg-in s) (seg-out s)))
          (for/list ([o obs] #:when (eq? (obs-observer o) who)) (claim-clause (obs->claim 'x o)))))
