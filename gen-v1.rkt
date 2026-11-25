#lang racket/base
;; v1 案件生成（spec-v1.md §2、§3、§7.1）
;;
;; 地图：1–2 条走廊为主干，房间挂在走廊上（1 分钟），户外挂在走廊上（2 分钟）；可选一扇户外与某房间之间的窗（双向）。
;; 人：非凶手分成 2–3 人一组，各组有一个基地（房间或户外）；凶手单独一个房间，或以 p-culprit-joins 加入一组。
;; 路径：每人从基地出发、最后回到基地；非凶手 0–2 次往返（去别组的基地、走廊或户外，停留 5–20 分钟，不进作案房间）；
;;       凶手一次往返进作案房间，在案发窗口内停留 ≥ d 分钟。
;; 物证：1–2 条精确的短时位置事实（从真实路径中截取）。
;; 记忆：memory.rkt 采样；记混 / 没注意到的概率按 min(1, (5/n)²) 缩放。
;; 检查（§7.1）：静态校验；每个非凶手只用其他非凶手的记忆和物证就被排除；凶手不被诚实记忆排除。
;;
;; racket gen-v1.rkt --seed N [--npcs 4..8] [--out 文件]
;; racket gen-v1.rkt --batch K [--npcs ...]     批量统计：接受率、尝试次数、诚实冲突数
(require racket/list racket/match racket/format
         "map.rkt" "world.rkt" "claims.rkt" "case1.rkt" "memory.rkt" "exclude.rkt")
(provide generate-case1 case-checks honest-conflicts p-culprit-joins)

;; 凶手加入某组（与别人同住）的概率
(define p-culprit-joins (make-parameter 0.5))

(define PERSON-NAMES '("管家" "厨师" "园丁" "女仆" "司机" "秘书" "医生" "律师" "侄子" "表妹"))
(define ROOM-NAMES '("厨房" "书房" "客厅" "餐厅" "琴房" "储藏室" "卧室" "画室" "台球室"))
(define T0 (hhmm->min 1930))
(define T1 (hhmm->min 2100))

(define (shuffle* xs rng)
  (let loop ([xs xs] [acc '()])
    (if (null? xs) acc
        (let ([x (list-ref xs (random (length xs) rng))]) (loop (remq x xs) (cons x acc))))))
(define (pick xs rng) (list-ref xs (random (length xs) rng)))
(define (rnd lo hi rng) (+ lo (random (add1 (- hi lo)) rng)))   ; [lo, hi]
(define (chance p rng) (< (random rng) p))

(define (clock m) (format "~a:~a~a" (quotient m 60) (if (< (remainder m 60) 10) "0" "") (remainder m 60)))

;; 生成一个候选案件（datum，时间 HHMM）。不做检查。
(define (candidate rng n)
  (define ids (for/list ([i n]) (string->symbol (format "E~a" (add1 i)))))
  (define names (for/hasheq ([e ids] [nm (take (shuffle* PERSON-NAMES rng) n)]) (values e nm)))
  (define culprit (pick ids rng))
  (define innocents (remq culprit ids))
  ;; 分组：2–3 人一组
  (define groups
    (let loop ([xs (shuffle* innocents rng)] [acc '()])
      (cond [(null? xs) acc]
            [(<= (length xs) 3) (cons xs acc)]
            [(= (length xs) 4) (list* (take xs 2) (drop xs 2) acc)]
            [else (let ([k (rnd 2 3 rng)]) (loop (drop xs k) (cons (take xs k) acc)))])))
  (define joins? (chance (p-culprit-joins) rng))
  ;; 地图
  (define n-corr (if (> (length groups) 2) 2 1))
  (define corrs (for/list ([i n-corr]) (string->symbol (format "hall~a" i))))
  (define outdoor? (chance 0.6 rng))
  (define n-rooms (+ (length groups) (if outdoor? 0 1) (if joins? 0 1) 1 (rnd 0 1 rng)))   ; 基地 + 凶手房间 + 作案房间 + 空房
  (define rooms (for/list ([i n-rooms]) (string->symbol (format "r~a" i))))
  (define room-names (for/hasheq ([r rooms] [nm (shuffle* ROOM-NAMES rng)]) (values r nm)))
  (define crime-room (last rooms))
  (define room-corr (for/hasheq ([r rooms]) (values r (pick corrs rng))))
  (define bases-pool (append (if outdoor? '(garden) '()) (remq crime-room rooms)))
  (define group-bases (take (shuffle* bases-pool rng) (length groups)))
  (define culprit-base
    (if joins?
        (pick group-bases rng)
        (pick (or (let ([free (remove* group-bases (remq 'garden (remq crime-room rooms)))]) (and (pair? free) free))
                  (remq crime-room rooms))
              rng)))
  (define window
    (and outdoor? (chance 0.5 rng)
         (let ([rs (remq crime-room rooms)]) (and (pair? rs) (pick rs rng)))))
  (define map-items
    (append
     (for/list ([c corrs] [i (in-naturals)]) `(node ,c corridor ,(if (= i 0) "走廊" "后走廊")))
     (for/list ([r rooms]) `(node ,r room ,(hash-ref room-names r)))
     (if outdoor? '((node garden outdoor "花园")) '())
     (if (= n-corr 2) `((edge ,(first corrs) ,(second corrs) ,(rnd 1 2 rng))) '())
     (for/list ([r rooms]) `(edge ,r ,(hash-ref room-corr r) 1))
     (if outdoor? `((edge garden ,(first corrs) 2)) '())
     (if window `((window garden ,window) (window ,window garden)) '())))
  (define m (datum->map (cons 'map map-items)))
  (define base-of
    (for*/hasheq ([(g b) (in-parallel groups group-bases)] [e g]) (values e b)))
  (define base (λ (e) (if (eq? e culprit) culprit-base (hash-ref base-of e))))
  ;; 作案：窗口 [clo, chi]，至少 d 分钟
  (define d (rnd 12 18 rng))
  (define clo (+ T0 (rnd 30 50 rng)))   ; 结束不晚于 T0 + 50 + 18 + 15 = 20:53，在时间窗内
  (define chi (+ clo d (rnd 8 15 rng)))
  ;; 路径
  (define dests-for
    (λ (e) (remove* (list crime-room (base e))
                    (remove-duplicates (append group-bases corrs (if outdoor? '(garden) '()))))))
  (define paths
    (for/list ([e ids])
      (cond
        [(eq? e culprit)
         (define arrive (rnd clo (- chi d) rng))
         (define dep (- arrive (dist m (base e) crime-room)))
         (build-path m e (base e) (list (list crime-room dep (+ d (rnd 0 4 rng)))) T0 T1)]
        [else
         (define k (rnd 0 2 rng))
         (define exs
           (sort (for/list ([i k])
                   (list (pick (dests-for e) rng) (+ T0 (rnd 5 110 rng)) (rnd 5 20 rng)))
                 < #:key second))
         (build-path m e (base e) exs T0 T1)])))
  ;; 物证：1–2 条短时位置事实
  (define evidence
    (for/list ([i (rnd 1 2 rng)])
      (define p (pick paths rng))
      (define ss (filter (λ (s) (>= (- (seg-out s) (seg-in s)) 2)) (path-segs p)))
      (define s (pick ss rng))
      (define a (rnd (seg-in s) (max (seg-in s) (- (seg-out s) 2)) rng))
      (define b (min (seg-out s) (+ a (rnd 2 5 rng))))
      `(,(string->symbol (format "EV~a" (add1 i)))
        ,(format "~a的痕迹" (hash-ref names (path-who p)))
        (at ,(path-who p) ,(seg-node s) ,(min->hhmm a) ,(min->hhmm b)))))
  (define obs (derive-observations m paths T0 T1))
  ;; 观察条数随人数约按人数平方增长；记混 / 没注意到的概率按 (5/n)² 缩放，使可发现的错记每局约 0.5–1 条（见 spec-v1.md §9）
  (define scale (min 1 (expt (/ 5 n) 2)))
  (define noise (hash-set* DEFAULT-NOISE
                           'p-confuse (* scale (hash-ref DEFAULT-NOISE 'p-confuse))
                           'p-miss (* scale (hash-ref DEFAULT-NOISE 'p-miss))))
  (define mem (sample-memory m paths obs rng #:noise noise #:window (cons T0 T1)))
  `((title "地图版案件")
    (briefing ,(format "被害人在~a遇害，时间约 ~a–~a。找出凶手。"
                       (hash-ref room-names crime-room) (clock clo) (clock chi)))
    (window ,(min->hhmm T0) ,(min->hhmm T1))
    (persons ,@(for/list ([e ids]) (list e (hash-ref names e))))
    (culprit ,culprit)
    (crime ,crime-room ,(min->hhmm clo) ,(min->hhmm chi) ,d)
    (map ,@map-items)
    (paths ,@(for/list ([p paths])
               `(path ,(path-who p) ,@(for/list ([s (path-segs p)])
                                        (list (seg-node s) (min->hhmm (seg-in s)) (min->hhmm (seg-out s)))))))
    (evidence ,@evidence)
    (memory ,@(for/list ([e ids]) (cons e (map clause->datum (hash-ref mem e)))))))

;; ---------------------------------------------------------------- 检查

(define (evidence-claims c) (map third (case1-evidence c)))

;; 返回失败原因列表（空表示通过）
(define (case-checks c)
  (define m (case1-map c))
  (define t0 (case1-t0 c))
  (define t1 (case1-t1 c))
  (define culprit (case1-culprit c))
  (define mem (case1-memory* c))
  (define innocents (remq culprit (person-ids c)))
  (define errs (case1-errors c))
  (if (pair? errs)
      errs
      (append
       (for/list ([y innocents]
                  #:unless (excluded? m t0 t1 y
                                      (append (memory->claims mem #:only (remq y innocents)) (evidence-claims c))
                                      (case1-crime c)))
         (format "~a 无法被排除" y))
       (if (excluded? m t0 t1 culprit (append (memory->claims mem #:only innocents) (evidence-claims c))
                      (case1-crime c))
           (list "凶手被诚实记忆排除")
           '()))))

;; 诚实误差统计（只用非凶手的记忆和物证）：
;;   wrong      与真相不符的诚实声明条数（记混地点、没注意到某人造成）
;;   detectable 其中出现在某个极小冲突核里的条数（玩家能发现的矛盾来源）
;;   cores      极小冲突核个数（同一条错记可能与多条负观察各成一个核）
(define (honest-conflicts c)
  (define m (case1-map c))
  (define mem (case1-memory* c))
  (define ids (person-ids c))
  (define innocents (remq (case1-culprit c) ids))
  (define cls (append (memory->claims mem #:only innocents) (evidence-claims c)))
  (define truth (for/hasheq ([p (case1-paths c)]) (values (path-who p) p)))
  (define wrong
    (for/list ([cl cls]
               #:unless (for/and ([y (claim-subjects cl ids)])
                          (path-satisfies? m (hash-ref truth y) (constraints-on m cl y))))
      (claim-id cl)))
  (define cores
    (append* (for/list ([y ids]) (conflict-cores m (case1-t0 c) (case1-t1 c) y cls))))
  (define in-cores (remove-duplicates (append* cores)))
  (hasheq 'wrong (length wrong)
          'detectable (length (filter (λ (id) (memq id in-cores)) wrong))
          'cores (length cores)))

;; 返回 (values datum 尝试次数 最后一次失败原因)
(define (generate-case1 seed #:npcs [n 5] #:attempts [attempts 100])
  (let loop ([a 0] [last-errs '()])
    (cond
      [(>= a attempts) (values #f attempts last-errs)]
      [else
       (define rng (make-pseudo-random-generator))
       (parameterize ([current-pseudo-random-generator rng]) (random-seed (+ (* seed 1000) a)))
       (define d (candidate rng n))
       (define errs (with-handlers ([exn:fail? (λ (e) (list (exn-message e)))])
                      (case-checks (datum->case1 d))))
       (if (null? errs) (values d (add1 a) '()) (loop (add1 a) errs))])))

(module+ main
  (require racket/cmdline racket/pretty)
  (define seed (make-parameter 1))
  (define npcs (make-parameter 5))
  (define out (make-parameter #f))
  (define batch (make-parameter #f))
  (command-line #:once-each
                ["--seed" s "种子" (seed (string->number s))]
                ["--npcs" s "人数 4..8" (npcs (string->number s))]
                ["--out" f "输出文件" (out f)]
                ["--batch" k "批量统计 K 个种子" (batch (string->number k))])
  (cond
    [(batch)
     (define t0 (current-inexact-milliseconds))
     (define rows
       (for/list ([s (in-range 1 (add1 (batch)))])
         (define-values (d a errs) (generate-case1 s #:npcs (npcs)))
         (define hc (and d (honest-conflicts (datum->case1 d))))
         (list s a (and d #t) hc errs)))
     (define ok (filter third rows))
     (define (stat k)
       (define xs (map (λ (r) (hash-ref (fourth r) k)) ok))
       (format "均值 ~a 分布 ~a" (~r (/ (apply + xs) (max 1 (exact->inexact (length xs)))) #:precision 2)
               (sort (for/list ([g (group-by values xs)]) (cons (car g) (length g))) < #:key car)))
     (printf "npcs ~a：~a/~a 个种子生成成功；平均尝试 ~a 次；用时 ~a s\n  错记 ~a\n  可发现的错记 ~a\n  冲突核 ~a\n"
             (npcs) (length ok) (length rows)
             (~r (/ (apply + (map second ok)) (max 1 (exact->inexact (length ok)))) #:precision 2)
             (round (/ (- (current-inexact-milliseconds) t0) 1000))
             (stat 'wrong) (stat 'detectable) (stat 'cores))
     (for ([r rows] #:unless (third r))
       (printf "  种子 ~a 失败：~a\n" (first r) (take (fifth r) (min 3 (length (fifth r))))))]
    [else
     (define-values (d a errs) (generate-case1 (seed) #:npcs (npcs)))
     (unless d (eprintf "生成失败：~a\n" errs) (exit 1))
     (eprintf "第 ~a 次尝试通过\n" a)
     (if (out)
         (with-output-to-file (out) #:exists 'replace (λ () (pretty-write d)))
         (pretty-write d))]))
