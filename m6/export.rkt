#lang racket/base
;; v1 M6：把案件（凶手视角）与教师候选故事导出成 JSON 行，供 m6/net.py 训练。
;;   racket m6/export.rkt --out m6/data.jsonl runs/sft-v1b.rktd runs/sft-v2-pilot*.rktd runs/sft-v2.rktd
;; 输入只含凶手知道的东西（与 teacher-v1.rkt 的提示同一范围，spec §5.1）：
;;   地图、案情（作案地点 / 时段 / 时长）、物证、自己的真实行踪、自己的观察
;; 每个 (npcs, seed) 一行；多个文件里的同一案件合并候选（故事去重）。
;; 时间一律为相对 t0 的分钟数。
;; 行格式：
;;   npcs seed T                            T = t1 − t0
;;   nodes: [[kind deg crime win_out win_in] ...]   kind 0 房间 1 走廊 2 户外
;;   edges: [[i j w] ...]                   双向各一条
;;   persons: 人数（含自己），self: 自己的下标
;;   facts: [[type node person a b x] ...]  node / person 为下标，无则 −1；类型见 FACT-TYPES
;;   cands: [[story key] ...]               story = [[node in out] ...]，key 为校验器排序键
;;   local / omni: 两个规则规划器的 key
(require racket/list racket/match racket/cmdline json
         "../map.rkt" "../world.rkt" "../claims.rkt" "../case1.rkt" "../gen-v1.rkt")
(provide case->features FACT-TYPES)

(define FACT-TYPES '(crime truth saw saw-door watch present door-watch door-event evidence))
(define (ty k) (index-of FACT-TYPES k))

(define (case->features c)
  (define m (case1-map c))
  (define who (case1-culprit c))
  (define t0 (case1-t0 c))
  (define ids (node-ids m))
  (define (ni n) (index-of ids n))
  (define pids (person-ids c))
  (define (pi p) (or (index-of pids p) -1))
  (define (rt t) (- t t0))
  (match-define (list cn clo chi cd) (case1-crime c))
  (define truth (findf (λ (p) (eq? (path-who p) who)) (case1-paths c)))
  (define wins (gmap-windows m))
  (define nodes
    (for/list ([n ids])
      (list (case (kind-of m n) [(room) 0] [(corridor) 1] [else 2])
            (length (neighbors m n))
            (if (eq? n cn) 1 0)
            (if (assq n wins) 1 0)
            (if (for/or ([w wins]) (eq? (cdr w) n)) 1 0))))
  (define edges
    (for*/list ([a ids] [e (neighbors m a)]) (list (ni a) (ni (car e)) (cdr e))))
  (define (f type node person a b [x 0]) (list (ty type) (if node (ni node) -1) (if person (pi person) -1) (rt a) (rt b) x))
  (define facts
    (append
     (list (f 'crime cn #f clo chi cd))
     (for/list ([s (path-segs truth)])
       (f 'truth (seg-node s) #f (seg-in s) (seg-out s)
          (if (and (eq? (seg-node s) cn) (< (max (seg-in s) clo) (min (seg-out s) chi))) 1 0)))
     (append*
      (for/list ([o (case1-observations c)] #:when (eq? (obs-observer o) who))
        (match (obs-kind o)
          ['saw (list (f 'saw (obs-node o) (obs-subject o) (obs-from o) (obs-to o)))]
          ['saw-door (list (f 'saw-door (obs-node o) (obs-subject o) (obs-from o) (obs-to o) (if (eq? (obs-dir o) 'in) 1 0)))]
          ['saw-only
           (cons (f 'watch (obs-node o) #f (obs-from o) (obs-to o) (length (obs-subject o)))
                 (for/list ([x (obs-subject o)]) (f 'present (obs-node o) (first x) (second x) (third x))))]
          ['doors-only
           (cons (f 'door-watch (obs-node o) #f (obs-from o) (obs-to o) (length (obs-subject o)))
                 (for/list ([x (obs-subject o)]) (f 'door-event (obs-node o) (first x) (third x) (third x) (if (eq? (second x) 'in) 1 0))))])))
     (for/list ([e (case1-evidence c)])
       (match (claim-clause (third e))
         [(list 'at p n a b) (f 'evidence n p a b)]))))
  (hasheq 'T (- (case1-t1 c) t0) 'nodes nodes 'node_ids (map symbol->string ids) 'edges edges
          'persons (length pids) 'self (pi who) 'facts facts))

(define (story->rel c sd)
  ;; sd = (story (node in out) ...)，时刻为 HHMM
  (define ids (node-ids (case1-map c)))
  (for/list ([s (cdr sd)])
    (match s [(list n a b) (list (index-of ids n) (- (hhmm->min a) (case1-t0 c)) (- (hhmm->min b) (case1-t0 c)))])))

(define (read-rows file)
  ;; 逐行读；文件正在被追加时最后一行可能不完整，读失败就停
  (with-input-from-file file
    (λ () (let loop ([acc '()])
            (define x (with-handlers ([exn:fail? (λ (e) eof)]) (read)))
            (if (eof-object? x) (reverse acc) (loop (cons x acc)))))))

(module+ main
  (define out (make-parameter "m6/data.jsonl"))
  (define fresh (make-parameter #f))
  (define seeds (make-parameter "1001 1300"))
  (define files (command-line #:once-each
                              ["--out" f "输出 JSONL" (out f)]
                              ["--fresh" ns "只导出新案件（无教师候选，供强化学习），人数列表，如 \"4 5 6 8\"" (fresh ns)]
                              ["--seeds" r "--fresh 的种子范围 \"起 止\"（含两端；跳过 seed % 10 == 0，那是测试集的划分）" (seeds r)]
                              #:args fs fs))
  (when (fresh)
    (define ns (map string->number (regexp-split #rx" +" (fresh))))
    (define r (map string->number (regexp-split #rx" +" (seeds))))
    (with-output-to-file (out) #:exists 'replace
      (λ () (for* ([n ns] [s (in-range (first r) (add1 (second r)))] #:unless (zero? (modulo s 10)))
              (define-values (d a e) (generate-case1 s #:npcs n))
              (when d
                (write-json (hash-set* (case->features (datum->case1 d)) 'npcs n 'seed s 'cands '()))
                (newline)))))
    (eprintf "导出新案件 → ~a\n" (out))
    (exit 0))
  (define by-case (make-hash)) ; (npcs . seed) -> row 列表
  (for* ([f files] [r (read-rows f)])
    (hash-update! by-case (cons (hash-ref r 'npcs) (hash-ref r 'seed)) (λ (l) (cons r l)) '()))
  (define keys (sort (hash-keys by-case) (λ (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))
  (with-output-to-file (out) #:exists 'replace
    (λ ()
      (for ([k keys])
        (define rows (hash-ref by-case k))
        (define-values (d a e) (generate-case1 (cdr k) #:npcs (car k)))
        (define c (datum->case1 d))
        (define seen (make-hash))
        (define cands
          (for*/list ([r rows] [x (hash-ref r 'candidates)]
                      #:when (hash-ref x 'story #f)
                      #:unless (hash-ref seen (hash-ref x 'story) #f))
            (hash-set! seen (hash-ref x 'story) #t)
            (list (story->rel c (hash-ref x 'story)) (hash-ref x 'key))))
        (write-json (hash-set* (case->features c) 'npcs (car k) 'seed (cdr k) 'cands cands
                               'local (hash-ref (car rows) 'local) 'omni (hash-ref (car rows) 'omni)))
        (newline))))
  (eprintf "导出 ~a 案 → ~a\n" (length keys) (out)))
