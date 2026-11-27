#lang racket/base
;; 凶手故事网络（M6，spec-v1.md §16、§18）的纯 Racket 推理：游戏后台直接运行，不需要 Python。
;; 权重：models/storynet-rl3.bin（m6/export-weights.py 从 PyTorch checkpoint 一次性导出）。
;; 计算与 m6/net.py 的 StoryNet + generate 逐项对应：
;;   编码器 3 层、解码器 2 层，norm_first，4 头，前馈 4d、ReLU，无最终 LayerNorm；离开头为 Linear-GELU-Linear。
;;   输入 token：地点 / 通道 / 事实（m6/export.rkt 的 case->features）；地点与人用槽位引用，生成时每次随机重排。
;;   逐段生成 (地点, 离开)：进入 = 上一段离开 + 步行分钟；地点只能选上一段的邻居且走过去不超过 T；离开 = T 结束。
;; 与 PyTorch 的一致性由 test-v1.rkt 检查（固定槽位、贪心时与 m6/big-ckpt-rl3.jsonl 的贪心故事逐段相同）。
(require racket/flonum racket/list racket/match racket/runtime-path racket/unsafe/ops
         "world.rkt" "case1.rkt" "claims.rkt" "engine-v1.rkt" "m6/export.rkt")
(provide load-storynet storynet-generate storynet-planner storynet-loaded?)

(define-runtime-path DEFAULT-WEIGHTS "models/storynet-rl3.bin")

(define MAX-NODES 12) (define MAX-PERSONS 12) (define MAX-SEGS 48) (define TMAX 120) (define HEADS 4)

;; ---------------------------------------------------------------- 权重

(struct tensor (shape data))   ; data：flvector，行优先

(define (read-u32 in) (integer-bytes->integer (read-bytes 4 in) #f #f))

(define (load-weights file)
  (call-with-input-file file
    (λ (in)
      (unless (equal? (read-bytes 4 in) #"SNW1") (error 'storynet "~a 不是 SNW1 权重文件" file))
      (define d (read-u32 in))
      (define n (read-u32 in))
      (define h (make-hash))
      (for ([_ n])
        (define name (bytes->string/utf-8 (read-bytes (read-u32 in) in)))
        (define shape (for/list ([_ (read-u32 in)]) (read-u32 in)))
        (define cnt (apply * shape))
        (define bs (read-bytes (* 4 cnt) in))
        (define v (make-flvector cnt))
        (for ([i cnt]) (flvector-set! v i (floating-point-bytes->real bs #f (* 4 i) (+ 4 (* 4 i)))))
        (hash-set! h name (tensor shape v)))
      (values d h))))

(struct net (d w))
(define current-net #f)
(define (load-storynet [file DEFAULT-WEIGHTS])
  (define-values (d w) (load-weights file))
  (set! current-net (net d w))
  current-net)
(define (storynet-loaded?) (and current-net #t))
(define (the-net) (or current-net (load-storynet)))

(define (W nt name) (tensor-data (hash-ref (net-w nt) name (λ () (error 'storynet "缺少权重 ~a" name)))))

;; ---------------------------------------------------------------- 线性代数（行优先 flvector）

;; y[n×out] = x[n×in] · Wᵀ + b；W 从 woff 起是 out×in，b 从 boff 起
(define (linear x n in w out b #:woff [woff 0] #:boff [boff 0])
  (define y (make-flvector (* n out)))
  (for* ([i (in-range n)] [o (in-range out)])
    (define xi (unsafe-fx* i in))
    (define wo (unsafe-fx+ woff (unsafe-fx* o in)))
    (let loop ([k 0] [acc (unsafe-flvector-ref b (unsafe-fx+ boff o))])
      (if (unsafe-fx< k in)
          (loop (unsafe-fx+ k 1)
                (unsafe-fl+ acc (unsafe-fl* (unsafe-flvector-ref x (unsafe-fx+ xi k))
                                            (unsafe-flvector-ref w (unsafe-fx+ wo k)))))
          (unsafe-flvector-set! y (unsafe-fx+ (unsafe-fx* i out) o) acc))))
  y)

(define (layernorm x n d g b)
  (define y (make-flvector (* n d)))
  (for ([i n])
    (define base (* i d))
    (define mean (/ (for/fold ([s 0.0]) ([k d]) (fl+ s (flvector-ref x (+ base k)))) (->fl d)))
    (define var (/ (for/fold ([s 0.0]) ([k d]) (let ([z (fl- (flvector-ref x (+ base k)) mean)]) (fl+ s (fl* z z)))) (->fl d)))
    (define inv (fl/ 1.0 (flsqrt (fl+ var 1e-5))))
    (for ([k d])
      (flvector-set! y (+ base k) (fl+ (fl* (fl* (fl- (flvector-ref x (+ base k)) mean) inv) (flvector-ref g k))
                                       (flvector-ref b k)))))
  y)

(define (add! x y) (for ([i (flvector-length x)]) (unsafe-flvector-set! x i (unsafe-fl+ (unsafe-flvector-ref x i) (unsafe-flvector-ref y i)))) x)
(define (->fl n) (exact->inexact n))

;; 多头注意力：q 来自 xq（Lq 行），k / v 已算好（Lk 行）；causal 时第 i 行只看 j ≤ i
(define (attend Q Lq K V Lk d causal?)
  (define hd (quotient d HEADS))
  (define scale (fl/ 1.0 (flsqrt (->fl hd))))
  (define out (make-flvector (* Lq d)))
  (define s (make-flvector Lk))
  (for* ([h HEADS] [i Lq])
    (define off (* h hd))
    (define lim (if causal? (add1 i) Lk))
    (define mx
      (for/fold ([mx -inf.0]) ([j lim])
        (define v (fl* scale (let loop ([c 0] [acc 0.0])
                               (if (unsafe-fx< c hd)
                                   (loop (unsafe-fx+ c 1)
                                         (unsafe-fl+ acc (unsafe-fl* (unsafe-flvector-ref Q (unsafe-fx+ (unsafe-fx* i d) (unsafe-fx+ off c)))
                                                                     (unsafe-flvector-ref K (unsafe-fx+ (unsafe-fx* j d) (unsafe-fx+ off c))))))
                                   acc))))
        (flvector-set! s j v)
        (flmax mx v)))
    (define z (for/fold ([z 0.0]) ([j lim]) (let ([e (flexp (fl- (flvector-ref s j) mx))]) (flvector-set! s j e) (fl+ z e))))
    (for ([j lim])
      (define p (fl/ (flvector-ref s j) z))
      (for ([c hd])
        (define o (+ (* i d) off c))
        (unsafe-flvector-set! out o (unsafe-fl+ (unsafe-flvector-ref out o) (unsafe-fl* p (unsafe-flvector-ref V (+ (* j d) off c))))))))
  out)

(define (kv nt prefix x L d)
  (define wi (W nt (string-append prefix ".in_proj_weight"))) (define bi (W nt (string-append prefix ".in_proj_bias")))
  (values (linear x L d wi d bi #:woff (* d d) #:boff d)
          (linear x L d wi d bi #:woff (* 2 d d) #:boff (* 2 d))))

(define (mha nt prefix xq Lq K V Lk d causal?)
  (define wi (W nt (string-append prefix ".in_proj_weight"))) (define bi (W nt (string-append prefix ".in_proj_bias")))
  (define Q (linear xq Lq d wi d bi))
  (define o (attend Q Lq K V Lk d causal?))
  (linear o Lq d (W nt (string-append prefix ".out_proj.weight")) d (W nt (string-append prefix ".out_proj.bias"))))

(define (ff nt prefix x L d)
  (define h (linear x L d (W nt (string-append prefix ".linear1.weight")) (* 4 d) (W nt (string-append prefix ".linear1.bias"))))
  (for ([i (flvector-length h)]) (when (fl< (flvector-ref h i) 0.0) (flvector-set! h i 0.0)))   ; ReLU
  (linear h L (* 4 d) (W nt (string-append prefix ".linear2.weight")) d (W nt (string-append prefix ".linear2.bias"))))

(define (ln nt prefix x L d) (layernorm x L d (W nt (string-append prefix ".weight")) (W nt (string-append prefix ".bias"))))

(define (encoder-layer nt i x L d)
  (define p (format "enc.layers.~a" i))
  (define y (ln nt (string-append p ".norm1") x L d))
  (define-values (K V) (kv nt (string-append p ".self_attn") y L d))
  (add! x (mha nt (string-append p ".self_attn") y L K V L d #f))
  (add! x (ff nt p (ln nt (string-append p ".norm2") x L d) L d)))

;; 解码层；mem-kv：该层交叉注意力的 (K . V)，只算一次
(define (decoder-layer nt i x L mem-kv Lm d)
  (define p (format "dec.layers.~a" i))
  (define y (ln nt (string-append p ".norm1") x L d))
  (define-values (K V) (kv nt (string-append p ".self_attn") y L d))
  (add! x (mha nt (string-append p ".self_attn") y L K V L d #t))
  (add! x (mha nt (string-append p ".multihead_attn") (ln nt (string-append p ".norm2") x L d) L (car mem-kv) (cdr mem-kv) Lm d #f))
  (add! x (ff nt p (ln nt (string-append p ".norm3") x L d) L d)))

;; erf（Abramowitz–Stegun 7.1.26，绝对误差 ≤ 1.5e-7），用于精确 GELU
(define (erf x)
  (define s (if (fl< x 0.0) -1.0 1.0))
  (define a (flabs x))
  (define t (fl/ 1.0 (fl+ 1.0 (fl* 0.3275911 a))))
  (define y (fl- 1.0 (fl* (fl* t (fl+ 0.254829592 (fl* t (fl+ -0.284496736 (fl* t (fl+ 1.421413741 (fl* t (fl+ -1.453152027 (fl* t 1.061405429)))))))))
                          (flexp (fl- 0.0 (fl* a a))))))
  (fl* s y))
(define (gelu x) (fl* (fl* 0.5 x) (fl+ 1.0 (erf (fl/ x (flsqrt 2.0))))))

;; ---------------------------------------------------------------- 输入 token

(define (row! dst doff nt name r d)
  (define src (W nt name))
  (for ([k d]) (unsafe-flvector-set! dst (+ doff k) (unsafe-fl+ (unsafe-flvector-ref dst (+ doff k)) (unsafe-flvector-ref src (+ (* r d) k))))))

(define (tokens nt f node-perm person-perm d)
  (define nodes (hash-ref f 'nodes)) (define edges (hash-ref f 'edges)) (define facts (hash-ref f 'facts))
  (define self (hash-ref f 'self))
  (define L (+ (length nodes) (length edges) (length facts)))
  (define x (make-flvector (* L d)))
  (for ([nd nodes] [i (in-naturals)])
    (match-define (list k dg c wo wi) nd)
    (define o (* i d))
    (row! x o nt "tok_type.weight" 0 d) (row! x o nt "slot.weight" (vector-ref node-perm i) d)
    (row! x o nt "kind.weight" k d) (row! x o nt "deg.weight" (min dg 7) d)
    (row! x o nt "flag.0.weight" c d) (row! x o nt "flag.1.weight" wo d) (row! x o nt "flag.2.weight" wi d))
  (for ([e edges] [i (in-naturals (length nodes))])
    (match-define (list a b w) e)
    (define o (* i d))
    (row! x o nt "tok_type.weight" 1 d) (row! x o nt "slot.weight" (vector-ref node-perm a) d)
    (row! x o nt "slot2.weight" (vector-ref node-perm b) d) (row! x o nt "wemb.weight" (min w 15) d))
  (for ([fa facts] [i (in-naturals (+ (length nodes) (length edges)))])
    (match-define (list t nd p a b xx) fa)
    (define o (* i d))
    (define (clip v) (max 0 (min v (sub1 TMAX))))
    (row! x o nt "tok_type.weight" (+ t 2) d)
    (row! x o nt "slot.weight" (if (>= nd 0) (vector-ref node-perm nd) MAX-NODES) d)
    (row! x o nt "person.weight" (cond [(< p 0) MAX-PERSONS] [(= p self) (add1 MAX-PERSONS)] [else (vector-ref person-perm p)]) d)
    (row! x o nt "ta.weight" (clip a) d) (row! x o nt "tb.weight" (clip b) d) (row! x o nt "xemb.weight" (min xx 31) d))
  (values x L))

;; ---------------------------------------------------------------- 生成

(define (pick logits allowed greedy? temp rng)
  ;; allowed：可选下标列表；贪心时取最大（并列取下标最小，与 torch.argmax 一致）
  (cond
    [greedy? (for/fold ([b #f]) ([j allowed]) (if (or (not b) (fl> (flvector-ref logits j) (flvector-ref logits b))) j b))]
    [else
     (define mx (for/fold ([m -inf.0]) ([j allowed]) (flmax m (flvector-ref logits j))))
     (define ps (for/list ([j allowed]) (flexp (fl/ (fl- (flvector-ref logits j) mx) temp))))
     (define z (apply + ps))
     (define u (* z (random rng)))
     (let loop ([js allowed] [ps ps] [acc 0.0])
       (cond [(null? (cdr js)) (car js)]
             [(< u (+ acc (car ps))) (car js)]
             [else (loop (cdr js) (cdr ps) (+ acc (car ps)))]))]))

;; 返回 [(地点下标 进入 离开) ...]（相对 t0 的分钟）
;; #:shuffle? #f 时槽位不重排（与 net.py 的 augment=False 相同，用于和 PyTorch 对照）
(define (storynet-generate f #:greedy? [greedy? #f] #:temp [temp 1.0] #:shuffle? [shuffle? #t]
                           #:rng [rng (current-pseudo-random-generator)])
  (define nt (the-net))
  (define d (net-d nt))
  (define nn (length (hash-ref f 'nodes)))
  (define P (hash-ref f 'persons))
  (define T (hash-ref f 'T))
  (define (perm n) (list->vector (if shuffle?
                                     (take (parameterize ([current-pseudo-random-generator rng]) (shuffle (range MAX-NODES))) n)
                                     (range n))))
  (define node-perm (perm nn))
  (define person-perm (perm P))
  (define adj (make-vector nn '()))   ; 下标 → ((邻居 . 分钟) ...)
  (for ([e (hash-ref f 'edges)]) (match-define (list a b w) e) (vector-set! adj a (cons (cons b w) (vector-ref adj a))))
  ;; 编码
  (define-values (x L) (tokens nt f node-perm person-perm d))
  (for ([i 3]) (encoder-layer nt i x L d))
  (define mem x)
  (define node-h (let ([v (make-flvector (* nn d))]) (for ([k (* nn d)]) (flvector-set! v k (flvector-ref mem k))) v))
  (define mem-kv (for/list ([i 2]) (let-values ([(K V) (kv nt (format "dec.layers.~a.multihead_attn" i) mem L d)]) (cons K V))))
  (define dnode (linear node-h nn d (W nt "dnode.weight") d (W nt "dnode.bias")))
  (define kn (linear node-h nn d (W nt "k.weight") d (W nt "k.bias")))
  (define (seg-row! dst o n a b)
    (for ([k d]) (flvector-set! dst (+ o k) (+ (flvector-ref dst (+ o k)) (flvector-ref dnode (+ (* n d) k)))))
    (row! dst o nt "din.weight" (min a (sub1 TMAX)) d) (row! dst o nt "dout.weight" (min b (sub1 TMAX)) d))
  (let loop ([i 0] [segs '()])   ; segs 逆序
    (cond
      [(>= i MAX-SEGS) (reverse segs)]
      [else
       (define Ld (add1 i))
       (define xd (make-flvector (* Ld d)))
       (for ([k d]) (flvector-set! xd k (flvector-ref (W nt "start") k)))
       (for ([s (reverse segs)] [p (in-naturals 1)]) (seg-row! xd (* p d) (first s) (second s) (third s)))
       (for ([p Ld]) (row! xd (* p d) nt "pos.weight" p d))
       (for ([l 2] [m mem-kv]) (decoder-layer nt l xd Ld m L d))
       (define h (let ([v (make-flvector d)]) (for ([k d]) (flvector-set! v k (flvector-ref xd (+ (* i d) k)))) v))
       ;; 地点
       (define qh (linear h 1 d (W nt "q.weight") d (W nt "q.bias")))
       (define nl (make-flvector nn))
       (for ([j nn]) (flvector-set! nl j (fl/ (for/fold ([acc 0.0]) ([k d]) (fl+ acc (fl* (flvector-ref qh k) (flvector-ref kn (+ (* j d) k)))))
                                              (flsqrt (->fl d)))))
       (define prev (and (pair? segs) (car segs)))
       (define allowed (if prev
                           (for/list ([j nn] #:when (let ([w (assv j (vector-ref adj (first prev)))]) (and w (<= (+ (third prev) (cdr w)) T)))) j)
                           (range nn)))
       (define n (pick nl allowed greedy? temp rng))
       (define entry (if prev (+ (third prev) (cdr (assv n (vector-ref adj (first prev))))) 0))
       ;; 离开
       (define in3 (make-flvector (* 3 d)))
       (for ([k d]) (flvector-set! in3 k (flvector-ref h k)) (flvector-set! in3 (+ d k) (flvector-ref node-h (+ (* n d) k))))
       (row! in3 (* 2 d) nt "entry.weight" (min entry (sub1 TMAX)) d)
       (define hid (linear in3 1 (* 3 d) (W nt "leave.0.weight") (* 2 d) (W nt "leave.0.bias")))
       (for ([k (flvector-length hid)]) (flvector-set! hid k (gelu (flvector-ref hid k))))
       (define ll (linear hid 1 (* 2 d) (W nt "leave.2.weight") TMAX (W nt "leave.2.bias")))
       (define minw (for/fold ([m +inf.0]) ([e (vector-ref adj n)]) (min m (cdr e))))
       (define lallowed (for/list ([t TMAX] #:when (and (>= t entry) (or (= t T) (<= (+ t minw) T)))) t))
       (define o (pick ll lallowed greedy? temp rng))
       (define segs* (cons (list n entry o) segs))
       (if (>= o T) (reverse segs*) (loop (add1 i) segs*))])))

;; ---------------------------------------------------------------- 游戏用的规划器

;; 凶手规划器：用网络采样故事；只按凶手自己知道的东西过滤——路径可行、不自认（照实说出作案）。
;; 不用校验器挑"不被拆穿"的：那要读别人的诚实记忆，凶手在对局里拿不到（spec §16.4）。
;; 最多试 tries 次，都不行时退回 fallback（局部规划器）。
;; seed：给定时用它初始化随机数，同一案件同一 seed 得到同一个故事（对局可复现）
(define (storynet-planner #:tries [tries 8] #:fallback [fallback #f] #:temp [temp 1.0] #:seed [seed #f])
  (λ (c)
    (define rng (let ([r (make-pseudo-random-generator)])
                  (when seed (parameterize ([current-pseudo-random-generator r]) (random-seed seed)))
                  r))
    (define f (case->features c))
    (define ids (map string->symbol (hash-ref f 'node_ids)))
    (define who (case1-culprit c))
    (define t0 (case1-t0 c))
    (define (->path st) (path who (for/list ([s st]) (seg (list-ref ids (first s)) (+ t0 (second s)) (+ t0 (third s))))))
    (or (for/or ([_ tries])
          (define p (->path (storynet-generate f #:temp temp #:rng rng)))
          (and (null? (path-errors (case1-map c) p t0 (case1-t1 c)))
               (not (admits-claims? c who (for/list ([s (path-segs p)] [i (in-naturals)])
                                            (claim (string->symbol (format "S#~a" i)) who
                                                   (list 'at who (seg-node s) (seg-in s) (seg-out s)) TOL-SELF 'testimony))))
               p))
        (if fallback (fallback c) (error 'storynet "~a 次采样都没有合法故事" tries)))))
