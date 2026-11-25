#lang racket/base
;; 思考格式可行性测试（v3：扩展动作空间 + 生成案件）。在有实质选择的决策点上运行
;;   思考任务（解析失败时重试 1 次）
;;   决策 A：给思考任务的输出（对局按 A 的选择继续）
;;   决策 B：不给分析（对照组）
;; 三次调用都开 DeepSeek 自身的思考模式。其余决策点用非思考模式的单次选择推进对局。
;; 有实质选择 = 存在暴露选项，或者选项多于 2 个。
;; racket think-eval.rkt [--max N] [--games G] [--per-game K] [--gen "npcs slots layers secrets"] [--out 文件]
(require racket/list racket/match racket/cmdline racket/format racket/string
         "kb.rkt" "engine.rkt" "llm.rkt" "think.rkt" "eval.rkt" "gen.rkt")

(define (admission? cd a)
  (define c (case-data-crime cd))
  (for/or ([p (action-props a)])
    (define s (prop-st p))
    (and (eq? (prop-pol p) 'pos) (eq? (st-agent s) (st-agent c)) (eq? (st-loc s) (st-loc c)) (overlap? s c))))

(define (known-to-detective? g p)
  (define cd (game-cd g))
  (define pool (append (map third (case-data-evidence cd)) (map claim-prop (game-claims g))))
  (for/or ([q pool])
    (and (eq? (prop-pol q) (prop-pol p))
         (st? (prop-st q)) (st? (prop-st p))
         (eq? (st-agent (prop-st q)) (st-agent (prop-st p)))
         (eq? (st-loc (prop-st q)) (st-loc (prop-st p)))
         (overlap? (prop-st q) (prop-st p)))))

(define (shuffle/rng rng xs) (parameterize ([current-pseudo-random-generator rng]) (shuffle xs)))

(define (decision-info cd actions d order)
  (define n (hash-ref d 'choice))
  (define a (and n (list-ref order (sub1 n))))
  (hasheq 'valid (and n #t) 'pos n
          'index (and a (index-of actions a))
          'kind (and a (action-kind a)) 'exposing (and a (action-exposing? a))
          'refutable (and a (action-refutable? a)) 'admission (and a (admission? cd a))
          'ms (hash-ref d 'ms) 'tokens (hash-ref d 'tokens) 'output (hash-ref d 'output)))

(define (think-path cd g view actions rng seed)
  (define order1 (shuffle/rng rng actions))
  (define t (think-task cd view order1))
  (define th (hash-ref t 'thought))
  (define order2 (shuffle/rng rng actions))
  (define order3 (shuffle/rng rng actions))
  (define dA (decide-task cd view order2 (if th (thought->text cd th order1 order2) "（无）")))
  (define dB (decide-task cd view order3 #f))
  ;; 风险标注按原始 actions 的下标记录（同一种类可能有多个编造选项）
  (define risk-of
    (if th
        (for/hasheqv ([r (hash-ref th 'risks)]) (values (index-of actions (list-ref order1 (sub1 (car r)))) (cdr r)))
        (hasheqv)))
  (define secrets (if th (hash-ref th 'secrets) '()))
  (define threats (if th (hash-ref th 'threats) '()))
  (hasheq 'seed seed 'turn (game-turn g)
          'npc (car (cdr (assq 'self (cdr view))))
          'n (length actions)
          'kinds (map action-kind actions)
          'exposing (map action-exposing? actions)
          'refutable (map action-refutable? actions)
          'parse-ok (and th #t) 'attempts (hash-ref t 'attempts)
          'first-error (hash-ref t 'first-error) 'error (hash-ref t 'error)
          'secrets (length secrets)
          'secrets-true (count (λ (p) (prop-true? cd p)) secrets)
          'secrets-known (count (λ (p) (known-to-detective? g p)) secrets)
          'threats (length threats)
          'threats-true (count (λ (p) (prop-true? cd p)) threats)
          'threats-known (count (λ (p) (known-to-detective? g p)) threats)
          'risk risk-of
          'predict (and th (hash-ref th 'predict)) 'plan (and th (hash-ref th 'plan))
          'A (decision-info cd actions dA order2) 'B (decision-info cd actions dB order3)
          'think-ms (hash-ref t 'ms) 'think-tokens (hash-ref t 'tokens)
          'think-outputs (hash-ref t 'outputs) 'think-prompt (hash-ref t 'prompt)))

(define (run case-for games max-n per-game out)
  (define records '())
  (define o (open-output-file out #:exists 'replace))
  (for ([seed (in-range 1 (add1 games))] #:break (>= (length records) max-n))
    (define cd (case-for seed))
    (define in-game 0)
    (define gbox (box #f))
    (define fast (make-llm-policy cd #:seed seed))
    (define rng (let ([r (make-pseudo-random-generator)])
                  (parameterize ([current-pseudo-random-generator r]) (random-seed (+ 1000 seed))) r))
    (define (policy view actions)
      (cond
        [(and (or (findf action-exposing? actions) (> (length actions) 2))
              (< (length records) max-n) (< in-game per-game))
         (set! in-game (add1 in-game))
         (define rec (think-path cd (unbox gbox) view actions rng seed))
         (set! records (cons rec records))
         (write rec o) (newline o) (flush-output o)
         (define A (hash-ref rec 'A))
         (define B (hash-ref rec 'B))
         (define (tag d) (string-append (if (hash-ref d 'exposing) "（暴露）" "") (if (hash-ref d 'refutable) "（可拆穿）" "")))
         (printf "[~a] 局~a 第~a轮 ~a ~a个选项 解析~a~a secret~a/threat~a(侦探已知~a) A选~a~a B选~a~a 用时 ~a+~a+~a s\n"
                 (length records) seed (hash-ref rec 'turn) (hash-ref rec 'npc) (hash-ref rec 'n)
                 (if (hash-ref rec 'parse-ok) "成功" (format "失败:~a" (hash-ref rec 'error)))
                 (if (= 2 (hash-ref rec 'attempts)) (format "（重试，首次错误:~a）" (hash-ref rec 'first-error)) "")
                 (hash-ref rec 'secrets) (hash-ref rec 'threats) (hash-ref rec 'threats-known)
                 (hash-ref A 'kind) (tag A)
                 (hash-ref B 'kind) (tag B)
                 (round (/ (hash-ref rec 'think-ms) 1000)) (round (/ (hash-ref A 'ms) 1000)) (round (/ (hash-ref B 'ms) 1000)))
         (flush-output)
         (if (hash-ref A 'index) (list-ref actions (hash-ref A 'index)) (rule-policy view actions))]
        [else (fast view actions)]))
    (define g (new-game cd #:seed seed #:policy policy #:ext? #t))
    (set-box! gbox g)
    (define ps (pstate (make-hash) (make-hasheq)
                       (let ([r (make-pseudo-random-generator)])
                         (parameterize ([current-pseudo-random-generator r]) (random-seed seed)) r)
                       0.2))
    (let loop ()
      (when (and (eq? (game-status g) 'playing) (< (length records) max-n))
        (apply-move! g ps (player-move g ps))
        (loop))))
  (close-output-port o)
  (reverse records))

(define (pct a b) (if (zero? b) "n/a" (string-append (~r (* 100. (/ a b)) #:precision 1) "%")))

(define (decision-summary label rs key)
  (define ds (map (λ (r) (hash-ref r key)) rs))
  (define n (length ds))
  (define exp-rs (filter (λ (r) (ormap values (hash-ref r 'exposing))) rs))
  (define fab-chosen (filter (λ (d) (memq (hash-ref d 'kind) '(fabricate frame))) ds))
  (printf "决策 ~a：编号合法 ~a\n" label (pct (count (λ (d) (hash-ref d 'valid)) ds) n))
  (printf "  存在暴露选项时选了暴露选项：~a/~a\n"
          (count (λ (r) (hash-ref (hash-ref r key) 'exposing)) exp-rs) (length exp-rs))
  (printf "  选了编造（fabricate/frame）~a 次，其中可被拆穿 ~a；选了自认在作案现场 ~a 次\n"
          (length fab-chosen) (pct (count (λ (d) (hash-ref d 'refutable)) fab-chosen) (length fab-chosen))
          (count (λ (d) (hash-ref d 'admission)) ds))
  (printf "  选中位置 / 选项数 的均值：~a（均匀随机为 0.5 左右）\n"
          (~r (exact->inexact (/ (apply + (for/list ([r rs] #:when (hash-ref (hash-ref r key) 'pos))
                                            (/ (- (hash-ref (hash-ref r key) 'pos) 1/2) (hash-ref r 'n))))
                                 (max 1 n)))
              #:precision 2))
  (printf "  选择分布：~s\n" (sort (hash->list (for/fold ([h (hasheq)]) ([d ds]) (hash-update h (hash-ref d 'kind) add1 0))) > #:key cdr))
  (printf "  平均用时 ~a ms，~a token\n"
          (round (/ (apply + (map (λ (d) (hash-ref d 'ms)) ds)) (max 1 n)))
          (round (/ (apply + (map (λ (d) (hash-ref d 'tokens)) ds)) (max 1 n)))))

(define (summarize rs)
  (define n (length rs))
  (define ok (filter (λ (r) (hash-ref r 'parse-ok)) rs))
  (define first-ok (filter (λ (r) (and (hash-ref r 'parse-ok) (= 1 (hash-ref r 'attempts)))) rs))
  (printf "\n== 思考格式可行性 v3：~a 个有实质选择的决策点 ==\n" n)
  (printf "思考输出可解析：首次 ~a/~a（~a）；重试后 ~a/~a（~a）\n"
          (length first-ok) n (pct (length first-ok) n) (length ok) n (pct (length ok) n))
  (printf "首次解析错误：~s\n" (filter values (map (λ (r) (hash-ref r 'first-error)) rs)))
  (printf "重试后仍失败：~s\n" (filter values (map (λ (r) (hash-ref r 'error)) rs)))
  (define (sum k) (apply + (map (λ (r) (hash-ref r k)) ok)))
  (printf "secret ~a 条：与真相相符 ~a，侦探已掌握 ~a\n" (sum 'secrets) (pct (sum 'secrets-true) (sum 'secrets)) (pct (sum 'secrets-known) (sum 'secrets)))
  (printf "threat ~a 条：与真相相符 ~a，侦探确实已掌握 ~a（v1 为 40%）\n" (sum 'threats) (pct (sum 'threats-true) (sum 'threats)) (pct (sum 'threats-known) (sum 'threats)))
  (define (labels pred)
    (for*/list ([r ok] [i (in-range (hash-ref r 'n))] #:when (pred r i)) (hash-ref (hash-ref r 'risk) i #f)))
  (define (fab? r i) (memq (list-ref (hash-ref r 'kinds) i) '(fabricate frame)))
  (define exp-l (labels (λ (r i) (list-ref (hash-ref r 'exposing) i))))
  (define fab-ref-l (labels (λ (r i) (and (fab? r i) (list-ref (hash-ref r 'refutable) i)))))
  (define fab-safe-l (labels (λ (r i) (and (fab? r i) (not (list-ref (hash-ref r 'refutable) i))))))
  (printf "风险标注：暴露选项标 high ~a（~a 个）；可拆穿的编造标 high ~a（~a 个）；不可拆穿的编造标 low ~a（~a 个）\n"
          (pct (count (λ (x) (eq? x 'high)) exp-l) (length exp-l)) (length exp-l)
          (pct (count (λ (x) (eq? x 'high)) fab-ref-l) (length fab-ref-l)) (length fab-ref-l)
          (pct (count (λ (x) (eq? x 'low)) fab-safe-l) (length fab-safe-l)) (length fab-safe-l))
  (printf "选项数分布：~s\n" (sort (hash->list (for/fold ([h (hasheqv)]) ([r rs]) (hash-update h (hash-ref r 'n) add1 0))) < #:key car))
  (define consistent
    (for/list ([r ok])
      (define rk (hash-ref r 'risk))
      (define lv (hash-ref rk (hash-ref (hash-ref r 'A) 'index) #f))
      (or (eq? lv 'low) (not (for/or ([(k v) rk]) (eq? v 'low))))))
  (printf "决策 A 与自己的风险标注一致：~a\n" (pct (count values consistent) (length consistent)))
  (printf "A 与 B 选择相同：~a\n" (pct (count (λ (r) (eqv? (hash-ref (hash-ref r 'A) 'index) (hash-ref (hash-ref r 'B) 'index))) rs) n))
  (printf "plan 分布：~s\n" (sort (hash->list (for/fold ([h (hash)]) ([r ok]) (hash-update h (let ([p (hash-ref r 'plan)]) (if (pair? p) (car p) p)) add1 0))) > #:key cdr))
  (printf "predict 类型：~s\n" (sort (hash->list (for/fold ([h (hash)]) ([r ok]) (hash-update h (car (hash-ref r 'predict)) add1 0))) > #:key cdr))
  (printf "思考任务平均用时 ~a ms，~a token\n"
          (round (/ (apply + (map (λ (r) (hash-ref r 'think-ms)) rs)) (max 1 n)))
          (round (/ (apply + (map (λ (r) (hash-ref r 'think-tokens)) rs)) (max 1 n))))
  (decision-summary "A（带分析）" rs 'A)
  (decision-summary "B（不带分析，对照）" rs 'B))

(module+ main
  (define max-n (make-parameter 40))
  (define games (make-parameter 20))
  (define out (make-parameter "runs/think-v3.rktd"))
  (define case-file (make-parameter "case-manor.rktd"))
  (define gen (make-parameter #f))
  (define per-game (make-parameter 1000))
  (command-line
   #:once-each
   ["--max" n "最多测试的决策点数" (max-n (string->number n))]
   ["--games" n "最多局数" (games (string->number n))]
   ["--out" f "逐条记录输出" (out f)]
   ["--case" f "案件文件" (case-file f)]
   ["--gen" spec "每局用生成案件，参数 \"npcs slots layers secrets\"，案件种子 = 局号" (gen (map string->number (string-split spec)))]
   ["--per-game" k "每局最多测试的决策点数" (per-game (string->number k))])
  (define case-for
    (if (gen)
        (λ (seed) (datum->case (apply (λ (n s l k) (generate-case seed #:npcs n #:slots s #:layers l #:secrets k)) (gen))))
        (let ([cd (load-case (case-file))]) (λ (_) cd))))
  (summarize (run case-for (games) (max-n) (per-game) (out))))
