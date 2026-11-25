#lang racket/base
;; 思考格式可行性测试（v2）：在含暴露选项的决策点上运行
;;   思考任务（解析失败时重试 1 次）
;;   决策 A：给思考任务的输出（对局按 A 的选择继续）
;;   决策 B：不给分析（对照组）
;; 三次调用都开 DeepSeek 自身的思考模式。其余决策点用非思考模式的单次选择推进对局。
;; racket think-eval.rkt [--max N] [--games G] [--out 文件]
(require racket/list racket/match racket/cmdline racket/format racket/string
         "kb.rkt" "engine.rkt" "llm.rkt" "think.rkt" "eval.rkt")

(define (known-to-detective? g p)
  (define cd (game-cd g))
  (define pool (append (map third (case-data-evidence cd)) (map claim-prop (game-claims g))))
  (for/or ([q pool])
    (and (eq? (prop-pol q) (prop-pol p))
         (eq? (st-agent (prop-st q)) (st-agent (prop-st p)))
         (eq? (st-loc (prop-st q)) (st-loc (prop-st p)))
         (overlap? (prop-st q) (prop-st p)))))

(define (shuffle/rng rng xs) (parameterize ([current-pseudo-random-generator rng]) (shuffle xs)))

(define (decision-info d order)
  (define n (hash-ref d 'choice))
  (define a (and n (list-ref order (sub1 n))))
  (hasheq 'valid (and n #t) 'pos n
          'kind (and a (action-kind a)) 'exposing (and a (action-exposing? a))
          'ms (hash-ref d 'ms) 'tokens (hash-ref d 'tokens) 'output (hash-ref d 'output)))

(define (think-path cd g view actions rng seed)
  (define order1 (shuffle/rng rng actions))
  (define t (think-task cd view order1))
  (define th (hash-ref t 'thought))
  (define order2 (shuffle/rng rng actions))
  (define order3 (shuffle/rng rng actions))
  (define dA (decide-task cd view order2 (if th (thought->text cd th order1 order2) "（无）")))
  (define dB (decide-task cd view order3 #f))
  (define risk-of
    (if th
        (for/hasheq ([r (hash-ref th 'risks)]) (values (action-kind (list-ref order1 (sub1 (car r)))) (cdr r)))
        (hasheq)))
  (define secrets (if th (hash-ref th 'secrets) '()))
  (define threats (if th (hash-ref th 'threats) '()))
  (hasheq 'seed seed 'turn (game-turn g)
          'npc (car (cdr (assq 'self (cdr view))))
          'n (length actions)
          'kinds (map action-kind actions)
          'exposing-kinds (map action-kind (filter action-exposing? actions))
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
          'A (decision-info dA order2) 'B (decision-info dB order3)
          'think-ms (hash-ref t 'ms) 'think-tokens (hash-ref t 'tokens)
          'think-outputs (hash-ref t 'outputs) 'think-prompt (hash-ref t 'prompt)))

(define (run cd games max-n out)
  (define records '())
  (define o (open-output-file out #:exists 'replace))
  (for ([seed (in-range 1 (add1 games))] #:break (>= (length records) max-n))
    (define gbox (box #f))
    (define fast (make-llm-policy cd #:seed seed))
    (define rng (let ([r (make-pseudo-random-generator)])
                  (parameterize ([current-pseudo-random-generator r]) (random-seed (+ 1000 seed))) r))
    (define (policy view actions)
      (cond
        [(and (findf action-exposing? actions) (< (length records) max-n))
         (define rec (think-path cd (unbox gbox) view actions rng seed))
         (set! records (cons rec records))
         (write rec o) (newline o) (flush-output o)
         (define A (hash-ref rec 'A))
         (define B (hash-ref rec 'B))
         (printf "[~a] 局~a 第~a轮 ~a 解析~a~a secret~a/threat~a(侦探已知~a) A选~a~a B选~a~a 用时 ~a+~a+~a s\n"
                 (length records) seed (hash-ref rec 'turn) (hash-ref rec 'npc)
                 (if (hash-ref rec 'parse-ok) "成功" (format "失败:~a" (hash-ref rec 'error)))
                 (if (= 2 (hash-ref rec 'attempts)) (format "（重试，首次错误:~a）" (hash-ref rec 'first-error)) "")
                 (hash-ref rec 'secrets) (hash-ref rec 'threats) (hash-ref rec 'threats-known)
                 (hash-ref A 'kind) (if (hash-ref A 'exposing) "（暴露）" "")
                 (hash-ref B 'kind) (if (hash-ref B 'exposing) "（暴露）" "")
                 (round (/ (hash-ref rec 'think-ms) 1000)) (round (/ (hash-ref A 'ms) 1000)) (round (/ (hash-ref B 'ms) 1000)))
         (flush-output)
         (or (and (hash-ref A 'kind) (findf (λ (a) (eq? (action-kind a) (hash-ref A 'kind))) actions))
             (rule-policy view actions))]
        [else (fast view actions)]))
    (define g (new-game cd #:seed seed #:policy policy))
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
  (printf "决策 ~a：编号合法 ~a；选了暴露选项 ~a/~a（~a）\n" label
          (pct (count (λ (d) (hash-ref d 'valid)) ds) n)
          (count (λ (d) (hash-ref d 'exposing)) ds) n (pct (count (λ (d) (hash-ref d 'exposing)) ds) n))
  (for ([k (sort (remove-duplicates (map (λ (r) (hash-ref r 'n)) rs)) <)])
    (define xs (for/list ([r rs] #:when (= (hash-ref r 'n) k) #:when (hash-ref (hash-ref r key) 'pos))
                 (hash-ref (hash-ref r key) 'pos)))
    (printf "  ~a 个选项（~a 次）选中位置：~a\n" k (length xs)
            (string-join (for/list ([p (in-range 1 (add1 k))])
                           (format "第~a位 ~a" p (pct (count (λ (x) (= x p)) xs) (length xs))))
                         " ")))
  (printf "  选择分布：~s\n" (sort (hash->list (for/fold ([h (hasheq)]) ([d ds]) (hash-update h (hash-ref d 'kind) add1 0))) > #:key cdr))
  (printf "  平均用时 ~a ms，~a token\n"
          (round (/ (apply + (map (λ (d) (hash-ref d 'ms)) ds)) (max 1 n)))
          (round (/ (apply + (map (λ (d) (hash-ref d 'tokens)) ds)) (max 1 n)))))

(define (summarize rs)
  (define n (length rs))
  (define ok (filter (λ (r) (hash-ref r 'parse-ok)) rs))
  (define first-ok (filter (λ (r) (and (hash-ref r 'parse-ok) (= 1 (hash-ref r 'attempts)))) rs))
  (printf "\n== 思考格式可行性 v2：~a 个含暴露选项的决策点 ==\n" n)
  (printf "思考输出可解析：首次 ~a/~a（~a）；重试后 ~a/~a（~a）\n"
          (length first-ok) n (pct (length first-ok) n) (length ok) n (pct (length ok) n))
  (printf "首次解析错误：~s\n" (filter values (map (λ (r) (hash-ref r 'first-error)) rs)))
  (printf "重试后仍失败：~s\n" (filter values (map (λ (r) (hash-ref r 'error)) rs)))
  (define (sum k) (apply + (map (λ (r) (hash-ref r k)) ok)))
  (printf "secret ~a 条：与真相相符 ~a，侦探已掌握 ~a\n" (sum 'secrets) (pct (sum 'secrets-true) (sum 'secrets)) (pct (sum 'secrets-known) (sum 'secrets)))
  (printf "threat ~a 条：与真相相符 ~a，侦探确实已掌握 ~a（v1 为 40%）\n" (sum 'threats) (pct (sum 'threats-true) (sum 'threats)) (pct (sum 'threats-known) (sum 'threats)))
  (define exp-pairs (for*/list ([r ok] [k (hash-ref r 'exposing-kinds)]) (hash-ref (hash-ref r 'risk) k #f)))
  (define safe-pairs (for*/list ([r ok] [k (hash-ref r 'kinds)] #:unless (memq k (hash-ref r 'exposing-kinds)))
                       (hash-ref (hash-ref r 'risk) k #f)))
  (printf "暴露选项被标为 high：~a；非暴露选项被标为 low：~a\n"
          (pct (count (λ (x) (eq? x 'high)) exp-pairs) (length exp-pairs))
          (pct (count (λ (x) (eq? x 'low)) safe-pairs) (length safe-pairs)))
  (define consistent
    (for/list ([r ok])
      (define rk (hash-ref r 'risk))
      (define lv (hash-ref rk (hash-ref (hash-ref r 'A) 'kind) #f))
      (or (eq? lv 'low) (not (for/or ([(k v) rk]) (eq? v 'low))))))
  (printf "决策 A 与自己的风险标注一致：~a\n" (pct (count values consistent) (length consistent)))
  (printf "A 与 B 选择相同：~a\n" (pct (count (λ (r) (eq? (hash-ref (hash-ref r 'A) 'kind) (hash-ref (hash-ref r 'B) 'kind))) rs) n))
  (printf "plan 分布：~s\n" (sort (hash->list (for/fold ([h (hash)]) ([r ok]) (hash-update h (hash-ref r 'plan) add1 0))) > #:key cdr))
  (printf "思考任务平均用时 ~a ms，~a token\n"
          (round (/ (apply + (map (λ (r) (hash-ref r 'think-ms)) rs)) (max 1 n)))
          (round (/ (apply + (map (λ (r) (hash-ref r 'think-tokens)) rs)) (max 1 n))))
  (decision-summary "A（带分析）" rs 'A)
  (decision-summary "B（不带分析，对照）" rs 'B))

(module+ main
  (define max-n (make-parameter 40))
  (define games (make-parameter 20))
  (define out (make-parameter "runs/think-v2.rktd"))
  (define case-file (make-parameter "case-manor.rktd"))
  (command-line
   #:once-each
   ["--max" n "最多测试的决策点数" (max-n (string->number n))]
   ["--games" n "最多局数" (games (string->number n))]
   ["--out" f "逐条记录输出" (out f)]
   ["--case" f "案件文件" (case-file f)])
  (define cd (load-case (case-file)))
  (summarize (run cd (games) (max-n) (out))))
