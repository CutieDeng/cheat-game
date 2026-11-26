#!/bin/bash
# 在大测试集（m6/test-big.jsonl，每种人数 100 案）上分片并行评测一个 checkpoint。
# 用法：bash m6/eval-big.sh <ckpt> [分片数，缺省 9]
# 输出：runs/m6-big-<名>.rktd（逐案结果）、runs/m6-big-<名>.txt（eval.rkt 汇总 + 按预算胜率）
set -euo pipefail
cd "$(dirname "$0")/.."
m=${1:?ckpt}; n=${2:-9}; b=$(basename "$m" .pt)
d="m6/shards-$b"
rm -rf "$d"; mkdir -p "$d"
python3 -W ignore m6/net.py sample --all --data m6/test-big.jsonl --ckpt "$m" --out "m6/big-$b.jsonl" --k 8
awk -v n="$n" -v p="$d/part-" '{ print > (p (NR % n)) }' "m6/big-$b.jsonl"
pids=()
for f in "$d"/part-*; do
  racket m6/eval.rkt --model "$f" --teacher runs/sft-v2.rktd --out "$f.rktd" > "$f.txt" 2>&1 &
  pids+=($!)
done
for p in "${pids[@]}"; do wait "$p"; done
cat "$d"/part-*.rktd > "runs/m6-big-$b.rktd"
racket m6/eval.rkt --from "runs/m6-big-$b.rktd" > "runs/m6-big-$b.txt"
racket m6/budget-metric.rkt "runs/m6-big-$b.rktd" >> "runs/m6-big-$b.txt"
