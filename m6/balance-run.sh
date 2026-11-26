#!/bin/bash
# 平衡验证，分片并行。用法：bash m6/balance-run.sh <标签> [分片数，缺省 9]
set -euo pipefail
cd "$(dirname "$0")/.."
tag=${1:?标签}; n=${2:-9}
d="m6/shards-balance-$tag"; rm -rf "$d"; mkdir -p "$d"
pids=()
for i in $(seq 0 $((n - 1))); do
  racket m6/balance-check.rkt --strong m6/big-ckpt-rl3.jsonl --weak m6/big-ckpt-v2.jsonl --shard "$i" --of "$n" --out "$d/part-$i.rktd" > "$d/part-$i.txt" 2>&1 &
  pids+=($!)
done
for p in "${pids[@]}"; do wait "$p"; done
cat "$d"/part-*.rktd > "runs/m6-balance-$tag.rktd"
racket m6/balance-check.rkt --from "runs/m6-balance-$tag.rktd" > "runs/m6-balance-$tag.txt"
