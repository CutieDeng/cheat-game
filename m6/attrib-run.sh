#!/bin/bash
# 逐案归因，分片并行。用法：bash m6/attrib-run.sh <模型采样 JSONL> <标签> [分片数，缺省 9]
set -euo pipefail
cd "$(dirname "$0")/.."
src=${1:?采样文件}; tag=${2:?标签}; n=${3:-9}
d="m6/shards-attrib-$tag"; rm -rf "$d"; mkdir -p "$d"
pids=()
for i in $(seq 0 $((n - 1))); do
  racket m6/attrib.rkt --model "$src" --shard "$i" --of "$n" --out "$d/part-$i.rktd" > "$d/part-$i.txt" 2>&1 &
  pids+=($!)
done
for p in "${pids[@]}"; do wait "$p"; done
cat "$d"/part-*.rktd > "runs/m6-attrib-$tag.rktd"
racket m6/attrib.rkt --from "runs/m6-attrib-$tag.rktd" > "runs/m6-attrib-$tag.txt"
