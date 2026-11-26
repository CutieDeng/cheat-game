#!/bin/bash
# 强化学习一次完整流程：RL 训练 → 在新测试集（m6/test-fresh.jsonl）上采样并评测 RL 后与 RL 前的模型。
# 用法：bash m6/rl-run.sh <标签> <初始 ckpt> [rl.py 的额外参数]
set -euo pipefail
cd "$(dirname "$0")/.."
tag=${1:?标签}; init=${2:?初始 ckpt}; shift 2
python3 -W ignore m6/rl.py --init "$init" --out "m6/ckpt-$tag.pt" --log "runs/m6-$tag-train.jsonl" "$@"
for m in "$init" "m6/ckpt-$tag.pt"; do
  b=$(basename "$m" .pt)
  [ -s "runs/m6-fresh-$b.txt" ] && continue   # 已评测过的模型不重复评测
  python3 -W ignore m6/net.py sample --all --data m6/test-fresh.jsonl --ckpt "$m" --out "m6/fresh-$b.jsonl" --k 8
  racket m6/eval.rkt --model "m6/fresh-$b.jsonl" --teacher runs/sft-v2.rktd --out "runs/m6-fresh-$b.rktd" > "runs/m6-fresh-$b.txt"
done
