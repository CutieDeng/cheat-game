#!/bin/bash
# 在新测试集（m6/test-fresh.jsonl）上采样并评测一个 checkpoint。用法：bash m6/eval-fresh.sh <ckpt>
set -euo pipefail
cd "$(dirname "$0")/.."
m=${1:?ckpt}; b=$(basename "$m" .pt)
python3 -W ignore m6/net.py sample --all --data m6/test-fresh.jsonl --ckpt "$m" --out "m6/fresh-$b.jsonl" --k 8
racket m6/eval.rkt --model "m6/fresh-$b.jsonl" --teacher runs/sft-v2.rktd --out "runs/m6-fresh-$b.rktd" > "runs/m6-fresh-$b.txt"
