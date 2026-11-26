#!/bin/bash
# v1 M6 一次完整流程：导出 → 训练 → 采样 → 评测。用法：bash m6/run.sh <标签> [net.py train 的额外参数]
set -euo pipefail
cd "$(dirname "$0")/.."
tag=${1:?标签}; shift || true
racket m6/export.rkt --out m6/data.jsonl runs/sft-v1b.rktd runs/sft-v2-pilot.rktd runs/sft-v2-pilot2.rktd runs/sft-v2-pilot3.rktd runs/sft-v2.rktd
python3 -W ignore m6/net.py train --data m6/data.jsonl --out "m6/ckpt-$tag.pt" "$@"
python3 -W ignore m6/net.py sample --data m6/data.jsonl --ckpt "m6/ckpt-$tag.pt" --out "m6/samples-$tag.jsonl" --k 8
racket m6/eval.rkt --model "m6/samples-$tag.jsonl" --teacher runs/sft-v2.rktd --out "runs/m6-$tag.rktd" | tee "runs/m6-$tag.txt"
