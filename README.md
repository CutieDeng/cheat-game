# cheat-game

推理对局：询问在场者的行踪、出示证物和证词拆穿谎言，在行动点用完前指认凶手。

NPC 的回答由引擎的规则策略给出（`engine.rkt`），不需要大模型；`llm.rkt` / `think*.rkt` 是接本地教师模型的评测代码，玩游戏用不到。

## 运行

需要 Racket 9（最小安装即可，只用到自带的 `racket/tcp`、`json`、`openssl`），不需要额外的包。

```sh
racket server.rkt --port 8080        # 浏览器打开 http://127.0.0.1:8080/
racket main.rkt                      # 终端菜单版
racket main.rkt --case cases/net-n6-d2.rktd
```

## 规则要点

- 行动点：提问 1、出示 2、指认 0；预算缺省为 max(30, 6 × 人数)。用掉 2/3 后进入对质阶段，NPC 压力更大。
- 拆穿凶手两条行踪谎言，或凶手自己说出案发时在现场，才能指认成功；指认错人或证据不足都直接失败。
- 同一个问题问到第 5 次起，对方的压力会下降。

## 开发

```sh
raco test test.rkt
racket gen.rkt --seed 1 --npcs 5 --slots 4 --layers 3 --secrets 1 --out f.rktd   # 生成案件
racket solve.rkt f.rktd                                                          # 可解性检查
```

设计与评测记录见 `design.md`。
