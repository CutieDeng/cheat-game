"""v1 M6：凶手故事网络的强化学习（spec-v1.md §8.4 第 3 步）。

算法：组内相对优势的 REINFORCE + 对参考策略（模仿学习的初始模型）的 KL 约束。
  - 每轮取 C 个案件，每案用当前策略采 k 个故事（槽位随机重排，温度 1）。
  - 每个故事由 m6/score-server.rkt（verify-v1.rkt，含侦探存活分）打出排序键
    (合法且不自认, 未被拆穿定罪, 侦探行动点, −拆穿来源数)。
  - 组内优势：A_i = (#{j: key_i > key_j} − #{j: key_j > key_i}) / (k − 1)，按字典序比较；全相同则为 0。
    只用排名，不需要给键的各分量定权重。--rank ap 时比较顺序改为 (合法, 侦探 AP, 未被拆穿, −来源数)。
  - 损失：−mean(A_i · Σ_t log π(a_t)) + β · mean(KL_k3(π_ref ‖ π))，KL 按每步 k3 估计。
训练案件来自 m6/rl-cases.jsonl（新种子，seed % 10 ≠ 0，不需要教师数据）；测试集仍是 m6/data.jsonl 里 seed % 10 == 0 的案件。

  python3 m6/rl.py --init m6/ckpt-v2.pt --out m6/ckpt-rl1.pt --iters 100
  python3 m6/rl.py --init m6/ckpt-rl1.pt --ref m6/ckpt-v2.pt --out m6/ckpt-rl2.pt --iters 200 --seed 2   # 接着训
"""
import argparse, json, math, queue, random, subprocess, sys, threading, time
from concurrent.futures import ThreadPoolExecutor

import torch
import torch.nn.functional as F

sys.path.insert(0, __file__.rsplit("/", 1)[0])
import net  # noqa: E402


class Scorer:
    """W 个 racket 打分进程，线程池分发；按 (npcs, seed, 故事) 缓存。"""

    def __init__(self, w):
        self.procs = queue.Queue()
        for _ in range(w):
            self.procs.put(subprocess.Popen(["racket", "m6/score-server.rkt"], stdin=subprocess.PIPE,
                                            stdout=subprocess.PIPE, text=True, bufsize=1))
        self.pool = ThreadPoolExecutor(w)
        self.cache = {}
        self.lock = threading.Lock()

    def _one(self, req):
        k = (req["npcs"], req["seed"], json.dumps(req["story"]))
        with self.lock:
            if k in self.cache:
                return self.cache[k]
        p = self.procs.get()
        try:
            p.stdin.write(json.dumps(req) + "\n")
            p.stdin.flush()
            r = json.loads(p.stdout.readline())
        finally:
            self.procs.put(p)
        with self.lock:
            self.cache[k] = r
        return r

    def score(self, reqs):
        return list(self.pool.map(self._one, reqs))

    def close(self):
        while not self.procs.empty():
            p = self.procs.get()
            p.stdin.close()
            p.wait()


def key_gt(a, b):
    return tuple(a) > tuple(b)


def rank_adv(keys):
    k = len(keys)
    return [(sum(key_gt(a, b) for b in keys) - sum(key_gt(b, a) for b in keys)) / max(k - 1, 1) for a in keys]


def step_logps(model, bt):
    """教师强制下每段的 log π(地点) + log π(离开)，返回 (B×L, 有效掩码)。"""
    mem, mem_mask, node_h = model.encode(bt)
    tn, tin, tout = bt["tn"], bt["tin"], bt["tout"]
    valid = tn >= 0
    h = model.decode(model.dec_inputs(node_h, tn, tin, tout), mem, mem_mask)
    prev = torch.cat([torch.full_like(tn[:, :1], -1), tn[:, :-1]], 1)
    prev_out = torch.cat([torch.zeros_like(tout[:, :1]), tout[:, :-1]], 1)
    nl = model.node_logits(h, node_h, bt["node_mask"], prev, prev_out, bt["T"], bt["adjw"])
    ll = model.leave_logits(h, node_h, tn, tin, bt["T"], bt["adjw"])
    lpn = torch.gather(F.log_softmax(nl, -1), 2, tn.clamp(min=0).unsqueeze(-1))[..., 0]
    lpt = torch.gather(F.log_softmax(ll, -1), 2, tout.unsqueeze(-1))[..., 0]
    return torch.where(valid, lpn + lpt, torch.zeros_like(lpn)), valid


def load_model(path, dev):
    ck = torch.load(path, map_location=dev)
    m = net.StoryNet(d=ck["d"], drop=0.0).to(dev)   # 强化学习不用 dropout：采样与求梯度是同一个分布
    m.load_state_dict(ck["state"])
    return m, ck["d"]


def main(a):
    torch.manual_seed(a.seed); random.seed(a.seed)
    dev = torch.device("cpu")
    model, d = load_model(a.init, dev)
    ref, _ = load_model(a.ref or a.init, dev)   # KL 参考：缺省为初始模型；接着训练时应指向最初的模仿学习模型
    ref.eval()
    for p in ref.parameters():
        p.requires_grad_(False)
    opt = torch.optim.AdamW(model.parameters(), lr=a.lr, weight_decay=0.0)
    cases = net.load(a.cases)
    by_n = {}
    for r in cases:
        by_n.setdefault(r["npcs"], []).append(r)
    scorer = Scorer(a.workers)
    log = open(a.log, "w", encoding="utf-8") if a.log else None
    t0 = time.time()
    try:
        for it in range(1, a.iters + 1):
            # 各人数均衡取案件
            ns = sorted(by_n)
            batch = [random.choice(by_n[ns[i % len(ns)]]) for i in range(a.cases_per_iter)]
            samples = [net.Sample(r, None, augment=True) for r in batch for _ in range(a.k)]
            model.eval()
            with torch.no_grad():
                stories = net.generate(model, net.collate(samples, dev), temp=1.0)
            reqs = [dict(npcs=s.r["npcs"], seed=s.r["seed"], story=[[s.r["node_ids"][n], x, y] for n, x, y in st])
                    for s, st in zip(samples, stories)]
            ts = time.time()
            res = scorer.score(reqs)
            ts = time.time() - ts
            keys = [r["key"] for r in res]
            # --rank ap：排名比较顺序改为 (合法, 侦探 AP, 未被拆穿, −来源数)；统计仍按原键
            rkeys = [[k[0], k[2], k[1], k[3]] for k in keys] if a.rank == "ap" else keys
            adv = []
            for g in range(len(batch)):
                adv += rank_adv(rkeys[g * a.k:(g + 1) * a.k])
            # 截断过长的故事（未在 MAX_SEGS 内结束的，打分已判不可行）
            for s, st in zip(samples, stories):
                s.story = st
            model.train()
            A = torch.tensor(adv, dtype=torch.float32)
            idx = list(range(len(samples)))
            random.shuffle(idx)
            tot_pg = tot_kl = 0.0
            for i in range(0, len(idx), a.mb):
                ids = idx[i:i + a.mb]
                bt = net.collate([samples[j] for j in ids], dev)
                lp, valid = step_logps(model, bt)
                with torch.no_grad():
                    lr_, _ = step_logps(ref, bt)
                pg = -(A[ids] * lp.sum(1)).mean()
                delta = torch.where(valid, lr_ - lp, torch.zeros_like(lp))
                kl = ((torch.exp(delta) - delta - 1) * valid).sum() / valid.sum()
                loss = (pg + a.beta * kl) * len(ids) / len(idx)
                loss.backward()
                tot_pg += pg.item() * len(ids) / len(idx)
                tot_kl += kl.item() * len(ids) / len(idx)
            torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            opt.step(); opt.zero_grad()
            # 统计
            st = {}
            for n in ns:
                ks = [k for s, k in zip(samples, keys) if s.r["npcs"] == n]
                valid_ = [k for k in ks if k[0] == 1]
                st[n] = (sum(k[0] == 1 and k[1] == 1 for k in ks) / len(ks),
                         sum(k[0] == 0 for k in ks) / len(ks),
                         sum(k[2] for k in valid_) / max(len(valid_), 1))
            line = (f"it {it:4d}  " + "  ".join(f"{n}人 未拆穿 {u:.2f} 非法 {b:.2f} AP {ap:.1f}" for n, (u, b, ap) in st.items())
                    + f"  pg {tot_pg:+.3f} kl {tot_kl:.4f}  打分 {ts:.1f} s  共 {time.time() - t0:.0f} s")
            print(line, flush=True)
            if log:
                log.write(json.dumps(dict(it=it, stats={str(k): v for k, v in st.items()}, pg=tot_pg, kl=tot_kl)) + "\n")
                log.flush()
            if it % a.save_every == 0 or it == a.iters:
                torch.save(dict(state=model.state_dict(), d=d), a.out)
    finally:
        scorer.close()
    print(f"已保存 {a.out}", flush=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--init", default="m6/ckpt-v2.pt")
    ap.add_argument("--out", default="m6/ckpt-rl.pt")
    ap.add_argument("--ref", default=None, help="KL 参考模型（缺省 = --init）")
    ap.add_argument("--cases", default="m6/rl-cases.jsonl")
    ap.add_argument("--iters", type=int, default=100)
    ap.add_argument("--cases-per-iter", type=int, default=32)
    ap.add_argument("--k", type=int, default=8)
    ap.add_argument("--mb", type=int, default=64)
    ap.add_argument("--lr", type=float, default=2e-5)
    ap.add_argument("--beta", type=float, default=0.05)
    ap.add_argument("--workers", type=int, default=10)
    ap.add_argument("--save-every", type=int, default=10)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--log", default=None)
    ap.add_argument("--rank", choices=["exposed", "ap"], default="exposed",
                    help="组内排名：exposed = (合法, 未被拆穿, AP, −来源数)；ap = (合法, AP, 未被拆穿, −来源数)")
    main(ap.parse_args())
