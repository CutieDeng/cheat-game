"""v1 M6：凶手故事的专用小网络（spec-v1.md §8 方案 A）。

输入：m6/export.rkt 导出的案件特征（凶手视角）。
  - 实体 token：地点（类型、度数、是否作案地点、窗户）、通道（两端、分钟数）、事实（案情 / 自己的真实行踪 /
    自己的观察 / 物证），事实通过"槽位"引用地点和人。
  - 槽位每个样本随机重排：网络只能靠结构区分地点和人，不能记住 id。
输出：逐段生成 (地点, 离开时刻)；进入时刻 = 上一段离开 + 步行分钟（与 teacher-v1.rkt 的新格式一致）。
  - 地点头：指针网络，只能选上一段地点的邻居（首段任意）→ 路径相邻性由结构保证。
  - 离开头：分类 0..T，屏蔽早于进入时刻、以及离开后走到任何邻居都会超过 T 的时刻；离开 = T 即结束。
训练：只用校验器判为"合法且未被拆穿"的教师候选（key 前两位为 1 1），按种子划分：seed % 10 == 0 为测试集。

  python3 m6/net.py train  --data m6/data.jsonl --out m6/ckpt.pt
  python3 m6/net.py sample --data m6/data.jsonl --ckpt m6/ckpt.pt --out m6/samples.jsonl --k 8
"""
import argparse, json, math, random, sys, time

import torch
import torch.nn as nn
import torch.nn.functional as F

N_FACT_TYPES = 9           # 与 export.rkt 的 FACT-TYPES 一致
MAX_NODES = 12
MAX_PERSONS = 12
MAX_SEGS = 48
TMAX = 120                 # 时间表大小（T = 90）


def load(path):
    return [json.loads(l) for l in open(path, encoding="utf-8")]


def is_test(r):
    return r["seed"] % 10 == 0


def clip_story(story, T):
    """V2 只要求覆盖到 T，允许超出；训练目标截到 T：丢掉 T 之后才进入的段，最后一段离开 = T。"""
    s = [list(x) for x in story if x[1] <= T]
    s[-1][2] = T
    return s


def good_targets(r):
    out, seen = [], set()
    for story, key in r["cands"]:
        story = clip_story(story, r["T"])
        if key[0] == 1 and key[1] == 1 and len(story) <= MAX_SEGS:
            t = tuple(map(tuple, story))
            if t not in seen:
                seen.add(t)
                out.append(story)
    return out


def adjacency(r):
    n = len(r["nodes"])
    adj = [dict() for _ in range(n)]
    for i, j, w in r["edges"]:
        adj[i][j] = w
    return adj


# ---------------------------------------------------------------- 张量化

def encode_case(r, node_perm, person_perm):
    """返回一个案件的输入张量（未 batch）。node_perm / person_perm：下标 → 槽位。"""
    nodes = torch.tensor([[k, min(d, 7), c, wo, wi] for k, d, c, wo, wi in r["nodes"]], dtype=torch.long)
    node_slot = torch.tensor(node_perm, dtype=torch.long)
    edges = torch.tensor([[node_perm[i], node_perm[j], min(w, 15)] for i, j, w in r["edges"]], dtype=torch.long)
    facts = []
    for t, nd, p, a, b, x in r["facts"]:
        ns = node_perm[nd] if nd >= 0 else MAX_NODES          # MAX_NODES = 无
        if p < 0:
            ps = MAX_PERSONS                                  # 无
        elif p == r["self"]:
            ps = MAX_PERSONS + 1                              # 自己
        else:
            ps = person_perm[p]
        facts.append([t, ns, ps, max(0, min(a, TMAX - 1)), max(0, min(b, TMAX - 1)), min(x, 31)])
    facts = torch.tensor(facts, dtype=torch.long)
    return nodes, node_slot, edges, facts


def encode_story(story, node_perm):
    """story：[[node in out] ...] → 解码器输入 / 目标。"""
    nodes = [s[0] for s in story]
    ins = [s[1] for s in story]
    outs = [s[2] for s in story]
    return nodes, ins, outs


class Sample:
    def __init__(self, r, story, augment=True):
        n, P = len(r["nodes"]), r["persons"]
        node_perm = random.sample(range(MAX_NODES), n) if augment else list(range(n))
        person_perm = random.sample(range(MAX_PERSONS), P) if augment else list(range(P))
        self.r = r
        self.case = encode_case(r, node_perm, person_perm)
        self.story = story
        self.adj = adjacency(r)


def collate(samples, device):
    B = len(samples)
    Nn = max(len(s.r["nodes"]) for s in samples)
    Ne = max(len(s.r["edges"]) for s in samples)
    Nf = max(len(s.r["facts"]) for s in samples)
    L = max(len(s.story) for s in samples) if samples[0].story is not None else 0
    nodes = torch.zeros(B, Nn, 5, dtype=torch.long)
    node_slot = torch.zeros(B, Nn, dtype=torch.long)
    node_mask = torch.zeros(B, Nn, dtype=torch.bool)
    edges = torch.zeros(B, Ne, 3, dtype=torch.long)
    edge_mask = torch.zeros(B, Ne, dtype=torch.bool)
    facts = torch.zeros(B, Nf, 6, dtype=torch.long)
    fact_mask = torch.zeros(B, Nf, dtype=torch.bool)
    # 邻接（下标空间）：adjw[b, i, j] = 分钟数，0 = 不相连
    adjw = torch.zeros(B, Nn, Nn, dtype=torch.long)
    T = torch.zeros(B, dtype=torch.long)
    for b, s in enumerate(samples):
        nd, ns, ed, fa = s.case
        nodes[b, :len(nd)] = nd; node_slot[b, :len(nd)] = ns; node_mask[b, :len(nd)] = True
        edges[b, :len(ed)] = ed; edge_mask[b, :len(ed)] = True
        facts[b, :len(fa)] = fa; fact_mask[b, :len(fa)] = True
        for i, j, w in s.r["edges"]:
            adjw[b, i, j] = w
        T[b] = s.r["T"]
    batch = dict(nodes=nodes, node_slot=node_slot, node_mask=node_mask, edges=edges, edge_mask=edge_mask,
                 facts=facts, fact_mask=fact_mask, adjw=adjw, T=T)
    if L:
        tn = torch.full((B, L), -1, dtype=torch.long)
        tin = torch.zeros(B, L, dtype=torch.long)
        tout = torch.zeros(B, L, dtype=torch.long)
        for b, s in enumerate(samples):
            for i, (n, a, o) in enumerate(s.story):
                tn[b, i] = n; tin[b, i] = a; tout[b, i] = o
        batch.update(tn=tn, tin=tin, tout=tout)
    return {k: v.to(device) for k, v in batch.items()}


# ---------------------------------------------------------------- 模型

class StoryNet(nn.Module):
    def __init__(self, d=128, heads=4, enc_layers=3, dec_layers=2, drop=0.1):
        super().__init__()
        self.d = d
        self.slot = nn.Embedding(MAX_NODES + 1, d)          # 地点槽位（+无）
        self.kind = nn.Embedding(3, d)
        self.deg = nn.Embedding(8, d)
        self.flag = nn.ModuleList([nn.Embedding(2, d) for _ in range(3)])
        self.tok_type = nn.Embedding(2 + N_FACT_TYPES, d)    # 0 地点 1 通道 2.. 事实
        self.slot2 = nn.Embedding(MAX_NODES + 1, d)          # 通道另一端
        self.wemb = nn.Embedding(16, d)
        self.person = nn.Embedding(MAX_PERSONS + 2, d)       # 槽位 / 无 / 自己
        self.ta = nn.Embedding(TMAX, d)
        self.tb = nn.Embedding(TMAX, d)
        self.xemb = nn.Embedding(32, d)
        el = nn.TransformerEncoderLayer(d, heads, 4 * d, drop, batch_first=True, norm_first=True)
        self.enc = nn.TransformerEncoder(el, enc_layers)
        # 解码
        self.start = nn.Parameter(torch.randn(d) * 0.02)
        self.pos = nn.Embedding(MAX_SEGS + 1, d)
        self.din = nn.Embedding(TMAX, d)
        self.dout = nn.Embedding(TMAX, d)
        self.dnode = nn.Linear(d, d)
        dl = nn.TransformerDecoderLayer(d, heads, 4 * d, drop, batch_first=True, norm_first=True)
        self.dec = nn.TransformerDecoder(dl, dec_layers)
        self.q = nn.Linear(d, d)
        self.k = nn.Linear(d, d)
        self.entry = nn.Embedding(TMAX, d)
        self.leave = nn.Sequential(nn.Linear(3 * d, 2 * d), nn.GELU(), nn.Linear(2 * d, TMAX))

    def encode(self, bt):
        nd = bt["nodes"]
        node_tok = (self.tok_type.weight[0] + self.slot(bt["node_slot"]) + self.kind(nd[..., 0]) + self.deg(nd[..., 1])
                    + self.flag[0](nd[..., 2]) + self.flag[1](nd[..., 3]) + self.flag[2](nd[..., 4]))
        ed = bt["edges"]
        edge_tok = self.tok_type.weight[1] + self.slot(ed[..., 0]) + self.slot2(ed[..., 1]) + self.wemb(ed[..., 2])
        fa = bt["facts"]
        fact_tok = (self.tok_type(fa[..., 0] + 2) + self.slot(fa[..., 1]) + self.person(fa[..., 2])
                    + self.ta(fa[..., 3]) + self.tb(fa[..., 4]) + self.xemb(fa[..., 5]))
        x = torch.cat([node_tok, edge_tok, fact_tok], 1)
        mask = torch.cat([bt["node_mask"], bt["edge_mask"], bt["fact_mask"]], 1)
        h = self.enc(x, src_key_padding_mask=~mask)
        Nn = node_tok.shape[1]
        return h, mask, h[:, :Nn]

    def dec_inputs(self, node_h, tn, tin, tout):
        """解码器输入：位置 0 = 起始；位置 i = 第 i−1 段（地点编码 + 进入 + 离开）。"""
        B, L = tn.shape
        idx = tn.clamp(min=0)
        seg = self.dnode(torch.gather(node_h, 1, idx.unsqueeze(-1).expand(-1, -1, self.d))) + self.din(tin) + self.dout(tout)
        x = torch.cat([self.start.expand(B, 1, -1), seg[:, :-1]], 1)
        return x + self.pos(torch.arange(L, device=tn.device)).unsqueeze(0)

    def decode(self, x, mem, mem_mask):
        L = x.shape[1]
        causal = torch.triu(torch.ones(L, L, dtype=torch.bool, device=x.device), 1)
        return self.dec(x, mem, tgt_mask=causal, memory_key_padding_mask=~mem_mask)

    def node_logits(self, h, node_h, node_mask, prev, prev_out, T, adjw):
        """h: B×L×d；prev: B×L 上一段地点下标（−1 = 首段）；prev_out: 上一段离开时刻。
        只允许上一段地点的邻居，且走到那里不超过 T。"""
        logits = torch.einsum("bld,bnd->bln", self.q(h), self.k(node_h)) / math.sqrt(self.d)
        B, L, N = logits.shape
        allowed = node_mask.unsqueeze(1).expand(B, L, N).clone()
        has_prev = prev >= 0
        w = torch.gather(adjw, 1, prev.clamp(min=0).unsqueeze(-1).expand(-1, -1, N))      # B×L×N
        nb = (w > 0) & (prev_out.unsqueeze(-1) + w <= T.view(B, 1, 1))
        allowed = torch.where(has_prev.unsqueeze(-1), allowed & nb, allowed)
        return logits.masked_fill(~allowed, -1e9)

    def leave_logits(self, h, node_h, node, entry, T, adjw):
        nh = torch.gather(node_h, 1, node.clamp(min=0).unsqueeze(-1).expand(-1, -1, self.d))
        logits = self.leave(torch.cat([h, nh, self.entry(entry.clamp(max=TMAX - 1))], -1))
        B, L, K = logits.shape
        t = torch.arange(K, device=h.device).view(1, 1, K)
        # 离开后最近的邻居要多少分钟
        w = torch.gather(adjw, 1, node.clamp(min=0).unsqueeze(-1).expand(-1, -1, adjw.shape[2])).float()
        minw = torch.where(w > 0, w, torch.full_like(w, 1e9)).min(-1).values.long()   # B×L
        Tb = T.view(B, 1, 1)
        ok = (t >= entry.unsqueeze(-1)) & ((t == Tb) | (t + minw.unsqueeze(-1) <= Tb))
        return logits.masked_fill(~ok, -1e9)

    def loss(self, bt):
        mem, mem_mask, node_h = self.encode(bt)
        tn, tin, tout = bt["tn"], bt["tin"], bt["tout"]
        valid = tn >= 0
        h = self.decode(self.dec_inputs(node_h, tn, tin, tout), mem, mem_mask)
        prev = torch.cat([torch.full_like(tn[:, :1], -1), tn[:, :-1]], 1)
        prev_out = torch.cat([torch.zeros_like(tout[:, :1]), tout[:, :-1]], 1)
        nl = self.node_logits(h, node_h, bt["node_mask"], prev, prev_out, bt["T"], bt["adjw"])
        ll = self.leave_logits(h, node_h, tn, tin, bt["T"], bt["adjw"])
        ln = F.cross_entropy(nl[valid], tn[valid])
        lt = F.cross_entropy(ll[valid], tout[valid])
        with torch.no_grad():
            an = (nl[valid].argmax(-1) == tn[valid]).float().mean()
            at = (ll[valid].argmax(-1) == tout[valid]).float().mean()
        return ln + lt, dict(ln=ln.item(), lt=lt.item(), an=an.item(), at=at.item())

# 逐段生成。解码器输入的约定：dec_inputs(tn) 取 tn[:, :-1] 作为第 1.. 位置的输入，
# 因此生成第 i 段时 tn 的长度必须是 i+1（末尾补一个不被使用的占位）。
def generate(model, bt, temp=1.0, greedy=False):
    mem, mem_mask, node_h = model.encode(bt)
    dev = node_h.device
    B = node_h.shape[0]
    T, adjw = bt["T"], bt["adjw"]
    segs_n = torch.zeros(B, 0, dtype=torch.long, device=dev)
    segs_in = torch.zeros(B, 0, dtype=torch.long, device=dev)
    segs_out = torch.zeros(B, 0, dtype=torch.long, device=dev)
    done = torch.zeros(B, dtype=torch.bool, device=dev)
    stories = [[] for _ in range(B)]
    pad = torch.zeros(B, 1, dtype=torch.long, device=dev)
    for i in range(MAX_SEGS):
        tn = torch.cat([segs_n, pad], 1); tin = torch.cat([segs_in, pad], 1); tout = torch.cat([segs_out, pad], 1)
        h = model.decode(model.dec_inputs(node_h, tn, tin, tout), mem, mem_mask)[:, -1:]
        prev = segs_n[:, -1:] if i > 0 else torch.full((B, 1), -1, dtype=torch.long, device=dev)
        prev_out = segs_out[:, -1:] if i > 0 else torch.zeros(B, 1, dtype=torch.long, device=dev)
        nl = model.node_logits(h, node_h, bt["node_mask"], prev, prev_out, T, adjw)[:, 0]
        n = nl.argmax(-1) if greedy else torch.multinomial(F.softmax(nl / temp, -1), 1)[:, 0]
        if i == 0:
            entry = torch.zeros(B, dtype=torch.long, device=dev)
        else:
            entry = segs_out[:, -1] + adjw[torch.arange(B, device=dev), prev[:, 0], n]
        ll = model.leave_logits(h, node_h, n.view(B, 1), entry.view(B, 1), T, adjw)[:, 0]
        o = ll.argmax(-1) if greedy else torch.multinomial(F.softmax(ll / temp, -1), 1)[:, 0]
        # 已结束的序列继续占位推进：时刻钳在 T，避免越出时间嵌入表
        entry = torch.where(done, T, entry).clamp(max=TMAX - 1)
        o = torch.where(done, T, o)
        for b in range(B):
            if not done[b]:
                stories[b].append([int(n[b]), int(entry[b]), int(o[b])])
        done = done | (o >= T)
        if bool(done.all()):
            break
        segs_n = torch.cat([segs_n, n.view(B, 1)], 1)
        segs_in = torch.cat([segs_in, entry.view(B, 1)], 1)
        segs_out = torch.cat([segs_out, o.view(B, 1)], 1)
    return stories


# ---------------------------------------------------------------- 训练 / 采样

def train(args):
    torch.manual_seed(args.seed); random.seed(args.seed)
    dev = torch.device(args.device)
    rows = load(args.data)
    tr = [(r, s) for r in rows if not is_test(r) for s in good_targets(r)]
    te = [(r, s) for r in rows if is_test(r) for s in good_targets(r)]
    print(f"训练样本 {len(tr)}（{sum(1 for r in rows if not is_test(r) and good_targets(r))} 案），"
          f"测试样本 {len(te)}（{sum(1 for r in rows if is_test(r) and good_targets(r))} 案）", flush=True)
    model = StoryNet(d=args.d).to(dev)
    print(f"参数量 {sum(p.numel() for p in model.parameters()) / 1e6:.2f} M", flush=True)
    opt = torch.optim.AdamW(model.parameters(), lr=args.lr, weight_decay=0.01)
    steps = args.epochs * math.ceil(len(tr) / args.bs)
    sched = torch.optim.lr_scheduler.OneCycleLR(opt, max_lr=args.lr, total_steps=steps, pct_start=0.1)
    t0 = time.time()
    for ep in range(args.epochs):
        model.train()
        random.shuffle(tr)
        agg = dict(ln=0, lt=0, an=0, at=0); nb = 0
        for i in range(0, len(tr), args.bs):
            bt = collate([Sample(r, s) for r, s in tr[i:i + args.bs]], dev)
            loss, st = model.loss(bt)
            opt.zero_grad(); loss.backward()
            nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            opt.step(); sched.step()
            for k in agg: agg[k] += st[k]
            nb += 1
        model.eval()
        with torch.no_grad():
            ev = dict(ln=0, lt=0, an=0, at=0); ne = 0
            for i in range(0, len(te), 128):
                _, st = model.loss(collate([Sample(r, s, augment=False) for r, s in te[i:i + 128]], dev))
                for k in ev: ev[k] += st[k]
                ne += 1
        f = lambda d, n: " ".join(f"{k} {v / max(n, 1):.3f}" for k, v in d.items())
        print(f"ep {ep + 1:3d}  train {f(agg, nb)} | test {f(ev, ne)}  ({time.time() - t0:.0f} s)", flush=True)
    torch.save(dict(state=model.state_dict(), d=args.d), args.out)
    print(f"已保存 {args.out}", flush=True)


def sample(args):
    torch.manual_seed(args.seed); random.seed(args.seed)
    dev = torch.device(args.device)
    ck = torch.load(args.ckpt, map_location=dev)
    model = StoryNet(d=ck["d"]).to(dev)
    model.load_state_dict(ck["state"]); model.eval()
    rows = [r for r in load(args.data) if args.all or is_test(r)]
    t0 = time.time()
    with open(args.out, "w", encoding="utf-8") as out:
        for r in rows:
            g = generate(model, collate([Sample(r, None, augment=False)], dev), greedy=True)[0]
            ss = generate(model, collate([Sample(r, None, augment=True) for _ in range(args.k)], dev), temp=args.temp)
            ids = r["node_ids"]
            name = lambda st: [[ids[n], a, b] for n, a, b in st]
            out.write(json.dumps(dict(npcs=r["npcs"], seed=r["seed"], greedy=name(g), samples=[name(s) for s in ss]),
                                 ensure_ascii=False) + "\n")
    print(f"采样 {len(rows)} 案 × (1 贪心 + {args.k})，{time.time() - t0:.1f} s", flush=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("cmd", choices=["train", "sample"])
    ap.add_argument("--data", default="m6/data.jsonl")
    ap.add_argument("--out", default="m6/ckpt.pt")
    ap.add_argument("--ckpt", default="m6/ckpt.pt")
    ap.add_argument("--epochs", type=int, default=40)
    ap.add_argument("--bs", type=int, default=64)
    ap.add_argument("--lr", type=float, default=5e-4)
    ap.add_argument("--d", type=int, default=128)
    ap.add_argument("--k", type=int, default=8)
    ap.add_argument("--temp", type=float, default=1.0)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--device", default="cpu")
    ap.add_argument("--all", action="store_true", help="sample：对文件里全部案件采样（默认只采 seed % 10 == 0 的测试集）")
    a = ap.parse_args()
    train(a) if a.cmd == "train" else sample(a)
