"""把 m6/net.py 的 checkpoint 导出成 story-net.rkt 读的二进制权重文件（一次性转换，游戏运行时不需要 Python）。

  python3 m6/export-weights.py m6/ckpt-rl3.pt models/storynet-rl3.bin

格式（小端）：
  魔数 b"SNW1"，u32 d，u32 张量数
  每个张量：u32 名字字节数，名字（UTF-8），u32 维数，各维 u32，float32 数据（行优先）
"""
import struct, sys

import torch

src, dst = sys.argv[1], sys.argv[2]
ck = torch.load(src, map_location="cpu")
st = ck["state"]
with open(dst, "wb") as f:
    f.write(b"SNW1")
    f.write(struct.pack("<II", ck["d"], len(st)))
    for name, t in st.items():
        t = t.detach().float().contiguous()
        nb = name.encode("utf-8")
        f.write(struct.pack("<I", len(nb)))
        f.write(nb)
        f.write(struct.pack("<I", t.dim()))
        f.write(struct.pack(f"<{t.dim()}I", *t.shape))
        f.write(t.numpy().astype("<f4").tobytes())
print(f"{len(st)} 个张量，{sum(t.numel() for t in st.values())} 个参数 → {dst}")
