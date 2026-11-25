目前项目已经打通了确定性符号内核与对抗可解性底座（分层 KB、时间区间冲突检测、压力与受控遗忘、网状多路径案件生成），但距离“所有 NPC 由深度学习模型驱动的完整侦探游戏”，还停留在规则引擎 + 外部大模型 Prompt 验证阶段。

下图标出了完整架构中已经建成的底座（灰框）与亟待补齐的核心缺口（蓝框为最紧迫的 DL 运行时双核，浅灰为待补的闭环与交互层）：

<svg viewBox="0 0 680 280" xmlns="http://www.w3.org/2000/svg">
  <style>
    :root {
      --lumi-ink: light-dark(#000000, #e0e0e0);
      --lumi-muted: light-dark(#5f6368, #9aa0a6);
      --lumi-dim: light-dark(#f2f0f0, #242528);
      --lumi-container: light-dark(#e8e6e6, #2d2f33);
      --lumi-outline: light-dark(#727676, #9a9b9c);
      --lumi-accent-bg: light-dark(#dcf1ff, #192967);
      --lumi-primary: light-dark(#3186ff, #9dd2ff);
      --lumi-sans: 'Google Sans Flex', var(--font-family, 'Google Sans', sans-serif);
      --lumi-mono: 'Google Sans Code', ui-monospace, monospace;
    }
    text { font-weight: 400; stroke: none; }
    .t-l1 { font-family: var(--lumi-sans); font-size: 15px; fill: var(--lumi-ink); }
    .t-l2 { font-family: var(--lumi-mono); font-size: 12px; font-style: italic; fill: var(--lumi-ink); }
    .t-l4 { font-family: var(--lumi-sans); font-size: 12px; fill: var(--lumi-muted); }
    .node-step { fill: var(--lumi-dim); stroke: none; }
    .node-focal { fill: var(--lumi-accent-bg); stroke: var(--lumi-primary); stroke-width: 1.25; }
    .node-store { fill: var(--lumi-container); stroke: var(--lumi-outline); stroke-width: 1; }
    .edge-arrow { fill: none; stroke: var(--lumi-ink); stroke-width: 1.3; stroke-linecap: round; stroke-dasharray: 0 4; marker-end: url(#lumi-caret); }
  </style>
  <defs>
    <marker id="lumi-caret" viewBox="0 0 10 10" refX="7.5" refY="5" markerUnits="userSpaceOnUse" markerWidth="10" markerHeight="10" orient="auto">
      <path d="M 3 1.2 L 7.5 5 L 3 8.8" fill="none" stroke="var(--lumi-ink)" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round"/>
    </marker>
  </defs>

  <rect class="node-store" x="20" y="24" width="164" height="80" rx="40"/>
  <text class="t-l1" x="102" y="54" text-anchor="middle" dominant-baseline="central">符号引擎与 KB</text>
  <text class="t-l4" x="102" y="76" text-anchor="middle" dominant-baseline="central">已建 · 缺观察/视线视图</text>

  <line class="edge-arrow" x1="192" y1="64" x2="248" y2="64"/>

  <rect class="node-focal" x="256" y="24" width="164" height="80" rx="40"/>
  <text class="t-l1" x="338" y="54" text-anchor="middle" dominant-baseline="central">策略核 DL 模型</text>
  <text class="t-l2" x="338" y="76" text-anchor="middle" dominant-baseline="central">缺口: 蒸馏 + CFG解码</text>

  <line class="edge-arrow" x1="428" y1="64" x2="484" y2="64"/>

  <rect class="node-focal" x="492" y="24" width="164" height="80" rx="40"/>
  <text class="t-l1" x="574" y="54" text-anchor="middle" dominant-baseline="central">表层台词渲染</text>
  <text class="t-l2" x="574" y="76" text-anchor="middle" dominant-baseline="central">缺口: 替代 text.rkt</text>

  <rect class="node-store" x="20" y="176" width="164" height="80" rx="40"/>
  <text class="t-l1" x="102" y="206" text-anchor="middle" dominant-baseline="central">案件生成与校验</text>
  <text class="t-l4" x="102" y="228" text-anchor="middle" dominant-baseline="central">已建 · gen-net / solve</text>

  <rect class="node-step" x="256" y="176" width="164" height="80" rx="40"/>
  <text class="t-l1" x="338" y="206" text-anchor="middle" dominant-baseline="central">蒸馏与自博弈训练</text>
  <text class="t-l4" x="338" y="228" text-anchor="middle" dominant-baseline="central">缺口 · rollout 偏移</text>

  <rect class="node-step" x="492" y="176" width="164" height="80" rx="40"/>
  <text class="t-l1" x="574" y="206" text-anchor="middle" dominant-baseline="central">玩家端与多NPC总线</text>
  <text class="t-l4" x="574" y="228" text-anchor="middle" dominant-baseline="central">缺口 · CLI / 孤立审讯</text>

  <line class="edge-arrow" x1="102" y1="168" x2="102" y2="112"/>
  <line class="edge-arrow" x1="574" y1="112" x2="574" y2="168"/>
  <line class="edge-arrow" x1="484" y1="216" x2="428" y2="216"/>
  <line class="edge-arrow" x1="338" y1="168" x2="338" y2="112"/>
</svg>


按工程优先级与架构耦合度，当前项目需要补齐的差距集中在以下 5 个层面：

层面	当前实现状态	核心差距	优先级
视图信息与教师数据质量	assemble-view 只含自身行踪与掩护；教师思考未带来遗憾下降	输入缺自身观察与空间视线，导致模型无法判断编造安全性；rollout 存在对手与后续策略偏移	P0
策略输出协议（ABI）与约束解码	llm.rkt 仅在枚举列表中选编号，依赖重试与规则回退	未实现“组装式多子句输出”，缺少运行时动态 CFG 约束解码，无法支撑低延迟小模型 0 幻觉输出	P0
学生模型训练与实时推理	仅通过 HTTP 串行调用本地 DeepSeek V4 Flash（53–85s/步）	缺少轻量化学生模型架构、SFT/偏好蒸馏/RL 训练管线及亚秒级本地推理（含 KV-Cache 复用）	P1
双系统演出层（Surface Realizer）	text.rkt 纯字符串模板拼接	缺少受压力/情绪/人设驱动的台词渲染模型，策略核与演出层之间缺少情绪与迟疑信号契约	P1
世界观、多 NPC 交互与玩家端	单人轮询、仅 at/rel 命题、CLI 编号菜单	缺少多 NPC 同场对质/合谋总线、物理证物与移动耗时建模、玩家自然语言审讯前端与推理看板	P2

1. 视图盲区与训练信号失真（最紧迫的根因）

从 design.md 的 think-v3 / think-v4 评测数据看，教师模型目前有两个硬伤：一是不可拆穿的编造只有 2.9%–14.3% 标为 low（几乎把所有编造全标成高危），二是带思考分析的 A 组遗憾（0.048）与不带分析的 B 组（0.052）基本无差别。

结合源码看，这不只是 Prompt 问题，而是输入契约和估值机制的结构性缺口：

⚬ view 缺少决定推理成败的局部环境信息：在 engine.rkt 的 assemble-view 中，prefix 只放了 truth、cover、rels、hide 和 crime，既没有放入该 NPC 自己掌握的他人观察（knowledge），也没有放入 gen-net.rkt 生成的房间视线拓扑。NPC 连“20:00 厨房里其实有厨师”或“走廊能看见书房门口”都看不到，自然算不出哪个 fabricate 安全。
⚬ 回放估值存在策略偏移：rollout.rkt 在评估某个动作时，后续步由固定规则 rule-policy 接管，侦探方也只是启发式玩家或全知检查器。这导致 think-v4 中 6 个得分最高的 fabricate 其实全都是可被拆穿的，高分只是因为弱对手拆穿慢。直接拿这套估值做蒸馏或 RL 奖励，会训出“专门针对弱启发式玩家钻空子”的畸形策略。
⚬ 匿名句柄抽象未打通到模型侧：main.md 设计了匿名寄存器句柄（E0..E31, L0..L15）以实现新案件免重训，但 llm.rkt 和 think.rkt 仍把视图渲染为中文人名/地名喂给模型，解析时再反向查表转回符号。

2. 模型 I/O 协议（ABI）与约束解码缺位

当前 llm.rkt 依赖引擎先用 legal-answers 穷举所有合法选项，模型只输出一个选项编号。这种设计在真实游戏里扩展性会迅速触顶：

⚬ 组装式回答尚未落地：design.md 末尾已经规划了让模型直接输出 1–3 个子句（(at ...) / (not-at ...) / (rel ...) / (retract ...)），但引擎侧的语法校验、临场编造的遗忘机制（把模型自己说过的假话从 view 中删除以诱发自相矛盾）和单句/多句组合的可解性检查都还没实现。
⚬ 缺少运行时动态 CFG 约束解码：目前在 think.rkt 里，选项一多首次可解析率就掉到 45%–65%，只能靠把报错贴回给模型重试。一旦切到“组装式输出”或参数更小的学生模型，必须引入动态文法约束（如 llguidance / XGrammar），每轮根据当前合法实体 ID、时间槽和权限动态生成 Grammar Mask，在采样层把非法输出物理清零。

3. 深度学习学生模型、训练闭环与实时推理

目前仓库里还没有任何神经网络训练或推理代码，所有实验都跑在外部 ds4-server 上：

⚬ 训练管线从零待建：需要建立“批量网状案件生成 → 教师轨迹采样 + 改进版 Rollout 筛选（Best-of-K / DPO / GRPO）→ 轻量策略模型微调”的完整数据与训练管线。
⚬ 推理延迟高出游戏门槛两个数量级：当前思考模式单步耗时 53–85 秒，非思考模式也要 2.3 秒；真实审讯交互要求单轮端到端延迟压到 1 秒以内。这需要模型参数量下探（例如 0.5B–3B 专精策略模型或轻量状态模型），并结合 prefix（案件固定真相/掩护/视线）的 KV-Cache 前缀复用，每轮只增量计算动态的 flags、pinned、recent 和 q。
⚬ 对抗可解性边界待收紧：net-v2 中仍有 3 例静态模型判可达、动态消融却 stuck 的边界案例尚未查明；且目前对抗检查只测了“单句不可拆穿编造”，还没覆盖“编造 + 回避 + 诬陷组合”的强对抗模型。

4. 表层台词渲染与双系统解耦

目前 text.rkt 是纯确定性模板，所有角色说话腔调完全一致，没有推理游戏的戏剧张力：

⚬ 缺少 Surface Realizer（台词渲染模型）：需要按 main.md 的双系统思路，接入一个看不到案件真相的小语言模型，只接收 [角色人设卡] + [策略核定好的结构化命题] + [当前压力/阶段/是否说漏嘴] 来生成台词，从物理上杜绝“写台词时顺嘴剧透或胡编细节”。
⚬ 演出信号契约缺失：engine.rkt 的 response 结构目前只含 (response kind claims)，没有向表现层暴露压力变化量、当前强度、策略模型的思考长度/熵值（可用于驱动角色的迟疑停顿、微表情或语音颤抖）。

5. 多 NPC 博弈深度、世界模型与玩家端体验

即使模型全部就位，玩法和产品层还有三块明显短板：

⚬ 多 NPC 之间完全隔离：目前是玩家 1 对 1 轮询，缺少 main.md 提到的双轨消息总线。NPC 看不到其他人在对质中的发言，无法在引擎前置计算 [MEMORY_CONFLICT] 后触发实时插话、当面互咬、合谋圆谎或把锅甩给真凶。
⚬ 推理维度偏单一：kb.rkt 只有 at（位置）和 rel（4 种静态动机）两种谓词，且不计算移动路程时间。要支撑更像侦探游戏的推理体验，后续需要扩展物品持有/转移、物理状态（门锁、凶器、现场痕迹）以及空间拓扑耗时约束。
⚬ 玩家交互与可视化面板：main.rkt 仍是终端数字菜单。当案件扩展到 6–8 人网状结构时，纯文本笔记极难消化；既需要图形化的时间线/视线图/证据连线板，若想支持自由文本审讯，还需在前端加一层把自然语言映射到确定性查询指令的 NLU 解析器。

建议的补齐顺序

1. 先修视图与估值（低成本、高杠杆）：把 NPC 自身的 knowledge 和房间视线拓扑写进 assemble-view，重跑一次 think-eval，确认教师模型能真正区分“安全编造”与“可拆穿编造”，否则后面蒸馏全是噪声。
2. 落组装式输出 + 约束解码：把 engine.rkt 从“选编号”切到“子句校验 + 临场谎言遗忘”，并在推理侧接上 CFG 约束解码，彻底消灭格式错误和重试开销。
3. 打通最小学生模型闭环：用修改后的 rollout（最好配一个更强的侦探策略）在 gen-net 批量生成的数百个案件上合成轨迹，训出第一个轻量本地策略模型，验证延迟和博弈强度。
4. 再补双系统演出与多 NPC 交互：接入隔离真相的台词渲染器，把压力/熵值接到台词风格上，最后扩展双轨消息总线与图形化推理界面。
