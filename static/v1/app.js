// 地图版浏览器端：只经 /api/v1 访问对局，不含规则。所有服务端文本经 textContent 写入。
"use strict";

const STORE_KEY = "cheat-game-v1-id";
const SVG = "http://www.w3.org/2000/svg";
const $ = (id) => document.getElementById(id);

const ui = {
  state: null,
  target: null,     // 当前对象（人物 id）
  kind: "self",     // self | about | place
  arg: null,        // about：人物 id；place：地点 id
  slot: null,
  armed: false,     // 指认按钮是否已点过一次
  busy: false,
};

const KINDS = [["self", "他自己在哪"], ["about", "有没有见过某人"], ["place", "在某地见过谁"]];

// ---------------------------------------------------------------- 网络

async function api(method, path, body) {
  const opts = { method, headers: {} };
  if (body !== undefined) {
    opts.headers["Content-Type"] = "application/json";
    opts.body = JSON.stringify(body);
  }
  const res = await fetch("/api/v1/" + path, opts);
  let data = null;
  try { data = await res.json(); } catch (_) { /* 非 JSON */ }
  if (!res.ok) throw new Error((data && data.error) || `HTTP ${res.status}`);
  return data;
}

async function run(fn) {
  if (ui.busy) return;
  ui.busy = true;
  render();
  try { await fn(); } catch (e) { toast(e.message); } finally { ui.busy = false; render(); }
}

function toast(msg) {
  const t = $("toast");
  t.textContent = msg;
  t.hidden = false;
  clearTimeout(toast.timer);
  toast.timer = setTimeout(() => { t.hidden = true; }, 3500);
}

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

// ---------------------------------------------------------------- 状态

const playing = () => ui.state && ui.state.status === "playing" && !ui.busy;
const canAccuse = () => ui.state && (ui.state.status === "playing" || ui.state.status === "accuse-only") && !ui.busy;
const apLeft = () => ui.state.ap_budget - ui.state.ap_used;
const personName = (id) => { const p = ui.state && ui.state.persons.find((x) => x.id === id); return p ? p.name : "—"; };
const placeName = (id) => { const n = ui.state && ui.state.map.nodes.find((x) => x.id === id); return n ? n.name : "—"; };

function setState(s) {
  const fresh = !ui.state || ui.state.id !== s.id;
  ui.state = s;
  localStorage.setItem(STORE_KEY, s.id);
  if (fresh) {
    ui.target = s.persons[0].id; ui.kind = "self"; ui.arg = null; ui.slot = null; ui.armed = false;
    drawMap();
  }
  if (s.status === "won" || s.status === "lost") showEnding();
}

async function newGame(ev) {
  if (ev) ev.preventDefault();
  const body = { npcs: Number($("npcs").value), planner: $("omni").checked ? "omni" : "local",
                 manner: $("manner").checked ? "rule" : "full" };
  const seed = $("seed").value.trim();
  if (seed) body.seed = Number(seed);
  await run(async () => { setState(await api("POST", "games", body)); $("ending").hidden = true; });
}

async function ask() {
  const s = ui.state;
  const body = { kind: ui.kind, npc: ui.target, slot: ui.slot };
  if (ui.kind !== "self") body.arg = ui.arg;
  await run(async () => setState((await api("POST", `games/${s.id}/ask`, body)).state));
}

async function present(item) {
  await run(async () => setState((await api("POST", `games/${ui.state.id}/present`, { npc: ui.target, item })).state));
}

async function accuse() {
  if (!ui.armed) { ui.armed = true; render(); return; }
  ui.armed = false;
  await run(async () => setState((await api("POST", `games/${ui.state.id}/accuse`, { npc: ui.target })).state));
}

// ---------------------------------------------------------------- 地图

// 走廊排成一行；挂在走廊上的房间 / 户外轮流放在上下两侧
function layout(map) {
  const pos = {};
  const corr = map.nodes.filter((n) => n.kind === "corridor");
  const adj = (id) => map.edges.filter((e) => e.a === id || e.b === id).map((e) => (e.a === id ? e.b : e.a));
  const W = 150, H = 70;
  corr.forEach((c, i) => { pos[c.id] = { x: 120 + i * 360, y: 170 }; });
  const placed = new Set(corr.map((c) => c.id));
  corr.forEach((c) => {
    const others = adj(c.id).filter((x) => !placed.has(x));
    const top = others.filter((_, i) => i % 2 === 0), bottom = others.filter((_, i) => i % 2 === 1);
    [[top, 50], [bottom, 290]].forEach(([row, y]) => {
      row.forEach((id, i) => {
        pos[id] = { x: pos[c.id].x + (i - (row.length - 1) / 2) * W, y };
        placed.add(id);
      });
    });
  });
  map.nodes.forEach((n, i) => { if (!pos[n.id]) pos[n.id] = { x: 120 + i * W, y: 400 }; });
  const xs = Object.values(pos).map((p) => p.x);
  const minX = Math.min(...xs) - 70, maxX = Math.max(...xs) + 70;
  return { pos, box: [minX, 10, maxX - minX, 330], H };
}

function svg(tag, attrs, text) {
  const e = document.createElementNS(SVG, tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text !== undefined) e.textContent = text;
  return e;
}

function drawMap() {
  const s = ui.state, m = s.map, g = $("map");
  const { pos, box } = layout(m);
  g.setAttribute("viewBox", box.join(" "));
  g.replaceChildren();
  for (const e of m.edges) {
    const a = pos[e.a], b = pos[e.b];
    g.append(svg("line", { x1: a.x, y1: a.y, x2: b.x, y2: b.y, class: "edge" }));
    g.append(svg("text", { x: (a.x + b.x) / 2 + 6, y: (a.y + b.y) / 2, class: "edge-label" }, `${e.minutes} 分`));
  }
  for (const w of m.windows) {
    const a = pos[w.from], b = pos[w.to];
    g.append(svg("line", { x1: a.x, y1: a.y + 8, x2: b.x, y2: b.y + 8, class: "window" }));
  }
  for (const n of m.nodes) {
    const p = pos[n.id];
    const w = n.kind === "corridor" ? 130 : 110, h = 44;
    const grp = svg("g", { class: `node ${n.kind}${n.id === s.crime.place ? " crime" : ""}`, "data-id": n.id, tabindex: 0 });
    grp.append(svg("rect", { x: p.x - w / 2, y: p.y - h / 2, width: w, height: h, rx: 8 }));
    grp.append(svg("text", { x: p.x, y: p.y + 5, "text-anchor": "middle" }, n.name));
    grp.addEventListener("click", () => { if (!playing()) return; ui.kind = "place"; ui.arg = n.id; render(); });
    g.append(grp);
  }
  $("map-note").textContent =
    (m.windows.length ? "虚线是窗户（单向看得见对面屋里）。" : "") +
    "同一地点的人互相看得见；站在走廊上能看见相邻房间的门口，看不见屋里。";
}

function highlightMap() {
  for (const n of $("map").querySelectorAll(".node")) {
    n.classList.toggle("selected", ui.kind === "place" && n.dataset.id === ui.arg);
  }
}

// ---------------------------------------------------------------- 渲染

function fill(list, items, empty) { list.replaceChildren(...(items.length ? items : [el("li", "empty", empty)])); }

function chip(label, on, disabled, onClick) {
  const b = el("button", on ? "chip on" : "chip", label);
  b.type = "button";
  b.disabled = disabled;
  b.addEventListener("click", onClick);
  return b;
}

function render() {
  const s = ui.state;
  if (!s) return;
  const target = personName(ui.target);
  $("ap-text").textContent = s.status === "playing" ? `行动点 ${s.ap_used} / ${s.ap_budget}`
    : s.status === "accuse-only" ? `行动点用完（${s.ap_used} / ${s.ap_budget}）· 只能指认`
    : `${s.status === "won" ? "已破案" : "失败"} · 用了 ${s.ap_used} / ${s.ap_budget}`;
  $("ap-fill").style.width = `${Math.min(100, (100 * s.ap_used) / s.ap_budget)}%`;
  $("briefing").textContent = s.briefing;
  $("rule").textContent =
    `作案需要在${s.crime.place_name}连续待至少 ${s.crime.min_minutes} 分钟。胜利：指认凶手，并且 (1) 其他每个人都已被排除——只用别人的证词和物证，他不可能在案发时段在${s.crime.place_name}待够这么久；或 (2) 凶手关于自己行踪的说法已被至少 ${s.expose_to_win} 个不同来源出示后证明矛盾；或 (3) 凶手亲口说过案发时段他在${s.crime.place_name}。` +
    ` 消耗：提问 ${s.costs.ask}，出示 ${s.costs.present}，指认 ${s.costs.accuse}。`;

  // 在场者
  fill($("persons"), s.persons.map((p) => {
    const li = el("li", p.id === ui.target ? "selected" : "");
    li.append(el("span", "", p.name));
    const n = new Set(s.exposed.filter((e) => e.person === p.id).map((e) => e.source)).size;
    if (n > 0) li.append(el("span", "count", `被拆穿 ×${n}`));
    li.addEventListener("click", () => {
      ui.target = p.id; ui.armed = false;
      if (ui.kind === "about" && ui.arg === p.id) ui.arg = null;
      render();
    });
    return li;
  }), "");

  // 提问
  $("target-name").textContent = target;
  $("kinds").replaceChildren(...KINDS.map(([k, label]) =>
    chip(label, ui.kind === k, !playing(), () => { ui.kind = k; ui.arg = null; render(); })));
  $("arg-row").hidden = ui.kind === "self";
  $("arg-label").textContent = ui.kind === "about" ? "见过谁" : "哪里";
  const args = ui.kind === "about"
    ? s.persons.filter((p) => p.id !== ui.target).map((p) => [p.id, p.name])
    : s.map.nodes.map((n) => [n.id, n.name]);
  $("args").replaceChildren(...args.map(([id, name]) => chip(name, ui.arg === id, !playing(), () => { ui.arg = id; render(); })));
  $("slots").replaceChildren(...s.slots.map((sl) => chip(sl.label, ui.slot === sl.index, !playing(), () => { ui.slot = sl.index; render(); })));
  const ready = playing() && ui.slot !== null && (ui.kind === "self" || ui.arg !== null) && apLeft() >= s.costs.ask;
  const slotLabel = ui.slot === null ? "（选时段）" : s.slots[ui.slot].label;
  $("preview").textContent =
    ui.kind === "self" ? `问${target}：${slotLabel} 你在哪？`
    : ui.kind === "about" ? `问${target}：${slotLabel} 你见过${ui.arg ? personName(ui.arg) : "（选人）"}吗？`
    : `问${target}：${slotLabel} 你在${ui.arg ? placeName(ui.arg) : "（选地点）"}见过谁？`;
  $("ask-btn").disabled = !ready;
  $("ask-btn").textContent = `提问 · ${s.costs.ask} 点`;
  highlightMap();

  // 证物与证词
  const canPresent = playing() && apLeft() >= s.costs.present;
  const presentBtn = (item) => {
    const b = el("button", "small", `出示给${target} · ${s.costs.present} 点`);
    b.type = "button";
    b.disabled = !canPresent;
    b.addEventListener("click", () => present(item));
    return b;
  };
  fill($("evidence"), s.evidence.map((e) => {
    const li = el("li");
    const t = el("div", "txt");
    t.append(el("b", "", e.name), el("div", "", e.text));
    li.append(t, presentBtn(e.id));
    return li;
  }), "没有物证");
  const only = $("only-target").checked;
  const heard = s.heard.filter((c) => !only || c.speaker === ui.target);
  $("heard-count").textContent = s.heard.length ? `共 ${s.heard.length} 条` : "";
  fill($("heard"), heard.slice().reverse().map((c) => {
    const li = el("li");
    const t = el("div", "txt");
    t.append(el("b", "", `${c.speaker_name}：`), document.createTextNode(c.text));
    li.append(t, presentBtn(c.id));
    return li;
  }), only ? `${target}还没说过什么` : "还没听到证词");
  fill($("exposed"), s.exposed.map((e) =>
    el("li", "", `${e.person_name}「${e.claim_text}」被${e.source_name}拆穿`)), "还没有");

  // 指认
  const acc = $("accuse-btn");
  acc.disabled = !canAccuse();
  acc.textContent = ui.armed ? `再点一次：确认指认${target}` : `指认${target}是凶手`;
  acc.classList.toggle("armed", ui.armed);

  renderLog();
}

let rendered = { id: null, n: -1 };
function renderLog() {
  const s = ui.state, box = $("log");
  if (rendered.id === s.id && rendered.n === s.log.length) return;
  rendered = { id: s.id, n: s.log.length };
  const nodes = [];
  if (!s.log.length) nodes.push(el("div", "hint", "左边选一个人，下面选问题和时段。也可以把证物或证词出示给某人，看他的说法是否矛盾。"));
  for (const e of s.log) {
    const d = el("div", `entry ${e.kind}`);
    d.append(el("div", "meta", `已用 ${e.ap_used} 点 · ${e.npc_name}`));
    if (e.kind === "ask") {
      d.append(el("div", "q", e.question));
      const tone = { curt: "（不耐烦）", perfunctory: "（敷衍，只说这个时段）" }[e.mode] || "";
      if (e.mode === "refuse") d.append(el("div", "a conflict", `${e.npc_name}：「……我不想再说了。你去问别人吧。」`));
      else if (!e.answers.length) d.append(el("div", "a muted", `${e.npc_name}${tone}：「不清楚。」`));
      for (const a of e.answers) d.append(el("div", "a", `${e.npc_name}${tone}：「${a.text}」`));
    } else if (e.kind === "present") {
      d.append(el("div", "q", `向${e.npc_name}出示：${e.item_text}`));
      if (!e.conflicts.length) d.append(el("div", "a muted", `${e.npc_name}：「这和我说的不矛盾。」`));
      for (const c of e.conflicts) d.append(el("div", "a conflict", `与他说过的「${c.claim.text}」矛盾（${c.type}）。${e.npc_name}：「……可能是我记错了。」`));
      if (e.new_exposure) d.append(el("div", "a exposed", `${e.npc_name}关于自己行踪的说法被拆穿。`));
    } else {
      d.append(el("div", "q", `指认${e.npc_name}`), el("div", e.won ? "a won" : "a conflict", e.text));
    }
    nodes.push(d);
  }
  box.replaceChildren(...nodes);
  box.scrollTop = box.scrollHeight;
}

function showEnding() {
  const s = ui.state, r = s.reveal;
  $("ending-title").textContent = s.status === "won" ? "破案" : "失败";
  const last = s.log[s.log.length - 1];
  $("ending-text").textContent =
    (last && last.kind === "accuse" ? last.text : `行动点用完（${s.ap_budget} 点）。`) + `\n用了 ${s.ap_used} 点。凶手是${r.culprit_name}。`;
  const box = $("reveal");
  box.replaceChildren();
  const seg = (xs) => xs.map((x) => `${x.place_name} ${x.from}–${x.to}`).join(" → ");
  box.append(el("h3", "", "凶手编的故事"), el("p", "", seg(r.story)), el("h3", "", "真实行踪"));
  const ul = el("ul", "paths");
  for (const p of r.paths) ul.append(el("li", p.person === r.culprit ? "culprit" : "", `${p.person_name}：${seg(p.segs)}`));
  box.append(ul);
  $("ending").hidden = false;
}

// ---------------------------------------------------------------- 启动

async function boot() {
  $("newgame").addEventListener("submit", newGame);
  $("ask-btn").addEventListener("click", ask);
  $("accuse-btn").addEventListener("click", accuse);
  $("only-target").addEventListener("change", render);
  $("ending-close").addEventListener("click", () => { $("ending").hidden = true; });
  $("ending-new").addEventListener("click", () => newGame());
  const id = localStorage.getItem(STORE_KEY);
  if (id) {
    try { setState(await api("GET", `games/${id}`)); render(); return; }
    catch (e) { localStorage.removeItem(STORE_KEY); }
  }
  await newGame();
}

boot();
