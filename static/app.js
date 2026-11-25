// 浏览器端：只经 /api 访问对局，不含规则。所有服务端文本经 textContent 写入。
"use strict";

const STORE_KEY = "cheat-game-id";
const $ = (id) => document.getElementById(id);

const ui = {
  state: null,       // 服务端局面
  target: null,      // 当前询问对象（NPC id）
  subject: null,     // 询问的主体（人物 id）
  busy: false,
};

// ---------------------------------------------------------------- 网络

async function api(method, path, body) {
  const opts = { method, headers: {} };
  if (body !== undefined) {
    opts.headers["Content-Type"] = "application/json";
    opts.body = JSON.stringify(body);
  }
  const res = await fetch("/api/" + path, opts);
  let data = null;
  try { data = await res.json(); } catch (_) { /* 非 JSON */ }
  if (!res.ok) {
    const err = new Error((data && data.error) || `HTTP ${res.status}`);
    err.status = res.status;
    throw err;
  }
  return data;
}

async function run(fn) {
  if (ui.busy) return;
  ui.busy = true;
  render();
  try { await fn(); }
  catch (e) { toast(e.message); }
  finally { ui.busy = false; render(); }
}

function toast(msg) {
  const t = $("toast");
  t.textContent = msg;
  t.hidden = false;
  clearTimeout(toast.timer);
  toast.timer = setTimeout(() => { t.hidden = true; }, 3500);
}

// ---------------------------------------------------------------- 动作

function setState(s) {
  const fresh = !ui.state || ui.state.id !== s.id;
  ui.state = s;
  localStorage.setItem(STORE_KEY, s.id);
  if (fresh || !s.npcs.some((n) => n.id === ui.target)) ui.target = s.npcs[0] && s.npcs[0].id;
  if (fresh || !s.npcs.some((n) => n.id === ui.subject)) ui.subject = ui.target;
  if (s.status === "won" || s.status === "lost") showEnding();
}

async function newGame() {
  const body = { case: $("case-select").value || undefined, ext: $("ext").checked };
  const seed = $("seed").value.trim();
  if (seed !== "") body.seed = Number(seed);
  $("ending").hidden = true;
  setState(await api("POST", "games", body));
}

async function act(kind, body) {
  const r = await api("POST", `games/${ui.state.id}/${kind}`, body);
  setState(r.state);
}

const ask = (slot) => run(() => act("ask", { npc: ui.target, subject: ui.subject, slot }));
const present = (item) => run(() => act("present", { npc: ui.target, item }));
// 指认要点两次：第一次只把按钮变成确认态，4 秒内或换人前再点一次才发出
function accuse() {
  if (ui.armed !== ui.target) {
    ui.armed = ui.target;
    clearTimeout(accuse.timer);
    accuse.timer = setTimeout(() => { ui.armed = null; render(); }, 4000);
    render();
    return;
  }
  ui.armed = null;
  run(() => act("accuse", { npc: ui.target }));
}

// ---------------------------------------------------------------- 渲染

const npcName = (id) => {
  const n = ui.state && ui.state.npcs.find((x) => x.id === id);
  return n ? n.name : "—";
};
const playing = () => ui.state && ui.state.status === "playing" && !ui.busy;
// 按引擎给的 can.{ask,present,accuse} 判断此刻付得起哪种动作（status 为 accuse-only 时只能指认）
const can = (kind) => ui.state && ui.state.can && ui.state.can[kind] && !ui.busy;

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

function fill(list, items, emptyText) {
  list.replaceChildren(...(items.length ? items : [el("li", "empty", emptyText)]));
}

const CLAIM_TAG = { slip: "说漏嘴", retract: "改口", confess: "认罪" };

function render() {
  const s = ui.state;
  if (!s) return;
  const target = npcName(ui.target);

  document.title = `${s.title} · 推理对局`;
  $("title").textContent = s.title;
  $("briefing").textContent = s.briefing;
  $("turn").textContent = s.status === "playing"
    ? `行动点 ${s.ap_used} / ${s.ap_budget}`
    : s.status === "accuse-only" ? `行动点用尽（${s.ap_used} / ${s.ap_budget}）· 只能指认`
    : `${s.status === "won" ? "已破案" : "已失败"} · 用了 ${s.ap_used} / ${s.ap_budget} 行动点`;
  $("phase").hidden = s.phase !== "confront";

  // 左栏：在场者
  fill($("npc-list"), s.npcs.map((n) => {
    const li = el("li", n.id === ui.target ? "selected" : "");
    li.append(el("span", "", n.name));
    if (n.conflicts > 0) li.append(el("span", "count", `矛盾 ×${n.conflicts}`));
    li.addEventListener("click", () => { ui.target = n.id; ui.subject = n.id; render(); });
    return li;
  }), "");

  // 左栏：证物与证词，每项可出示给当前对象
  const presentBtn = (item) => {
    const b = el("button", "", `向${target}出示 · ${s.costs.present} 点`);
    b.disabled = !can("present");
    b.title = `出示消耗 ${s.costs.present} 行动点`;
    b.addEventListener("click", (ev) => { ev.stopPropagation(); present(item); });
    return b;
  };
  fill($("evidence-list"), s.evidence.map((e) => {
    const li = el("li");
    const txt = el("span", "txt");
    txt.append(el("b", "", e.name), el("br"), el("span", "", `表明：${e.text}`));
    li.append(txt, presentBtn(e.id));
    return li;
  }), "没有证物");

  const onlyTarget = $("claim-filter").checked;
  const claims = s.claims.filter((c) => !onlyTarget || c.speaker === ui.target).slice().reverse();
  $("claim-count").textContent = s.claims.length ? `共 ${s.claims.length} 条` : "";
  fill($("claim-list"), claims.map((c) => {
    const li = el("li");
    const txt = el("span", "txt");
    txt.append(el("span", "id", `#${c.id}`), el("b", "", c.speaker_name), document.createTextNode(`：${c.text}`));
    if (CLAIM_TAG[c.kind]) txt.append(el("span", `tag ${c.kind}`, CLAIM_TAG[c.kind]));
    li.append(txt, presentBtn(c.id));
    return li;
  }), onlyTarget ? `${target}还没说过什么` : "还没有人说过什么");

  // 中栏：对话
  renderChat(s);

  // 提问面板
  $("ask-target").textContent = target;
  fill($("subject-choices"), s.npcs.map((n) => {
    const b = el("button", n.id === ui.subject ? "on" : "", n.id === ui.target ? `${n.name}（本人）` : n.name);
    b.disabled = !playing();
    b.addEventListener("click", () => { ui.subject = n.id; render(); });
    return b;
  }), "");
  const subj = ui.subject === ui.target ? "你" : npcName(ui.subject);
  const topics = s.slots.map((sl) => {
    const b = el("button", "topic", `${sl.label} 在哪`);
    b.title = `问${target}：${subj} ${sl.label} 在哪里？`;
    b.disabled = !can("ask");
    b.addEventListener("click", () => ask(sl.index));
    return b;
  });
  const rel = el("button", "topic", "和被害人的关系");
  rel.title = `问${target}：${subj}和被害人是什么关系？`;
  rel.disabled = !can("ask");
  rel.addEventListener("click", () => ask("rel"));
  topics.push(rel);
  fill($("topic-choices"), topics, "");

  // 提问消耗
  $("ask-cost").textContent = `每问 ${s.costs.ask} 点`;

  // 右栏
  const used = s.ap_used, lim = s.ap_budget;
  $("meter-fill").style.width = `${Math.min(100, (100 * used) / lim)}%`;
  $("meter-fill").classList.toggle("confront", s.phase === "confront");
  $("meter-phase").style.left = `${(100 * s.phase_ap) / lim}%`;
  $("turn-detail").textContent =
    `已用 ${used} 点，剩 ${Math.max(0, lim - used)} 点。用到 ${s.phase_ap} 点后进入对质阶段（NPC 压力更大）。` +
    `消耗：提问 ${s.costs.ask}、出示 ${s.costs.present}、指认 ${s.costs.accuse}。`;
  fill($("exposed-list"), s.exposed.map((c) => el("li", "", c.note)), "还没有拆穿谁");
  fill($("evaded-list"), s.evaded.map((h) => el("li", "", h.note)), "没有");
  const acc = $("accuse-btn");
  const armed = ui.armed === ui.target;
  acc.textContent = armed ? `再点一次：确认指认${target}` : `指认${target}是凶手 · ${s.costs.accuse} 点`;
  acc.classList.toggle("armed", armed);
  acc.disabled = !can("accuse");
}

let renderedLog = { id: null, n: -1 };
function renderChat(s) {
  const chat = $("chat");
  if (renderedLog.id === s.id && renderedLog.n === s.log.length) return;
  renderedLog = { id: s.id, n: s.log.length };
  if (s.log.length === 0) {
    const p = el("div", "placeholder");
    p.append(el("div", "", "在左侧选一个人作为询问对象，"), el("div", "", "再在下方选择要问的人和时间段。"),
             el("div", "", "拿证物或别人的证词去和他的说法对质。"));
    chat.replaceChildren(p);
    return;
  }
  const nodes = [];
  for (const e of s.log) {
    nodes.push(el("div", "round", `行动 ${e.step} · ${e.npc_name} · 消耗 ${e.cost} 点`));
    nodes.push(el("div", "msg you", e.question));
    const m = el("div", `msg npc k-${e.response.kind}`);
    const lines = e.response.text.split("\n");
    lines.forEach((line, i) => {
      if (i > 0) m.append(document.createTextNode("\n"));
      if (line.startsWith("（") || i > 0) m.append(el("span", "narr", line));
      else m.append(document.createTextNode(line));
    });
    if (e.response.clauses.length) {
      const cl = el("span", "clauses");
      for (const c of e.response.clauses) cl.append(el("code", "", c));
      m.append(cl);
    }
    nodes.push(m);
  }
  chat.replaceChildren(...nodes);
  chat.scrollTop = chat.scrollHeight;
}

function showEnding() {
  const s = ui.state;
  const won = s.status === "won";
  $("ending-title").textContent = won ? "破案" : "失败";
  const last = s.log[s.log.length - 1];
  const why = last && last.action === "accuse" ? last.response.text
    : `行动点用尽（${s.ap_budget} 点）。`;
  $("ending-text").textContent = `${why}\n\n用了 ${s.ap_used} 行动点。`;
  $("ending").hidden = false;
}

// ---------------------------------------------------------------- 启动

async function loadCases() {
  const cases = await api("GET", "cases");
  const sel = $("case-select");
  sel.replaceChildren(...cases.filter((c) => !c.error).map((c) => {
    const o = el("option", "", `${c.title}（${c.npcs} 人，${c.ap_budget} 行动点）`);
    o.value = c.file;
    return o;
  }));
}

async function boot() {
  $("newgame").addEventListener("submit", (ev) => { ev.preventDefault(); run(newGame); });
  $("accuse-btn").addEventListener("click", accuse);
  $("claim-filter").addEventListener("change", render);
  $("show-clauses").addEventListener("change", (ev) => $("chat").classList.toggle("show-clauses", ev.target.checked));
  $("ending-close").addEventListener("click", () => { $("ending").hidden = true; });
  $("ending-new").addEventListener("click", () => run(newGame));
  await run(async () => {
    await loadCases();
    const id = localStorage.getItem(STORE_KEY);
    if (id) {
      try {
        setState(await api("GET", `games/${id}`));
        $("case-select").value = ui.state.case;
        return;
      } catch (e) {
        if (e.status !== 404) throw e;
        localStorage.removeItem(STORE_KEY);
      }
    }
    await newGame();
  });
}

boot();
