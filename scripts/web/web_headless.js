// Claude Code
//
// Copyright (C) 2026 Yoann Padioleau
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// version 2.1 as published by the Free Software Foundation.
//
// Run a compiled web Playground app (a .bc.js from examples/web/ or
// games/<genre>/web/) in node, without a browser, with a tiny fake DOM, to debug
// hangs and exceptions that are hard to observe in Chrome (a tab stuck in
// an infinite loop can't even show its console).
//
// usage:
//   node scripts/web/web_headless.js _build/default/games/puzzle/web/Tetris.bc.js [frames] [keys]
// e.g.
//   timeout 10 node scripts/web/web_headless.js \
//     _build/default/games/puzzle/web/Tetris.bc.js 300 ArrowLeft,ArrowUp,' '
//
// It simulates [frames] animation frames at 60Hz (default 300 = 5s),
// pressing (keydown then keyup) the comma-separated [keys] one after the
// other every 30 frames, and prints progress to stderr (every 30 frames,
// each key press, and any frame slower than 50ms) so that, if the app
// hangs, the last line tells you roughly where.
// Always run it under `timeout` and check the exit code (124 = hang).
// With DUMP=1 in the environment, it also prints the DOM tree (i.e., the
// rendered shapes and their attributes) every 30 frames.
//
// Only the DOM operations used by playground/platforms/web/Playground_platform.ml
// are faked; if the app starts calling a new one, you'll get a
// "... is not a function" TypeError pointing at it.

const path = require("path");

const [, , bcjs, framesArg, keysArg] = process.argv;
if (!bcjs) {
  console.error("usage: web_headless.js <app.bc.js> [frames] [keys]");
  process.exit(2);
}
const nframes = parseInt(framesArg || "300", 10);
const keys = keysArg ? keysArg.split(",") : [];

// ---------------------------------------------------------------------------
// Fake DOM
// ---------------------------------------------------------------------------

class FakeElement {
  constructor(tag) {
    this.tagName = tag;
    this.attrs = {};
    this.style = {};
    this.children = [];
    this.parentNode = null;
    this.textContent = "";
  }
  setAttribute(k, v) { this.attrs[k] = String(v); }
  getAttribute(k) { return this.attrs[k]; }
  removeAttribute(k) { delete this.attrs[k]; }
  appendChild(c) {
    c.parentNode = this;
    this.children.push(c);
    return c;
  }
  removeChild(c) {
    this.children.splice(this.children.indexOf(c), 1);
    c.parentNode = null;
    return c;
  }
  replaceChild(newc, oldc) {
    this.children[this.children.indexOf(oldc)] = newc;
    newc.parentNode = this;
    oldc.parentNode = null;
    return oldc;
  }
  get firstChild() { return this.children[0] || null; }
  get nextSibling() {
    if (!this.parentNode) return null;
    const sibs = this.parentNode.children;
    return sibs[sibs.indexOf(this) + 1] || null;
  }
  hasChildNodes() { return this.children.length > 0; }
  // only used for mouse coordinates; identity transform
  createSVGPoint() {
    return { x: 0, y: 0, matrixTransform() { return { x: this.x, y: this.y }; } };
  }
  getScreenCTM() { return { inverse() { return {}; } }; }
}

const body = new FakeElement("body");
let rafCallbacks = [];
const listeners = [];

globalThis.document = {
  body,
  createElementNS: (_ns, tag) => new FakeElement(tag),
  createElement: (tag) => new FakeElement(tag),
};
globalThis.window = globalThis;
globalThis.requestAnimationFrame = (f) => { rafCallbacks.push(f); return rafCallbacks.length; };
globalThis.addEventListener = (kind, f) => listeners.push({ kind, f });
globalThis.Image = class { set src(url) { this._src = url; } };
// claude: Playground_platform.flags reads the URL's parameters; give
// some with SEARCH in the environment, e.g. SEARCH='?camera=lock'
globalThis.location = { search: process.env.SEARCH || "" };
// window.onload is assigned by run_app; we call it ourselves below
globalThis.onload = null;

// ---------------------------------------------------------------------------
// Run
// ---------------------------------------------------------------------------

function countShapes(e) {
  return e.children.reduce((n, c) => n + 1 + countShapes(c), 0);
}

// DUMP=1 in the environment prints the DOM tree every 30 frames
function dump(e, indent) {
  for (const c of e.children) {
    const attrs = Object.entries(c.attrs).map(([k, v]) => `${k}="${v}"`).join(" ");
    const text = c.textContent ? ` "${c.textContent}"` : "";
    process.stderr.write(`${indent}<${c.tagName} ${attrs}>${text}\n`);
    dump(c, indent + "  ");
  }
}

function fire(kind, props) {
  for (const l of listeners) {
    if (l.kind === kind) l.f({ type: kind, buttons: 0, ...props });
  }
}

require(path.resolve(bcjs));
if (typeof globalThis.onload !== "function") {
  console.error("app did not set window.onload");
  process.exit(1);
}
globalThis.onload();

let now = 0; // ms since "page load", like the real rAF timestamp
// make the wall clock follow the simulated frames too (the frames below
// run much faster than real time), otherwise time-based game logic
// (e.g., Tetris' drop speed) would see almost no time passing
const wallClockStart = Date.now();
const realDateNow = Date.now;
Date.now = () => wallClockStart + now;
for (let i = 0; i < nframes; i++) {
  if (keys.length > 0 && i % 30 === 15) {
    const key = keys[Math.floor(i / 30) % keys.length];
    process.stderr.write(`frame ${i}: press '${key}'\n`);
    fire("keydown", { key });
    fire("keyup", { key });
  }
  now += 1000 / 60;
  const cbs = rafCallbacks;
  rafCallbacks = [];
  const t0 = realDateNow();
  for (const f of cbs) f(now);
  const dt = realDateNow() - t0;
  if (i % 30 === 0 || dt > 50) {
    process.stderr.write(`frame ${i}: ${dt}ms, ${countShapes(body)} DOM nodes\n`);
    if (process.env.DUMP) dump(body, "  ");
  }
}
console.log(`OK: ${nframes} frames`);
