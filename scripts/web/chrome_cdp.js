// Claude Code
//
// Copyright (C) 2026 Yoann Padioleau
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License
// version 2.1 as published by the Free Software Foundation.
//
// Drive a real Chrome from node, over the DevTools protocol: open a
// page, let it run for a given number of *real* seconds, then report
// what the console said, how many frames the page drew, and take a
// screenshot.
//
// Why not just `chrome --headless --screenshot`: that takes the shot as
// soon as the page "loads", and --virtual-time-budget runs on virtual
// time, which stops while the page's own JavaScript is busy. A page
// that spends its first seconds building a world (games/fps/web/
// TinyMinecraft.html) is therefore screenshotted blank, whether or not
// it works. Here the waiting is real.
//
// usage:
//   node scripts/web/chrome_cdp.js <url> [seconds] [out.png] [keys]
// e.g.
//   node scripts/web/chrome_cdp.js \
//     file://$PWD/_build/default/games/fps/web/TinyMinecraft.html 30 /tmp/mc.png
//
// claude: [keys], keys pressed at given seconds, "second:key" separated
// by spaces ("5:/ 6:s 7:Escape"), each held 200 ms (a key down and up at
// once falls between two frames and the game never sees it), a
// screenshot after each (out-1.png, out-2.png, ...). A page that freezes
// on a key (the code map's /) is found this way, over http:// (a page
// that fetches its data needs a server: python3 -m http.server).
// CHROME=... names the browser (default: macOS's Chrome if there, else
// google-chrome).
//
// It prints the page's console messages and uncaught errors, and a line
// per second with the number of animation frames the page has drawn
// since it started (so a page that is slow, stuck, or dead is easy to
// tell apart), and the longest gap between two frames in that second (a
// freeze of 3 s is a gap of 3000 ms). An uncaught exception is printed
// with its stack, a frame repeated shown once with its count: "at map
// x5000" is a recursion as deep as a list, a stack overflow in a
// browser, where the stack is much smaller than a native program's.
//
// No dependencies: the WebSocket client below is the ~60 lines of RFC
// 6455 that a client needs (masked text frames out, unmasked frames in).

const http = require("http");
const net = require("net");
const crypto = require("crypto");
const { spawn } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const [, , url, secondsArg, outArg, keysArg] = process.argv;
if (!url) {
  console.error("usage: chrome_cdp.js <url> [seconds] [out.png]");
  process.exit(2);
}
const seconds = parseInt(secondsArg || "20", 10);
const out = outArg || "/tmp/chrome_cdp.png";
const keys = (keysArg || "").split(" ").filter((k) => k).map((k) => {
  const i = k.indexOf(":");
  return { at: parseInt(k.slice(0, i), 10), key: k.slice(i + 1) };
});
const port = 9333 + (process.pid % 200);

// ---------------------------------------------------------------------------
// A minimal WebSocket client (RFC 6455): enough for the DevTools protocol
// ---------------------------------------------------------------------------

function connect(wsUrl, onMessage, onOpen) {
  const u = new URL(wsUrl);
  const key = crypto.randomBytes(16).toString("base64");
  const socket = net.connect(Number(u.port), u.hostname, () => {
    socket.write(
      `GET ${u.pathname}${u.search} HTTP/1.1\r\n` +
        `Host: ${u.host}\r\n` +
        "Upgrade: websocket\r\nConnection: Upgrade\r\n" +
        `Sec-WebSocket-Key: ${key}\r\nSec-WebSocket-Version: 13\r\n\r\n`
    );
  });
  let buf = Buffer.alloc(0);
  let open = false;
  socket.on("data", (chunk) => {
    buf = Buffer.concat([buf, chunk]);
    if (!open) {
      const end = buf.indexOf("\r\n\r\n");
      if (end < 0) return;
      buf = buf.subarray(end + 4);
      open = true;
      onOpen();
    }
    // frames: FIN/opcode, length (7, 7+16 or 7+64 bits), payload
    for (;;) {
      if (buf.length < 2) return;
      const len0 = buf[1] & 0x7f;
      let offset = 2;
      let len = len0;
      if (len0 === 126) {
        if (buf.length < 4) return;
        len = buf.readUInt16BE(2);
        offset = 4;
      } else if (len0 === 127) {
        if (buf.length < 10) return;
        len = Number(buf.readBigUInt64BE(2));
        offset = 10;
      }
      if (buf.length < offset + len) return;
      const payload = buf.subarray(offset, offset + len);
      buf = buf.subarray(offset + len);
      const opcode = buf.length >= 0 ? payload : payload; // (text only)
      void opcode;
      try {
        onMessage(JSON.parse(payload.toString("utf8")));
      } catch {
        /* a ping or a split message: ignored, CDP retries nothing */
      }
    }
  });
  socket.on("error", (e) => console.error("socket error:", e.message));
  return {
    send(obj) {
      const data = Buffer.from(JSON.stringify(obj), "utf8");
      const mask = crypto.randomBytes(4);
      const masked = Buffer.from(data);
      for (let i = 0; i < masked.length; i++) masked[i] ^= mask[i % 4];
      let header;
      if (data.length < 126) header = Buffer.from([0x81, 0x80 | data.length]);
      else if (data.length < 65536) {
        header = Buffer.alloc(4);
        header[0] = 0x81;
        header[1] = 0x80 | 126;
        header.writeUInt16BE(data.length, 2);
      } else {
        header = Buffer.alloc(10);
        header[0] = 0x81;
        header[1] = 0x80 | 127;
        header.writeBigUInt64BE(BigInt(data.length), 2);
      }
      socket.write(Buffer.concat([header, mask, masked]));
    },
    close() {
      socket.destroy();
    },
  };
}

// ---------------------------------------------------------------------------
// Chrome
// ---------------------------------------------------------------------------

const mac = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const chrome = process.env.CHROME || (fs.existsSync(mac) ? mac : "google-chrome");
const profile = fs.mkdtempSync(path.join(os.tmpdir(), "chrome_cdp_"));
const child = spawn(
  chrome,
  [
    "--headless=new",
    "--no-sandbox",
    "--use-angle=swiftshader",
    "--enable-unsafe-swiftshader",
    "--window-size=800,800",
    "--disable-gpu-vsync",
    `--remote-debugging-port=${port}`,
    `--user-data-dir=${profile}`,
    "about:blank",
  ],
  { stdio: ["ignore", "ignore", "pipe"] }
);
child.stderr.on("data", () => {});

function get(p) {
  return new Promise((resolve, reject) => {
    http
      .get({ host: "127.0.0.1", port, path: p }, (res) => {
        let body = "";
        res.on("data", (d) => (body += d));
        res.on("end", () => resolve(JSON.parse(body)));
      })
      .on("error", reject);
  });
}

async function waitForChrome() {
  for (let i = 0; i < 100; i++) {
    try {
      return await get("/json/list");
    } catch {
      await new Promise((r) => setTimeout(r, 100));
    }
  }
  throw new Error("Chrome did not open its debugging port");
}

(async () => {
  const targets = await waitForChrome();
  const page = targets.find((t) => t.type === "page");
  let id = 0;
  // claude: the requests sent once the handshake is answered: written
  // before it, they reached Chrome ahead of the upgrade request, and the
  // script waited forever (seen with Chrome 154 on macOS)
  let opened;
  const open = new Promise((r) => (opened = r));
  const waiting = new Map();
  const ws = connect(
    page.webSocketDebuggerUrl,
    (msg) => {
      if (msg.id && waiting.has(msg.id)) {
        waiting.get(msg.id)(msg.result);
        waiting.delete(msg.id);
      } else if (msg.method === "Runtime.consoleAPICalled") {
        console.log("console:", msg.params.args.map((a) => a.value ?? a.description).join(" "));
      } else if (msg.method === "Runtime.exceptionThrown") {
        const d = msg.params.exceptionDetails;
        console.log("EXCEPTION:", d.text, d.exception && d.exception.description.split("\n")[0]);
        // the stack, a frame repeated shown once with its count
        const frames = (d.stackTrace ? d.stackTrace.callFrames : []).map((f) => `${f.functionName || "(anonymous)"} ${f.url.split("/").pop()}:${f.lineNumber + 1}`);
        const runs = [];
        for (const f of frames) {
          if (runs.length && runs[runs.length - 1].f === f) runs[runs.length - 1].n++;
          else runs.push({ f, n: 1 });
        }
        for (const r of runs.slice(0, 12)) console.log(`    at ${r.f}${r.n > 1 ? ` x${r.n}` : ""}`);
      } else if (msg.method === "Log.entryAdded") {
        console.log(`log[${msg.params.entry.level}]:`, msg.params.entry.text);
      }
    },
    () => opened()
  );
  await open;
  const send = (method, params) =>
    new Promise((resolve) => {
      const mine = ++id;
      waiting.set(mine, resolve);
      ws.send({ id: mine, method, params: params || {} });
    });

  await send("Runtime.enable");
  await send("Log.enable");
  await send("Page.enable");
  // count the frames the page draws, from before its own scripts run
  await send("Page.addScriptToEvaluateOnNewDocument", {
    source:
      "window.__frames = 0; window.__gap = 0; window.__last = 0; var r = window.requestAnimationFrame;" +
      "window.requestAnimationFrame = function (f) { return r(function (t) { window.__frames++;" +
      " if (window.__last) window.__gap = Math.max(window.__gap, t - window.__last); window.__last = t; return f(t); }); };",
  });
  await send("Page.navigate", { url });

  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  let shots = 0;
  for (let s = 1; s <= seconds; s++) {
    await sleep(1000);
    for (const k of keys.filter((k) => k.at === s)) {
      // a key's code and virtual key code, as a browser sends them
      const named = { "/": ["Slash", 191], " ": ["Space", 32], Enter: ["Enter", 13], Escape: ["Escape", 27], Tab: ["Tab", 9], Backspace: ["Backspace", 8], ArrowLeft: ["ArrowLeft", 37], ArrowUp: ["ArrowUp", 38], ArrowRight: ["ArrowRight", 39], ArrowDown: ["ArrowDown", 40] };
      const [code, vk] = named[k.key] || ["Key" + k.key.toUpperCase(), k.key.toUpperCase().charCodeAt(0)];
      const text = k.key.length === 1 ? k.key : undefined;
      console.log(`t=${s}s key ${k.key}`);
      await send("Input.dispatchKeyEvent", { type: "keyDown", key: k.key, code, text, windowsVirtualKeyCode: vk });
      await sleep(200);
      await send("Input.dispatchKeyEvent", { type: "keyUp", key: k.key, code, windowsVirtualKeyCode: vk });
      await sleep(300);
      const shot = await send("Page.captureScreenshot", { format: "png" });
      const file = out.replace(/\.png$/, `-${++shots}.png`);
      fs.writeFileSync(file, Buffer.from(shot.data, "base64"));
      console.log("  screenshot:", file);
    }
    const res = await send("Runtime.evaluate", {
      expression:
        "JSON.stringify({frames: window.__frames|0, longest_gap_ms: Math.round((function () { var g = window.__gap; window.__gap = 0; return g; })()), canvas: (document.getElementsByTagName('canvas')[0]||{}).width|0," +
        " text: (document.body && document.body.innerText || '').slice(0, 120)})",
      returnByValue: true,
    });
    console.log(`t=${s}s ${res.result && res.result.value}`);
  }
  const shot = await send("Page.captureScreenshot", { format: "png" });
  fs.writeFileSync(out, Buffer.from(shot.data, "base64"));
  console.log("screenshot:", out);
  ws.close();
  child.kill();
  fs.rmSync(profile, { recursive: true, force: true });
  process.exit(0);
})();
