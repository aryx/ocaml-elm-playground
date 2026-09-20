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
// that spends its first seconds building a world (games3d/webgl/
// TinyMinecraft.html) is therefore screenshotted blank, whether or not
// it works. Here the waiting is real.
//
// usage:
//   node scripts/web/chrome_cdp.js <url> [seconds] [out.png]
// e.g.
//   node scripts/web/chrome_cdp.js \
//     file://$PWD/_build/default/games3d/webgl/TinyMinecraft.html 30 /tmp/mc.png
//
// It prints the page's console messages and uncaught errors, and a line
// per second with the number of animation frames the page has drawn
// since it started (so a page that is slow, stuck, or dead is easy to
// tell apart).
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

const [, , url, secondsArg, outArg] = process.argv;
if (!url) {
  console.error("usage: chrome_cdp.js <url> [seconds] [out.png]");
  process.exit(2);
}
const seconds = parseInt(secondsArg || "20", 10);
const out = outArg || "/tmp/chrome_cdp.png";
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

const chrome = process.env.CHROME || "google-chrome";
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
        console.log("EXCEPTION:", d.text, d.exception && d.exception.description);
      } else if (msg.method === "Log.entryAdded") {
        console.log(`log[${msg.params.entry.level}]:`, msg.params.entry.text);
      }
    },
    () => {}
  );
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
      "window.__frames = 0; var r = window.requestAnimationFrame;" +
      "window.requestAnimationFrame = function (f) { return r(function (t) { window.__frames++; return f(t); }); };",
  });
  await send("Page.navigate", { url });

  for (let s = 1; s <= seconds; s++) {
    await new Promise((r) => setTimeout(r, 1000));
    const res = await send("Runtime.evaluate", {
      expression:
        "JSON.stringify({frames: window.__frames|0, canvas: (document.getElementsByTagName('canvas')[0]||{}).width|0," +
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
