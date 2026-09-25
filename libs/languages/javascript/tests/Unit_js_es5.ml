(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_js_es5.mli *)

let run (s : string) : string =
  let t = Js_eval.create ~now:(fun () -> 1_000_000_000_000.) () in
  match Js_eval.eval t s with Ok v -> Js_value.display v | Error e -> Printf.sprintf "line %d: %s" e.line e.message

let check (what : string) (s : string) (expected : string) : unit = Alcotest.(check string) what expected (run s)

let tests =
  Testo.categorize "Js ES5"
    [
      Testo.create "prototypes, new, instanceof" (fun () ->
          check "a constructor and a method on its prototype"
            "function Point(x, y) { this.x = x; this.y = y; }\nPoint.prototype.sum = function () { return this.x + this.y; };\nvar p = new Point(1, 2);\n[p.sum(), p instanceof Point, p instanceof Array, p.hasOwnProperty('x'), p.hasOwnProperty('sum')]"
            "[3, true, false, true, false]";
          check "Object.create: a chain" "var a = {hi: function () { return 'hi ' + this.n; }};\nvar b = Object.create(a);\nb.n = 'b';\n[b.hi(), Object.getPrototypeOf(b) === a]"
            "[\"hi b\", true]";
          check "new Error: its message" "var r;\ntry { throw new TypeError('no') } catch (e) { r = [e.name, e.message] }\nr" "[\"TypeError\", \"no\"]");
      Testo.create "call, apply, bind: hn.js's Array.prototype calls" (fun () ->
          check "indexOf, slice, forEach called on an array"
            "var a = ['x', 'y', 'z'];\nvar seen = [];\nArray.prototype.forEach.call(a, function (v) { seen.push(v) });\n[Array.prototype.indexOf.call(a, 'y'), Array.prototype.slice.call(a, 1, 2), seen.length]"
            "[1, [\"y\"], 3]";
          check "apply and bind" "function f(a, b) { return this.k + a + b; }\nvar o = {k: 1};\n[f.apply(o, [2, 3]), f.bind(o, 10)(20), f.call(o, 4, 5)]" "[6, 31, 10]");
      Testo.create "var hoisted, arguments, ==" (fun () ->
          check "a var in a block is the function's; used before its line" "function f() { if (true) { var x = 1; } return [x, y]; var y = 2; }\nf()" "[1, undefined]";
          check "a for's var: one for all its closures" "var fs = [];\nfor (var i = 0; i < 3; i++) fs.push(function () { return i; });\nfs.map(function (f) { return f(); })" "[3, 3, 3]";
          check "arguments" "function f() { return arguments.length + arguments[1]; }\nf(1, 10, 100)" "13";
          check "==: its conversions" "[1 == '1', null == undefined, null == 0, '' == 0, true == 1, [1] == 1, 'a' != 'a']"
            "[true, true, false, true, true, true, false]");
      Testo.create "the matcher's worked example" (fun () ->
          match Js_regexp.compile "a(b+)c" "" with
          | Ok re ->
              Alcotest.(check (option (list (option (pair int int))))) "[1, 6), group 1 [2, 5)" (Some [ Some (1, 6); Some (2, 5) ])
                (Option.map Array.to_list (Js_regexp.exec re "xabbbcx" 0))
          | Error e -> Alcotest.fail e);
      Testo.create "regular expressions" (fun () ->
          check "hn.js's ranks" "var a = '12. Show'.match(/[0-9]+/); [a[0], a.index]" "[\"12\", 0]";
          check "g: every match; i" "['a1b22c333'.match(/\\d+/g), /ABC/i.test('xabcx')]" "[[\"1\", \"22\", \"333\"], true]";
          check "replace: $1, a function" "['John Smith'.replace(/(\\w+) (\\w+)/, '$2, $1'), 'a-b-c'.replace(/-/g, function (m) { return '+' })]"
            "[\"Smith, John\", \"a+b+c\"]";
          check "split, exec, groups, alternation, classes" "var m = /(\\w+)@(x|y)\\.org$/.exec('mail: me@y.org');\n[m[1], m[2], 'a, b ,c'.split(/\\s*,\\s*/), /^[^0-9]+$/.test('abc')]"
            "[\"me\", \"y\", [\"a\", \"b\", \"c\"], true]";
          check "a / dividing is not one" "var x = 10, y = 2; [x / y, (x) / 5]" "[5, 2]";
          check "lazy, bounds, a word's edge" "['<a><b>'.match(/<.+?>/)[0], /^a{2,3}$/.test('aaaa'), 'cat category'.replace(/\\bcat\\b/g, 'dog')]"
            "[\"<a>\", false, \"dog category\"]");
      Testo.create "Date, URIs, splice" (fun () ->
          check "the host's clock: 2001-09-09T01:46:40Z" "var d = new Date(); [d.getFullYear(), d.getMonth(), d.getDate(), d.getHours(), Date.now()]"
            "[2001, 8, 9, 1, 1000000000000]";
          check "encodeURIComponent" "encodeURIComponent('a b&c/é')" "a%20b%26c%2F%C3%A9";
          check "splice" "var a = [1, 2, 3, 4]; var r = a.splice(1, 2, 'x'); [a, r]" "[[1, \"x\", 4], [2, 3]]");
    ]
