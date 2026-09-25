(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_js_eval.mli *)

(* the program's last value as the console shows it, or its error *)
let run ?(budget = 10_000_000) (s : string) : string =
  let t = Js_eval.create () in
  Js_eval.set_budget t budget;
  match Js_eval.eval t s with
  | Ok v -> Js_value.display v
  | Error e -> Printf.sprintf "line %d: %s" e.line e.message

let check (what : string) (s : string) (expected : string) : unit = Alcotest.(check string) what expected (run s)

(* an expression's value inside [ ]: strings shown quoted *)
let value (s : string) (expected : string) : unit = check s ("[" ^ s ^ "]") ("[" ^ expected ^ "]")

let tests =
  Testo.categorize "Js_eval"
    [
      Testo.create "closures: the notes' counter" (fun () ->
          check "each counter its own n"
            "function counter() {\n  let n = 0;\n  return () => { n = n + 1; return n; };\n}\nconst c = counter();\nconst d = counter();\n[c(), c(), d()]"
            "[1, 2, 1]");
      Testo.create "a let per iteration" (fun () ->
          check "for (let i ...): each function its own i" "const fs = [];\nfor (let i = 0; i < 3; i++) fs.push(() => i);\nfs.map(f => f())"
            "[0, 1, 2]");
      Testo.create "the coercions: Wat" (fun () ->
          value "1 + 2" "3";
          value "\"1\" + 2" "\"12\"";
          value "1 + \"2\"" "\"12\"";
          value "\"3\" * \"4\"" "12";
          value "true + 1" "2";
          value "[] + []" "\"\"";
          value "[] + {}" "\"[object Object]\"";
          value "[1, 2] + [3]" "\"1,23\"";
          value "\"b\" + \"a\" + +\"a\" + \"a\"" "\"baNaNa\"";
          value "0.1 + 0.2" "0.30000000000000004";
          value "typeof null" "\"object\"";
          value "\"10\" < \"9\"" "true";
          value "10 < 9" "false";
          value "0 || \"x\"" "\"x\"";
          value "1 === 1.0" "true");
      Testo.create "this: a method's object, an arrow's outer this" (fun () ->
          check "o.get()" "const o = {n: 1, get: function () { return this.n }};\no.get()" "1";
          check "an arrow in a method" "const o = {n: 2, f: function () { return [1].map(x => this.n) }};\no.f()" "[2]";
          check "a plain call: undefined" "function f() { return typeof this }\nf()" "undefined");
      Testo.create "errors, named and on their line" (fun () ->
          check "not defined" "let a = 1\nb + 1" "line 2: ReferenceError: b is not defined";
          check "not a function" "let f = 3\nf()" "line 2: TypeError: f is not a function";
          check "a method not a function" "const o = {}\no.m()" "line 2: TypeError: o.m is not a function";
          check "reading undefined's" "let o\n\no.y" "line 3: TypeError: Cannot read properties of undefined (reading 'y')";
          check "a const" "const k = 1\nk = 2" "line 2: TypeError: Assignment to constant variable.";
          check "inside a function: its own line" "function f() {\n  return x\n}\nf()" "line 2: ReferenceError: x is not defined";
          check "a syntax error" "let = 1" "line 1: SyntaxError: expected a name, not '='");
      Testo.create "throw and try" (fun () ->
          check "caught" "let m;\ntry { null.x } catch (e) { m = e.name + \": \" + e.message }\nm"
            "TypeError: Cannot read properties of null (reading 'x')";
          check "thrown and not caught" "throw 3" "line 1: Uncaught 3";
          check "an object thrown" "throw {code: 7}" "line 1: Uncaught {code: 7}");
      Testo.create "the limits: recursion, and a loop that never ends" (fun () ->
          check "fib" "function fib(n) { return n < 2 ? n : fib(n - 1) + fib(n - 2) }\nfib(15)" "610";
          check "recursion without end" "function f() { return f() }\nf()" "line 1: RangeError: Maximum call stack size exceeded";
          Alcotest.(check string) "while (true)" "line 1: RangeError: the script ran too long (a loop that never ends?)"
            (run ~budget:10_000 "while (true) {}"));
      Testo.create "functions declared below their call" (fun () -> check "hoisted" "f()\nfunction f() { return 1 }" "1");
      Testo.create "arrays: holes, length" (fun () ->
          check "a[2] = 5" "const a = [];\na[2] = 5;\n[a.length, a[0], a[2]]" "[3, undefined, 5]";
          check "length cut" "const a = [1, 2, 3];\na.length = 1;\na" "[1]");
      Testo.create "the built-ins" (fun () ->
          value "Math.max(1, 3, 2)" "3";
          value "parseInt(\"42px\")" "42";
          value "\"a,b\".split(\",\")" "[\"a\", \"b\"]";
          value "[3, 1, 2].sort()" "[1, 2, 3]";
          value "[10, 9, 1].sort()" "[1, 10, 9]";
          value "[10, 9, 1].sort((a, b) => a - b)" "[1, 9, 10]";
          value "[1, 2, 3].reduce((a, b) => a + b)" "6";
          value "[1, 2, 3, 4].filter(x => x % 2 === 0)" "[2, 4]";
          value "JSON.stringify({a: [1, \"x\"], b: undefined})" "\"{\\\"a\\\":[1,\\\"x\\\"]}\"";
          value "\"abc\".slice(-2)" "\"bc\"";
          value "\"abc\".toUpperCase().length" "3";
          value "Object.keys({b: 1, a: 2})" "[\"b\", \"a\"]";
          let lines = ref [] in
          let t = Js_eval.create ~log:(fun l -> lines := l :: !lines) () in
          ignore (Js_eval.eval t "console.log(\"a\", 1, [1, \"b\"], {x: null})");
          Alcotest.(check (list string)) "console.log" [ "a 1 [1, \"b\"] {x: null}" ] !lines;
          let random () = Js_eval.eval (Js_eval.create ~seed:7 ()) "Math.random()" |> Result.map Js_value.display in
          Alcotest.(check bool) "Math.random: the same seed, the same number" true (random () = random ()));
    ]
