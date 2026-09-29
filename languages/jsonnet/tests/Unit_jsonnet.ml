(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_jsonnet.mli *)

let rec show (v : Json.t) : string =
  match v with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> Printf.sprintf "%g" n
  | String s -> Printf.sprintf "%S" s
  | Array vs -> "[" ^ String.concat ", " (List.map show vs) ^ "]"
  | Object fs -> "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ ": " ^ show v) fs) ^ "}"

let run ?(files = []) text = match Jsonnet.eval ~read:(fun p -> List.assoc_opt p files) ~path:"main.jsonnet" text with Ok v -> show v | Error e -> "error: " ^ e
let check ?files what text expected = Alcotest.(check string) what expected (run ?files text)

let tokens text = String.concat " " (List.map (fun (t : Jsonnet_lexer.token) -> Jsonnet_lexer.to_string t.kind) (Jsonnet_lexer.tokenize text))

let tests =
  Testo.categorize "Jsonnet"
    [
      Testo.create "the lexer's worked example" (fun () ->
          Alcotest.(check string) "Jsonnet_lexer.mli's" {|local x = "a" ; { [ x ] +:: "it's" } the end|} (tokens "local x = 'a'; { [x]+:: @'it''s' }");
          Alcotest.(check string) "operators less their last - and !" {|a * - b { a : - 1 } x == ! y the end|} (tokens "a*-b {a:-1} x==!y");
          Alcotest.(check string) "comments, three kinds" "1 + 2 the end" (tokens "1 # one\n+ /* plus */ 2 // two"));
      Testo.create "text blocks" (fun () ->
          check "the indentation off, the last newline kept" "local s = |||\n  a\n    b\n|||; s" {|"a\n  b\n"|};
          check "|||- chomps it" "|||-\n  x\n|||" {|"x"|});
      Testo.create "JSON is jsonnet" (fun () ->
          check "the fields sorted, as jsonnet prints them" {|{ "b": [1, 2.5, "x", null], "a": { "c": true } }|} {|{a: {c: true}, b: [1, 2.5, "x", null]}|});
      Testo.create "Jsonnet.mli's worked example: late binding, super, hidden" (fun () ->
          check "y seen from the whole object" "local base = { x: 1, y: self.x + 1, name:: 'base' };\nbase + { x: 10, z: super.y }" "{x: 10, y: 11, z: 11}");
      Testo.create "fields: +:, :::, computed, null names, methods" (fun () ->
          check "+: adds to the field below" "{ a: [1], b: { x: 1 } } + { a+: [2], b+: { y: 2 } }" "{a: [1, 2], b: {x: 1, y: 2}}";
          check "::: shows what was hidden" "{ a:: 1, b: 2 } + { a::: 3 }" "{a: 3, b: 2}";
          check "an inherited visibility" "{ a:: 1 } + { a: 2 }" "{}";
          check "computed names, null leaves out" "local k = 'key'; { [k]: 1, [null]: 2, ['x' + 'y']: 3 }" "{key: 1, xy: 3}";
          check "a method, e { } as +" "local o = { f(x):: x * 2, v: self.f(21) }; o { w: 1 }" "{v: 42, w: 1}";
          check "object locals see self" "{ local twice = self.a * 2, a: 3, b: twice }" "{a: 3, b: 6}";
          check "$ the outermost" "{ a: 1, b: { c: $.a + 1 } }" "{a: 1, b: {c: 2}}";
          check "in, and in super" "local o = { a: 1 } + { b: 'a' in super, c: 'z' in super }; [o.b, o.c, 'a' in o]" "[true, false, true]");
      Testo.create "locals, functions, conditionals" (fun () ->
          check "a local function, recursion" "local fact(n) = if n <= 1 then 1 else n * fact(n - 1); fact(10)" "3.6288e+06";
          check "defaults, named arguments" "local f(a, b = a + 1, c = 10) = [a, b, c]; [f(1), f(1, c = 3), f(b = 5, a = 0)]" "[[1, 2, 10], [1, 2, 3], [0, 5, 10]]";
          check "closures" "local adder(n) = function(x) x + n; local add2 = adder(2); add2(40)" "42";
          check "if without else" "[if false then 1]" "[null]";
          check "lazy: an error never asked for" "local x = error 'boom'; { a: 1, b:: x }" "{a: 1}");
      Testo.create "arrays, strings, comprehensions" (fun () ->
          check "a comprehension, for and if" "[x * y for x in [1, 2, 3] for y in [10, 100] if x != 2]" "[10, 100, 30, 300]";
          check "an object comprehension" "{ [k]: std.length(k) for k in ['a', 'bb'] }" "{a: 1, bb: 2}";
          check "slices" "[[1, 2, 3, 4, 5][1:3], 'hello'[1:], [0, 1, 2, 3, 4, 5, 6][::3]]" {|[[2, 3], "ello", [0, 3, 6]]|};
          check "+ between a string and anything" "['n = ' + 3, 'o: ' + { a: [1] }, 1 + 2.5]" {|["n = 3", "o: {\"a\": [1]}", 3.5]|};
          check "== on arrays and objects" "[[1, [2]] == [1, [2]], { a: 1, b:: 2 } == { a: 1 }, 'a' < 'b', [1, 2] < [1, 3]]" "[true, true, true, true]");
      Testo.create "std" (fun () ->
          check "map, filter, fold, join, range" "local sq(x) = x * x; [std.map(sq, std.range(1, 4)), std.filter(function(x) x % 2 == 0, std.range(1, 6)), std.foldl(function(a, b) a + b, [1, 2, 3], 0), std.join(', ', ['a', 'b'])]"
            {|[[1, 4, 9, 16], [2, 4, 6], 6, "a, b"]|};
          check "format and %" "[std.format('%s is %d, %05.2f', ['pi', 3, 3.14159]), '%(name)s: %(v)-4s|' % { name: 'x', v: 'ab' } , '%x' % 255]" {|["pi is 3, 03.14", "x: ab  |", "ff"]|};
          check "objects" "local o = { b: 1, a:: 2 }; [std.objectFields(o), std.objectFieldsAll(o), std.objectHas(o, 'a'), std.objectHasAll(o, 'a'), std.get(o, 'z', 'none')]"
            {|[["b"], ["a", "b"], false, true, "none"]|};
          check "strings" "[std.split('a/b/c', '/'), std.strReplace('aXbX', 'X', '-'), std.startsWith('codemap', 'code'), std.asciiUpper('ix'), std.substr('hello', 1, 3)]"
            {|[["a", "b", "c"], "a-b-", true, "IX", "ell"]|};
          check "sort, uniq, member, type" "[std.sort([3, 1, 2]), std.uniq([1, 1, 2, 1]), std.member([1, 2], 2), std.type({}), std.length({ a: 1, b:: 2 })]"
            {|[[1, 2, 3], [1, 2, 1], true, "object", 1]|};
          check "mergePatch" "std.mergePatch({ a: 1, b: { c: 2, d: 3 } }, { a: null, b: { c: 4 } })" "{b: {c: 4, d: 3}}");
      Testo.create "imports, relative to the importing file" (fun () ->
          let files = [ ("lib/colors.libsonnet", "{ red: '#e00000', dark(c):: c + '80' }"); ("lib/layers.libsonnet", "local c = import 'colors.libsonnet'; { caps: { color: c.dark(c.red) } }"); ("notes.txt", "hello\n") ] in
          check ~files "import, an import's import, importstr" "local l = import 'lib/layers.libsonnet'; { layer: l.caps, text: importstr 'notes.txt' }"
            {|{layer: {color: "#e0000080"}, text: "hello\n"}|};
          check ~files "a file that is not there" "import 'nope.jsonnet'" "error: main.jsonnet:1: cannot import nope.jsonnet");
      Testo.create "the plan's config" (fun () ->
          let files = [ (".codemap/layers.libsonnet", "{ capabilities: { name: 'capabilities', rules: [{ pattern: 'Cap.$X', 'color-by': '$X' }] } }") ] in
          check ~files "plan_codemap_v2.md's root .codemapconfig"
            "local layers = import '.codemap/layers.libsonnet';  // shared\n{\n  title: 'Pictures and games',\n  colors: { games: '#e08030' },\n  dirs: { games: { summary: 'The games.' } },\n  layers: [layers.capabilities],\n}"
            {|{colors: {games: "#e08030"}, dirs: {games: {summary: "The games."}}, layers: [{name: "capabilities", rules: [{color-by: "$X", pattern: "Cap.$X"}]}], title: "Pictures and games"}|});
      Testo.create "errors" (fun () ->
          check "a parse error, its line" "{\n  a: 1\n  b: 2\n}" "error: main.jsonnet:3: expected ,, not b";
          check "a field that does not exist" "local o = { a: 1 };\no.b" "error: main.jsonnet:2: field does not exist: b";
          check "error's message" "{ a: error 'no ' + 'way' }" "error: main.jsonnet:1: no way";
          check "an assert" "{ assert self.a > 1 : 'a too small', a: 1 }" "error: main.jsonnet:1: object assertion failed: a too small";
          check "a recursion that never ends" "local f(x) = f(x + 1); f(0)" "error: main.jsonnet:1: too deep: a recursion that does not end?";
          check "types" "1 + 'a' + true - 1" "error: main.jsonnet:1: - between a string and a number");
    ]
