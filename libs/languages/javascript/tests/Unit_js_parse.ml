(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_js_parse.mli *)

let expr (s : string) : string =
  match Js_parse.parse_expression s with
  | Ok e -> Js_ast.expr_to_string e
  | Error e -> Printf.sprintf "error on line %d: %s" e.line e.message

let program (s : string) : string list =
  match Js_parse.parse s with
  | Ok stmts -> List.map Js_ast.stmt_to_string stmts
  | Error e -> [ Printf.sprintf "error on line %d: %s" e.line e.message ]

let check_expr (s : string) (expected : string) : unit = Alcotest.(check string) s expected (expr s)
let check_program (what : string) (s : string) (expected : string list) : unit = Alcotest.(check (list string)) what expected (program s)

let error_line (s : string) : int = match Js_parse.parse s with Ok _ -> 0 | Error e -> e.line

let tests =
  Testo.categorize "Js_parse"
    [
      Testo.create "the worked example: expressions" (fun () ->
          check_expr "1 + 2 * 3 - 4" "((1 + (2 * 3)) - 4)";
          check_expr "a = b = c || d && e" "(a = (b = (c || (d && e))))";
          check_expr "-x.y(1)[0]" "(-(((x.y)(1))[0]))";
          check_expr "f(x => x * 2, 3)" "(f((x) => (x * 2), 3))");
      Testo.create "associativity: the + 1" (fun () ->
          check_expr "1 - 2 - 3" "((1 - 2) - 3)";
          check_expr "a ? b : c ? d : e" "(a ? b : (c ? d : e))";
          check_expr "x += y *= 2" "(x += (y *= 2))");
      Testo.create "prefix and postfix" (fun () ->
          check_expr "!a && -b" "((!a) && (-b))";
          check_expr "typeof x === \"number\"" "((typeof x) === \"number\")";
          check_expr "i++ + ++j" "((i++) + (++j))";
          check_expr "-a * b" "((-a) * b)");
      Testo.create "literals: arrays, objects, functions" (fun () ->
          check_expr "[1, [2], {a: 1, 'b c': [x]}]" "[1, [2], {a: 1, b c: [x]}]";
          check_expr "(a, b) => { return a + b }" "(a, b) => (a + b)";
          check_expr "(a, b) => { let s = a + b; return s }" "Arrow [a; b] [Let s (a + b); Return s]";
          check_expr "function f(n) { return n }" "Function f [n] [Return n]";
          check_expr "o.m(1).length" "(((o.m)(1)).length)");
      Testo.create "the worked example: statements, and semicolons" (fun () ->
          check_program "the notes' section 3"
            "let a = 1\nlet b = a + 1; if (b > a) { b = 0 } else b = 1\nfunction f() {\n  return\n  a\n}"
            [ "Let a 1"; "Let b (a + 1)"; "If ((b > a), Block [Expr (b = 0)], Expr (b = 1))"; "Function f [] [Return; Expr a]" ]);
      Testo.create "the statements" (fun () ->
          check_program "loops"
            "for (let i = 0; i < 3; i++) s += i\nfor (const x of xs) { if (x) break; else continue }\nwhile (n > 0) n--"
            [ "For (Let i 0, (i < 3), (i++), Expr (s += i))";
              "For_of (Const x, xs, Block [If (x, Break, Continue)])";
              "While ((n > 0), Expr (n--))" ];
          check_program "try, throw, a block, an empty statement"
            "try { throw \"no\" } catch (e) { console.log(e) } ; { }"
            [ "Try [Throw \"no\"] catch e [Expr ((console.log)(e))]"; "Empty"; "Block []" ];
          check_program "a newline before ++ ends the statement" "x\n++y" [ "Expr x"; "Expr (++y)" ]);
      Testo.create "mistakes, on their line" (fun () ->
          check_program "a missing )" "let a = f(1,\n2;" [ "error on line 2: expected ')', not ';'" ];
          check_program "two expressions on one line" "a b" [ "error on line 1: expected ';' or a new line, not 'b'" ];
          check_program "not a target" "1 = 2" [ "error on line 1: that cannot be assigned to" ];
          Alcotest.(check int) "class: left out, said so" 3 (error_line "a\nb\nclass C {}");
          Alcotest.(check int) "a lexer's mistake too" 2 (error_line "a\n'b"));
    ]
