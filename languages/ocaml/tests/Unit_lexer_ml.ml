(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_lexer_ml.mli *)

(* the tokens, as "Kind text" separated by two spaces *)
let show (src : string) : string =
  Lexer_ml.tokens src
  |> List.map (fun (t : Token_ml.t) -> Token_ml.show_kind t.kind ^ " " ^ t.text)
  |> String.concat "  "

let check (what : string) (src : string) (expected : string) : unit = Alcotest.(check string) what expected (show src)

let tests =
  Testo.categorize "Lexer_ml"
    [
      Testo.create "the worked example" (fun () ->
          check "Lexer_ml.mli's" {|let f ~x = (* hi *) M.g x "s" 'c' 'a|}
            {|Keyword let  Lident f  Label ~x  Operator =  Comment (* hi *)  Uident M  Operator .  Lident g  Lident x  String "s"  Char 'c'  Type_var 'a|});
      Testo.create "comments" (fun () ->
          check "nested" "(* a (* b *) c *) x" "Comment (* a (* b *) c *)  Lident x";
          check "a string with *) inside" {|(* "*)" *) x|} {|Comment (* "*)" *)  Lident x|};
          check "a quote character" {|(* '"' *) x|} {|Comment (* '"' *)  Lident x|};
          check "unclosed: to the end" "x (* y" "Lident x  Comment (* y");
      Testo.create "strings" (fun () ->
          check "escapes" {|"a\"b" c|} {|String "a\"b"  Lident c|};
          check "quoted" "{|a \"|} b" "String {|a \"|}  Lident b";
          check "quoted with an id" "{id|x |} y|id} z" "String {id|x |} y|id}  Lident z";
          check "unclosed" {|x "abc|} {|Lident x  String "abc|});
      Testo.create "numbers, operators, keywords" (fun () ->
          check "numbers" "1_000 0xFF 3L 1. 1e-3 2.5" "Int 1_000  Int 0xFF  Int 3L  Float 1.  Float 1e-3  Float 2.5";
          check "longest match" "x :: y |> z := !w" "Lident x  Operator ::  Lident y  Operator |>  Lident z  Operator :=  Operator !  Lident w";
          check "binding operators" "let* x = y and+ z" "Keyword let*  Lident x  Operator =  Lident y  Keyword and+  Lident z";
          check "attributes" "[@warning \"-8\"]" {|Punctuation [@  Lident warning  String "-8"  Punctuation ]|};
          check "f' is a name" "f' x" "Lident f'  Lident x");
      Testo.create "places" (fun () ->
          let toks = Lexer_ml.tokens "let x =\n  (* c\n *) 42" in
          Alcotest.(check (list (pair int int))) "line, column" [ (1, 0); (1, 4); (1, 6); (2, 2); (3, 4) ]
            (List.map (fun (t : Token_ml.t) -> (t.line, t.col)) toks));
    ]
