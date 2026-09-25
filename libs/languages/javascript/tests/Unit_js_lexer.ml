(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_js_lexer.mli *)

(* the tokens as the notes write them, a newline before one as "|" *)
let tokens (s : string) : string list =
  Js_lexer.tokenize s
  |> List.map (fun (t : Js_lexer.token) -> (if t.newline_before then "| " else "") ^ Js_lexer.to_string t.kind)

let check (what : string) (s : string) (expected : string list) : unit =
  Alcotest.(check (list string)) what (expected @ [ "Eof" ]) (tokens s)

let error (s : string) : int * string =
  match Js_lexer.tokenize s with _ -> Alcotest.fail "no error" | exception Js_lexer.Error (line, msg) -> (line, msg)

let tests =
  Testo.categorize "Js_lexer"
    [
      Testo.create "the worked example" (fun () ->
          check "two lines" "let s = \"a\" + 'b'; // two strings\nx=>x===1"
            [ "Keyword let"; "Name s"; "Punct ="; "String \"a\""; "Punct +"; "String \"b\""; "Punct ;"; "| Name x"; "Punct =>";
              "Name x"; "Punct ==="; "Number 1" ]);
      Testo.create "the longest match" (fun () ->
          check "=== !== => ++ +=" "a===b!==c=>d++ +=e"
            [ "Name a"; "Punct ==="; "Name b"; "Punct !=="; "Name c"; "Punct =>"; "Name d"; "Punct ++"; "Punct +="; "Name e" ]);
      Testo.create "numbers" (fun () ->
          check "integer, fraction, exponent, hexadecimal, .5" "7 0.25 1e3 0xff .5"
            [ "Number 7"; "Number 0.25"; "Number 1000"; "Number 255"; "Number 0.5" ]);
      Testo.create "strings: escapes decoded" (fun () ->
          check "\\n, \\', \\u00e9" "'a\\nb' \"it\\'s\" \"caf\\u00e9\""
            [ "String \"a\\nb\""; "String \"it's\""; "String \"caf\\195\\169\"" ]);
      Testo.create "comments dropped, their newlines kept" (fun () ->
          check "a /* two\nlines */ b // end\nc" "a /* two\nlines */ b // end\nc" [ "Name a"; "| Name b"; "| Name c" ]);
      Testo.create "keywords, and of, which is not one" (fun () ->
          check "let of" "let of = typeof x" [ "Keyword let"; "Name of"; "Punct ="; "Keyword typeof"; "Name x" ]);
      Testo.create "mistakes, on their line" (fun () ->
          Alcotest.(check (pair int string)) "a string not closed" (2, "a string never closed on its line") (error "x\n'abc\ny'");
          Alcotest.(check int) "a comment not closed: its first line" 1 (fst (error "/* a\nb"));
          Alcotest.(check int) "a template literal" 1 (fst (error "`a`"));
          Alcotest.(check int) "#" 3 (fst (error "a\nb\n#")));
    ]
