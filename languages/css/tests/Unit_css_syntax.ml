(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_css_syntax.mli *)
open Css_syntax

let token (t : token) : string =
  match t with
  | Ident s -> "Ident " ^ s
  | Function s -> "Function " ^ s
  | At_keyword s -> "@" ^ s
  | Hash s -> "Hash " ^ s
  | String s -> Printf.sprintf "String %S" s
  | Url s -> "Url " ^ s
  | Delim c -> Printf.sprintf "Delim %c" c
  | Number f -> Printf.sprintf "Number %g" f
  | Percentage f -> Printf.sprintf "Percentage %g" f
  | Dimension (f, u) -> Printf.sprintf "Dimension %g %s" f u
  | Whitespace -> "_"
  | Colon -> ":"
  | Semicolon -> ";"
  | Comma -> ","

(* each style rule's selectors' text and its declarations *)
let rules (s : string) : string list =
  List.map
    (fun r ->
      match r with
      | Style_rule { prelude; declarations } ->
          to_string prelude ^ " { "
          ^ String.concat "; " (List.map (fun d -> d.name ^ ": " ^ to_string d.value ^ if d.important then " !" else "") declarations)
          ^ " }"
      | At_rule { name; prelude; block } -> "@" ^ name ^ " " ^ to_string prelude ^ (match block with Some _ -> " {...}" | None -> ";"))
    (parse_stylesheet s)

let check what s expected = Alcotest.(check (list string)) what expected (rules s)

let tests =
  Testo.categorize "Css_syntax"
    [
      Testo.create "the worked example: tokens" (fun () ->
          Alcotest.(check (list string))
            "a:hover > .x { color: #f00 !important }"
            [ "Ident a"; ":"; "Ident hover"; "_"; "Delim >"; "_"; "Delim ."; "Ident x"; "_"; "Delim {"; "_"; "Ident color"; ":"; "_";
              "Hash f00"; "_"; "Delim !"; "Ident important"; "_"; "Delim }" ]
            (List.map token (tokenize "a:hover > .x { color: #f00 !important }")));
      Testo.create "numbers, their kinds" (fun () ->
          Alcotest.(check (list string)) "2 50% 1.5em -.5 1e3px"
            [ "Number 2"; "_"; "Percentage 50"; "_"; "Dimension 1.5 em"; "_"; "Number -0.5"; "_"; "Dimension 1000 px" ]
            (List.map token (tokenize "2 50% 1.5em -.5 1e3px")));
      Testo.create "what a split on braces cuts wrong" (fun () ->
          check "a } in a string" "a { content: \"}\"; color: red }" [ "a { content: \"}\"; color: red }" ];
          check "a ; in a url()" "b { background: url(x.png?a=1;b=2) }" [ "b { background: url(x.png?a=1;b=2) }" ];
          check "a comment" "/* } */ i { margin: 0 auto }" [ "i { margin: 0 auto }" ]);
      Testo.create "errors skipped, not fatal" (fun () ->
          check "the notes' example" "p { color: red; width: ; margin: 0 }" [ "p { color: red; margin: 0 }" ];
          check "!important" "p { color: red ! important }" [ "p { color: red ! }" ];
          check "a rule without its block: dropped" "p { color: red } q" [ "p { color: red }" ]);
      Testo.create "at-rules" (fun () ->
          check "@media and @import" "@import url(x.css); @media (max-width: 800px) { p { color: blue } } @supports (display: grid) { }"
            [ "@import url(x.css);"; "@media (max-width: 800px) {...}"; "@supports (display: grid) {...}" ];
          match parse_stylesheet "@media screen { p { color: blue } h1 { margin: 0 } }" with
          | [ At_rule { block = Some b; _ } ] ->
              Alcotest.(check int) "@media's block read as rules" 2 (List.length (rules_of_block b))
          | _ -> Alcotest.fail "no @media");
      Testo.create "values written back" (fun () ->
          let value s = match parse_declarations s with [ d ] -> to_string d.value | _ -> "?" in
          Alcotest.(check (list string)) "functions, commas, calc"
            [ "rgb(1, 2, 3)"; "calc(100% - 2em)"; "var(--gap, 8px)"; "\"a b\"" ]
            (List.map value [ "color: rgb(1, 2, 3)"; "width: calc(100% - 2em)"; "margin: var(--gap, 8px)"; "font-family: 'a b'" ]));
    ]
