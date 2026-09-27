(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_html_tree.mli *)

(* the body of [html]'s tree, blank text left out, as indented lines
 * (from the body's children, one level less) *)
let body (html : string) : string list =
  let root = Dom.without_blank_text (Html_tree.of_string html) in
  match Dom.find_all "body" root with
  | [ b ] ->
      List.concat_map
        (fun (n : Dom.node) ->
          match n with
          | Element e -> Dom.to_lines e
          | Text s -> [ "\"" ^ String.concat "\\n" (String.split_on_char '\n' s) ^ "\"" ])
        b.children
  | _ -> Alcotest.fail "no body"

let check (what : string) (html : string) (expected : string list) : unit =
  Alcotest.(check (list string)) what expected (body html)

let tests =
  Testo.categorize "Html_tree"
    [
      Testo.create "the worked example: four repairs" (fun () ->
          let html = "<title>Lunch</title>\n<h1>Menu</h1>\n<p>Soup of the day\n<p>Salads:\n<ul>\n<li>tomato\n<li><b>cucumber</b>\n</ul>\n" in
          Alcotest.(check (list string))
            "notes_browser.md section 4"
            [
              "html";
              "  head";
              "    title";
              "      \"Lunch\"";
              "  body";
              "    h1";
              "      \"Menu\"";
              "    p";
              "      \"Soup of the day\\n\"";
              "    p";
              "      \"Salads:\\n\"";
              "    ul";
              "      li";
              "        \"tomato\\n\"";
              "      li";
              "        b";
              "          \"cucumber\"";
            ]
            (Dom.to_lines (Dom.without_blank_text (Html_tree.of_string html))));
      Testo.create "html, head and body implied" (fun () ->
          Alcotest.(check (list string))
            "a text alone" [ "html"; "  head"; "  body"; "    \"hello\"" ]
            (Dom.to_lines (Html_tree.of_string "hello"));
          Alcotest.(check (list string))
            "the doctype and comments dropped" [ "html"; "  head"; "  body"; "    \"x\"" ]
            (Dom.to_lines (Html_tree.of_string "<!DOCTYPE html><!-- c -->x")));
      Testo.create "end tags left out" (fun () ->
          check "p by p" "<p>a<p>b" [ "p"; "  \"a\""; "p"; "  \"b\"" ];
          check "p by a block" "<p>a<ul><li>b</ul>" [ "p"; "  \"a\""; "ul"; "  li"; "    \"b\"" ];
          check "dt and dd" "<dl><dt>a<dd>b<dt>c</dl>" [ "dl"; "  dt"; "    \"a\""; "  dd"; "    \"b\""; "  dt"; "    \"c\"" ];
          check "a heading by a heading" "<h1>a<h2>b" [ "h1"; "  \"a\""; "h2"; "  \"b\"" ]);
      Testo.create "the search stops: nested lists, a blockquote, a cell" (fun () ->
          check "an inner li does not end the outer one" "<ul><li>a<ul><li>b</ul><li>c</ul>"
            [ "ul"; "  li"; "    \"a\""; "    ul"; "      li"; "        \"b\""; "  li"; "    \"c\"" ];
          check "nor across a blockquote" "<li>a<blockquote><li>b"
            [ "li"; "  \"a\""; "  blockquote"; "    li"; "      \"b\"" ];
          check "a cell" "<table><tr><td><p>a</td><td>b</table>"
            [ "table {Netscape}"; "  tr {Netscape}"; "    td {Netscape}"; "      p"; "        \"a\""; "    td {Netscape}"; "      \"b\"" ]);
      Testo.create "stray and misnested end tags" (fun () ->
          check "a stray </b> ignored" "<p>a</b>b" [ "p"; "  \"ab\"" ];
          check "</p> alone makes an empty p" "a</p>b" [ "\"a\""; "p"; "\"b\"" ];
          check "misnested: ours, not the adoption agency's" "<b><i>x</b>y</i>" [ "b"; "  i"; "    \"x\""; "\"y\"" ]);
      Testo.create "void elements" (fun () ->
          check "never pushed" "<p>a<br>b<img src=x>c" [ "p"; "  \"a\""; "  br"; "  \"b\""; "  img src=\"x\""; "  \"c\"" ]);
      Testo.create "Netscape's extensions, marked in the tree" (fun () ->
          check "an element, and a core one's attributes" "<center><hr size=4 noshade>x</center>"
            [ "center {Netscape}"; "  hr {Netscape: size=\"4\" noshade=\"\"}"; "  \"x\"" ];
          check "the same repairs: font is inline" "<p>a<font color=red>b<p>c"
            [ "p"; "  \"a\""; "  font color=\"red\" {Netscape}"; "    \"b\""; "p"; "  \"c\"" ]);
      Testo.create "the head and the body" (fun () ->
          check "<body>'s attributes" "<body bgcolor=white><p>x" [ "p"; "  \"x\"" ];
          let body = List.hd (Dom.find_all "body" (Html_tree.of_string "<body bgcolor=white>")) in
          Alcotest.(check (option string)) "bgcolor, Netscape's" (Some "white") (Dom.attribute ~extensions:true "bgcolor" body);
          Alcotest.(check (option string)) "not a core attribute" None (Dom.attribute "bgcolor" body);
          check "after </body>, still the body" "<body>a</body>b" [ "\"ab\"" ];
          check "a title after the body started stays there" "<p>x<title>T</title>"
            [ "p"; "  \"x\""; "  title"; "    \"T\"" ];
          Alcotest.(check string)
            "text in the title is the title" "T"
            (Dom.text_content (List.hd (Dom.find_all "title" (Html_tree.of_string "<title>T</title><p>x")))));
      Testo.create "a newline after <pre> dropped" (fun () ->
          check "one" "<pre>\nx\n</pre>" [ "pre"; "  \"x\\n\"" ]);
    ]
