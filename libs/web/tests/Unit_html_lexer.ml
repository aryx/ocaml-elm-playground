(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_html_lexer.mli *)

(* the tokens of [html], as the notes write them *)
let check (what : string) (html : string) (expected : string list) : unit =
  Alcotest.(check (list string)) what expected (List.map Html_lexer.to_string (Html_lexer.tokenize html))

let tests =
  Testo.categorize "Html_lexer"
    [
      Testo.create "the worked example" (fun () ->
          check "notes_browser.md section 3" "<p class=intro>Caf&eacute; <a href=\"menu.html\">menu</a>"
            [
              "Start_tag \"p\" [class = \"intro\"]";
              "Text \"Caf\xC3\xA9 \"";
              "Start_tag \"a\" [href = \"menu.html\"]";
              "Text \"menu\"";
              "End_tag \"a\"";
            ]);
      Testo.create "attributes: three quotes, none, twice" (fun () ->
          check "quoted three ways" "<td align=center valign='top' bgcolor=\"#ffffff\">"
            [ "Start_tag \"td\" [align = \"center\"; valign = \"top\"; bgcolor = \"#ffffff\"]" ];
          check "no value" "<hr noshade size=2>" [ "Start_tag \"hr\" [noshade = \"\"; size = \"2\"]" ];
          check "the first of two" "<a href=a href=b>" [ "Start_tag \"a\" [href = \"a\"]" ];
          check "a quote in the other quotes" "<img alt='say \"hi\"'>"
            [ "Start_tag \"img\" [alt = \"say \\\"hi\\\"\"]" ];
          check "no space after a quoted value" "<a href=\"x\"title=y>" [ "Start_tag \"a\" [href = \"x\"; title = \"y\"]" ];
          check "spaces around =" "<a href = \"x\" >" [ "Start_tag \"a\" [href = \"x\"]" ]);
      Testo.create "names are lowercased" (fun () ->
          check "tags and attributes" "<P ALIGN=Center>x</P>"
            [ "Start_tag \"p\" [align = \"Center\"]"; "Text \"x\""; "End_tag \"p\"" ]);
      Testo.create "a '<' that starts no tag is text" (fun () ->
          check "a < b" "a < b" [ "Text \"a < b\"" ];
          check "<3" "I <3 you" [ "Text \"I <3 you\"" ];
          check "at the end" "x<" [ "Text \"x<\"" ];
          check "</ at the end" "x</" [ "Text \"x</\"" ];
          check "</> is nothing" "a</>b" [ "Text \"ab\"" ]);
      Testo.create "entities, in text and in values" (fun () ->
          check "in a value" "<a href=\"?a=1&amp;b=2\">&lt;tag&gt;</a>"
            [ "Start_tag \"a\" [href = \"?a=1&b=2\"]"; "Text \"<tag>\""; "End_tag \"a\"" ];
          check "unquoted too" "<a title=caf&eacute;>" [ "Start_tag \"a\" [title = \"caf\xC3\xA9\"]" ]);
      Testo.create "comments and doctypes" (fun () ->
          check "a comment" "a<!-- b -->c" [ "Text \"a\""; "Comment \" b \""; "Text \"c\"" ];
          check "<!--> is empty" "<!-->x" [ "Comment \"\""; "Text \"x\"" ];
          check "<!---> too" "<!--->x" [ "Comment \"\""; "Text \"x\"" ];
          check "never closed" "<!-- a" [ "Comment \" a\"" ];
          check "a doctype" "<!DOCTYPE html>" [ "Doctype \"html\"" ];
          check "HTML 2.0's" "<!doctype HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">"
            [ "Doctype \"HTML PUBLIC \\\"-//IETF//DTD HTML 2.0//EN\\\"\"" ];
          check "<?xml ?> is a bogus comment" "<?xml version=\"1.0\"?>" [ "Comment \"?xml version=\\\"1.0\\\"?\"" ];
          check "</3 too" "</3>" [ "Comment \"3\"" ]);
      Testo.create "self-closing" (fun () ->
          check "<br/>" "<br/>" [ "Start_tag \"br\" [] /" ];
          check "<img src=a />" "<img src=a />" [ "Start_tag \"img\" [src = \"a\"] /" ];
          check "a lone / is nothing" "<a / href=x>" [ "Start_tag \"a\" [href = \"x\"]" ]);
      Testo.create "script and style: no tags inside" (fun () ->
          check "RAWTEXT" "<script>if (a<b) x(\"&amp;\")</script>"
            [ "Start_tag \"script\" []"; "Text \"if (a<b) x(\\\"&amp;\\\")\""; "End_tag \"script\"" ];
          check "its end tag's case ignored" "<style>p{}</STYLE>x"
            [ "Start_tag \"style\" []"; "Text \"p{}\""; "End_tag \"style\""; "Text \"x\"" ];
          check "not closed by </scripts" "<script>a</scripts>b</script>"
            [ "Start_tag \"script\" []"; "Text \"a</scripts>b\""; "End_tag \"script\"" ];
          check "never closed" "<script>a<b" [ "Start_tag \"script\" []"; "Text \"a<b\"" ]);
      Testo.create "title and textarea: entities, no tags" (fun () ->
          check "RCDATA" "<title>a<b> &amp; c</title>"
            [ "Start_tag \"title\" []"; "Text \"a<b> & c\""; "End_tag \"title\"" ]);
      Testo.create "a tag cut off by the end is dropped" (fun () ->
          check "in a value" "x<a href=\"y" [ "Text \"x\"" ];
          check "in a name" "x<a hr" [ "Text \"x\"" ]);
      Testo.create "line ends normalized" (fun () ->
          check "CR LF and CR" "a\r\nb\rc" [ "Text \"a\\nb\\nc\"" ]);
    ]
