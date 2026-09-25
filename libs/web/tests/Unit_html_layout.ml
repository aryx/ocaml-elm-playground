(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_html_layout.mli *)

(* the tests' font: a character as wide as its look's size *)
let metrics (l : Looks.t) (s : string) : float = l.size *. float_of_int (String.length s)

let page ?(width = 200.) (html : string) : Html_layout.box =
  Html_layout.layout metrics ~root:(Looks.root ~size:10.) ~width (Html_tree.of_string html)

let near = Alcotest.float 1e-6

(* the fragments: text, x, baseline *)
let fragments (b : Html_layout.box) : (string * float * float) list =
  List.map (fun (f : Html_layout.fragment) -> (f.text, f.x, f.baseline)) (Html_layout.fragments b)

let fragment = Alcotest.(triple string near near)

(* the block boxes of an element name, in order *)
let rec blocks (name : string) (b : Html_layout.box) : Html_layout.box list =
  (match b.kind with Block e when e.name = name -> [ b ] | _ -> []) @ List.concat_map (blocks name) b.children

let tests =
  Testo.categorize "Html_layout"
    [
      Testo.create "the worked example" (fun () ->
          let p = page "<h1>Menu</h1><p>Soup of the day</p>" in
          Alcotest.check near "the page's height" 90. p.height;
          let body = List.hd (blocks "body" p) in
          Alcotest.(check (list near)) "body: x, y, width, height" [ 8.; 8.; 184.; 74. ] [ body.x; body.y; body.width; body.height ];
          Alcotest.check near "h1 at" 21.4 (List.hd (blocks "h1" p)).y;
          Alcotest.check near "p at: the margins collapsed" 58.8 (List.hd (blocks "p" p)).y;
          Alcotest.(check (list fragment))
            "the words"
            [ ("Menu", 8., 39.4); ("Soup", 8., 67.8); ("of", 58., 67.8); ("the", 88., 67.8); ("day", 128., 67.8) ]
            (fragments p));
      Testo.create "a line's baseline: the tallest look" (fun () ->
          (* big is 1.17 em: 11.7, 9.36 + 1.17 above the baseline *)
          let p = page "<p>a <big>B</big></p>" in
          let line = List.hd (List.hd (List.hd (blocks "p" p)).children).lines in
          Alcotest.check near "above the baseline" (9.36 +. 1.17) (line.baseline -. line.top);
          Alcotest.check near "the line's height" (11.7 *. 1.2) line.height;
          Alcotest.(check (list near))
            "one baseline" [ line.baseline; line.baseline ]
            (List.map (fun (f : Html_layout.fragment) -> f.baseline) line.fragments));
      Testo.create "spaces: between words, not around them" (fun () ->
          Alcotest.(check (list fragment))
            "collapsed, and none before a line"
            [ ("a", 8., 17.); ("b", 28., 17.) ]
            (fragments (page "  a   \n  b  "));
          Alcotest.(check (list fragment))
            "none between a word and a link's punctuation"
            [ ("see", 8., 17.); ("home", 48., 17.); (".", 88., 17.) ]
            (fragments (page "see <a href=x>home</a>.")));
      Testo.create "looks: inherited, and each element's" (fun () ->
          let looks =
            List.map (fun (f : Html_layout.fragment) -> (f.text, f.look.bold, f.look.italic, f.look.link))
              (Html_layout.fragments (page "<h2><i>a</i> <a href=u>b</a></h2>"))
          in
          Alcotest.(check (list (pair string (pair bool (pair bool (option string))))))
            "bold from h2, italic from i, a link"
            [ ("a", (true, (true, None))); ("b", (true, (false, Some "u"))) ]
            (List.map (fun (t, b, i, l) -> (t, (b, (i, l)))) looks));
      Testo.create "blocks: indents and rules" (fun () ->
          let p = page "<ul><li>x</ul><blockquote>y</blockquote><hr>" in
          Alcotest.(check (list near)) "a list indented 40" [ 48.; 144. ] (List.map (fun (b : Html_layout.box) -> b.x) (blocks "li" p) @ [ (List.hd (blocks "ul" p)).width ]);
          Alcotest.check near "a blockquote narrowed on both sides" 104. (List.hd (blocks "blockquote" p)).width;
          Alcotest.(check bool) "hr is a rule" true
            (List.exists (fun (b : Html_layout.box) -> match b.kind with Rule _ -> true | _ -> false) (List.hd (blocks "body" p)).children));
      Testo.create "lines: broken only where the page says" (fun () ->
          Alcotest.(check (list fragment))
            "br"
            [ ("a", 8., 17.); ("b", 8., 29.) ]
            (fragments (page "a<br>b"));
          Alcotest.(check (list fragment))
            "pre keeps its spaces and lines"
            [ ("  x  y", 8., 27.); (" z", 8., 39.) ]
            (fragments (page "<pre>\n  x  y\n z\n</pre>"));
          Alcotest.(check (list fragment))
            "a long paragraph stays one line (phase 4 breaks it)"
            [ ("aaaaaaaaaaaaaaaaaaaaaaaaa", 8., 28.2); ("b", 268., 28.2) ]
            (fragments (page "<p>aaaaaaaaaaaaaaaaaaaaaaaaa b")));
      Testo.create "centred" (fun () ->
          Alcotest.(check (list fragment))
            "center: the slack halved" [ ("ab", 90., 17.) ]
            (fragments (page "<center>ab</center>"));
          Alcotest.(check (list fragment))
            "align=right" [ ("ab", 172., 28.2) ]
            (fragments (page "<p align=right>ab")));
      Testo.create "not shown: the head, scripts" (fun () ->
          Alcotest.(check (list fragment)) "only y" [ ("y", 8., 17.) ] (fragments (page "<title>T</title><script>x</script>y")));
    ]
