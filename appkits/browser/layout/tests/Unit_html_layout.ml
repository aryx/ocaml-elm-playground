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

(* laid out by Mosaic (HTML 2.0), or by Netscape ([extensions]) *)
let page ?(extensions = false) ?(width = 200.) (html : string) : Html_layout.box =
  Html_layout.layout metrics ~root:(Looks.root ~extensions ~size:10. ()) ~width (Html_tree.of_string html)

let netscape = page ~extensions:true

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
            "pre's lines are never broken"
            [ ("aaaaaaaaaaaaaaaaaaaaaaaaa b", 8., 27.) ]
            (fragments (page "<pre>aaaaaaaaaaaaaaaaaaaaaaaaa b</pre>")));
      Testo.create "lines broken at the width: the worked example" (fun () ->
          Alcotest.(check (list fragment))
            "190 fits in 192, 260 does not"
            [ ("Soup", 8., 28.2); ("of", 58., 28.2); ("the", 88., 28.2); ("day", 128., 28.2); ("and", 168., 28.2); ("salads", 8., 40.2) ]
            (fragments (page ~width:208. "<p>Soup of the day and salads</p>"));
          Alcotest.(check (list fragment))
            "a word wider than a line: a line of its own"
            [ ("aaaaaaaaaaaaaaaaaaaaaaaaa", 8., 28.2); ("b", 8., 40.2) ]
            (fragments (page "<p>aaaaaaaaaaaaaaaaaaaaaaaaa b")));
      Testo.create "a unit is not broken: a link and its full stop" (fun () ->
          Alcotest.(check (list fragment))
            "home. goes down whole"
            [ ("xxxxxxxxxxxxxxxx", 8., 28.2); ("home", 8., 40.2); (".", 48., 40.2) ]
            (fragments (page ~width:208. "<p>xxxxxxxxxxxxxxxx <a href=u>home</a>.")));
      Testo.create "the breaker is the caller's" (fun () ->
          let one_a_line : Html_layout.breaker = fun ~measure:_ units -> List.init (Array.length units) (fun i -> (i, i)) in
          let p = Html_layout.layout metrics ~breaker:one_a_line ~root:(Looks.root ~size:10. ()) ~width:200. (Html_tree.of_string "<p>a b c") in
          Alcotest.(check (list fragment)) "a unit a line" [ ("a", 8., 28.2); ("b", 8., 40.2); ("c", 8., 52.2) ] (fragments p));
      Testo.create "images: the worked example" (fun () ->
          (* width= and height= are Netscape's *)
          let p = netscape "<p>A <img src=g.gif width=30 height=50> B" in
          (* the p at 19.2; the image's 50 above the baseline: 69.2 *)
          Alcotest.(check (list fragment)) "A, the image, B" [ ("A", 8., 69.2); ("", 28., 69.2); ("B", 68., 69.2) ] (fragments p);
          let line = List.hd (List.hd (List.hd (blocks "p" p)).children).lines in
          Alcotest.check near "the line: 50 above, the text's 3 below" 53. line.height;
          Alcotest.(check (option string))
            "the picture's src" (Some "g.gif")
            (Option.map (fun (pic : Html_layout.picture) -> pic.src) (List.nth line.fragments 1).picture));
      Testo.create "images: alt text until the size is known" (fun () ->
          let html = "<p><img src=a.gif alt=pic>" in
          Alcotest.(check (list fragment)) "no size: the alt text" [ ("pic", 8., 28.2) ] (fragments (page html));
          let sized =
            Html_layout.layout metrics
              ~picture_size:(fun src -> if src = "a.gif" then Some (20., 40.) else None)
              ~root:(Looks.root ~size:10. ()) ~width:200. (Html_tree.of_string html)
          in
          Alcotest.(check (list fragment)) "decoded: its size" [ ("", 8., 59.2) ] (fragments sized));
      Testo.create "images: align=middle" (fun () ->
          let p = netscape "<p>A <img src=g width=30 height=50 align=middle>" in
          let line = List.hd (List.hd (List.hd (blocks "p" p)).children).lines in
          Alcotest.(check (pair near near)) "25 above, 25 below" (25., 50.) (line.baseline -. line.top, line.height));
      Testo.create "a form's controls: boxes in the line" (fun () ->
          (* the field: 10 cells of 6, and 8; the button: "Submit Query"
           * (120) and 14; the line's top 8 (the form a block of no
           * margin), its baseline three quarters of the button down *)
          let p = page ~width:400. "<form>Name: <input name=n size=10> <input type=submit></form>" in
          let placed = List.map (fun (f : Html_layout.fragment) -> (f.x, f.width)) (Html_layout.fragments p) in
          Alcotest.(check (list (pair near near))) "Name:, the field, the button" [ (8., 50.); (68., 68.); (146., 134.) ] placed;
          let line = List.hd (List.hd (List.hd (blocks "form" p)).children).lines in
          Alcotest.check near "the button's 12.75 above the baseline" 20.75 line.baseline;
          Alcotest.(check (list bool)) "the two boxes are controls" [ false; true; true ]
            (List.map (fun (f : Html_layout.fragment) -> f.control <> None) (Html_layout.fragments p));
          Alcotest.(check (list fragment)) "a hidden one: nothing" [ ("x", 8., 17.) ]
            (fragments (page "<form><input type=hidden name=h value=v>x</form>")));
      Testo.create "list markers" (fun () ->
          let p = page "<ul><li>a<li>b</ul><ol><li>x<li>y</ol>" in
          let markers = List.map (fun (b : Html_layout.box) -> b.marker) (blocks "li" p) in
          Alcotest.(check (list string))
            "bullets, then numbers" [ "bullet"; "bullet"; "1"; "2" ]
            (List.map (function Some Html_layout.Bullet -> "bullet" | Some (Number n) -> string_of_int n | None -> "none") markers);
          Alcotest.(check (option near)) "at the first line's baseline" (Some 28.2) (Html_layout.first_baseline (List.hd (blocks "li" p))));
      Testo.create "centred" (fun () ->
          Alcotest.(check (list fragment))
            "center: the slack halved" [ ("ab", 90., 17.) ]
            (fragments (netscape "<center>ab</center>"));
          Alcotest.(check (list fragment))
            "align=right" [ ("ab", 172., 28.2) ]
            (fragments (netscape "<p align=right>ab"));
          Alcotest.(check (list fragment))
            "div's align=, HTML 3.2's" [ ("ab", 172., 17.) ]
            (fragments (page "<div align=right>ab</div>")));
      Testo.create "Netscape's extensions: unknown to Mosaic" (fun () ->
          (* claude: an unknown tag is ignored, its content shown *)
          Alcotest.(check (list fragment)) "center, ignored" [ ("ab", 8., 17.) ] (fragments (page "<center>ab</center>"));
          Alcotest.(check (list fragment)) "p's align, ignored" [ ("ab", 8., 28.2) ] (fragments (page "<p align=right>ab"));
          Alcotest.(check (list fragment)) "font size, ignored" [ ("ab", 8., 17.) ] (fragments (page "<font size=7>ab</font>"));
          Alcotest.(check (list fragment)) "a floated image, inline" [ ("pic", 8., 17.); ("x", 48., 17.) ]
            (fragments (page "<img src=g width=40 height=30 align=left alt=pic> x")));
      Testo.create "font sizes and colours" (fun () ->
          let f = List.hd (Html_layout.fragments (netscape "<font size=+2 color=red>ab</font>")) in
          Alcotest.check near "size 5: 1.5 of the root's" 15. f.look.size;
          Alcotest.(check (triple int int int)) "red" (255, 0, 0) f.look.color;
          let f = List.hd (Html_layout.fragments (netscape "<h1><font size=3>ab</font></h1>")) in
          Alcotest.check near "size 3 is the root's, in an h1 too" 10. f.look.size;
          Alcotest.(check (option (triple int int int))) "#rrggbb" (Some (0x99, 0, 0x10)) (Looks.color_of_string "#990010");
          Alcotest.(check (option near)) "-1 from 3: 2" (Some 0.8125) (Looks.font_scale "-1");
          Alcotest.(check (option near)) "kept within 1..7" (Some 3.) (Looks.font_scale "+9"));
      Testo.create "floats: the worked example" (fun () ->
          let p = netscape "<img src=g.gif width=40 height=30 align=left>aa bb cc dd ee ff" in
          (* the image at 8..48, 8..38; the lines from 48 + 6 *)
          Alcotest.(check (list fragment))
            "two lines beside it, then the image"
            [ ("aa", 54., 17.); ("bb", 84., 17.); ("cc", 114., 17.); ("dd", 144., 17.); ("ee", 54., 29.); ("ff", 84., 29.);
              ("", 8., 38.) ]
            (fragments p));
      Testo.create "floats: on the right, over the next paragraph" (fun () ->
          let p = netscape "<p><img src=g width=40 height=30 align=right>a<p>b" in
          (* the image at 152..192 (the body's right edge 192), from
           * 19.2; the second p's line, at 42.4, still beside it *)
          Alcotest.(check (list fragment)) "a, the image, b" [ ("a", 8., 28.2); ("", 152., 49.2); ("b", 8., 51.4) ] (fragments p));
      Testo.create "floats: br clear" (fun () ->
          let p = netscape "<img src=g width=40 height=30 align=left>a<br clear=all>b" in
          Alcotest.(check (list fragment)) "b below the image" [ ("a", 54., 17.); ("b", 8., 47.); ("", 8., 38.) ] (fragments p));
      Testo.create "style sheets: looks and boxes" (fun () ->
          let styled sheet html =
            let tree = Html_tree.of_string html in
            Html_layout.layout metrics ~style:(Css.cascade (Css.parse sheet) tree) ~root:(Looks.root ~size:10. ()) ~width:200. tree
          in
          let f = List.hd (Html_layout.fragments (styled "p { font-size: 2em; color: #f00 } em { font-size: 50% }" "<p>ab <em>c</em>")) in
          Alcotest.check near "2em of the root's 10" 20. f.look.size;
          Alcotest.(check (triple int int int)) "#f00 is red" (255, 0, 0) f.look.color;
          let em = List.nth (Html_layout.fragments (styled "p { font-size: 2em } em { font-size: 50% }" "<p>ab <em>c</em>")) 1 in
          Alcotest.check near "50% of its parent's 20" 10. em.look.size;
          (* margin: 5px 20px: the p from 8 + 20, its line 5 below the
           * body's top (the body's 8 and the p's 5, not collapsed with
           * each other here) *)
          Alcotest.(check (list fragment)) "margins" [ ("ab", 28., 22.) ]
            (fragments (styled "p { margin: 5px 20px }" "<p>ab"));
          Alcotest.(check (list fragment)) "display: none" [ ("b", 8., 17.) ]
            (fragments (styled ".x { display: none }" "<div class=x>a</div><div>b</div>"));
          Alcotest.(check (option (triple int int int))) "a block's background" (Some (0, 0, 255))
            (List.hd (blocks "div" (styled "div { background-color: blue }" "<div>a</div>"))).background);
      Testo.create "a table" (fun () ->
          (* padding 1, spacing 2, border 1: a's column 12, bb's 22;
           * the table 1 + 2 + 12 + 2 + 22 + 2 + 1 = 42 wide from x 8,
           * its cells from 11 and 25, their text 1 inside; the row
           * from 8 + 1 + 2 = 11, 12 + 2 high *)
          let p = netscape "<table border=1><tr><td>a<td>bb</table>" in
          Alcotest.(check (list fragment)) "a, bb" [ ("a", 12., 21.); ("bb", 26., 21.) ] (fragments p);
          let t = List.hd (blocks "table" p) in
          Alcotest.(check (pair near near)) "42 wide, 20 high" (42., 20.) (t.width, t.height);
          Alcotest.(check (list (pair near near))) "the cells' rectangles" [ (11., 12.); (25., 22.) ]
            (List.map (fun (b : Html_layout.box) -> (b.x, b.width)) (blocks "td" p)));
      Testo.create "a table: a centred cell measured, not far away" (fun () ->
          let p = netscape "<table><tr><th>ab</table>" in
          (* claude: the th's column is its word's 20 and the padding *)
          Alcotest.(check (list near)) "the column 22 wide" [ 22. ]
            (List.map (fun (b : Html_layout.box) -> b.width) (blocks "th" p)));
      Testo.create "a table: unknown to Mosaic" (fun () ->
          Alcotest.(check (list fragment)) "the cells run together" [ ("a", 8., 17.); ("b", 18., 17.) ]
            (fragments (page "<table><tr><td>a<td>b</table>")));
      Testo.create "rules: Netscape's size, width, align" (fun () ->
          let rule html = match blocks "body" (netscape html) with [ b ] -> List.hd b.children | _ -> Alcotest.fail "no body" in
          let r = rule "<hr size=6 width=50%>" in
          Alcotest.(check (triple near near near)) "centred, half, 6 thick" (54., 92., 6.) (r.x, r.width, r.height);
          let r = rule "<hr width=40 align=right>" in
          Alcotest.(check (pair near near)) "40 on the right" (152., 40.) (r.x, r.width);
          let r = match blocks "body" (page "<hr size=6 width=50%>") with [ b ] -> List.hd b.children | _ -> Alcotest.fail "no body" in
          Alcotest.(check (triple near near near)) "Mosaic: the whole line, 2 thick" (8., 184., 2.) (r.x, r.width, r.height));
      Testo.create "not shown: the head, scripts" (fun () ->
          Alcotest.(check (list fragment)) "only y" [ ("y", 8., 17.) ] (fragments (page "<title>T</title><script>x</script>y")));
    ]
