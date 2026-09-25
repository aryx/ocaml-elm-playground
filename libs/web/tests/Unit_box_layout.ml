(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_box_layout.mli *)

(* the tests' font: a character as wide as its look's size *)
let metrics (l : Looks.t) (s : string) : float = l.size *. float_of_int (String.length s)

(* the page laid out in a window [width] wide, the root's font 10 (a
 * line 12 high), [css] the page's sheet *)
let page ?(width = 200.) ?(css = "") (html : string) : Box_layout.box =
  let root = Html_tree.of_string html in
  let media : Cascade.media = { width; height = 600. } in
  let sheet : Cascade.sheet = { origin = Author; rules = Css_syntax.parse_stylesheet ("html { font-size: 10px } " ^ css) } in
  Box_layout.layout metrics ~viewport:(width, 600.) (Computed.styles media [ sheet ] root) root

(* the box of the element of id [id] *)
let rec find (id : string) (b : Box_layout.box) : Box_layout.box option =
  match b.element with
  | Some e when Dom.attribute "id" e = Some id -> Some b
  | _ -> List.find_map (find id) b.children

let box id p = match find id p with Some b -> b | None -> Alcotest.fail ("no box " ^ id)
let near = Alcotest.float 1e-6

(* x, y, width, height *)
let geometry (b : Box_layout.box) = [ b.x; b.y; b.width; b.height ]

(* the words, left to right *)
let words (b : Box_layout.box) : (string * float) list =
  List.filter_map (fun (f : Html_layout.fragment) -> if f.text = "" then None else Some (f.text, f.x)) (Box_layout.fragments b)
  |> List.stable_sort (fun (_, a) (_, b) -> compare a b)

let word = Alcotest.(pair string near)

let tests =
  Testo.categorize "Box_layout"
    [
      Testo.create "the worked example: auto margins centre" (fun () ->
          let p = page {|<body style="margin: 8px"><div id=d style="width: 100px; margin: 0 auto; padding: 5px; border: 2px solid">ab</div>|} in
          Alcotest.(check (list near)) "the div: x, y, width, height" [ 43.; 8.; 114.; 26. ] (geometry (box "d" p));
          Alcotest.(check (list word)) "its word, inside padding and border" [ ("ab", 50.) ] (words p));
      Testo.create "notes_css_engine.md's centring" (fun () ->
          let p = page ~width:976. {|<body style="margin: 0"><div id=d style="width: 400px; padding: 10px; border: 1px solid; margin: 0 auto">x</div>|} in
          let d = box "d" p in
          Alcotest.(check (list near)) "277 each side: x, width" [ 277.; 422. ] [ d.x; d.width ]);
      Testo.create "box-sizing: border-box" (fun () ->
          let p = page {|<div id=d style="box-sizing: border-box; width: 100px; padding: 10px">x</div>|} in
          Alcotest.check near "the border box is the width" 100. (box "d" p).width);
      Testo.create "margins collapse: through the body, between siblings" (fun () ->
          let p = page {|<body style="margin: 8px"><p id=a style="margin: 20px 0">a</p><p id=b style="margin: 10px 0">b</p>|} in
          Alcotest.check near "the body's 8 and the p's 20: 20" 20. (box "a" p).y;
          Alcotest.check near "the body starts there too" 20. (Option.get (find "a" p)).y;
          Alcotest.check near "20 between the two, not 30" (20. +. 12. +. 20.) (box "b" p).y);
      Testo.create "an empty block's margins are one" (fun () ->
          let p = page {|<body style="margin: 0"><p id=a style="margin: 0">a</p><div style="margin: 15px 0"></div><p id=b style="margin: 5px 0">b</p>|} in
          Alcotest.check near "15, once" (12. +. 15.) (box "b" p).y);
      Testo.create "a float: the lines beside it shortened" (fun () ->
          let p =
            page {|<body style="margin: 8px"><div id=f style="float: left; width: 40px; height: 30px"></div>aa bb cc|}
          in
          Alcotest.(check (list near)) "the float: x, y, width" [ 8.; 8.; 40. ] (let f = box "f" p in [ f.x; f.y; f.width ]);
          Alcotest.(check (list word)) "the words from its right" [ ("aa", 48.); ("bb", 78.); ("cc", 108.) ] (words p));
      Testo.create "clear: below the float" (fun () ->
          let p = page {|<body style="margin: 0"><div style="float: left; width: 40px; height: 30px"></div><div id=c style="clear: left">x</div>|} in
          Alcotest.check near "at the float's bottom" 30. (box "c" p).y);
      Testo.create "an inline-block: shrink-to-fit, in the line" (fun () ->
          let p = page {|<body style="margin: 0">a <span id=s style="display: inline-block; padding: 2px">bcd</span> e|} in
          let s = box "s" p in
          Alcotest.(check (list near)) "its width: its word and padding; x after 'a '" [ 34.; 20. ] [ s.width; s.x ];
          Alcotest.(check (list word)) "e after it" [ ("a", 0.); ("bcd", 22.); ("e", 64.) ] (words p));
      Testo.create "position: relative and absolute" (fun () ->
          let p =
            page
              {|<body style="margin: 0"><div id=r style="position: relative; top: 5px; margin-left: 10px"><div id=a style="position: absolute; left: 3px; top: 7px; width: 20px">x</div>y</div>|}
          in
          Alcotest.check near "relative: moved down 5" 5. (box "r" p).y;
          let a = box "a" p in
          Alcotest.(check (list near)) "absolute: in its positioned parent" [ 13.; 7.; 20. ] [ a.x; a.y; a.width ]);
      Testo.create "a list's markers" (fun () ->
          let p = page "<ol><li id=a>x<li id=b>y</ol><ul><li id=c>z</ul>" in
          Alcotest.(check (list bool))
            "2, then a bullet" [ true; true ]
            [ (box "b" p).marker = Some (Number 2); (box "c" p).marker = Some Bullet ]);
      Testo.create "a table: Table_layout's columns" (fun () ->
          let p = page {|<body style="margin: 8px"><table id=t><tr><td id=a>a<td id=b>bb</table>|} in
          let t = box "t" p in
          (* each cell its word and its 1 of padding each side: 12 and
           * 22; 2 of spacing around them *)
          Alcotest.(check (list near)) "the table: x, width" [ 8.; 2. +. 12. +. 2. +. 22. +. 2. ] [ t.x; t.width ];
          Alcotest.(check (list near)) "the cells' x" [ 10.; 24. ] [ (box "a" p).x; (box "b" p).x ];
          Alcotest.(check (list word)) "the words" [ ("a", 11.); ("bb", 25.) ] (words p));
      Testo.create "presentational hints: Hacker News' table" (fun () ->
          let p = page {|<body style="margin: 0"><table id=t width="50%" cellpadding=0 cellspacing=0><tr><td id=a bgcolor=ff6600>a</table>|} in
          let a = box "a" p in
          Alcotest.check near "width=50%" 100. (box "t" p).width;
          Alcotest.(check (list int)) "bgcolor=" [ 255; 102; 0 ] [ a.style.background.r; a.style.background.g; a.style.background.b ];
          Alcotest.(check (list word)) "no padding" [ ("a", 0.) ] (words p));
      Testo.create "a picture: its size, max-width" (fun () ->
          let p = page {|<body style="margin: 0"><img src=a.png width=400 height=100 style="max-width: 100%">|} in
          match List.filter_map (fun (f : Html_layout.fragment) -> Option.map (fun (pic : Html_layout.picture) -> (f.width, pic.height)) f.picture) (Box_layout.fragments p) with
          | [ (w, h) ] -> Alcotest.(check (list near)) "scaled to the page" [ 200.; 50. ] [ w; h ]
          | _ -> Alcotest.fail "one picture");
    ]
