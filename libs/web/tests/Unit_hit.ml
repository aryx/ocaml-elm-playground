(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_hit.mli *)

(* the tests' font: a character as wide as its look's size *)
let metrics (l : Looks.t) (s : string) : float = l.size *. float_of_int (String.length s)

let page (html : string) : Html_layout.box =
  Html_layout.layout metrics ~root:(Looks.root ~size:10. ()) ~width:200. (Html_tree.of_string html)

let tests =
  Testo.categorize "Hit"
    [
      Testo.create "the worked example" (fun () ->
          let p = page "<p>the <a href=recipes.html>recipes</a>" in
          Alcotest.(check (option string)) "on recipes" (Some "recipes.html") (Hit.link_at p ~x:60. ~y:25.);
          Alcotest.(check (option string)) "on the" None (Hit.link_at p ~x:20. ~y:25.);
          Alcotest.(check (option string)) "right of the line" None (Hit.link_at p ~x:150. ~y:25.);
          Alcotest.(check (option string)) "below it" None (Hit.link_at p ~x:60. ~y:40.));
      Testo.create "spaces: inside a link, not between two" (fun () ->
          (* "a" 8..18, "b" 28..38 (one link); "c" 48..58 (another) *)
          let p = page "<p><a href=u>a b</a> <a href=v>c</a>" in
          Alcotest.(check (option string)) "between a and b" (Some "u") (Hit.link_at p ~x:23. ~y:25.);
          Alcotest.(check (option string)) "between b and c" None (Hit.link_at p ~x:43. ~y:25.);
          Alcotest.(check (option string)) "on c" (Some "v") (Hit.link_at p ~x:50. ~y:25.));
      Testo.create "anchors: a name, an id, a block's id" (fun () ->
          let p = page "<p>a<p>b <a name=x>here</a><h2 id=y>t</h2><a name=top></a><p>c" in
          (* p1 19.2..31.2; p2 42.4..54.4; h2 (15, margin 11.25) 65.65..83.65;
           * the empty anchor where inline content after the h2 would
           * start, its margin below: 83.65 + 11.25 *)
          Alcotest.(check (option (float 1e-6))) "a name: its line's top" (Some 42.4) (Hit.anchor p "x");
          Alcotest.(check (option (float 1e-6))) "a block's id: the block's top" (Some 65.65) (Hit.anchor p "y");
          Alcotest.(check (option (float 1e-6))) "an anchor alone" (Some 94.9) (Hit.anchor p "top");
          Alcotest.(check (option (float 1e-6))) "none" None (Hit.anchor p "z"));
      Testo.create "the fragment under a point: a control" (fun () ->
          (* "Name:" 8..58, the field 68..136 *)
          let p = Html_layout.layout metrics ~root:(Looks.root ~size:10. ()) ~width:400. (Html_tree.of_string "<form>Name: <input name=n size=10></form>") in
          let at x = Option.map (fun (f : Html_layout.fragment) -> (f.text, f.control <> None)) (Hit.fragment_at p ~x ~y:15.) in
          Alcotest.(check (option (pair string bool))) "on Name:" (Some ("Name:", false)) (at 20.);
          Alcotest.(check (option (pair string bool))) "on the field" (Some ("", true)) (at 100.);
          Alcotest.(check (option (pair string bool))) "in the space" None (at 63.));
      Testo.create "an anchor keeps the space around it" (fun () ->
          let p = page "a <a name=x></a>b" in
          Alcotest.(check (list (pair string (float 1e-6))))
            "a, then b a space later"
            [ ("a", 8.); ("b", 28.) ]
            (List.map (fun (f : Html_layout.fragment) -> (f.text, f.x)) (Html_layout.fragments p)));
    ]
