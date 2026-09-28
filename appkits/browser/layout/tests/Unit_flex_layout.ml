(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_flex_layout.mli *)

let item ?(grow = 0.) ?(shrink = 1.) ?(min_size = 0.) ?(auto_before = false) (base : float) : Flex_layout.item =
  { base; grow; shrink; min_size; max_size = infinity; auto_before; auto_after = false }

let near = Alcotest.float 0.05
let sizes = Alcotest.(list near)

let tests =
  Testo.categorize "Flex_layout"
    [
      Testo.create "the worked example: grow" (fun () ->
          let items = [| item 100.; item ~grow:1. 100.; item ~grow:2. 100. |] in
          let s = Flex_layout.resolve ~room:600. ~gap:10. items in
          Alcotest.check sizes "100, 193.3, 286.7" [ 100.; 193.3; 286.7 ] (Array.to_list s);
          Alcotest.check sizes "at 0, 110, 313.3" [ 0.; 110.; 313.3 ] (Array.to_list (Flex_layout.place ~justify:Start ~room:600. ~gap:10. items s)));
      Testo.create "the worked example: shrink" (fun () ->
          let s = Flex_layout.resolve ~room:250. ~gap:10. [| item 100.; item 100.; item 100. |] in
          Alcotest.check sizes "76.7 each" [ 76.67; 76.67; 76.67 ] (Array.to_list s));
      Testo.create "a minimum freezes an item, the others shrink more" (fun () ->
          let s = Flex_layout.resolve ~room:200. ~gap:0. [| item ~min_size:100. 150.; item 150. |] in
          Alcotest.check sizes "100, then the rest" [ 100.; 100. ] (Array.to_list s));
      Testo.create "justify-content and auto margins" (fun () ->
          let items = [| item 20.; item 20. |] in
          let place justify = Array.to_list (Flex_layout.place ~justify ~room:100. ~gap:0. items [| 20.; 20. |]) in
          Alcotest.check sizes "space-between" [ 0.; 80. ] (place Space_between);
          Alcotest.check sizes "center" [ 30.; 50. ] (place Center);
          Alcotest.check sizes "an auto margin takes the room" [ 0.; 80. ]
            (Array.to_list (Flex_layout.place ~justify:Center ~room:100. ~gap:0. [| item 20.; item ~auto_before:true 20. |] [| 20.; 20. |])));
      Testo.create "lines when wrapping" (fun () ->
          Alcotest.(check (list (pair int int))) "two and one" [ (0, 1); (2, 2) ]
            (Flex_layout.lines ~wrap:true ~room:100. ~gap:10. [| item 40.; item 40.; item 40. |]));
    ]
