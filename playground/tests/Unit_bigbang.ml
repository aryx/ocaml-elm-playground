(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Bigbang: the worked examples of Bigbang.mli *)

open Playground

let t = Testo.create
let size = Alcotest.(pair (float 1e-9) (float 1e-9))

(* beside's and above's: 20 x 10 and 30 x 40 *)
let test_sizes () =
  let a = Bigbang.rectangle 20. 10. Solid red and b = Bigbang.rectangle 30. 40. Solid blue in
  let dims i = (Bigbang.width i, Bigbang.height i) in
  Alcotest.check size "beside" (50., 40.) (dims (Bigbang.beside a b));
  Alcotest.check size "above" (30., 50.) (dims (Bigbang.above a b));
  Alcotest.check size "overlay" (30., 40.) (dims (Bigbang.overlay a b))

(* place_image: in a 100 x 300 scene, (50, 0) is its top middle: 150 up
 * from its center *)
let test_place () =
  let scene = Bigbang.place_image (Bigbang.circle 5. Solid red) 50. 0. (Bigbang.empty_scene 100. 300.) in
  match (Bigbang.to_shape scene).form with
  | Group [ _; dot ] -> Alcotest.check size "the top middle" (0., 150.) (dot.x, dot.y)
  | _ -> Alcotest.fail "a scene and a dot"

(* key_events's: "ArrowLeft" held, "a" pressed *)
let test_keys () =
  let kb keys = { initial_computer.keyboard with keys = List.fold_left (fun s k -> Set_.add k s) Set_.empty keys } in
  Alcotest.(check (pair (list string) (list string))) "pressed" ([ "a" ], []) (Bigbang.key_events (kb [ "ArrowLeft" ]) (kb [ "ArrowLeft"; "a" ]));
  Alcotest.(check (pair (list string) (list string))) "released" ([], [ "left" ]) (Bigbang.key_events (kb [ "ArrowLeft" ]) (kb []))

let tests = Testo.categorize "Bigbang" [ t "sizes" test_sizes; t "place_image" test_place; t "key events" test_keys ]
