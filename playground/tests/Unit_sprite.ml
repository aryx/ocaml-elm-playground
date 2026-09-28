(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Sprite *)

let t = Testo.create

let crab =
  [ "..#.....#.."; "...#...#..."; "..#######.."; ".##.###.##."; "###########"; "#.#######.#"; "#.#.....#.#"; "...##.##..." ]

let shapes (shape : Playground.shape) : Playground.shape list =
  match shape.form with Group shapes -> shapes | _ -> [ shape ]

let area (shape : Playground.shape) : float =
  List.fold_left
    (fun acc (s : Playground.shape) -> match s.form with Rectangle (_, w, h) -> acc +. (w *. h) | _ -> acc)
    0. (shapes shape)

let test_runs () =
  Alcotest.(check (list (triple int int char)))
    "..###.#" [ (0, 2, '.'); (2, 3, '#'); (5, 1, '.'); (6, 1, '#') ] (Sprite.runs "..###.#");
  Alcotest.(check (list (triple int int char))) "empty" [] (Sprite.runs "")

let test_crab () =
  let palette = [ ('#', Playground.green) ] in
  let rle = Sprite.pixels 6. palette crab and squares = Sprite.pixels_squares 6. palette crab in
  Alcotest.(check int) "46 pixels" 46 (List.length (shapes squares));
  Alcotest.(check int) "18 rectangles" 18 (List.length (shapes rle));
  Alcotest.(check (float 1e-9)) "the same area" (area squares) (area rle);
  Alcotest.(check (float 1e-9)) "46 pixels of 6x6" (46. *. 36.) (area rle)

(* the crab's top-left pixel, (2, 0), in a 66x48 sprite centered on (0, 0) *)
let test_position () =
  let first = List.hd (shapes (Sprite.pixels 6. [ ('#', Playground.green) ] crab)) in
  Alcotest.(check (pair (float 1e-9) (float 1e-9))) "center of pixel (2, 0)" (-18., 21.) (first.x, first.y)

let test_flip () =
  Alcotest.(check (list string)) "##.." [ "..##" ] (Sprite.flip [ "##.." ]);
  Alcotest.(check (list string)) "short rows completed" [ "..##"; "...#" ] (Sprite.flip [ "##.."; "#" ])

let test_animation () =
  Alcotest.(check char) "cycle 5 [a; b]" 'b' (Sprite.cycle 5 [ 'a'; 'b' ]);
  Alcotest.(check char) "cycle 0" 'a' (Sprite.cycle 0 [ 'a'; 'b' ]);
  Alcotest.(check char) "4 fps at 1.3 s: frame 5" 'b' (Sprite.frame 4. (Playground.Time 1.3) [ 'a'; 'b' ]);
  Alcotest.(check char) "a posix time" 'a' (Sprite.frame 4. (Playground.Time 1789590431.5) [ 'a'; 'b' ])

(* the crab to an XPM file and back: the '.' pixels transparent, the
 * palette's colors kept *)
let test_xpm () =
  let palette = [ ('#', Playground.rgb 115 210 22) ] in
  let file = Sprite.to_xpm "crab" palette crab in
  Alcotest.(check bool) "a transparent '.'" true (String.length file > 0 && Option.is_some (String.index_opt file '.'));
  let palette', rows = Sprite.of_xpm file in
  Alcotest.(check (list string)) "the rows" crab rows;
  Alcotest.(check bool) "the palette" true (palette' = palette);
  (* a Hex color, as Playground's named colors are, comes back as Rgb *)
  let palette', _ = Sprite.of_xpm (Sprite.to_xpm "crab" [ ('#', Playground.green) ] crab) in
  Alcotest.(check bool) "green, #73d216" true (palette' = [ ('#', Playground.rgb 0x73 0xd2 0x16) ])

(* TinyMario's hero, as TinyAseprite writes it, and its level, as
 * TinyTiled writes it: the files read and written back, byte for byte,
 * so that exporting an unchanged sprite or level changes nothing *)
let test_mario_files () =
  [ "stand"; "walk1"; "walk2"; "jump"; "level" ]
  |> List.iter (fun pose ->
         let name = "mario_" ^ pose in
         let ic = open_in_bin ("../../games/platform/" ^ name ^ ".xpm") in
         let text = really_input_string ic (in_channel_length ic) in
         close_in ic;
         let palette, rows = Sprite.of_xpm text in
         Alcotest.(check string) name text (Sprite.to_xpm name palette rows))

let tests =
  Testo.categorize "Sprite"
    [
      t "runs" test_runs;
      t "XPM files" test_xpm;
      t "TinyMario's hero, read and written back" test_mario_files;
      t "the crab, with and without runs" test_crab;
      t "position" test_position;
      t "flip" test_flip;
      t "cycle and frame" test_animation;
    ]
