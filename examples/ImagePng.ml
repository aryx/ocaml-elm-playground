(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* PNG, taken apart: what the filters do to the bytes before DEFLATE
 * compresses them.
 *
 * On the left, the picture. On the right, what PNG compresses instead
 * of it: each byte replaced by its difference from a prediction made
 * from its neighbors (graphics/images/png/Png.mli), drawn as brightness
 * -- black for 0, a perfect prediction; bright for a big miss. Beside
 * each row, the filter it used, in its color: None, Sub (the left
 * neighbor), Up, Average, Paeth.
 *
 * "a" encodes the picture the way Png.encode does by default, each row
 * with the filter whose differences sum the smallest; "0" to "4" force
 * one filter on every row. With None the right side is the picture's
 * own bytes; with Sub or Up the flat areas turn black and only the
 * edges are left; the sizes below are the same DEFLATE (our
 * Deflate.mli) on each.
 *
 * And they hold a surprise: on this drawing, None is the smallest, and
 * the adaptive choice isn't. Flat colors already repeat, and LZ77
 * finds those runs without any help; the filters shine on gradients
 * and photographs, where no two neighbors are equal but their
 * differences are. The adaptive rule (the PNG specification's
 * suggestion) is only a guess at what DEFLATE will make of a row --
 * better encoders (zopfli, pngcrush) try the filters for real and keep
 * the smallest, an exercise.
 *
 * What it uses: the Playground, Scene2d (the keys), Sprite.of_rgba, and
 * graphics/images's Png directly (encode ?filter, scanlines); the
 * picture, demo_picture.png (make_demo_pictures.py), embedded at build
 * time (Demo_pictures, see examples/dune).
 *)
open Playground

let picture : Rgba_image.t = Png.decode Demo_pictures.demo_picture_png
let pixel_size = 6.
let names = [| "None"; "Sub"; "Up"; "Average"; "Paeth" |]
let colors = [| rgb 200 200 215; rgb 230 90 80; rgb 90 170 240; rgb 240 200 90; rgb 140 220 140 |]

(* each way of filtering: None to Paeth, then the adaptive one *)
let files : string array =
  Array.init 6 (fun f -> Png.encode ~alpha:false ?filter:(if f < 5 then Some f else None) picture)

(* the filtered bytes of one encoding as a picture: a pixel as bright as
 * its three differences are big, read as signed bytes; and the
 * filters of its rows *)
let differences (file : string) : shape * shape =
  let rows = Png.scanlines file in
  let img = Rgba_image.create ~width:picture.width ~height:picture.height in
  rows
  |> List.iteri (fun y (_, bytes) ->
         for x = 0 to picture.width - 1 do
           let miss = ref 0 in
           for k = 0 to 2 do
             let v = Char.code (Bytes.get bytes ((x * 3) + k)) in
             miss := !miss + if v < 128 then v else 256 - v
           done;
           let g = min 255 (!miss * 2) in
           let o = ((y * picture.width) + x) * 4 in
           img.rgba.{o} <- g;
           img.rgba.{o + 1} <- g;
           img.rgba.{o + 2} <- g;
           img.rgba.{o + 3} <- 255
         done);
  let strip =
    rows
    |> List.mapi (fun y (f, _) ->
           rectangle colors.(f) 14. pixel_size
           |> move 0. ((float_of_int picture.height *. pixel_size /. 2.) -. ((float_of_int y +. 0.5) *. pixel_size)))
    |> group
  in
  (Sprite.of_rgba pixel_size img, strip)

let shown : (shape * shape) array = Array.map differences files

type model = int Scene2d.t (* the encoding shown, 0 to 5 *)

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let choice = List.fold_left (fun c (k, v) -> if key k then v else c) scenes.scene
      [ ("0", 0); ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("a", 5) ] in
  { scenes with scene = choice }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175

let sizes (choice : int) : shape list =
  List.init 6 (fun f ->
      let name = if f < 5 then Printf.sprintf "%d  %s" f names.(f) else "a  adaptive, row by row" in
      let color = if f < 5 then colors.(f) else white in
      let y = -60. -. (float_of_int f *. 38.) in
      [ text (if f = choice then color else grey) 1.4 name |> move (-150.) y;
        text (if f = choice then color else grey) 1.4 (Printf.sprintf "%5d bytes" (String.length files.(f)))
        |> move 150. y ])
  |> List.concat

let view (computer : computer) (s : model) : shape list =
  let choice = s.scene and screen = computer.screen in
  let bytes, strip = shown.(choice) in
  [ rectangle (rgb 18 20 30) screen.width screen.height;
    text white 2.2 "PNG, TAKEN APART" |> move_y 440.;
    Sprite.of_rgba pixel_size picture |> move (-210.) 190.;
    bytes |> move 220. 190.;
    strip |> move 12. 190.;
    text grey 1.3 "the picture" |> move (-210.) 10.;
    text grey 1.3 "what DEFLATE compresses: the misses" |> move 220. 10. ]
  @ sizes choice
  @ [ text grey 1.3 (Printf.sprintf "raw: %d bytes (64 x 48 x 3)" (picture.width * picture.height * 3)) |> move_y (-310.);
      text grey 1.3 "0 to 4: one filter on every row    a: the smallest, row by row" |> move_y (-400.);
      text (rgb 120 125 145) 1.2 "black is a perfect prediction -- but a flat drawing repeats anyway: here None wins"
      |> move_y (-440.) ]

let app = game view update (Scene2d.start 5)
let main = Playground_platform.run_app app
