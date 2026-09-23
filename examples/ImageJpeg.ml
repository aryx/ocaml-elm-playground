(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* JPEG, taken apart: a picture as 8 x 8 blocks of cosine patterns, and
 * what is left when only the first few patterns of each block are kept.
 *
 * On the left, the picture as it was drawn (a PNG: every pixel exact).
 * On the right, the same picture from a JPEG of it, each block decoded
 * from only its first n coefficients, in zigzag order (graphics/images/
 * jpeg/Jpeg.mli's ?keep): with 1, each block is its average, a mosaic;
 * with a few, the slow changes are back -- the sky, the hills -- and
 * the sharp edges still blur and ring; with all 64, the JPEG as it is.
 * That is JPEG's bet: its quantization throws away most of the high
 * frequencies, the last coefficients of the zigzag, and the eye misses
 * little of it (notes_images.md, sections 8 and 9).
 *
 * Below, the 64 patterns themselves (Dct.idct of one coefficient),
 * the ones kept lit, in the zigzag's order from the top left.
 *
 * Up and down: one coefficient more or less; right and left: 8. "b":
 * the color's upsampling, `Triangle (libjpeg's) or `Box (each sample
 * repeated: look at the roof's edge).
 *
 * This picture is a drawing, flat colors and sharp edges: the JPEG is
 * bigger than the PNG. JPEG is for photographs; ImagePng and ImageLzw
 * take the other two formats apart.
 *
 * What it uses: the Playground, Scene2d (the keys), Sprite.of_rgba (a
 * decoded picture as shapes), and graphics/images's Png, Jpeg and Dct
 * directly; the picture, demo_picture.{png,jpg} (make_demo_pictures.py),
 * embedded at build time (Demo_pictures, see examples/dune).
 *)
open Playground

type state = { keep : int; box : bool; (* the JPEG decoded so, as shapes *) jpeg : shape }
type model = state Scene2d.t

let pixel_size = 6.

let decode ~(keep : int) ~(box : bool) : shape =
  Sprite.of_rgba pixel_size
    (Jpeg.decode ~keep ~upsampling:(if box then `Box else `Triangle) Demo_pictures.demo_picture_jpg)

let with_keep (keep : int) ~(box : bool) : state = { keep; box; jpeg = decode ~keep ~box }

let initial_model : model = Scene2d.start (with_keep 6 ~box:false)
let png : shape = Sprite.of_rgba pixel_size (Png.decode Demo_pictures.demo_picture_png)

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let pressed f = Scene2d.pressed f scenes in
  let keep =
    if pressed (fun k -> k.kup) then m.keep + 1
    else if pressed (fun k -> k.kdown) then m.keep - 1
    else if pressed (fun k -> k.kright) then m.keep + 8
    else if pressed (fun k -> k.kleft) then m.keep - 8
    else m.keep
  in
  let keep = max 1 (min 64 keep) in
  let box = if pressed (fun kb -> Set_.mem "b" kb.keys) then not m.box else m.box in
  if keep = m.keep && box = m.box then scenes else { scenes with scene = with_keep keep ~box }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175

(* position [k] in the zigzag: which pattern, (u, v) *)
let uv_of_zigzag (k : int) : int * int = (Jpeg.zigzag.(k) mod 8, Jpeg.zigzag.(k) / 8)

(* one of the 64 patterns, 8 x 8 cells of [cell] pixels, lit or dimmed *)
let pattern ~(u : int) ~(v : int) ~(lit : bool) (cell : number) : shape =
  let coefs = Array.make 64 0. in
  coefs.((v * 8) + u) <- 1.;
  let f = Dct.idct coefs in
  List.init 64 (fun n ->
      let x = n mod 8 and y = n / 8 in
      (* the values are within -0.25 .. 0.25 *)
      let g = max 0 (min 255 (int_of_float (128. +. (f.(n) *. 500.)))) in
      let g = if lit then g else 40 + (g / 5) in
      rectangle (rgb g g g) cell cell |> move ((float_of_int x -. 3.5) *. cell) ((3.5 -. float_of_int y) *. cell))
  |> group

let patterns (keep : int) : shape list =
  List.init 64 (fun k ->
      let u, v = uv_of_zigzag k in
      pattern ~u ~v ~lit:(k < keep) 3. |> move ((float_of_int u -. 3.5) *. 28.) (-230. -. ((float_of_int v -. 3.5) *. 28.)))

let view (computer : computer) (s : model) : shape list =
  let m = s.scene and screen = computer.screen in
  [ rectangle (rgb 18 20 30) screen.width screen.height;
    text white 2.2 "JPEG, TAKEN APART" |> move_y 440.;
    png |> move (-210.) 180.;
    m.jpeg |> move 210. 180.;
    text grey 1.3 (Printf.sprintf "the picture: PNG, %d bytes, exact" (String.length Demo_pictures.demo_picture_png))
    |> move (-210.) 0.;
    text grey 1.3
      (Printf.sprintf "JPEG, %d bytes: %d of 64 coefficients a block" (String.length Demo_pictures.demo_picture_jpg) m.keep)
    |> move 210. 0.;
    text (rgb 240 200 90) 1.3 (if m.box then "color upsampling: box" else "color upsampling: triangle (libjpeg's)")
    |> move 210. (-30.) ]
  @ patterns m.keep
  @ [ text grey 1.2 "the 64 patterns, the kept ones lit" |> move_y (-360.);
      text grey 1.3 "up, down: one coefficient    right, left: eight    b: box or triangle upsampling" |> move_y (-410.);
      text (rgb 120 125 145) 1.2 "a drawing: the JPEG is bigger than the PNG -- JPEG is for photographs"
      |> move_y (-450.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
