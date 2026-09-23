(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* GIF's LZW, played back one code at a time: a dictionary learnt while
 * reading.
 *
 * The picture fills in as the codes are read (graphics/images/gif/
 * Lzw.mli's steps). On the right, the last codes, each with the pixels
 * it stands for: at first a code is one pixel, a color of the 16; soon
 * the dictionary knows runs of sky, then longer ones, and a code of a
 * few bits writes a dozen pixels. Each code but the first after a clear
 * adds an entry, the previous one's pixels plus this one's first; the
 * codes' width grows a bit each time the dictionary fills that many
 * bits, 5 (16 colors, the clear and end codes, and the first entries)
 * then 6, ... up to 12.
 *
 * Right: one code; left: back one; space: play and pause (a few codes a
 * frame); "r": from the start.
 *
 * What it uses: the Playground, Scene2d (the keys), Sprite.of_rgba, and
 * graphics/images's Gif and Lzw directly; the picture,
 * demo_picture.gif (make_demo_pictures.py, 16 colors), embedded at
 * build time (Demo_pictures, see examples/dune).
 *)
open Playground

let frame : Gif.frame =
  match snd (Gif.frames Demo_pictures.demo_picture_gif) with
  | f :: _ -> f
  | [] -> failwith "ImageLzw: a GIF without a frame"

let width = frame.patch.width
let height = frame.patch.height

let steps, pixels =
  let steps, pixels = Lzw.steps ~min_code_size:frame.min_code_size frame.lzw ~npixels:(width * height) in
  (Array.of_list steps, pixels)

let color (index : int) : int * int * int =
  let c k = Char.code frame.palette.[(3 * index) + k] in
  (c 0, c 1, c 2)

(* the picture once [n] codes are read: the pixels written so far *)
let picture (n : int) : Rgba_image.t =
  let upto = if n = 0 then 0 else let s = steps.(n - 1) in s.start + s.length in
  let img = Rgba_image.create ~width ~height in
  for i = 0 to min (width * height) upto - 1 do
    let r, g, b = color (Char.code (Bytes.get pixels i)) in
    img.rgba.{i * 4} <- r;
    img.rgba.{(i * 4) + 1} <- g;
    img.rgba.{(i * 4) + 2} <- b;
    img.rgba.{(i * 4) + 3} <- 255
  done;
  img

type state = { n : int; (* codes read *) playing : bool }
type model = state Scene2d.t

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let pressed f = Scene2d.pressed f scenes in
  let key k = pressed (fun kb -> Set_.mem k kb.keys) in
  let playing = if pressed (fun k -> k.kspace) then not m.playing else m.playing in
  let n =
    if key "r" then 0
    else if pressed (fun k -> k.kright) then m.n + 1
    else if pressed (fun k -> k.kleft) then m.n - 1
    else if playing then m.n + 3
    else m.n
  in
  let n = max 0 (min (Array.length steps) n) in
  { scenes with scene = { n; playing = playing && n < Array.length steps } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let pixel_size = 6.
let clear = 1 lsl frame.min_code_size

(* a code read: its number and width, the pixels it wrote as a strip of
 * squares, and the entry it added *)
let line (k : int) (y : number) ~(current : bool) : shape list =
  let s = steps.(k) in
  let what =
    if s.code = clear then "clear"
    else if s.code = clear + 1 then "end"
    else if s.code < clear then "a color"
    else "an entry"
  in
  let c = if current then white else grey in
  [ text c 1.2 (Printf.sprintf "%4d  (%2d bits)  %s" s.code s.width what) |> move 90. y;
    text c 1.2 (match s.added with Some e -> Printf.sprintf "adds %d" e | None -> "") |> move 430. y ]
  @ List.init (min s.length 16) (fun i ->
        let r, g, b = color (Char.code (Bytes.get pixels (s.start + i))) in
        rectangle (rgb r g b) 9. 14. |> move (190. +. (float_of_int i *. 10.)) y)
  @ if s.length > 16 then [ text grey 1.1 (Printf.sprintf "+%d" (s.length - 16)) |> move 370. y ] else []

let view (computer : computer) (s : model) : shape list =
  let m = s.scene and screen = computer.screen in
  let shown = List.init (min 14 m.n) (fun i -> m.n - 1 - i) in
  let bits = Array.fold_left (fun acc (st : Lzw.step) -> acc + st.width) 0 (Array.sub steps 0 m.n) in
  let written = if m.n = 0 then 0 else steps.(m.n - 1).start + steps.(m.n - 1).length in
  [ rectangle (rgb 18 20 30) screen.width screen.height;
    text white 2.2 "GIF'S LZW, ONE CODE AT A TIME" |> move_y 440.;
    rectangle (rgb 45 48 60) (float_of_int width *. pixel_size) (float_of_int height *. pixel_size)
    |> move (-250.) 180.;
    Sprite.of_rgba pixel_size (picture m.n) |> move (-250.) 180. ]
  @ List.concat (List.mapi (fun row k -> line k (330. -. (float_of_int row *. 30.)) ~current:(row = 0)) shown)
  @ [ text grey 1.3 (Printf.sprintf "%d of %d codes read" m.n (Array.length steps)) |> move (-250.) 0.;
      text grey 1.3 (Printf.sprintf "%d pixels written, from %d bits" written bits) |> move (-250.) (-35.);
      text grey 1.3
        (Printf.sprintf "%.2f bits a pixel, where 16 colors need 4"
           (if written = 0 then 0. else float_of_int bits /. float_of_int written))
      |> move (-250.) (-70.);
      text grey 1.3 "right: a code    left: back    space: play    r: from the start" |> move_y (-400.);
      text (rgb 120 125 145) 1.2 "the codes get longer runs: the dictionary learns the picture as it goes"
      |> move_y (-440.) ]

let app = game view update (Scene2d.start { n = 40; playing = false })
let main = Playground_platform.run_app app
