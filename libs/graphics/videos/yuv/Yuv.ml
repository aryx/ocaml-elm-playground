(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Yuv.mli *)

type range = Full | Studio

let clamp (v : float) : int = max 0 (min 255 (int_of_float (Float.round v)))

(* the studio range squeezes the full one: 256 levels of Y into 219
 * (16-235), of Cb and Cr into 224 (16-240), around 128 *)
let y_scale = 219. /. 255.
let c_scale = 224. /. 255.

let of_rgb (range : range) ((r, g, b) : int * int * int) : int * int * int =
  let r = float_of_int r and g = float_of_int g and b = float_of_int b in
  let y = (0.299 *. r) +. (0.587 *. g) +. (0.114 *. b) in
  let cb = 128. -. (0.168736 *. r) -. (0.331264 *. g) +. (0.5 *. b) in
  let cr = 128. +. (0.5 *. r) -. (0.418688 *. g) -. (0.081312 *. b) in
  match range with
  | Full -> (clamp y, clamp cb, clamp cr)
  | Studio -> (clamp (16. +. (y *. y_scale)), clamp (128. +. ((cb -. 128.) *. c_scale)), clamp (128. +. ((cr -. 128.) *. c_scale)))

let to_rgb (range : range) ((y, cb, cr) : int * int * int) : int * int * int =
  let y = float_of_int y and cb = float_of_int cb -. 128. and cr = float_of_int cr -. 128. in
  let y, cb, cr = match range with Full -> (y, cb, cr) | Studio -> ((y -. 16.) /. y_scale, cb /. c_scale, cr /. c_scale) in
  (clamp (y +. (1.402 *. cr)), clamp (y -. (0.344136 *. cb) -. (0.714136 *. cr)), clamp (y +. (1.772 *. cb)))

type chroma = C420 | C444
type planes = { width : int; height : int; chroma : chroma; y : Bytes.t; cb : Bytes.t; cr : Bytes.t }

let chroma_size (chroma : chroma) ~(width : int) ~(height : int) : int * int =
  match chroma with C444 -> (width, height) | C420 -> ((width + 1) / 2, (height + 1) / 2)

let of_image (range : range) (chroma : chroma) (img : Rgba_image.t) : planes =
  let width = img.width and height = img.height in
  let full = Array.init (width * height) (fun i -> of_rgb range (img.rgba.{4 * i}, img.rgba.{(4 * i) + 1}, img.rgba.{(4 * i) + 2})) in
  let y = Bytes.init (width * height) (fun i -> let v, _, _ = full.(i) in Char.chr v) in
  let cw, ch = chroma_size chroma ~width ~height in
  let side = match chroma with C444 -> 1 | C420 -> 2 in
  (* a color sample: the rounded average of the pixels it stands for,
   * side x side of them, fewer at an odd edge *)
  let sample (pick : int * int * int -> int) =
    Bytes.init (cw * ch) (fun i ->
        let cx = i mod cw and cy = i / cw in
        let sum = ref 0 and n = ref 0 in
        for py = cy * side to min (height - 1) ((cy * side) + side - 1) do
          for px = cx * side to min (width - 1) ((cx * side) + side - 1) do
            sum := !sum + pick full.((py * width) + px);
            incr n
          done
        done;
        Char.chr ((!sum + (!n / 2)) / !n))
  in
  { width; height; chroma; y; cb = sample (fun (_, cb, _) -> cb); cr = sample (fun (_, _, cr) -> cr) }

let to_image (range : range) (p : planes) : Rgba_image.t =
  let img = Rgba_image.create ~width:p.width ~height:p.height in
  let cw, _ = chroma_size p.chroma ~width:p.width ~height:p.height in
  let side = match p.chroma with C444 -> 1 | C420 -> 2 in
  for py = 0 to p.height - 1 do
    for px = 0 to p.width - 1 do
      (* the nearest color sample: the one this pixel's square shares *)
      let c = ((py / side) * cw) + (px / side) in
      let r, g, b = to_rgb range (Char.code (Bytes.get p.y ((py * p.width) + px)), Char.code (Bytes.get p.cb c), Char.code (Bytes.get p.cr c)) in
      let o = 4 * ((py * p.width) + px) in
      img.rgba.{o} <- r;
      img.rgba.{o + 1} <- g;
      img.rgba.{o + 2} <- b;
      img.rgba.{o + 3} <- 255
    done
  done;
  img
