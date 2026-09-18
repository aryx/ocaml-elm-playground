(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Help_overlay.mli *)

(* a bit bigger than the default [words], to be read at a glance *)
let scale = 1.3

let draw (fb : Framebuffer.t) (keys : (string * string) list) : unit =
  let w = float fb.width and h = float fb.height in
  (* Playground coordinates: the origin at the center, y up; words are
   * centered on their position, in font units of [unit] pixels *)
  let unit = scale *. Playground.words_font_size /. Hershey.units_per_em in
  let width_of text = snd (Hershey.layout text) *. unit in
  let line_height = 1.5 *. scale *. Playground.words_font_size and margin = 15. in
  (* two columns, the keys and what they do *)
  let max_width texts = List.fold_left (fun acc text -> Float.max acc (width_of text)) 0. texts in
  let keys_w = max_width (List.map fst keys) +. 20. in
  let box_w = keys_w +. max_width (List.map snd keys) +. (2. *. margin) in
  let box_h = (float (List.length keys) *. line_height) +. (2. *. margin) in
  let left = (-.w /. 2.) +. 20. and top = (h /. 2.) -. 20. in
  let background =
    Playground.rectangle (Playground.rgb 240 240 240) box_w box_h
    |> Playground.fade 0.9
    |> Playground.move (left +. (box_w /. 2.)) (top -. (box_h /. 2.))
  in
  (* left-aligned at x *)
  let text x y s = Playground.words Playground.black s |> Playground.scale scale |> Playground.move (x +. (width_of s /. 2.)) y in
  let lines =
    keys
    |> List.mapi (fun i (key, what) ->
           let y = top -. margin -. (line_height /. 2.) -. (float i *. line_height) in
           [ text (left +. margin) y key; text (left +. margin +. keys_w) y what ])
    |> List.concat
  in
  Shape_render_software.render ~options:Shape_render_software.default_options fb (background :: lines)
