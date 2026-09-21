(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

let kind = "picture"
let zoom = 2.

type tool = Pencil | Brush | Eraser | Fill

type state = {
  bits : Bitmap.t;
  tool : tool;
  pattern : Pattern.t;
  (* the dot the mouse was on at the previous frame of a drag *)
  last : (int * int) option;
}

(* the picture at its own size, against the left of the part's box *)
let size st = (float_of_int (Bitmap.width st.bits) *. zoom, float_of_int (Bitmap.height st.bits) *. zoom)
let left_top st (b : Widget.box) = (Widget.left b, b.y +. (snd (size st) /. 2.))

let dot_at st b (x, y) =
  let l, t = left_top st b in
  (int_of_float (Float.floor ((x -. l) /. zoom)), int_of_float (Float.floor ((t -. y) /. zoom)))

let input computer (b : Widget.box) st =
  let m = computer.mouse in
  let ((x, y) as p) = dot_at st b (m.mx, m.my) in
  let on = x >= 0 && y >= 0 && x < Bitmap.width st.bits && y < Bitmap.height st.bits in
  let stroke brush pat a = Bitmap.change st.bits (fun bits -> Paint.stroke bits ~brush pat a p) in
  match (m.mdown, st.last) with
  | true, None when on -> (
      match st.tool with
      | Fill -> { st with bits = Bitmap.change st.bits (fun bits -> Seed_fill.fill bits st.pattern x y); last = Some p }
      | Pencil -> { st with bits = stroke Paint.pencil st.pattern p; last = Some p }
      | Brush -> { st with bits = stroke (Paint.round 3) st.pattern p; last = Some p }
      | Eraser -> { st with bits = stroke (Paint.square 10) Pattern.white p; last = Some p })
  | true, Some a -> (
      match st.tool with
      | Fill -> st
      | Pencil -> { st with bits = stroke Paint.pencil st.pattern a; last = Some p }
      | Brush -> { st with bits = stroke (Paint.round 3) st.pattern a; last = Some p }
      | Eraser -> { st with bits = stroke (Paint.square 10) Pattern.white a; last = Some p })
  | false, _ -> { st with last = None }
  | true, None -> st

(* The picture's rectangles, remembered for the few bitmaps drawn
   last (compared by ==, which is right because a bitmap is never
   changed in place, see Bitmap.change) -- and made around (0, 0), so
   that they stay right wherever the part is laid out *)
let drawn : (Bitmap.t * shape list) list ref = ref []

let local_shapes bits =
  match List.find_opt (fun (b, _) -> b == bits) !drawn with
  | Some (_, s) -> s
  | None ->
      let w = float_of_int (Bitmap.width bits) *. zoom and h = float_of_int (Bitmap.height bits) *. zoom in
      let s =
        List.map
          (fun (x, y, rw, rh) ->
            rectangle black (float_of_int rw *. zoom) (float_of_int rh *. zoom)
            |> move
                 ((-.w /. 2.) +. ((float_of_int x +. (float_of_int rw /. 2.)) *. zoom))
                 ((h /. 2.) -. ((float_of_int y +. (float_of_int rh /. 2.)) *. zoom)))
          (Bitmap.rectangles bits)
      in
      drawn := List.filteri (fun i _ -> i < 7) ((bits, s) :: !drawn);
      s

let draw st (b : Widget.box) ~active:_ =
  let w, h = size st in
  let l, _ = left_top st b in
  [ group ((rectangle white w h :: local_shapes st.bits) @ Gui.shapes (Widget.frame black 1. { Widget.x = 0.; y = 0.; w; h })) |> move (l +. (w /. 2.)) b.y ]

let command c st =
  match c with
  | "Pencil" -> { st with tool = Pencil }
  | "Brush" -> { st with tool = Brush }
  | "Eraser" -> { st with tool = Eraser }
  | "Fill" -> { st with tool = Fill }
  | "Black" -> { st with pattern = Pattern.solid }
  | "Grey" -> { st with pattern = Pattern.grey }
  | "Bricks" -> { st with pattern = List.nth Pattern.palette 10 }
  | _ -> st

let rec part st : Component.part =
  {
    kind;
    height = (fun _ -> snd (size st));
    (* so many dots, two pixels each: a size of its own, to scale *)
    natural = Some (size st);
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = [ "Picture"; "Pencil"; "Brush"; "Eraser"; "Fill"; "Black"; "Grey"; "Bricks" ];
    command = (fun c -> part (command c st));
    save = (fun () -> Bitmap.to_string st.bits);
  }

let make bits = part { bits; tool = Brush; pattern = Pattern.solid; last = None }
let load s = make (Bitmap.of_string s)
