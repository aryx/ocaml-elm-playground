(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_labels.mli *)

type kind = Tab | Landmark | Capital | City | Def | Section

type label = {
  kind : kind;
  text : string;
  x : float;
  y : float;
  left : bool;
  px : float;
  rank : float;
  from_level : float;
  to_level : float;
  fw : float;
  fh : float;
  color : int * int * int;
  mutable minz : float;
  mutable maxz : float;
}

let label kind text ~x ~y ?(left = true) ~px ~rank ~from_level ~to_level ~fw ~fh color =
  { kind; text; x; y; left; px; rank; from_level; to_level; fw; fh; color; minz = Float.infinity; maxz = 0. }

(* as the map's words are drawn: half their height a character; a tab a
 * little bigger, its margins *)
let size (l : label) : float * float =
  let w = 0.5 *. l.px *. float_of_int (String.length l.text) in
  match l.kind with Tab | Landmark -> (w +. 8., l.px +. 6.) | _ -> (w, l.px)

(* the room kept round a label: the map's density *)
let margin_x = 22.
let margin_y = 10.

(* its box at zoom z, in the pixels of the whole layout at that zoom *)
let box (l : label) (z : float) : float * float * float * float =
  let w, h = size l in
  let x = l.x *. z and y = l.y *. z in
  let x0 = if l.left then x else x -. (w /. 2.) and y0 = match l.kind with Tab | Landmark -> y | _ -> y -. (h /. 2.) in
  (x0 -. margin_x, y0 -. margin_y, x0 +. w +. margin_x, y0 +. h +. margin_y)

let eligible (l : label) (z : float) (lv : float) : bool =
  lv >= l.from_level && lv < l.to_level
  &&
  let w, h = size l in
  match l.kind with
  | Tab -> l.fw *. z >= w +. 6. && l.fh *. z >= h *. 1.8
  | Def | Section | City -> l.fw *. z > 40.
  | Landmark | Capital -> true

(* claude: the boxes placed at a zoom, found by a grid of cells: a box is
 * in every cell it covers *)
let cell = 96.

let place ~(level : float -> float) ~(zmin : float) ~(zmax : float) (labels : label array) : unit =
  Array.iter (fun l -> l.minz <- Float.infinity; l.maxz <- 0.) labels;
  let order = Array.copy labels in
  Array.stable_sort (fun a b -> compare b.rank a.rank) order;
  let steps = 64 in
  let kept = ref [] in
  for i = 0 to steps - 1 do
    let z = zmin *. ((zmax /. zmin) ** (float_of_int i /. float_of_int (steps - 1))) in
    let lv = level z in
    let grid = Hashtbl.create 1024 in
    let cells (x0, y0, x1, y1) f =
      for cx = int_of_float (Float.floor (x0 /. cell)) to int_of_float (Float.floor (x1 /. cell)) do
        for cy = int_of_float (Float.floor (y0 /. cell)) to int_of_float (Float.floor (y1 /. cell)) do
          f (cx, cy)
        done
      done
    in
    let free b =
      let ok = ref true in
      let a0, b0, a1, b1 = b in
      cells b (fun k -> List.iter (fun (c0, d0, c1, d1) -> if a0 < c1 && c0 < a1 && b0 < d1 && d0 < b1 then ok := false) (Hashtbl.find_all grid k));
      !ok
    in
    let add b = cells b (fun k -> Hashtbl.add grid k b) in
    let now = ref [] in
    let taken l = l.maxz = z in
    (* the labels kept, in their places: they do not overlap, zooming in
     * spreads them *)
    List.iter
      (fun l ->
        if eligible l z lv then begin
          add (box l z);
          l.maxz <- z;
          now := l :: !now
        end)
      !kept;
    Array.iter
      (fun l ->
        if (not (taken l)) && eligible l z lv then begin
          let b = box l z in
          if free b then begin
            add b;
            if l.minz = Float.infinity then l.minz <- z;
            l.maxz <- z;
            now := l :: !now
          end
        end)
      order;
    kept := !now
  done

(* claude: a zoom step is (zmax/zmin)^(1/63); a label fades over a sixth
 * of the zooms around its limits, a factor of 1.12 *)
let fade = 1.12

let alpha (l : label) (z : float) : float =
  if l.minz = Float.infinity then 0.
  else
    let up = Float.log (z /. (l.minz /. fade)) /. Float.log fade and down = Float.log ((l.maxz *. fade) /. z) /. Float.log fade in
    Float.max 0. (Float.min 1. (Float.min up down))
