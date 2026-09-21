(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let dot b p x y = Bitmap.set b x y (Pattern.black p x y)

let line_dots (x0, y0) (x1, y1) =
  let dx = abs (x1 - x0) and dy = -abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 and sy = if y0 < y1 then 1 else -1 in
  (* err is how far the dot is from the true line, times 2 dx dy *)
  let rec go x y err acc =
    let acc = (x, y) :: acc in
    if x = x1 && y = y1 then List.rev acc
    else
      let e2 = 2 * err in
      let x, err = if e2 >= dy then (x + sx, err + dy) else (x, err) in
      let y, err = if e2 <= dx then (y + sy, err + dx) else (y, err) in
      go x y err acc
  in
  go x0 y0 (dx + dy) []

let stroke b ~brush p a z =
  List.iter (fun (x, y) -> List.iter (fun (dx, dy) -> dot b p (x + dx) (y + dy)) brush) (line_dots a z)

let pencil = [ (0, 0) ]

let round r =
  List.concat_map
    (fun dy -> List.filter_map (fun dx -> if (dx * dx) + (dy * dy) <= (r * r) + r then Some (dx, dy) else None)
                 (List.init ((2 * r) + 1) (fun i -> i - r)))
    (List.init ((2 * r) + 1) (fun i -> i - r))

let square n = List.concat_map (fun dy -> List.init n (fun dx -> (dx - (n / 2), dy - (n / 2)))) (List.init n Fun.id)

(* the corners in order: left <= right, top <= bottom *)
let order (x0, y0) (x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)

let fill_rect b p a z =
  let l, t, r, bo = order a z in
  for y = t to bo do
    for x = l to r do
      dot b p x y
    done
  done

let frame_rect b p a z =
  let l, t, r, bo = order a z in
  for x = l to r do
    dot b p x t;
    dot b p x bo
  done;
  for y = t to bo do
    dot b p l y;
    dot b p r y
  done

(* whether dot (x, y)'s centre is inside the oval of the rectangle *)
let in_oval (l, t, r, bo) x y =
  let a = float_of_int (r - l + 1) /. 2. and b = float_of_int (bo - t + 1) /. 2. in
  let cx = float_of_int l +. a and cy = float_of_int t +. b in
  let u = (float_of_int x +. 0.5 -. cx) /. a and v = (float_of_int y +. 0.5 -. cy) /. b in
  x >= l && x <= r && y >= t && y <= bo && (u *. u) +. (v *. v) <= 1.

let oval_dots edge_only b p a z =
  let box = order a z in
  let l, t, r, bo = box in
  for y = t to bo do
    for x = l to r do
      let inside = in_oval box in
      if inside x y then
        let edge = not (inside (x - 1) y && inside (x + 1) y && inside x (y - 1) && inside x (y + 1)) in
        if edge || not edge_only then dot b p x y
    done
  done

let frame_oval = oval_dots true
let fill_oval = oval_dots false
