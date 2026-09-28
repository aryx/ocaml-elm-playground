(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Indexed.mli *)

type t = { width : int; height : int; pixels : Bytes.t }

let create w h colour = { width = w; height = h; pixels = Bytes.make (w * h) (Char.chr colour) }
let get (t : t) x y = if x < 0 || y < 0 || x >= t.width || y >= t.height then 0 else Char.code (Bytes.get t.pixels ((y * t.width) + x))

let change (t : t) (f : t -> unit) : t =
  let c = { t with pixels = Bytes.copy t.pixels } in
  f c;
  c

let dot (t : t) x y colour = if x >= 0 && y >= 0 && x < t.width && y < t.height then Bytes.set t.pixels ((y * t.width) + x) (Char.chr colour)

type brush = Dots of (int * int) list | Piece of t * int

let round r = Dots (List.concat (List.init ((2 * r) + 1) (fun j -> List.filter_map (fun i -> let dx = i - r and dy = j - r in if (dx * dx) + (dy * dy) <= (r * r) + r then Some (dx, dy) else None) (List.init ((2 * r) + 1) Fun.id))))
let square n = Dots (List.concat (List.init n (fun j -> List.init n (fun i -> (i - (n / 2), j - (n / 2))))))

let stamp (t : t) (b : brush) colour ((x, y) : int * int) =
  match b with
  | Dots ds -> List.iter (fun (dx, dy) -> dot t (x + dx) (y + dy) colour) ds
  | Piece (p, transparent) ->
      (* its own colours, but the transparent one *)
      for j = 0 to p.height - 1 do
        for i = 0 to p.width - 1 do
          let c = get p i j in
          if c <> transparent then dot t (x + i - (p.width / 2)) (y + j - (p.height / 2)) c
        done
      done

(* Bresenham's: the error of the ideal line kept, a step along the
   longer axis each time, a step along the other when the error says *)
let line_dots ((x0, y0) : int * int) ((x1, y1) : int * int) : (int * int) list =
  let dx = abs (x1 - x0) and dy = -abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 and sy = if y0 < y1 then 1 else -1 in
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

let line t b colour a z = List.iter (stamp t b colour) (line_dots a z)

let order (x0, y0) (x1, y1) = (min x0 x1, min y0 y1, max x0 x1, max y0 y1)

let fill_rect t colour a z =
  let l, top, r, b = order a z in
  for y = top to b do
    for x = l to r do
      dot t x y colour
    done
  done

let frame_rect t colour a z =
  let l, top, r, b = order a z in
  line t (Dots [ (0, 0) ]) colour (l, top) (r, top);
  line t (Dots [ (0, 0) ]) colour (l, b) (r, b);
  line t (Dots [ (0, 0) ]) colour (l, top) (l, b);
  line t (Dots [ (0, 0) ]) colour (r, top) (r, b)

(* the dots of the ellipse inscribed in the rectangle: a row at a time,
   from its half-width at that height *)
let ellipse_rows a z : (int * int * int) list =
  let l, top, r, b = order a z in
  let cx = float_of_int (l + r) /. 2. and cy = float_of_int (top + b) /. 2. in
  let rx = Float.max 0.5 (float_of_int (r - l) /. 2.) and ry = Float.max 0.5 (float_of_int (b - top) /. 2.) in
  List.init (b - top + 1) (fun j ->
      let y = top + j in
      let dy = (float_of_int y -. cy) /. ry in
      let half = rx *. sqrt (Float.max 0. (1. -. (dy *. dy))) in
      (y, int_of_float (Float.round (cx -. half)), int_of_float (Float.round (cx +. half))))

let fill_ellipse t colour a z = List.iter (fun (y, x0, x1) -> for x = x0 to x1 do dot t x y colour done) (ellipse_rows a z)

let frame_ellipse t colour a z =
  (* each row's ends, joined to the row before's so the outline has no gaps *)
  let rows = ellipse_rows a z in
  List.iteri
    (fun i (y, x0, x1) ->
      match List.nth_opt rows (i - 1) with
      | Some (py, px0, px1) ->
          line t (Dots [ (0, 0) ]) colour (px0, py) (x0, y);
          line t (Dots [ (0, 0) ]) colour (px1, py) (x1, y)
      | None -> for x = x0 to x1 do dot t x y colour done)
    rows;
  match List.rev rows with (y, x0, x1) :: _ -> for x = x0 to x1 do dot t x y colour done | [] -> ()

let fill (t : t) colour ((x, y) : int * int) =
  let target = get t x y in
  if target <> colour && x >= 0 && y >= 0 && x < t.width && y < t.height then begin
    let stack = ref [ (x, y) ] in
    while !stack <> [] do
      match !stack with
      | (x, y) :: rest ->
          stack := rest;
          if x >= 0 && y >= 0 && x < t.width && y < t.height && get t x y = target then begin
            dot t x y colour;
            stack := (x + 1, y) :: (x - 1, y) :: (x, y + 1) :: (x, y - 1) :: !stack
          end
      | [] -> ()
    done
  end

let cut (t : t) a z =
  let l, top, r, b = order a z in
  let w = r - l + 1 and h = b - top + 1 in
  let p = create w h 0 in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      dot p i j (get t (l + i) (top + j))
    done
  done;
  p

let symmetric ~(order : int) ~(centre : int * int) ((x, y) : int * int) : (int * int) list =
  let cx, cy = centre in
  List.init order (fun k ->
      let a = 2. *. Float.pi *. float_of_int k /. float_of_int order in
      let dx = float_of_int (x - cx) and dy = float_of_int (y - cy) in
      (cx + int_of_float (Float.round ((dx *. cos a) -. (dy *. sin a))), cy + int_of_float (Float.round ((dx *. sin a) +. (dy *. cos a)))))
