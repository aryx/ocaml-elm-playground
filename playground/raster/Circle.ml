(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Circle.mli for the algorithms, with examples *)

(*****************************************************************************)
(* Midpoint circle *)
(*****************************************************************************)

(* The decision variable d is "is the midpoint inside the circle?",
 *   f(x, y) = x^2 + y^2 - r^2  (< 0 inside, > 0 outside)
 * at the next midpoint (x+1, y-1/2). Starting at (0, r), that's
 *   f(1, r - 1/2) = 1 + r^2 - r + 1/4 - r^2 = 5/4 - r
 * rounded to 1 - r (d only matters through its sign, and it's always
 * an integer + 1/4 afterwards, so dropping the 1/4 changes nothing).
 * After each step, d moves to the next midpoint, by expanding the
 * squares:
 *   stayed at y:      f(x+2, y-1/2) - f(x+1, y-1/2) = 2x + 3
 *   went down to y-1: f(x+2, y-3/2) - f(x+1, y-1/2) = 2(x - y) + 5 *)
let octant (r : int) : (int * int) list =
  let rec loop x y d acc =
    if x > y then List.rev acc
    else
      let acc = (x, y) :: acc in
      if d < 0 then loop (x + 1) y (d + (2 * x) + 3) acc
      else loop (x + 1) (y - 1) (d + (2 * (x - y)) + 5) acc
  in
  loop 0 r (1 - r) []

(* [half_widths r].(dy) is how far the circle extends left and right of
 * the center on the row dy rows above (or below) it. Each octant pixel
 * (x, y) gives it for two rows: row y (x wide), and, by the diagonal
 * symmetry, row x (y wide). For r = 5, from the octant
 * [(0, 5); (1, 5); (2, 5); (3, 4)]: rows 0..5 have half widths
 * 5, 5, 5, 4, 3, 2. *)
let half_widths (r : int) : int array =
  let widths = Array.make (r + 1) 0 in
  octant r
  |> List.iter (fun (x, y) ->
         widths.(y) <- max widths.(y) x;
         widths.(x) <- max widths.(x) y);
  widths

let fill (fb : Framebuffer.t) ~cx ~cy ~r ~rgb ~alpha =
  let widths = half_widths r in
  for dy = -r to r do
    let w = widths.(abs dy) in
    (* one span per row, so no pixel is painted twice (which would show
     * with alpha < 1) *)
    Framebuffer.fill_span fb ~y:(cy + dy) ~x0:(cx - w) ~x1:(cx + w + 1) ~rgb ~alpha
  done

let outline (fb : Framebuffer.t) ~cx ~cy ~r ~rgb ~alpha =
  octant r
  |> List.iter (fun (x, y) ->
         (* the 8 mirror images; (x, y) has y up, the framebuffer y down *)
         [ (x, y); (-x, y); (x, -y); (-x, -y); (y, x); (-y, x); (y, -x); (-y, -x) ]
         |> List.iter (fun (dx, dy) -> Framebuffer.plot fb ~x:(cx + dx) ~y:(cy - dy) ~rgb ~alpha))

(*****************************************************************************)
(* Ellipses as polygons *)
(*****************************************************************************)

let ellipse_points ~rx ~ry ~segments =
  List.init segments (fun i ->
      let angle = 2. *. Float.pi *. float i /. float segments in
      (rx *. cos angle, ry *. sin angle))

let segments_for_radius ?(tolerance = 0.25) (radius : float) : int =
  (* solve radius * (1 - cos (pi / n)) <= tolerance for n; tiny circles
   * still get a few sides *)
  if radius <= tolerance then 8
  else max 8 (int_of_float (Float.ceil (Float.pi /. acos (1. -. (tolerance /. radius)))))
