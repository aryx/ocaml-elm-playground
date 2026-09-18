(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Triangle.mli *)

type fill_rule = Epsilon | Top_left

(* claude: sub-pixel precision for the top-left rule: a coordinate
 * rounded to the nearest 1/256th of a pixel, so that the edge function
 * values below are computed exactly (see Triangle.mli) *)
let snap (x : float) : float = Float.round (x *. 256.) /. 256.

let fill (fb : Framebuffer.t) ~(fill_rule : fill_rule) ~(zbuffer : Zbuffer.t option)
    ~(interpolation : Interpolate.mode) ~(shading : Shading.mode)
    ~(color : u:float -> v:float -> brightness:float -> int) (v0 : Project.vertex) (v1 : Project.vertex)
    (v2 : Project.vertex) : unit =
  let edge (ax, ay) (bx, by) (px, py) = ((bx -. ax) *. (py -. ay)) -. ((by -. ay) *. (px -. ax)) in
  let point (v : Project.vertex) = match fill_rule with Epsilon -> (v.vx, v.vy) | Top_left -> (snap v.vx, snap v.vy) in
  let ((x0, y0) as p0) = point v0 and ((x1, y1) as p1) = point v1 and ((x2, y2) as p2) = point v2 in
  let min_x = max 0 (int_of_float (Float.round (Stdlib.min x0 (Stdlib.min x1 x2)))) in
  let max_x = min (fb.width - 1) (int_of_float (Float.round (Stdlib.max x0 (Stdlib.max x1 x2)))) in
  let min_y = max 0 (int_of_float (Float.round (Stdlib.min y0 (Stdlib.min y1 y2)))) in
  let max_y = min (fb.height - 1) (int_of_float (Float.round (Stdlib.max y0 (Stdlib.max y1 y2)))) in
  let area = edge p0 p1 p2 in
  (* claude: bugfix -- was a strict ">= 0."/"<= 0." test here, which is
   * exactly correct in real-number math but not in floating point, and
   * caused a visible bug: a rectangular face (e.g. one face of a
   * `box`) is always split into 2 triangles sharing a diagonal edge
   * (see fan_triangles), and for a pixel sitting exactly on that
   * shared edge, both triangles compute an edge-function value that is
   * mathematically exactly 0 -- so with a strict ">= 0." test, *both*
   * triangles would consider that pixel "inside" and draw it (harmless
   * double-drawing, not a bug). In practice, floating-point rounding
   * (the two triangles reach that shared edge via different vertex
   * triples, e.g. edge p1-p2 for one triangle vs. edge p0-p2 for
   * dealing with the same physical line, so the arithmetic isn't
   * bit-for-bit identical) can nudge the computed value to something
   * like -1e-10 instead of exactly 0 for *both* triangles at once, at
   * that same pixel -- so *neither* draws it, leaving a 1-pixel-wide
   * gap exactly along the diagonal. This is a well-known rasterizer
   * artifact usually called a "crack" or "T-junction gap". It's
   * angle-dependent (only shows up for the specific projected
   * orientations where rounding happens to tip a shared-edge value
   * across zero), which is why it only appeared "sometimes, when the
   * camera moves" instead of being reliably reproducible on both
   * sides.
   *
   * The fix: nudge the boundary very slightly towards "inside" (an
   * epsilon tolerance) instead of testing against exactly 0, so a
   * shared edge is now *reliably* inside for both triangles even after
   * rounding error, trading a theoretical, invisible sub-pixel amount
   * of double-drawing for the elimination of the gap. (Real GPU
   * rasterizers instead use a "top-left fill rule" -- a tie-breaking
   * convention that assigns each shared-edge pixel to exactly one of
   * the two triangles, so there is neither a gap nor double-drawing at
   * all -- a bit more bookkeeping, below, with [Top_left], the "t"
   * key.) *)
  let epsilon = 1e-4 in
  (* claude: the top-left rule. With area > 0 (a triangle clockwise on
   * the screen, y going down), the interior is on the positive side of
   * each edge a -> b, and the edge is a "top" edge when it goes right
   * horizontally (the triangle below it), a "left" edge when it goes
   * up (the triangle to its right); with area < 0, the same after
   * reversing the edge. Decided once per edge, per triangle. *)
  let top_left (ax, ay) (bx, by) =
    let dx = bx -. ax and dy = by -. ay in
    let dx, dy = if area > 0. then (dx, dy) else (-.dx, -.dy) in
    dy < 0. || (dy = 0. && dx > 0.)
  in
  let tl0 = top_left p1 p2 and tl1 = top_left p2 p0 and tl2 = top_left p0 p1 in
  (* on the interior side of an edge, or exactly on it if it's a top or
   * left one *)
  let covers w tl = if area > 0. then w > 0. || (w = 0. && tl) else w < 0. || (w = 0. && tl) in
  (* claude: perf -- barycentric coordinates are "w / area" for each of
   * w0/w1/w2 (3 divisions per pixel); computing 1/area once here and
   * multiplying by it instead (inv_area, 1 division total + 3 cheaper
   * multiplications per pixel) is behaviorally identical, just avoids
   * redoing the same division 3 times per pixel. Just an algebraic
   * rewrite of "w /. area" as "w *. (1. /. area)", not a change in what
   * is computed -- feel free to inline it back to "w0 /. area" etc.
   * below if this ever gets in the way of reading the simpler
   * per-pixel math. *)
  let inv_area = 1. /. area in
  (* claude: "decide once per triangle, apply once per pixel": how to
   * interpolate depth/UV (see Interpolate) and how bright each pixel
   * is (see Shading), as closures built once here *)
  let interpolate = Interpolate.make interpolation v0 v1 v2 in
  let shade_pixel = Shading.make shading v0 v1 v2 in
  (* one pixel, given its 3 edge function values *)
  let pixel px py w0 w1 w2 =
    let inside =
      match fill_rule with
      | Epsilon ->
          if area > 0. then w0 >= -.epsilon && w1 >= -.epsilon && w2 >= -.epsilon
          else w0 <= epsilon && w1 <= epsilon && w2 <= epsilon
      | Top_left -> covers w0 tl0 && covers w1 tl1 && covers w2 tl2
    in
    if inside then begin
      let l0 = w0 *. inv_area and l1 = w1 *. inv_area and l2 = w2 *. inv_area in
      let (z, u, v) = interpolate ~l0 ~l1 ~l2 in
      (* without a z-buffer (the painter's algorithm), the "z" part
       * of the interpolation isn't needed, there's nothing to
       * compare it with *)
      let visible = match zbuffer with None -> true | Some zbuffer -> Zbuffer.test_and_set zbuffer ~x:px ~y:py z in
      if visible then begin
        let brightness = shade_pixel ~l0 ~l1 ~l2 in
        Framebuffer.plot fb ~x:px ~y:py ~rgb:(color ~u ~v ~brightness) ~alpha:1.
      end
    end
  in
  if area <> 0. then
    if not !Opti.enabled then
      (* the simple version: the 3 edge functions, at each pixel center *)
      for py = min_y to max_y do
        for px = min_x to max_x do
          let p = (float_of_int px +. 0.5, float_of_int py +. 0.5) in
          pixel px py (edge p1 p2 p) (edge p2 p0 p) (edge p0 p1 p)
        done
      done
    else begin
      (* claude: optimization (Opti.enabled): incremental edge functions.
       * An edge function (bx - ax) * (py - ay) - (by - ay) * (px - ax)
       * is linear in px: one pixel to the right, it changes by the
       * constant -(by - ay). So compute the 3 values at the first pixel
       * of each row, then just add. No multiplication per pixel, and no
       * allocation (the simple version builds a (px, py) pair per
       * pixel). The same values as the simple version with the top-left
       * rule (exact multiples of 1/65536, see Triangle.mli: the
       * additions are exact too); with the epsilon, they can differ in
       * the last bits, after many additions along a long row, which the
       * epsilon absorbs. *)
      let step0 = -.(y2 -. y1) and step1 = -.(y0 -. y2) and step2 = -.(y1 -. y0) in
      for py = min_y to max_y do
        let p = (float_of_int min_x +. 0.5, float_of_int py +. 0.5) in
        let w0 = ref (edge p1 p2 p) and w1 = ref (edge p2 p0 p) and w2 = ref (edge p0 p1 p) in
        for px = min_x to max_x do
          pixel px py !w0 !w1 !w2;
          w0 := !w0 +. step0;
          w1 := !w1 +. step1;
          w2 := !w2 +. step2
        done
      done
    end

let outline (fb : Framebuffer.t) ~(rgb : int) (v0 : Project.vertex) (v1 : Project.vertex) (v2 : Project.vertex) : unit
    =
  let p0 = (v0.vx, v0.vy) and p1 = (v1.vx, v1.vy) and p2 = (v2.vx, v2.vy) in
  Line.draw fb p0 p1 ~rgb ~alpha:1.;
  Line.draw fb p1 p2 ~rgb ~alpha:1.;
  Line.draw fb p2 p0 ~rgb ~alpha:1.
