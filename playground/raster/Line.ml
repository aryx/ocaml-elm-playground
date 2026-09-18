(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Line.mli for the algorithms, with examples *)

(*****************************************************************************)
(* Bresenham *)
(*****************************************************************************)

let bresenham (fb : Framebuffer.t) (x0, y0) (x1, y1) ~rgb ~alpha =
  let dx = abs (x1 - x0) and dy = abs (y1 - y0) in
  (* +1 or -1: which way to go *)
  let step_x = if x1 >= x0 then 1 else -1 and step_y = if y1 >= y0 then 1 else -1 in
  if dx >= dy then begin
    (* mostly horizontal: one pixel per column, like the (0, 0) -> (8, 3)
     * example in Line.mli *)
    let y = ref y0 and error = ref 0 in
    for i = 0 to dx do
      let x = x0 + (i * step_x) in
      if i > 0 then begin
        error := !error + (2 * dy);
        if !error > dx then begin
          y := !y + step_y;
          error := !error - (2 * dx)
        end
      end;
      Framebuffer.plot fb ~x ~y:!y ~rgb ~alpha
    done
  end
  else begin
    (* mostly vertical: the same, x and y swapped *)
    let x = ref x0 and error = ref 0 in
    for i = 0 to dy do
      let y = y0 + (i * step_y) in
      if i > 0 then begin
        error := !error + (2 * dx);
        if !error > dy then begin
          x := !x + step_x;
          error := !error - (2 * dy)
        end
      end;
      Framebuffer.plot fb ~x:!x ~y ~rgb ~alpha
    done
  end

(*****************************************************************************)
(* Cohen-Sutherland clipping *)
(*****************************************************************************)

let left = 1
let right = 2
let above = 4
let below = 8

let region_code ~width ~height (x, y) =
  (if x < 0. then left else if x > width then right else 0)
  lor if y < 0. then above else if y > height then below else 0

let clip ~width ~height p0 p1 =
  let code = region_code ~width ~height in
  let rec loop ((x0, y0) as p0) ((x1, y1) as p1) =
    let c0 = code p0 and c1 = code p1 in
    if c0 lor c1 = 0 then Some (p0, p1)
    else if c0 land c1 <> 0 then None
    else begin
      (* move an endpoint that is outside onto the line it's beyond;
       * the new point is on the segment, found by proportion, e.g. for
       * the line x = 0: y = y0 + (y1 - y0) * (0 - x0) / (x1 - x0) *)
      let c = if c0 <> 0 then c0 else c1 in
      let at_x x = (x, y0 +. ((y1 -. y0) *. (x -. x0) /. (x1 -. x0))) in
      let at_y y = (x0 +. ((x1 -. x0) *. (y -. y0) /. (y1 -. y0)), y) in
      let p =
        if c land left <> 0 then at_x 0.
        else if c land right <> 0 then at_x width
        else if c land above <> 0 then at_y 0.
        else at_y height
      in
      if c = c0 then loop p p1 else loop p0 p
    end
  in
  loop p0 p1

(*****************************************************************************)
(* Both *)
(*****************************************************************************)

let draw (fb : Framebuffer.t) p0 p1 ~rgb ~alpha =
  (* clip to a hair inside the right and bottom borders, so that the
   * pixel containing a clipped end is always a real one: x = width
   * exactly would be in the column just after the last *)
  let width = float fb.width -. 0.001 and height = float fb.height -. 0.001 in
  match clip ~width ~height p0 p1 with
  | None -> ()
  | Some ((x0, y0), (x1, y1)) ->
      let pixel v = int_of_float (Float.floor v) in
      bresenham fb (pixel x0, pixel y0) (pixel x1, pixel y1) ~rgb ~alpha
