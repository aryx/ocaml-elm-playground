(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fill.mli for the idea and the references *)

type fill_rule = Nonzero | Even_odd

(*****************************************************************************)
(* Edges *)
(*****************************************************************************)

(* An edge of the polygon, from the point of view of the rows it
 * crosses. Each row y is sampled at the height of its pixel centers,
 * y + 0.5, so an edge from height top to height bottom crosses the rows
 * whose center is in [top, bottom): from row ceil(top - 0.5) included
 * to row ceil(bottom - 0.5) excluded. For example an edge from y = 1.2
 * to y = 3.9 crosses rows 1, 2, 3 (centers 1.5, 2.5, 3.5).
 *
 * That the range is half-open matters at the corners: where two edges
 * meet, at a height that happens to be a row's center, exactly one of
 * the two counts the crossing. Counting it twice or zero times would
 * flip inside/outside for the rest of that row. *)
type edge = {
  first_row : int;
  end_row : int; (* excluded *)
  (* x where the edge crosses the current row (starting at first_row) *)
  mutable x : float;
  (* how much x moves from one row to the next: dx/dy *)
  slope : float;
  (* +1 if the edge goes down (y increasing), -1 if it goes up *)
  winding : int;
}

(* The first pixel, going right (or down), whose center is at or after
 * the coordinate [v]: pixel n spans [n, n+1), its center is n + 0.5,
 * so e.g. first_pixel 1.2 = 1 (center 1.5) and first_pixel 1.7 = 2 *)
let first_pixel (v : float) : int = int_of_float (Float.ceil (v -. 0.5))

(* The edge from (x0, y0) to (x1, y1), clipped to the rows [0, height);
 * None if it crosses no row: horizontal edges, for instance, never
 * cross a row, the edges before and after them do. *)
let make_edge ~height (x0, y0) (x1, y1) : edge option =
  let winding = if y1 > y0 then 1 else -1 in
  (* work from top to bottom whatever the edge's direction *)
  let (xt, yt), (_xb, yb) = if y0 < y1 then ((x0, y0), (x1, y1)) else ((x1, y1), (x0, y0)) in
  let first_row = max 0 (first_pixel yt) in
  let end_row = min height (first_pixel yb) in
  if first_row >= end_row then None
  else
    let slope = (x1 -. x0) /. (y1 -. y0) in
    (* x at the center of first_row, on the line through the two points *)
    let x = xt +. ((float first_row +. 0.5 -. yt) *. slope) in
    Some { first_row; end_row; x; slope; winding }

(* The polygon's edges: from each point to the next, and from the last
 * back to the first *)
let edges_of_polygon ~height (points : (float * float) list) : edge list =
  match points with
  | [] -> []
  | first :: _ ->
      let rec loop acc = function
        | p :: (q :: _ as rest) -> loop (make_edge ~height p q :: acc) rest
        | [ last ] -> make_edge ~height last first :: acc
        | [] -> acc
      in
      List.filter_map Fun.id (loop [] points)

(*****************************************************************************)
(* Scanlines *)
(*****************************************************************************)

let is_inside (rule : fill_rule) (winding : int) : bool =
  match rule with
  | Nonzero -> winding <> 0
  (* the parity of the winding number is the parity of the number of
   * crossings, since each crossing adds or removes 1 *)
  | Even_odd -> winding land 1 = 1

(* The spans of one row, given the edges crossing it sorted by x: walk
 * from left to right keeping the winding number of the current
 * position; each time we go from outside to inside, a span starts, and
 * it ends when we go back outside. For the "U" in Fill.mli with edges
 * going down on the left and up on the right, the winding number goes
 * 0 -> 1 -> 0 -> 1 -> 0 at x = 1, 3, 7, 9, giving the spans [1, 3) and
 * [7, 9). *)
let spans_of_row ~rule (crossings : edge list) : (float * float) list =
  let _winding, _span_start, spans =
    List.fold_left
      (fun (winding, span_start, spans) (e : edge) ->
        let winding' = winding + e.winding in
        match (is_inside rule winding, is_inside rule winding') with
        | false, true -> (winding', e.x, spans)
        | true, false -> (winding', e.x, (span_start, e.x) :: spans)
        | _ -> (winding', span_start, spans))
      (0, 0., []) crossings
  in
  List.rev spans

let scan ?(rule = Nonzero) ~height (contours : (float * float) list list) ~on_span =
  (* the "edge table": all the edges, by the row where they start *)
  let edges =
    List.concat (List.map (edges_of_polygon ~height) contours)
    |> List.sort (fun (e1 : edge) e2 -> compare e1.first_row e2.first_row)
  in
  match edges with
  | [] -> ()
  | first :: _ ->
      let last_row = List.fold_left (fun acc (e : edge) -> max acc e.end_row) 0 edges in
      (* going down row by row, [active] is the "active edge list", the
       * edges crossing the current row, and [pending] the edges below
       * it, not reached yet *)
      let rec loop y active pending =
        if y < last_row then begin
          let starting, pending = List.partition (fun (e : edge) -> e.first_row = y) pending in
          let active = List.filter (fun (e : edge) -> e.end_row > y) (starting @ active) in
          let crossings = List.sort (fun (e1 : edge) e2 -> compare e1.x e2.x) active in
          List.iter (fun (xa, xb) -> on_span ~y xa xb) (spans_of_row ~rule crossings);
          (* edge coherence: on the next row, each edge's crossing is
           * [slope] further, no need to intersect lines again *)
          List.iter (fun (e : edge) -> e.x <- e.x +. e.slope) active;
          loop (y + 1) active pending
        end
      in
      loop first.first_row [] edges

(*****************************************************************************)
(* Filling: pixel centers *)
(*****************************************************************************)

(* A span from xa to xb covers the pixels whose center is in [xa, xb),
 * the same rule as for rows *)
let polygons ?rule (fb : Framebuffer.t) contours ~rgb ~alpha =
  scan ?rule ~height:fb.height contours ~on_span:(fun ~y xa xb ->
      Framebuffer.fill_span fb ~y ~x0:(first_pixel xa) ~x1:(first_pixel xb) ~rgb ~alpha)

let polygon ?rule fb points ~rgb ~alpha = polygons ?rule fb [ points ] ~rgb ~alpha

(*****************************************************************************)
(* Filling with antialiasing: pixel coverage *)
(*****************************************************************************)

(* How much of pixel x the span [xa, xb) covers horizontally, from 0 to
 * 1; e.g. [0.5, 2.5) covers half of pixel 0, all of pixel 1, and half
 * of pixel 2 *)
let overlap (xa, xb) x = Float.max 0. (Float.min xb (float (x + 1)) -. Float.max xa (float x))

(* The original, simple version: a coverage array for the current pixel
 * row, where each sub-row's span adds its overlap to every pixel it
 * touches, one by one; then each pixel is plotted with its coverage.
 * Easy to follow, but a span across a 1000-pixel-wide window costs
 * 1000 additions per sub-row, and 1000 plots per row. *)
let polygons_aa_simple ?rule ?(subrows = 4) (fb : Framebuffer.t) contours ~rgb ~alpha =
  let n = float subrows in
  let coverage = Array.make fb.width 0. in
  let row = ref (-1) in
  let flush () =
    if !row >= 0 then
      for x = 0 to fb.width - 1 do
        if coverage.(x) > 0. then begin
          Framebuffer.plot fb ~x ~y:!row ~rgb ~alpha:(alpha *. Float.min 1. coverage.(x));
          coverage.(x) <- 0.
        end
      done
  in
  let stretched = List.map (List.map (fun (x, y) -> (x, y *. n))) contours in
  scan ?rule ~height:(fb.height * subrows) stretched ~on_span:(fun ~y:subrow xa xb ->
      let y = subrow / subrows in
      if y <> !row then begin
        flush ();
        row := y
      end;
      for x = max 0 (int_of_float (Float.floor xa)) to min (fb.width - 1) (int_of_float (Float.ceil xb) - 1) do
        coverage.(x) <- coverage.(x) +. (overlap (xa, xb) x /. n)
      done);
  flush ()

(* claude: optimization (Opti.enabled), what polygons_aa does instead.
 *
 * Coverage is accumulated per pixel row, over its sub-rows, as a list
 * of "cells", so that adding a span costs the same whatever its length.
 * A cell is a pixel x where something changes:
 *
 * - [partial]: coverage of pixel x alone, for the span's two end
 *   pixels, e.g. [0.5, 2.5) gives 0.5 to pixels 0 and 2 (divided by the
 *   number of sub-rows);
 * - [step]: full coverage starting (+1) or stopping (-1) at x, for the
 *   pixels in between: [0.5, 2.5) gives +1 at x = 1 and -1 at x = 2.
 *
 * At the end of the row, walking the cells from left to right with a
 * running sum of the steps gives every pixel's coverage: [partial] for
 * the cells' pixels, plus the running sum, which stays the same between
 * two cells -- so everything between two cells is one span of one
 * coverage, e.g. the 800 fully covered pixels inside a big rectangle.
 * A row costs a few cells per span, not one visit per pixel.
 *
 * (The "difference array" trick, kept sparse; the same idea as the cell
 * lists of libart and Anti-Grain Geometry, and font-rs's accumulation
 * buffer.) *)
type cell = { x : int; partial : float; step : float }

let add_span (cells : cell list ref) ~weight ~width xa xb =
  let xa = Float.max 0. xa and xb = Float.min (float width) xb in
  if xa < xb then begin
    let add x ~partial ~step = cells := { x; partial; step } :: !cells in
    let ia = int_of_float (Float.floor xa) and ib = int_of_float (Float.floor xb) in
    if ia = ib then
      (* the whole span within one pixel *)
      add ia ~partial:((xb -. xa) *. weight) ~step:0.
    else begin
      (* the left end pixel, from xa to its right side *)
      add ia ~partial:((float (ia + 1) -. xa) *. weight) ~step:0.;
      (* the right end pixel, from its left side to xb *)
      if ib < width then add ib ~partial:((xb -. float ib) *. weight) ~step:0.;
      (* everything in between, fully *)
      add (ia + 1) ~partial:0. ~step:weight;
      add ib ~partial:0. ~step:(-.weight)
    end
  end

(* Paint row y from its cells, left to right *)
let paint_row (fb : Framebuffer.t) (cells : cell list) ~y ~rgb ~alpha =
  let paint x0 x1 coverage =
    if x0 < x1 && coverage > 0. then
      Framebuffer.fill_span fb ~y ~x0 ~x1 ~rgb ~alpha:(alpha *. Float.min 1. coverage)
  in
  (* [full]: the running sum of steps; pixels from [next] on haven't been
   * painted yet *)
  let rec walk full next = function
    | [] -> ()
    | { x; _ } :: _ as cells ->
        (* all the cells of pixel x together *)
        let here, rest = List.partition (fun c -> c.x = x) cells in
        let partial = List.fold_left (fun acc c -> acc +. c.partial) 0. here in
        let step = List.fold_left (fun acc c -> acc +. c.step) 0. here in
        (* up to x, nothing changed: one span *)
        paint next x full;
        let full = full +. step in
        if partial <> 0. then begin
          paint x (x + 1) (partial +. full);
          walk full (x + 1) rest
        end
        else walk full x rest
  in
  walk 0. 0 (List.sort (fun c1 c2 -> compare c1.x c2.x) cells)

let polygons_aa_sparse ?rule ?(subrows = 4) (fb : Framebuffer.t) contours ~rgb ~alpha =
  let cells = ref [] in
  let weight = 1. /. float subrows in
  let row = ref (-1) in
  let flush () =
    if !row >= 0 then paint_row fb !cells ~y:!row ~rgb ~alpha;
    cells := []
  in
  (* the polygon stretched [subrows] times vertically: its rows are our
   * sub-rows, sampled at (k + 0.5) / subrows within each pixel row *)
  let stretched = List.map (List.map (fun (x, y) -> (x, y *. float subrows))) contours in
  scan ?rule ~height:(fb.height * subrows) stretched ~on_span:(fun ~y:subrow xa xb ->
      let y = subrow / subrows in
      if y <> !row then begin
        flush ();
        row := y
      end;
      add_span cells ~weight ~width:fb.width xa xb);
  flush ()

let polygons_aa ?rule ?subrows fb contours ~rgb ~alpha =
  if !Opti.enabled then polygons_aa_sparse ?rule ?subrows fb contours ~rgb ~alpha
  else polygons_aa_simple ?rule ?subrows fb contours ~rgb ~alpha
