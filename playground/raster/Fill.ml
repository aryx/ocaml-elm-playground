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

(* Fill one row, given the edges crossing it sorted by x: walk from left
 * to right keeping the winding number of the current position; each
 * time we go from outside to inside, a span starts, and it ends when we
 * go back outside. For the "U" in Fill.mli with edges going down on the
 * left and up on the right, the winding number goes 0 -> 1 -> 0 -> 1 -> 0
 * at x = 1, 3, 7, 9, giving the spans [1, 3) and [7, 9). A span from
 * xa to xb covers the pixels whose center is in [xa, xb), the same
 * rule as for rows. *)
let fill_row (fb : Framebuffer.t) ~rule ~y ~rgb ~alpha (crossings : edge list) =
  ignore
    (List.fold_left
      (fun (winding, span_start) (e : edge) ->
        let winding' = winding + e.winding in
        match (is_inside rule winding, is_inside rule winding') with
        | false, true -> (winding', e.x)
        | true, false ->
            Framebuffer.fill_span fb ~y ~x0:(first_pixel span_start)
              ~x1:(first_pixel e.x) ~rgb ~alpha;
            (winding', e.x)
        | _ -> (winding', span_start))
      (0, 0.) crossings
      : int * float)

let polygon ?(rule = Nonzero) (fb : Framebuffer.t) (points : (float * float) list) ~rgb ~alpha =
  (* the "edge table": all the edges, by the row where they start *)
  let edges =
    edges_of_polygon ~height:fb.height points
    |> List.sort (fun (e1 : edge) e2 -> compare e1.first_row e2.first_row)
  in
  match edges with
  | [] -> ()
  | first :: _ ->
      let last_row = List.fold_left (fun acc (e : edge) -> max acc e.end_row) 0 edges in
      (* going down row by row, [active] is the "active edge list", the
       * edges crossing the current row, and [pending] the edges below
       * it, not reached yet *)
      let rec scan y active pending =
        if y < last_row then begin
          let starting, pending = List.partition (fun (e : edge) -> e.first_row = y) pending in
          let active = List.filter (fun (e : edge) -> e.end_row > y) (starting @ active) in
          let crossings = List.sort (fun (e1 : edge) e2 -> compare e1.x e2.x) active in
          fill_row fb ~rule ~y ~rgb ~alpha crossings;
          (* edge coherence: on the next row, each edge's crossing is
           * [slope] further, no need to intersect lines again *)
          List.iter (fun (e : edge) -> e.x <- e.x +. e.slope) active;
          scan (y + 1) active pending
        end
      in
      scan first.first_row [] edges
