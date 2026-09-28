(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Table_layout.mli *)

type cell = { element : Dom.element; row : int; column : int; span : int }

let children (e : Dom.element) : Dom.element list =
  List.filter_map (fun (n : Dom.node) -> match n with Element c -> Some c | Text _ -> None) e.children

(*****************************************************************************)
(* The grid *)
(*****************************************************************************)

(* the rows, looked for through <thead>, <tbody>, <tfoot> *)
let rows (table : Dom.element) : Dom.element list =
  List.concat_map
    (fun (c : Dom.element) ->
      match c.name with
      | "tr" -> [ c ]
      | "thead" | "tbody" | "tfoot" -> List.filter (fun (r : Dom.element) -> r.name = "tr") (children c)
      | _ -> [])
    (children table)

let grid (table : Dom.element) : cell list * int =
  let cells =
    List.concat
      (List.mapi
         (fun row (tr : Dom.element) ->
           let _, cells =
             List.fold_left
               (fun (column, cells) (td : Dom.element) ->
                 if td.name <> "td" && td.name <> "th" then (column, cells)
                 else
                   let span =
                     match Option.bind (Dom.attribute "colspan" td) int_of_string_opt with Some n when n > 1 -> n | _ -> 1
                   in
                   (column + span, { element = td; row; column; span } :: cells))
               (0, []) (children tr)
           in
           List.rev cells)
         (rows table))
  in
  (cells, List.fold_left (fun n c -> max n (c.column + c.span)) 0 cells)

let caption (table : Dom.element) : Dom.element option =
  List.find_opt (fun (c : Dom.element) -> c.name = "caption") (children table)

(*****************************************************************************)
(* The widths *)
(*****************************************************************************)

let columns (n : int) (cells : (cell * (float * float)) list) ~(spacing : float) : (float * float) array =
  let mins = Array.make n 0. and maxs = Array.make n 0. in
  (* the one-column cells first: a column's is its cells' largest *)
  List.iter
    (fun (c, (mn, mx)) ->
      if c.span = 1 then (
        mins.(c.column) <- Float.max mins.(c.column) mn;
        maxs.(c.column) <- Float.max maxs.(c.column) mx))
    cells;
  (* then the others: what they need beyond their columns, spread *)
  List.iter
    (fun (c, (mn, mx)) ->
      if c.span > 1 then (
        let spanned = List.init c.span (fun i -> c.column + i) in
        let between = spacing *. float_of_int (c.span - 1) in
        let spread (a : float array) (need : float) =
          let have = List.fold_left (fun s i -> s +. a.(i)) between spanned in
          if need > have then List.iter (fun i -> a.(i) <- a.(i) +. ((need -. have) /. float_of_int c.span)) spanned
        in
        spread mins mn;
        spread maxs mx))
    cells;
  Array.init n (fun i -> (mins.(i), Float.max mins.(i) maxs.(i)))

let widths ~(room : float) ~(fixed : bool) (columns : (float * float) array) : float array =
  let sum f = Array.fold_left (fun s c -> s +. f c) 0. columns in
  let min_sum = sum fst and max_sum = sum snd in
  if room >= max_sum then
    if fixed && max_sum > 0. then Array.map (fun (_, mx) -> mx *. room /. max_sum) columns
    else Array.map snd columns
  else if room <= min_sum then Array.map fst columns
  else
    let extra = room -. min_sum and wraps = max_sum -. min_sum in
    Array.map (fun (mn, mx) -> mn +. (extra *. (mx -. mn) /. wraps)) columns
