(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Grid.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a item = {
  what : 'a;
  row : int;
  col : int;
  rowspan : int;
  colspan : int;
  sticky : string;
  size : float * float;
}

type 'a t = {
  items : 'a item list;
  gap : float;
  (* the size each column and row settled on *)
  widths : float list;
  heights : float list;
  col_weights : (int * float) list;
  row_weights : (int * float) list;
}

let item ?(rowspan = 1) ?(colspan = 1) ?(sticky = "") ~row ~col what size =
  { what; row; col; rowspan = max 1 rowspan; colspan = max 1 colspan; sticky; size }

let nth l i = match List.nth_opt l i with Some x -> x | None -> 0.
let sum l = List.fold_left ( +. ) 0. l
let set_nth l i v = List.mapi (fun j x -> if j = i then v else x) l

(*****************************************************************************)
(* The columns and rows *)
(*****************************************************************************)

(* The sizes of one axis: every cell that sits in a single track makes
 * that track at least as big as it is; then a cell that spans several
 * tracks, and does not fit in them, grows the last one -- which is
 * what Tk does, and why a wide title over two narrow columns widens
 * the second. *)
let tracks ~gap ~count ~span ~start ~extent items =
  let sizes = ref (List.init count (fun _ -> 0.)) in
  items
  |> List.iter (fun it ->
         if span it = 1 then sizes := set_nth !sizes (start it) (max (nth !sizes (start it)) (extent it)));
  items
  |> List.iter (fun it ->
         if span it > 1 then begin
           let first = start it and n = span it in
           let covered = ref 0. in
           for i = first to first + n - 1 do
             covered := !covered +. nth !sizes i
           done;
           let with_gaps = !covered +. (gap *. float_of_int (n - 1)) in
           if with_gaps < extent it then
             let last = first + n - 1 in
             sizes := set_nth !sizes last (nth !sizes last +. (extent it -. with_gaps))
         end);
  !sizes

let make ?(gap = 0.) ?(row_weights = []) ?(col_weights = []) items =
  let cols = List.fold_left (fun n it -> max n (it.col + it.colspan)) 0 items in
  let rows_ = List.fold_left (fun n it -> max n (it.row + it.rowspan)) 0 items in
  let widths =
    tracks ~gap ~count:cols ~span:(fun it -> it.colspan) ~start:(fun it -> it.col)
      ~extent:(fun it -> fst it.size) items
  in
  let heights =
    tracks ~gap ~count:rows_ ~span:(fun it -> it.rowspan) ~start:(fun it -> it.row)
      ~extent:(fun it -> snd it.size) items
  in
  { items; gap; widths; heights; col_weights; row_weights }

let extent gap sizes = sum sizes +. (gap *. float_of_int (max 0 (List.length sizes - 1)))
let measure t = (extent t.gap t.widths, extent t.gap t.heights)
let columns t = t.widths
let rows t = t.heights

(* the room left over goes to the tracks with a weight, in proportion:
 * a form gives its fields a weight and its labels none *)
let grown sizes weights room =
  let total = List.fold_left (fun acc (_, w) -> acc +. w) 0. weights in
  if total <= 0. || room <= 0. then sizes
  else List.mapi (fun i s -> match List.assoc_opt i weights with
                             | Some w -> s +. (room *. w /. total)
                             | None -> s) sizes

let starts gap sizes from =
  let _, out =
    List.fold_left (fun (x, acc) s -> (x +. s +. gap, (x, s) :: acc)) (from, []) sizes
  in
  List.rev out

(*****************************************************************************)
(* Placing *)
(*****************************************************************************)

let arrange (b : Widget.box) t =
  let widths = grown t.widths t.col_weights (b.w -. fst (measure t)) in
  let heights = grown t.heights t.row_weights (b.h -. snd (measure t)) in
  let w = extent t.gap widths and h = extent t.gap heights in
  (* with no weights the table stays its own size, in the middle of
   * the room it was given (Tk would put it top-left) *)
  let left = b.x -. (w /. 2.) and top = b.y +. (h /. 2.) in
  let xs = starts t.gap widths left in
  let ys = starts t.gap heights (-.top) in
  t.items
  |> List.map (fun it ->
         let x0, _ = List.nth xs it.col in
         let y0, _ = List.nth ys it.row in
         let cw = ref 0. and ch = ref 0. in
         for i = it.col to it.col + it.colspan - 1 do
           cw := !cw +. nth widths i
         done;
         for i = it.row to it.row + it.rowspan - 1 do
           ch := !ch +. nth heights i
         done;
         let cw = !cw +. (t.gap *. float_of_int (it.colspan - 1)) in
         let ch = !ch +. (t.gap *. float_of_int (it.rowspan - 1)) in
         (* the cell, in playground coordinates: y counted downwards
          * above, and turned back over here *)
         let cell : Widget.box = { Widget.x = x0 +. (cw /. 2.); y = -.(y0 +. (ch /. 2.)); w = cw; h = ch } in
         let has c = String.contains it.sticky c in
         let width = if has 'e' && has 'w' then cw else min cw (fst it.size) in
         let height = if has 'n' && has 's' then ch else min ch (snd it.size) in
         let x =
           if has 'w' && not (has 'e') then Widget.left cell +. (width /. 2.)
           else if has 'e' && not (has 'w') then Widget.right cell -. (width /. 2.)
           else cell.x
         in
         let y =
           if has 'n' && not (has 's') then Widget.top cell -. (height /. 2.)
           else if has 's' && not (has 'n') then Widget.bottom cell +. (height /. 2.)
           else cell.y
         in
         (it.what, { Widget.x; y; w = width; h = height }))
