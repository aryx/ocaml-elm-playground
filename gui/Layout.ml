(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Layout.mli *)

type constraints = { min_w : float; max_w : float; min_h : float; max_h : float }

let loose w h = { min_w = 0.; max_w = w; min_h = 0.; max_h = h }
let tight w h = { min_w = w; max_w = w; min_h = h; max_h = h }

type 'a t =
  | Leaf of 'a * (float * float)
  | Space of float
  | Spacer
  | Expand of 'a t
  | Stretch of 'a t
  | Pad of float * 'a t
  | Center of 'a t
  | Row of float * 'a t list
  | Column of float * 'a t list

let leaf key size = Leaf (key, size)
let space n = Space n
let spacer = Spacer
let expand t = Expand t
let stretch t = Stretch t
let pad n t = Pad (n, t)
let center t = Center t
let row ?(gap = 0.) kids = Row (gap, kids)
let column ?(gap = 0.) kids = Column (gap, kids)

(* A column is a row turned on its side, so everything below is
 * written once, along an axis: [main] is the direction it runs in,
 * [cross] the other one. *)
type axis = Horizontal | Vertical

let main axis (w, h) = match axis with Horizontal -> w | Vertical -> h
let cross axis (w, h) = match axis with Horizontal -> h | Vertical -> w

let of_axis axis ~main ~cross =
  match axis with Horizontal -> (main, cross) | Vertical -> (cross, main)

let clamp c (w, h) = (max c.min_w (min c.max_w w), max c.min_h (min c.max_h h))
let is_flexible = function Spacer | Expand _ -> true | _ -> false
let is_stretch = function Stretch _ -> true | _ -> false

(* [share] along the axis, whatever it is given across it *)
let along axis share c =
  match axis with
  | Horizontal -> { c with min_w = share; max_w = share; min_h = 0. }
  | Vertical -> { c with min_h = share; max_h = share; min_w = 0. }

(* Pass one. The flex rule, which is the whole of rows and columns:
 * measure the children that know their size, then share what is left
 * over between the flexible ones. *)
let rec child_sizes axis c gap kids =
  let room = main axis (c.max_w, c.max_h) in
  let gaps = gap *. float_of_int (max 0 (List.length kids - 1)) in
  let loose_c = { c with min_w = 0.; min_h = 0. } in
  let known =
    kids
    |> List.map (fun k ->
           match k with
           | Spacer | Expand _ -> None
           | Space n -> Some (of_axis axis ~main:n ~cross:0.)
           | k -> Some (measure loose_c k))
  in
  let used =
    List.fold_left
      (fun acc -> function Some s -> acc +. main axis s | None -> acc)
      0. known
  in
  let flexible = List.length (List.filter is_flexible kids) in
  let share =
    if flexible = 0 then 0.
    else max 0. (room -. gaps -. used) /. float_of_int flexible
  in
  List.map2
    (fun k known ->
      match (known, k) with
      | Some s, _ -> s
      | None, Spacer -> of_axis axis ~main:share ~cross:0.
      (* an expanded child is measured again, now that it knows how
       * much it got: constraints down, once more *)
      | None, Expand inner -> measure (along axis share c) inner
      | None, _ -> (0., 0.))
    kids known

and measure_line axis c gap kids =
  let sizes = child_sizes axis c gap kids in
  let gaps = gap *. float_of_int (max 0 (List.length kids - 1)) in
  let extent = List.fold_left (fun acc s -> acc +. main axis s) gaps sizes in
  let thickness = List.fold_left (fun acc s -> max acc (cross axis s)) 0. sizes in
  (* a line with a flexible child takes all the room it was offered:
   * that is what makes [spacer] push things apart *)
  let extent = if List.exists is_flexible kids then main axis (c.max_w, c.max_h) else extent in
  clamp c (of_axis axis ~main:extent ~cross:thickness)

and measure c t =
  match t with
  | Leaf (_, size) -> clamp c size
  (* both mean something only inside a row or a column, which handles
   * them itself (child_sizes): alone, they are nothing *)
  | Space _ | Spacer -> clamp c (0., 0.)
  | Expand inner | Stretch inner -> measure c inner
  | Pad (n, inner) ->
      let inner_c =
        {
          min_w = max 0. (c.min_w -. (2. *. n));
          max_w = max 0. (c.max_w -. (2. *. n));
          min_h = max 0. (c.min_h -. (2. *. n));
          max_h = max 0. (c.max_h -. (2. *. n));
        }
      in
      let w, h = measure inner_c inner in
      clamp c (w +. (2. *. n), h +. (2. *. n))
  | Center inner -> measure c inner
  | Row (gap, kids) -> measure_line Horizontal c gap kids
  | Column (gap, kids) -> measure_line Vertical c gap kids

(* Pass two: each parent hands its children the rectangle it decided
 * for them, and only the leaves come back. *)
let rec arrange (b : Widget.box) t =
  match t with
  | Leaf (key, _) -> [ (key, b) ]
  | Space _ | Spacer -> []
  | Expand inner | Stretch inner -> arrange b inner
  | Pad (n, inner) -> arrange (Widget.inset n b) inner
  | Center inner ->
      let w, h = measure (loose b.w b.h) inner in
      arrange { b with w; h } inner
  | Row (gap, kids) -> arrange_line Horizontal b gap kids
  | Column (gap, kids) -> arrange_line Vertical b gap kids

and arrange_line axis (b : Widget.box) gap kids =
  let sizes = child_sizes axis (loose b.w b.h) gap kids in
  (* a row walks right from its left edge, a column *down* from its
   * top: y is up in the playground, so a column's steps are negative *)
  let start, step =
    match axis with
    | Horizontal -> (Widget.left b, 1.)
    | Vertical -> (Widget.top b, -1.)
  in
  let _, places =
    List.fold_left2
      (fun (pos, acc) k size ->
        let extent = main axis size in
        (* across the axis, a child keeps the size it asked for and is
         * centered -- unless it was stretched, and then it fills *)
        let thickness =
          if is_stretch k then cross axis (b.w, b.h) else cross axis size
        in
        let center_of_child = pos +. (step *. extent /. 2.) in
        let w, h = of_axis axis ~main:extent ~cross:thickness in
        let child =
          match axis with
          | Horizontal -> { Widget.x = center_of_child; y = b.y; w; h }
          | Vertical -> { Widget.x = b.x; y = center_of_child; w; h }
        in
        (pos +. (step *. (extent +. gap)), acc @ arrange child k))
      (start, []) kids sizes
  in
  places
