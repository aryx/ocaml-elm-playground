(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scratch_look.mli *)

open Playground
module B = Scratch_blocks
module R = Scratch_run
module L = Block_layout

type theme = { colors : B.category -> float * float * float; zebra : bool }

let scratch2 =
  {
    colors =
      (function
      | B.Motion -> (74., 108., 212.)
      | B.Looks -> (138., 85., 215.)
      | B.Events -> (200., 131., 48.)
      | B.Control -> (225., 169., 26.)
      | B.Sensing -> (44., 165., 226.)
      | B.Operators -> (92., 183., 18.)
      | B.Variables -> (238., 125., 22.)
      | B.Pen -> (14., 154., 108.)
      | B.Lists -> (204., 91., 34.)
      | B.Other -> (150., 150., 150.));
    zebra = false;
  }

let snap =
  {
    colors =
      (function
      | B.Motion -> (74., 108., 212.)
      | B.Looks -> (143., 86., 227.)
      | B.Events | B.Control -> (230., 168., 34.)
      | B.Sensing -> (4., 148., 220.)
      | B.Operators -> (98., 194., 19.)
      | B.Variables -> (243., 118., 29.)
      | B.Pen -> (0., 161., 120.)
      | B.Lists -> (217., 77., 17.)
      | B.Other -> (150., 150., 150.));
    zebra = true;
  }

(*****************************************************************************)
(* Drawing blocks *)
(*****************************************************************************)

(* a text's width at the blocks' size, 12, as the software renderer
   draws it (Hershey's Futura, graphics/font); the other backends'
   fonts are near enough *)
let font_size = 12.
let measure s = snd (Hershey.layout s) *. font_size /. Hershey.units_per_em
let shade k (r, g, b) = rgb (int_of_float (r *. k)) (int_of_float (g *. k)) (int_of_float (b *. k))
let lighter (r, g, b) = (r +. ((255. -. r) *. 0.4), g +. ((255. -. g) *. 0.4), b +. ((255. -. b) *. 0.4))
let text color size s (x, y) = words color s |> scale (size /. words_font_size) |> move x y

(* a word from its left edge, at its estimated width *)
let label color s (x, y) = text color font_size s (x +. (measure s /. 2.), y)

let round_box color w h (x, y) =
  let r = h /. 2. in
  [ rectangle color (Float.max 0. (w -. h)) h |> move (x +. (w /. 2.)) (y -. r); circle color r |> move (x +. r) (y -. r); circle color r |> move (x +. w -. r) (y -. r) ]

let hexagon color w h (x, y) = polygon color [ (x, y -. (h /. 2.)); (x +. (h /. 2.), y); (x +. w -. (h /. 2.), y); (x +. w, y -. (h /. 2.)); (x +. w -. (h /. 2.), y -. h); (x +. (h /. 2.), y -. h) ]

(* the jigsaw's outline: a notch in the top, a tab under the bottom *)
let notch x y = [ (x +. 10., y); (x +. 14., y -. 4.); (x +. 26., y -. 4.); (x +. 30., y) ]
let tab x y = [ (x +. 30., y); (x +. 26., y -. 4.); (x +. 14., y -. 4.); (x +. 10., y) ]

let outline (s : B.spec) x y w h mouths =
  let bottom = y -. h in
  let top = if s.shape = B.Hat then [ (x, y -. L.arm); (x +. w, y -. L.arm) ] else [ (x, y) ] @ notch x y @ [ (x +. w, y) ] in
  let arms =
    List.concat
      (List.mapi
         (fun i (mt, mh) ->
           let mb = mt -. mh in
           let next_top = match List.nth_opt mouths (i + 1) with Some (t, _) -> t | None -> bottom +. L.arm in
           [ (x +. w, mt) ] @ tab (x +. L.arm) mt @ [ (x +. L.arm, mt); (x +. L.arm, mb) ] @ List.rev (notch (x +. L.arm) mb) @ [ (x +. w, mb); (x +. w, next_top) ])
         mouths)
  in
  let under = if s.shape = B.Cap || s.shape = B.C_cap then [] else tab x bottom in
  top @ arms @ [ (x +. w, bottom) ] @ under @ [ (x, bottom) ]

(* a ring: grey, the reporter's round, the script's a frame *)
let ring_grey = (195., 197., 205.)

let body c (s : B.spec) x y w h mouths =
  let draw k dy =
    match s.shape with
    | B.Reporter -> round_box (shade k c) w h (x, y +. dy)
    | B.Predicate -> [ hexagon (shade k c) w h (x, y +. dy) ]
    | B.Hat ->
        [ oval (shade k c) 80. 34. |> move (x +. 40.) (y -. L.arm +. dy); polygon (shade k c) (List.map (fun (a, b) -> (a, b +. dy)) (outline s x y w h mouths)) ]
    | B.Ring -> round_box (shade k ring_grey) w h (x, y +. dy)
    | B.Command_ring ->
        [ rectangle (shade k ring_grey) w h |> move (x +. (w /. 2.)) (y +. dy -. (h /. 2.)) ]
        @ if k = 1. then [ rectangle (shade 0.92 ring_grey) (w -. 8.) (h -. 8.) |> move (x +. (w /. 2.)) (y -. (h /. 2.)) ] else []
    | _ -> [ polygon (shade k c) (List.map (fun (a, b) -> (a, b +. dy)) (outline s x y w h mouths)) ]
  in
  draw 0.72 (-1.5) @ draw 1. 0.

(* Snap!'s zebra colouring: a block in a slot of a block of its colour,
   lighter than it (and the one in it darker again) *)
let zebras theme pieces =
  List.fold_left
    (fun known piece ->
      match piece with
      | L.Body { path; spec; _ } ->
          let parent = match List.rev path.steps with L.Arg _ :: rest -> Some { path with steps = List.rev rest } | _ -> None in
          let light =
            theme.zebra
            && match Option.bind parent (fun p -> List.assoc_opt p known) with Some (cat, light) -> cat = spec.category && not light | None -> false
          in
          (path, (spec.category, light)) :: known
      | _ -> known)
    [] pieces

let pieces_shapes theme ?caret pieces =
  let zebra = zebras theme pieces in
  List.concat_map
    (function
      | L.Body { path; spec; x; y; w; h; mouths } ->
          let c = theme.colors spec.category in
          let c = match List.assoc_opt path zebra with Some (_, true) -> lighter c | _ -> c in
          body c spec x y w h mouths
      | L.Label { x; y; text = t } -> [ label white t (x, y) ]
      | L.Slot { path; part; x; y; w; h; text = t } -> (
          let t = match caret with Some (p, typed) when p = path -> typed ^ "|" | _ -> t in
          let cx = x +. (w /. 2.) and cy = y -. (h /. 2.) in
          match part with
          | B.Num _ -> round_box white w h (x, y) @ [ text black 12. t (cx, cy) ]
          | B.Menu _ -> [ rectangle black w h |> move cx cy |> fade 0.25; text white 12. t (cx -. 4., cy); triangle white 3. |> rotate 180. |> move (x +. w -. 6.) (cy -. 1.) ]
          | B.Bool -> [ hexagon (rgb 0 0 0) w h (x, y) |> fade 0.2 ]
          | _ -> [ rectangle white w h |> move cx cy; text black 12. t (cx, cy) ]))
    pieces

(*****************************************************************************)
(* Drawing the stage *)
(*****************************************************************************)

type frame = { cx : float; cy : float; k : float }

let of_stage f (x, y) = (f.cx +. (x *. f.k), f.cy +. (y *. f.k))
let to_stage f (x, y) = ((x -. f.cx) /. f.k, (y -. f.cy) /. f.k)

(* Scratch 2's pen colour: 0 to 200 once round the hues *)
let hue_color h =
  let h = Float.rem (h *. 1.8) 360. /. 60. in
  let x = 1. -. Float.abs (Float.rem h 2. -. 1.) in
  let r, g, b = match int_of_float h with 0 -> (1., x, 0.) | 1 -> (x, 1., 0.) | 2 -> (0., 1., x) | 3 -> (0., x, 1.) | 4 -> (x, 0., 1.) | _ -> (1., 0., x) in
  rgb (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let segment color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  if len < 0.01 then circle color (width /. 2.) |> move ax ay
  else
    rectangle color (len +. width) width
    |> rotate (Float.atan2 (by -. ay) (bx -. ax) *. 180. /. Float.pi)
    |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

(* the costumes, facing right (left when [flip]: the rotation style
   left-right), in stage units round the sprite's middle; the cat's
   two, its legs apart and together *)
let costume ?(flip = false) (s : R.sprite) =
  let orange = rgb 250 165 40 and dark = rgb 60 40 20 and cream = rgb 255 240 220 in
  let fx x = if flip then -.x else x in
  let at x y sh = move (fx x) y sh in
  let poly c pts = polygon c (List.map (fun (x, y) -> (fx x, y)) pts) in
  match s.name with
  | "Pencil" ->
      (* the tip at the middle, where the pen draws *)
      [
        poly (rgb 240 200 120) [ (0., 0.); (8., 5.); (8., -5.) ];
        poly (rgb 250 200 40) [ (8., 5.); (40., 5.); (40., -5.); (8., -5.) ];
        rectangle (rgb 240 130 160) 6. 10. |> at 43. 0.;
        circle dark 1.5;
      ]
  | "Turtle" ->
      (* Snap!'s sprite with no costume: Logo's turtle, an arrowhead
         whose tip is where it is *)
      [ poly (rgb 60 60 70) [ (2., 0.); (-16., 10.); (-11., 0.); (-16., -10.) ]; poly (rgb 90 140 230) [ (-1., 0.); (-13., 7.); (-10., 0.); (-13., -7.) ] ]
  | _ ->
      let legs = if s.costume = 0 then [ (-14., -4.); (-6., 4.); (6., -4.); (14., 4.) ] else [ (-12., 0.); (-8., 0.); (8., 0.); (12., 0.) ] in
      List.map (fun (lx, tilt) -> rectangle orange 6. 18. |> rotate (fx (tilt *. 3.)) |> at lx (-20.)) legs
      @ [
          poly orange [ (-22., 3.); (-37., 18.); (-31., 22.); (-19., -3.) ];
          poly orange [ (-37., 18.); (-33., 29.); (-27., 27.); (-31., 17.) ];
          oval orange 48. 28. |> at (-4.) (-6.);
          poly orange [ (8., 20.); (12., 36.); (20., 24.) ];
          poly orange [ (22., 24.); (32., 36.); (32., 18.) ];
          circle orange 17. |> at 20. 10.;
          oval cream 20. 12. |> at 26. 2.;
          oval white 8. 10. |> at 16. 14.;
          oval white 8. 10. |> at 26. 14.;
          circle black 2. |> at 17. 14.;
          circle black 2. |> at 27. 14.;
          circle dark 2. |> at 30. 5.;
        ]

let sprite_shapes f (s : R.sprite) =
  let x, y = of_stage f (s.x, s.y) in
  let k = f.k *. s.size /. 100. in
  let flip = s.rotation = R.Left_right && s.direction < 0. in
  let turn = match s.rotation with R.All_around -> 90. -. s.direction | _ -> 0. in
  [ group (costume ~flip s) |> rotate turn |> scale k |> move x y ]

(* a list in a bubble as Snap! shows it: a table, an index and an item
   a row, its length under it *)
let list_table stage id (bx, top) =
  let items = R.items stage id in
  let shown = List.filteri (fun i _ -> i < 8) items in
  let w = 20. +. List.fold_left (fun acc v -> Float.max acc (measure (R.show stage v))) 30. shown +. 12. in
  let rows =
    List.concat
      (List.mapi
         (fun i v ->
           let y = top -. 12. -. (float_of_int i *. 18.) in
           [
             text (rgb 90 90 90) 10. (string_of_int (i + 1)) (bx +. 10., y);
             rectangle (rgb 250 250 250) (w -. 24.) 16. |> move (bx +. 20. +. ((w -. 24.) /. 2.)) y;
             label black (R.show stage v) (bx +. 24., y);
           ])
         shown)
  in
  let h = (18. *. float_of_int (List.length shown)) +. 24. in
  ( w,
    h,
    [ rectangle (rgb 120 120 130) (w +. 2.) (h +. 2.) |> move (bx +. (w /. 2.)) (top -. (h /. 2.)); rectangle (rgb 225 227 232) w h |> move (bx +. (w /. 2.)) (top -. (h /. 2.)) ]
    @ rows
    @ [ text (rgb 90 90 90) 10. (Printf.sprintf "length: %d" (List.length items)) (bx +. (w /. 2.), top -. h +. 6.) ] )

let bubble f stage (s : R.sprite) =
  match s.bubble with
  | Some v when s.visible -> (
      let x, y = of_stage f (s.x, s.y) in
      let r = s.radius *. s.size /. 100. *. f.k in
      let bx = x +. (r *. 0.6) and by = y +. r +. 22. in
      match v with
      | R.List id ->
          (* the table grows upwards, from where a text's bubble is *)
          let rows = min 8 (List.length (R.items stage id)) in
          let top = by -. 10. +. (18. *. float_of_int rows) +. 24. in
          let w, h, table = list_table stage id (bx, top) in
          [
            polygon (rgb 160 160 160) [ (bx +. 8., by -. 14.); (bx +. 20., by -. 14.); (bx +. 4., by -. 26.) ];
            rectangle (rgb 160 160 160) (w +. 10.) (h +. 10.) |> move (bx +. (w /. 2.)) (top -. (h /. 2.));
            rectangle white (w +. 8.) (h +. 8.) |> move (bx +. (w /. 2.)) (top -. (h /. 2.));
          ]
          @ table
      | v ->
          let said = R.show stage v in
          let w = Float.max 40. ((float_of_int (String.length said) *. 7.) +. 20.) in
          [
            polygon (rgb 160 160 160) [ (bx +. 8., by -. 14.); (bx +. 20., by -. 14.); (bx +. 4., by -. 26.) ];
            rectangle (rgb 160 160 160) (w +. 2.) 30. |> move (bx +. (w /. 2.)) by;
            rectangle white w 28. |> move (bx +. (w /. 2.)) by;
            polygon white [ (bx +. 9., by -. 13.); (bx +. 19., by -. 13.); (bx +. 5., by -. 23.) ];
            text black 13. said (bx +. (w /. 2.), by);
          ])
  | _ -> []

let stage_shapes f (stage : R.t) =
  let ink =
    List.rev_map
      (function
        | R.Line (a, b, hue, size) -> segment (hue_color hue) (Float.max 1. (size *. f.k)) (of_stage f a) (of_stage f b)
        | R.Stamp s -> group (sprite_shapes f s))
      stage.ink
  in
  [ rectangle white (480. *. f.k) (360. *. f.k) |> move f.cx f.cy ]
  @ ink
  @ List.concat_map (fun (s : R.sprite) -> if s.visible then sprite_shapes f s else []) stage.sprites
  @ List.concat_map (bubble f stage) stage.sprites
