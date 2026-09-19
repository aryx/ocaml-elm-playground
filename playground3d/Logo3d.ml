(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Logo3d.mli *)

open Playground
open Playground3d

type number = Playground.number

type command =
  | Forward of number
  (* the three rotations, in degrees, as in The Algorithmic Beauty of
   * Plants: [Yaw] around U (left positive), [Pitch] around L (down
   * positive), [Roll] around H (the left side up positive) *)
  | Yaw of number
  | Pitch of number
  | Roll of number
  | Home
  | Pen of bool
  | Pen_color of color
  | Pen_size of number
  | Dot of number
  | Show of bool
  | Repeat of int * command list

let forward d = Forward d
let back d = Forward (-.d)
let left a = Yaw a
let right a = Yaw (-.a)
let down a = Pitch a
let up a = Pitch (-.a)
let roll_right a = Roll a
let roll_left a = Roll (-.a)
let home = Home
let pen_up = Pen false
let pen_down = Pen true
let pen_color c = Pen_color c
let pen_size s = Pen_size s
let dot s = Dot s
let hide_turtle = Show false
let show_turtle = Show true
let repeat n program = Repeat (n, program)
let block program = Repeat (1, program)
let stop = Repeat (0, [])
let fd = forward
let bk = back
let lt = left
let rt = right
let pu = pen_up
let pd = pen_down

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

type vec = number * number * number

let add ((x1, y1, z1) : vec) ((x2, y2, z2) : vec) : vec = (x1 +. x2, y1 +. y2, z1 +. z2)
let scale (k : number) ((x, y, z) : vec) : vec = (k *. x, k *. y, k *. z)
let length ((x, y, z) : vec) : number = sqrt ((x *. x) +. (y *. y) +. (z *. z))
let sub (a : vec) (b : vec) : vec = add a (scale (-1.) b)

(* [turn a (u, v)]: u and v, two of the frame's directions, rotated by
 * [a] degrees in their plane, u towards v:
 *
 *    u' = u cos a + v sin a        v' = v cos a - u sin a
 *)
let turn (a : number) ((u, v) : vec * vec) : vec * vec =
  let r = a *. Float.pi /. 180. in
  let c = cos r and s = sin r in
  (add (scale c u) (scale s v), add (scale c v) (scale (-.s) u))

(*****************************************************************************)
(* The turtle, running a program *)
(*****************************************************************************)

type turtle = {
  pos : vec;
  (* its frame: heading, left, up; H x L = U *)
  h : vec;
  l : vec;
  u : vec;
  pen : bool;
  color : color;
  size : number;
  visible : bool;
}

let start =
  { pos = (0., 0., 0.); h = (0., 1., 0.); l = (-1., 0., 0.); u = (0., 0., 1.); pen = true; color = black; size = 2.; visible = true }

type run = {
  turtle : turtle;
  (* as in Logo.ml: the work left and done, and whether it ran out *)
  budget : number;
  spent : number;
  out : bool;
  (* the shapes, the last first *)
  drawn : shape3d list;
  (* the box around what's drawn: its corners *)
  low : vec;
  high : vec;
}

let turn_cost = 0.25

let affordable (r : run) (cost : number) : number = if cost <= r.budget then 1. else r.budget /. cost

let spend (r : run) (cost : number) : run =
  if cost <= r.budget then { r with budget = r.budget -. cost; spent = r.spent +. cost }
  else { r with budget = 0.; spent = r.spent +. r.budget; out = true }

let widen (r : run) ((x, y, z) : vec) (margin : number) : run =
  let (x1, y1, z1) = r.low and (x2, y2, z2) = r.high in
  { r with
    low = (Float.min x1 (x -. margin), Float.min y1 (y -. margin), Float.min z1 (z -. margin));
    high = (Float.max x2 (x +. margin), Float.max y2 (y +. margin), Float.max z2 (z +. margin)) }

(* a line from [a] to [b], a square tube around it, its sides along the
 * turtle's left and up: 4 sides and 2 ends, each counterclockwise seen
 * from outside (Playground3d.polygon3d's backface culling). With the
 * corners in the order c0 = L + U, c1 = -L + U, c2 = -L - U, c3 = L - U,
 * the side (c0, c1) faces U: (c1 - c0) x H = -2 L x H = 2 U. *)
let tube (t : turtle) (a : vec) (b : vec) : shape3d =
  let w = t.size /. 2. in
  let corner sl su p = add p (add (scale (sl *. w) t.l) (scale (su *. w) t.u)) in
  let ring p = [| corner 1. 1. p; corner (-1.) 1. p; corner (-1.) (-1.) p; corner 1. (-1.) p |] in
  let ra = ring a and rb = ring b in
  let sides = List.init 4 (fun k -> let k' = (k + 1) mod 4 in polygon3d t.color [ ra.(k); ra.(k'); rb.(k'); rb.(k) ]) in
  group3d
    (polygon3d t.color (Array.to_list rb) :: polygon3d t.color (List.rev (Array.to_list ra)) :: sides)

(* an octahedron: 8 triangles, (±r, 0, 0), (0, ±r, 0), (0, 0, ±r), each
 * turned to face out: (a, b, c)'s normal is (bc, ac, ab), outward when
 * a b c > 0, else the triangle is reversed *)
let octahedron (color : color) (r : number) ((x, y, z) : vec) : shape3d =
  let signs = [ 1.; -1. ] in
  group3d
    (List.concat_map
       (fun a ->
         List.concat_map
           (fun b ->
             List.map
               (fun c ->
                 let pts = [ (x +. (a *. r), y, z); (x, y +. (b *. r), z); (x, y, z +. (c *. r)) ] in
                 polygon3d color (if a *. b *. c > 0. then pts else List.rev pts))
               signs)
           signs)
       signs)

let rec run_command (r : run) (c : command) : run =
  let t = r.turtle in
  let rotate a apply =
    let cost = Float.abs a *. turn_cost in
    { (spend r cost) with turtle = apply (affordable r cost *. a) }
  in
  if r.out then r
  else
    match c with
    | Forward d ->
        let f = affordable r (Float.abs d) in
        let pos = add t.pos (scale (f *. d) t.h) in
        let r' = spend r (Float.abs d) in
        if t.pen && d <> 0. then
          let r' = { r' with drawn = tube t t.pos pos :: r'.drawn } in
          widen (widen r' t.pos t.size) pos t.size |> fun r' -> { r' with turtle = { t with pos } }
        else { r' with turtle = { t with pos } }
    | Yaw a -> rotate a (fun a -> let (h, l) = turn a (t.h, t.l) in { t with h; l })
    | Pitch a -> rotate a (fun a -> let (u, h) = turn a (t.u, t.h) in { t with h; u })
    | Roll a -> rotate a (fun a -> let (l, u) = turn a (t.l, t.u) in { t with l; u })
    | Home -> { r with turtle = { start with pen = t.pen; color = t.color; size = t.size; visible = t.visible } }
    | Pen b -> { r with turtle = { t with pen = b } }
    | Pen_color color -> { r with turtle = { t with color } }
    | Pen_size size -> { r with turtle = { t with size } }
    | Show visible -> { r with turtle = { t with visible } }
    | Dot size -> widen { r with drawn = octahedron t.color (size /. 2.) t.pos :: r.drawn } t.pos size
    | Repeat (n, program) ->
        let r = ref r in
        for _ = 1 to n do
          r := run_program !r program
        done;
        !r

and run_program (r : run) (program : command list) : run = List.fold_left run_command r program

let run (budget : number) (program : command list) : run =
  run_program
    { turtle = start; budget; spent = 0.; out = false; drawn = []; low = (0., 0., 0.); high = (0., 0., 0.) }
    program

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* the turtle: a pyramid, its tip ahead, its base behind; the top side
 * lighter, to see which way is its up *)
let turtle_shape (t : turtle) : shape3d =
  let at dh dl du = add t.pos (add (scale dh t.h) (add (scale dl t.l) (scale du t.u))) in
  let tip = at 14. 0. 0. in
  let base = [| at (-5.) 6. 6.; at (-5.) (-6.) 6.; at (-5.) (-6.) (-6.); at (-5.) 6. (-6.) |] in
  let side k color = polygon3d color [ base.(k); base.((k + 1) mod 4); tip ] in
  group3d
    [ side 0 (rgb 120 220 120); side 1 (rgb 40 160 70); side 2 (rgb 40 160 70); side 3 (rgb 40 160 70);
      polygon3d (rgb 20 90 40) [ base.(3); base.(2); base.(1); base.(0) ] ]

let draw_upto (budget : number) (program : command list) : shape3d list =
  let r = run budget program in
  List.rev r.drawn @ if r.turtle.visible then [ turtle_shape r.turtle ] else []

let draw (program : command list) : shape3d list = draw_upto infinity program
let work (program : command list) : number = (run infinity program).spent

let camera_of (r : run) (angle : number) : camera =
  let center = scale 0.5 (add r.low r.high) in
  (* the radius of a ball around the drawing; at 2.2 radii, a 60-degree
   * field of view sees it whole (1 / sin 30 = 2) *)
  let radius = Float.max 50. (length (sub r.high r.low) /. 2.) in
  let d = 2.2 *. radius and a = angle *. Float.pi /. 180. in
  let eye = add center (d *. sin a, 0.5 *. radius, d *. cos a) in
  camera ~eye ~target:center ~near:(radius /. 20.) ~far:(d +. (4. *. radius)) ()

let camera_around (program : command list) : number -> camera = camera_of (run infinity program)

(*****************************************************************************)
(* Applications *)
(*****************************************************************************)

let animation ?(speed = 300.) (program : command list) =
  (* the whole drawing, once, for the camera *)
  let whole = run infinity program in
  let view (computer : computer) (s : unit Scene2d.t) =
    (camera_of whole (spin 20. computer.time), draw_upto (s.elapsed *. speed) program)
  in
  let update (computer : computer) (s : unit Scene2d.t) =
    let s = Scene2d.update computer s in
    if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go () s else s
  in
  game3d view update (Scene2d.start ())
