(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Quake (id Software, 1996): three rooms of a level,
 * three runes to collect, and the exit. Arrows to turn and look, w/s
 * to walk, a/d to step sideways, space to jump, v to turn the
 * visibility set off and see what it was doing.
 *
 * Doom's renderer is the lesson of TinyDoom, and Descent's
 * portals that of TinyDescent. Quake's lesson is neither:
 * it is what happens to the level *before* the game runs. id shipped
 * three separate programs, and a level went through all three:
 *
 *    the map, brushes          qbsp     the BSP tree, its leaves
 *    (solid boxes)        --------->    and the faces to draw
 *                                             |
 *                              vis            |  for each leaf, the
 *                         <-----------+-------+  leaves it could ever
 *                              (the "potentially visible set")
 *                                             |
 *                             light           |  for each patch of a
 *                         <-----------+-------+  face, how much light
 *                              (the "lightmaps", with shadows)
 *
 * Then, at 60 frames a second, the game does almost nothing: find the
 * leaf the eye is in ([leaf_at]), draw the faces of the leaves in its
 * set, with their light already baked in. All three steps run here at
 * startup instead of in a tool, small enough to read:
 *
 * 1. [csg] and [build], "qbsp". The map is a list of solid boxes that
 *    may overlap (walls built as slabs, the easy way to draw a map);
 *    the parts of a face that are inside another box would be surfaces
 *    in the middle of the rock, so they go first ([csg], qbsp's
 *    CSGFaces). What is left is split by planes, chosen from the faces
 *    themselves, into a tree whose leaves are solid or empty
 *    ([build]): the back of a face is rock, the front is air, and that
 *    is the whole rule.
 *
 * 2. [pvs], "vis". Quake's great trick: which leaves can be seen from
 *    which, worked out once. In a level of rooms and corridors, from
 *    any one room most of the map is invisible, and none of it needs
 *    projecting, clipping or sorting. Ours samples: two leaves see
 *    each other if a straight line between sample points in them
 *    misses the rock ([sees]). Quake's vis did it exactly, clipping
 *    the sight through chains of portals with separating planes
 *    (Seth Teller's thesis on antipenumbrae), and took hours on a big
 *    level.
 *
 * 3. [lightmap], "light". Each face is cut into patches, and each
 *    patch asks every lamp: can I see you? ([lit]). A patch in shadow
 *    only gets the ambient light. Quake stored the answers as a small
 *    texture per face, stretched over it while drawing (its "surface
 *    cache"); here a patch is simply a small polygon of its own color,
 *    which the playground can draw -- a lightmap of one texel per
 *    patch.
 *
 * The two golden frames "doorway" and "everything" (tests/3d) are the
 * same picture, pixel for pixel: one drew 981 patches, the other all
 * 5888. That is the whole point of vis, and the "v" key shows it while
 * playing.
 *
 * What is missing, next to the real thing: textures (Quake multiplied
 * them by the lightmap), the "surface cache" that made that cheap, the
 * edge list and the spans the software renderer drew into
 * (docs/claude_notes/related-work/notes_vs_doom_quake.md), the
 * monsters, and the network. The BSP here decides what to draw, not
 * the order pixels are written: playground3d has a z-buffer, which
 * Quake used only for the moving things.
 *
 * Uses: playground3d (cached3d per leaf), and nothing else: the map,
 * the tools and the walking are all here, one section each. Not the
 * Segments kit (that is Descent's convex cells, not a BSP of solid
 * space), not Camera3d.
 *
 * References: the Quake source code (id Software, 1999): qbsp's
 * CSGFaces and SolidBSP, vis's BasePortalVis and PortalFlow, light's
 * LightFace; Michael Abrash, "Graphics Programming Black Book" (1997),
 * chapters 59-70, written while he was writing that renderer; John
 * Carmack's .plan files.
 *
 * Exercises: portals and a real [vis] (the leaves' shared faces, then
 * the flow), the patches merged where the light is flat (Quake's
 * lightmaps were 16 units per texel, ours are patches of geometry), a
 * light that moves (the lightmap can't: that is why Quake's monsters
 * were lit by a different, per-model light), textures, a door (a brush
 * that moves, and the leaf behind it).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Geometry *)
(*****************************************************************************)

type vec = number * number * number

let add (x1, y1, z1) (x2, y2, z2) : vec = (x1 +. x2, y1 +. y2, z1 +. z2)
let sub (x1, y1, z1) (x2, y2, z2) : vec = (x1 -. x2, y1 -. y2, z1 -. z2)
let times k (x, y, z) : vec = (k *. x, k *. y, k *. z)
let dot (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)
let length v = sqrt (dot v v)

let normalize (v : vec) : vec =
  let n = length v in
  if n = 0. then v else times (1. /. n) v

let coord ((x, y, z) : vec) (axis : int) : number = match axis with 0 -> x | 1 -> y | _ -> z

(* the point with [c] on [axis] and the other two coordinates [u], [v],
 * in the order axis + 1, axis + 2 *)
let point (axis : int) (c : number) (u : number) (v : number) : vec =
  let a = (axis + 1) mod 3 in
  let get i = if i = axis then c else if i = a then u else v in
  (get 0, get 1, get 2)

(* a box of solid rock, Quake's "brush" (its own could be any convex
 * shape; axis-aligned boxes keep every test to a comparison) *)
type box = { x0 : number; y0 : number; z0 : number; x1 : number; y1 : number; z1 : number }

let low (b : box) axis = match axis with 0 -> b.x0 | 1 -> b.y0 | _ -> b.z0
let high (b : box) axis = match axis with 0 -> b.x1 | 1 -> b.y1 | _ -> b.z1

let inside_box (b : box) ((x, y, z) : vec) : bool =
  x > b.x0 && x < b.x1 && y > b.y0 && y < b.y1 && z > b.z0 && z < b.z1

(* A face of the rock, seen from the air: the rectangle [u0..u1] x
 * [v0..v1] of the plane [axis] = [at], facing [positive] (the air is
 * on that side). *)
type face = { axis : int; positive : bool; at : number; u0 : number; u1 : number; v0 : number; v1 : number; rgb : int * int * int }

let normal (f : face) : vec = point f.axis (if f.positive then 1. else -1.) 0. 0.

let corners (f : face) : vec list =
  let p u v = point f.axis f.at u v in
  let cs = [ p f.u0 f.v0; p f.u1 f.v0; p f.u1 f.v1; p f.u0 f.v1 ] in
  (* counterclockwise seen from the air *)
  if f.positive then cs else List.rev cs

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

(* the rock, as boxes that may overlap: the floor, the ceilings, the
 * outer walls, the walls between the rooms (with their doorways left
 * out), a pillar and a platform:
 *
 *      z
 *      ^   +-------------------+     A: the start room, the pillar
 *      |   |         |    C    |     B: the tall room, the platform
 *      |   |  solid  +----+----+     C: the dark room
 *      |   |         | BC corridor   the exit is back in A
 *      |   +----+----+----+----+
 *      |   |    |    |         |
 *      |   | A  |####| AB   B  |     #### the wall with the doorway
 *      |   |    |    |         |
 *      |   +----+----+---------+
 *      +--------------------------> x
 *)
let rock : (box * (int * int * int)) list =
  let b x0 y0 z0 x1 y1 z1 = { x0; y0; z0; x1; y1; z1 } in
  let grey = (120, 118, 116) and brown = (124, 104, 84) and dark = (96, 92, 104) in
  [ (* the floor and the four outer walls *)
    (b 0. (-32.) 0. 1024. 0. 1024., brown);
    (b 0. (-32.) 0. 64. 256. 1024., grey);
    (b 960. (-32.) 0. 1024. 256. 1024., grey);
    (b 0. (-32.) 0. 1024. 256. 64., grey);
    (b 0. (-32.) 960. 1024. 256. 1024., grey);
    (* the ceilings: room B is taller than the others *)
    (b 64. 160. 64. 448. 256. 448., grey);
    (b 576. 224. 64. 960. 256. 448., grey);
    (b 576. 160. 576. 960. 256. 960., dark);
    (* the rock north of room A *)
    (b 64. (-32.) 448. 448. 256. 960., grey);
    (* the wall between A and B, with the doorway at z 224..288 *)
    (b 448. (-32.) 64. 576. 256. 224., grey);
    (b 448. (-32.) 288. 576. 256. 960., grey);
    (b 448. 96. 224. 576. 256. 288., grey);
    (* the wall between B and C, with the doorway at x 736..800 *)
    (b 576. (-32.) 448. 736. 256. 576., grey);
    (b 800. (-32.) 448. 960. 256. 576., grey);
    (b 736. 96. 448. 800. 256. 576., grey);
    (* the pillar in A, the platform in B *)
    (b 224. 0. 320. 288. 160. 384., brown);
    (b 640. 0. 128. 800. 64. 288., brown) ]

(* the lamps: where, how far they reach, how strong, what color *)
type lamp = { where : vec; reach : number; strength : number; tint : number * number * number }

let lamps : lamp list =
  [ { where = (352., 130., 352.); reach = 420.; strength = 2.86; tint = (1., 0.92, 0.8) };
    { where = (144., 120., 144.); reach = 260.; strength = 1.56; tint = (1., 0.9, 0.75) };
    { where = (512., 80., 256.); reach = 200.; strength = 1.82; tint = (1., 0.9, 0.7) };
    { where = (768., 190., 256.); reach = 500.; strength = 3.12; tint = (0.9, 0.95, 1.) };
    { where = (640., 120., 400.); reach = 300.; strength = 1.56; tint = (1., 0.95, 0.85) };
    { where = (768., 80., 512.); reach = 220.; strength = 1.56; tint = (1., 0.85, 0.7) };
    { where = (700., 140., 760.); reach = 420.; strength = 2.08; tint = (1., 0.6, 0.45) } ]

let ambient = 0.18

(* where the player starts, the three runes, and the exit pad *)
let start_at : vec = (128., 0., 256.)
let runes : vec list = [ (384., 40., 144.); (720., 104., 208.); (760., 40., 760.) ]
let exit_at : vec = (144., 0., 400.)

(*****************************************************************************)
(* qbsp, 1: the faces of the rock *)
(*****************************************************************************)

(* Every box has 6 sides; a side is a face of the level only where the
 * air is on the other side. Where another box is against it, it is a
 * surface inside the rock, which nothing can see: qbsp's CSGFaces
 * throws those away, and so does this. The other boxes' edges cut the
 * side into a grid (as in gamekits/segments/Segments.ml's [rock]), and a
 * piece is kept if the air just outside it is really air:
 *
 *      +-----+-----+          the side of a box, against another box
 *      |     |#####|          (####): the two pieces on the left are
 *      |     |#####|          faces of the level, the right one is
 *      +-----+-----+          dropped
 *)
let csg (boxes : (box * (int * int * int)) list) : face list =
  let all = List.map fst boxes in
  List.concat_map
    (fun ((b : box), rgb) ->
      List.concat_map
        (fun axis ->
          List.concat_map
            (fun positive ->
              let at = if positive then high b axis else low b axis in
              let out = if positive then 1. else -1. in
              let u = (axis + 1) mod 3 and v = (axis + 2) mod 3 in
              (* the other boxes that touch this plane from the air side *)
              let others =
                List.filter
                  (fun (o : box) ->
                    o != b && low o axis < at +. (out *. 0.5) && high o axis > at +. (out *. 0.5))
                  all
              in
              let cuts lo hi ends = List.sort_uniq compare (List.filter (fun x -> x > lo && x < hi) ends @ [ lo; hi ]) in
              let us = cuts (low b u) (high b u) (List.concat_map (fun o -> [ low o u; high o u ]) others) in
              let vs = cuts (low b v) (high b v) (List.concat_map (fun o -> [ low o v; high o v ]) others) in
              let rec pairs = function a :: (c :: _ as rest) -> (a, c) :: pairs rest | _ -> [] in
              List.concat_map
                (fun (ua, ub) ->
                  List.filter_map
                    (fun (va, vb) ->
                      let just_outside = point axis (at +. (out *. 0.5)) ((ua +. ub) /. 2.) ((va +. vb) /. 2.) in
                      if List.exists (fun o -> inside_box o just_outside) others then None
                      else Some { axis; positive; at; u0 = ua; u1 = ub; v0 = va; v1 = vb; rgb })
                    (pairs vs))
                (pairs us))
            [ false; true ])
        [ 0; 1; 2 ])
    boxes

let faces : face list = csg rock

(*****************************************************************************)
(* qbsp, 2: the tree *)
(*****************************************************************************)

(* A node splits space in two with one face's plane; a leaf is what is
 * left when no face crosses it any more: a box of solid rock, or a box
 * of air. The air leaves are numbered: they are what [vis] works on and
 * what the game draws. *)
type bsp =
  | Node of { axis : int; at : number; positive : bool; front : bsp; back : bsp }
  | Leaf of { solid : bool; id : int; box : box }

(* how far a face reaches along an axis (a point, if it lies on a plane
 * across it) *)
let span (f : face) (axis : int) : number * number =
  if f.axis = axis then (f.at, f.at)
  else if axis = (f.axis + 1) mod 3 then (f.u0, f.u1)
  else (f.v0, f.v1)

(* The plane to split by -- a face's own plane, with the side its air
 * is on: the one that cuts the fewest faces in two, and, all else
 * equal, the one that leaves the two sides most even. What qbsp's
 * SelectPartition weighs, and TinyDoom's node builder before
 * it. *)
let choose (fs : face list) : int * number * bool =
  let planes = List.sort_uniq compare (List.map (fun f -> (f.axis, f.at, f.positive)) fs) in
  let cost (axis, at, _) =
    let front, back, cut =
      List.fold_left
        (fun (fr, bk, sp) f ->
          let lo, hi = span f axis in
          if hi <= at then (fr, bk + 1, sp) else if lo >= at then (fr + 1, bk, sp) else (fr, bk, sp + 1))
        (0, 0, 0) fs
    in
    (3 * cut) + abs (front - back)
  in
  List.fold_left (fun best p -> if cost p < cost best then p else best) (List.hd planes) planes

(* a face cut by a plane: what of it is on the axis' high side, what on
 * its low side *)
let split (f : face) (axis : int) (at : number) : face option * face option =
  let lo, hi = span f axis in
  if hi <= at then (None, Some f)
  else if lo >= at then (Some f, None)
  else if axis = (f.axis + 1) mod 3 then (Some { f with u0 = at }, Some { f with u1 = at })
  else (Some { f with v0 = at }, Some { f with v1 = at })

let cut_box (b : box) (axis : int) (at : number) : box * box =
  let high_side = match axis with 0 -> { b with x0 = at } | 1 -> { b with y0 = at } | _ -> { b with z0 = at } in
  let low_side = match axis with 0 -> { b with x1 = at } | 1 -> { b with y1 = at } | _ -> { b with z1 = at } in
  (high_side, low_side)

(* qbsp's SolidBSP: the faces split by the chosen plane, the ones lying
 * on it and facing the same way kept out of both sides (they are the
 * plane), and each side built in turn. The rule that makes the leaves
 * solid or air is the faces' own: the air is in front of a face, the
 * rock behind it, so a piece of space with no faces left in it is air
 * if we came down the front of the last plane and rock if we came down
 * its back.
 *
 *          air   |   rock          the face, seen edge on: its plane
 *      <---------+--------->       splits the world, and everything
 *          front | back            beyond it on the back is inside
 *)
let air_leaves = ref 0

let rec build (fs : face list) (b : box) (solid : bool) : bsp =
  match fs with
  | [] ->
      let id = if solid then -1 else !air_leaves in
      if not solid then incr air_leaves;
      Leaf { solid; id; box = b }
  | _ ->
      let axis, at, positive = choose fs in
      let rest = List.filter (fun f -> not (f.axis = axis && f.at = at && f.positive = positive)) fs in
      let highs = List.filter_map (fun f -> fst (split f axis at)) rest in
      let lows = List.filter_map (fun f -> snd (split f axis at)) rest in
      let high_box, low_box = cut_box b axis at in
      (* the front of the plane is where its face's air is *)
      let front_faces, front_box, back_faces, back_box =
        if positive then (highs, high_box, lows, low_box) else (lows, low_box, highs, high_box)
      in
      Node { axis; at; positive; front = build front_faces front_box false; back = build back_faces back_box true }

(* the world, a little larger than the map *)
let world : box = { x0 = -64.; y0 = -96.; z0 = -64.; x1 = 1088.; y1 = 320.; z1 = 1088. }
let tree : bsp = build faces world false
let leaves : int = !air_leaves

let rec leaf_at (t : bsp) (p : vec) : bsp =
  match t with
  | Leaf _ -> t
  | Node n ->
      (* the high side of the plane is its front when the face's air is
       * on the high side *)
      let high = coord p n.axis >= n.at in
      leaf_at (if high = n.positive then n.front else n.back) p

let solid_at (p : vec) : bool = match leaf_at tree p with Leaf l -> l.solid | Node _ -> false
let leaf_id (p : vec) : int = match leaf_at tree p with Leaf l -> l.id | Node _ -> -1

(* every air leaf's box, by its number *)
let leaf_boxes : box array =
  let a = Array.make (max 1 leaves) world in
  let rec go t = match t with Leaf l -> if not l.solid then a.(l.id) <- l.box | Node n -> (go n.front; go n.back) in
  go tree;
  a

(* is the straight line from [a] to [b] all air? (the ray cast every
 * tool here needs: the light's shadows, the visibility, the walking) *)
let clear (a : vec) (b : vec) : bool =
  let n = int_of_float (length (sub b a) /. 8.) + 1 in
  let rec go k =
    k > n
    || (let f = float_of_int k /. float_of_int n in
        (not (solid_at (add a (times f (sub b a))))) && go (k + 1))
  in
  go 0

(*****************************************************************************)
(* vis: which leaves can see which *)
(*****************************************************************************)

(* A few points spread through a leaf: its middle, and the middles of
 * its eight halves, all pulled in from the walls. *)
let samples (b : box) : vec list =
  let inset lo hi f = lo +. ((hi -. lo) *. f) in
  let at fx fy fz = (inset b.x0 b.x1 fx, inset b.y0 b.y1 fy, inset b.z0 b.z1 fz) in
  at 0.5 0.5 0.5
  :: List.concat_map (fun fx -> List.concat_map (fun fy -> List.map (fun fz -> at fx fy fz) [ 0.25; 0.75 ]) [ 0.25; 0.75 ]) [ 0.25; 0.75 ]

(* two leaves see each other if any line between their sample points is
 * all air. Quake's vis worked it out exactly, by clipping sight
 * through chains of portals; sampling can miss a sliver of a view (and
 * would then leave a hole on screen), which is why Quake's was worth
 * the hours it took. *)
let sees (a : int) (b : int) : bool =
  a = b
  ||
  let pa = samples leaf_boxes.(a) and pb = samples leaf_boxes.(b) in
  List.exists (fun x -> List.exists (fun y -> clear x y) pb) pa

(* the set for a leaf, worked out the first time the player stands in
 * it and kept (Quake's was in the .bsp file, computed once for all) *)
let known : (int, bool array) Hashtbl.t = Hashtbl.create 64

let pvs (id : int) : bool array =
  match Hashtbl.find_opt known id with
  | Some set -> set
  | None ->
      let set = Array.init leaves (fun other -> sees id other) in
      Hashtbl.replace known id set;
      set

(*****************************************************************************)
(* light: the lightmaps, baked *)
(*****************************************************************************)

(* a patch's side, in units: Quake's lightmaps had one value every 16,
 * stretched over the face by the hardware or its surface cache; here
 * a patch is a polygon of its own, so they are bigger *)
let patch = 32.

(* How much light reaches a point on a surface facing [n]: every lamp
 * that is on the right side of it and can see it, weakened by the
 * angle it arrives at (Lambert's cosine law: a beam spread over a
 * slanted surface covers more of it) and by the distance. Nothing
 * bounces: Quake's light had no radiosity either (QRAD came with
 * Quake II). *)
let lit (p : vec) (n : vec) : number * number * number =
  List.fold_left
    (fun (r, g, b) (l : lamp) ->
      let towards = sub l.where p in
      let distance = length towards in
      let cosine = dot n (normalize towards) in
      if cosine <= 0. || distance > l.reach || not (clear p l.where) then (r, g, b)
      else
        let k = l.strength *. cosine *. (1. -. (distance /. l.reach)) in
        let tr, tg, tb = l.tint in
        (r +. (k *. tr), g +. (k *. tg), b +. (k *. tb)))
    (ambient, ambient, ambient) lamps

(* A face cut into patches, each with its own color and the leaf it
 * belongs to: the drawing unit and the visibility unit at once (Quake
 * split its faces at the nodes instead, so that each belongs to one
 * leaf). *)
let patches_of (f : face) : (int * shape3d) list =
  let n = normal f in
  let steps lo hi = List.init (max 1 (int_of_float (Float.ceil ((hi -. lo) /. patch)))) (fun i -> lo +. (float_of_int i *. patch)) in
  List.concat_map
    (fun u ->
      List.map
        (fun v ->
          let u1 = Float.min f.u1 (u +. patch) and v1 = Float.min f.v1 (v +. patch) in
          let middle = point f.axis f.at ((u +. u1) /. 2.) ((v +. v1) /. 2.) in
          let r, g, b = lit (add middle (times 0.5 n)) n in
          let fr, fg, fb = f.rgb in
          let c x k = int_of_float (Float.min 255. (float_of_int x *. k)) in
          let small = { f with u0 = u; u1; v0 = v; v1 } in
          (leaf_id (add middle (times 2. n)), polygon3d (rgb (c fr r) (c fg g) (c fb b)) (corners small)))
        (steps f.v0 f.v1))
    (steps f.u0 f.u1)

(* every patch, grouped by the leaf it is in: one cached3d shape per
 * leaf, so the GPU backends keep them and the game only picks which
 * ones to draw *)
let by_leaf : shape3d list array =
  let a = Array.make (max 1 leaves) [] in
  List.iter (fun (id, s) -> if id >= 0 then a.(id) <- s :: a.(id)) (List.concat_map patches_of faces);
  a

let drawn_of : int array = Array.map List.length by_leaf
let lit_leaves : shape3d array = Array.map (fun shapes -> cached3d shapes) by_leaf
let all_patches : int = Array.fold_left ( + ) 0 drawn_of

(*****************************************************************************)
(* The player *)
(*****************************************************************************)

let eye_height = 40.
let player_radius = 16.
let player_height = 56.

type model = {
  p : vec;
  (* how fast it is falling *)
  fall : number;
  yaw : number;
  pitch : number;
  runes : vec list;
  (* the visibility set in use, and the v key last frame *)
  set : bool;
  v_was : bool;
  frames : int;
  over : int option;
}

let initial_model : model =
  { p = start_at; fall = 0.; yaw = -90.; pitch = 0.; runes; set = true; v_was = false; frames = 0; over = None }

(* the player is a box: it fits where none of its corners is rock *)
let fits (x, y, z) : bool =
  let r = player_radius -. 1. in
  List.for_all
    (fun (dx, dz) -> List.for_all (fun dy -> not (solid_at (x +. dx, y +. dy, z +. dz))) [ 0.5; player_height /. 2.; player_height -. 0.5 ])
    [ (-.r, -.r); (r, -.r); (-.r, r); (r, r) ]

(* [towards p (dx, dy, dz)]: as far along it as the rock allows, a unit
 * at a time so that a fall ends flush with the floor, and whether
 * something stopped it *)
let towards (p : vec) (d : vec) : vec * bool =
  let total = length d in
  let step = if total = 0. then (0., 0., 0.) else times (1. /. total) d in
  let rec go p left =
    if left <= 0.001 then (p, false)
    else
      let this = times (Float.min 1. left) step in
      if fits (add p this) then go (add p this) (left -. 1.) else (p, true)
  in
  go p total

(* One axis at a time, so that a wall stops what goes into it and not
 * what slides along it; and where a step stops the walk, the same step
 * lifted: Quake let the player climb 18 units without jumping, which
 * is what stairs are made of. *)
let slide (p : vec) ((dx, _, dz) : vec) : vec =
  let step_up = 18. in
  List.fold_left
    (fun q d ->
      match towards q d with
      | q', false -> q'
      | q', true ->
          let lifted, _ = towards q' (0., step_up, 0.) in
          let over, _ = towards lifted d in
          if over = lifted then q' else fst (towards over (0., -.step_up, 0.)))
    p
    [ (dx, 0., 0.); (0., 0., dz) ]

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let v_now = Set_.mem "v" k.keys in
  let m = { m with set = (if v_now && not m.v_was then not m.set else m.set); v_was = v_now } in
  match m.over with
  | Some _ -> if k.kspace then initial_model else m
  | None ->
      let yaw = m.yaw +. (2.5 *. axis k.kleft k.kright) in
      let pitch = Float.max (-60.) (Float.min 60. (m.pitch +. (2. *. axis k.kup k.kdown))) in
      let a = yaw *. Float.pi /. 180. in
      let ahead = (-.(sin a), 0., -.(cos a)) and right = (cos a, 0., -.(sin a)) in
      let p = slide m.p (add (times (5. *. axis k.kw k.ks) ahead) (times (4. *. axis k.kd k.ka) right)) in
      (* gravity, and the jump: [fall] is how fast it is going down *)
      let standing = not (fits (add p (0., -1., 0.))) in
      let fall = if standing && k.kspace then -16. else if standing then 0. else m.fall +. 1.4 in
      let p, stopped = towards p (0., -.fall, 0.) in
      let fall = if stopped then 0. else fall in
      let runes = List.filter (fun r -> length (sub r (add p (0., eye_height /. 2., 0.))) > 48.) m.runes in
      let out = runes = [] && length (sub exit_at p) < 56. in
      { m with p; fall; yaw; pitch; runes; frames = m.frames + 1; over = (if out then Some m.frames else None) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let panel = 160.
let text color size str = words color str |> scale size

let spin (frames : int) (where : vec) (color : color) (size : number) : shape3d =
  let x, y, z = where in
  group3d [ cube color size |> rotate3d 30. (float_of_int frames *. 2.) 0. ] |> move3d x y z

let view_panel (screen : screen) (m : model) (seen : int) (patches : int) : shape list =
  let time = float_of_int (Option.value m.over ~default:m.frames) /. 60. in
  let stat i str = text (rgb 230 200 120) 2.5 str |> move (screen.left +. 230.) (screen.bottom +. 120. -. (float_of_int i *. 38.)) in
  [ rectangle (rgb 28 24 22) screen.width panel |> move_y (screen.bottom +. (panel /. 2.));
    stat 0 (Printf.sprintf "TIME %d:%02d" (int_of_float time / 60) (int_of_float time mod 60));
    stat 1 (Printf.sprintf "RUNES %d / %d" (List.length runes - List.length m.runes) (List.length runes));
    stat 2 (Printf.sprintf "LEAVES %d / %d" seen leaves);
    text (rgb 230 200 120) 2.5 (Printf.sprintf "PATCHES %d / %d" patches all_patches) |> move (screen.right -. 260.) (screen.bottom +. 120.);
    text (rgb 160 140 110) 2.5 (if m.set then "v: PVS on" else "v: PVS OFF, all drawn") |> move (screen.right -. 260.) (screen.bottom +. 82.) ]

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let eye = add m.p (0., eye_height, 0.) in
  let a = m.yaw *. Float.pi /. 180. and t = m.pitch *. Float.pi /. 180. in
  let ahead = (-.(sin a *. cos t), sin t, -.(cos a *. cos t)) in
  let fov = 2. *. atan (screen.height /. (screen.height -. panel)) *. 180. /. Float.pi in
  let cam = camera ~eye ~target:(add eye (times 10. ahead)) ~fov ~near:1. ~far:2000. () in
  (* the whole of the level the eye's leaf could ever see -- or all of
   * it, with the "v" key, to see what the set was saving *)
  let here = leaf_id eye in
  let set = if m.set && here >= 0 then pvs here else Array.make leaves true in
  let shown = List.filter (fun i -> set.(i)) (List.init leaves Fun.id) in
  let level = List.map (fun i -> lit_leaves.(i)) shown in
  let patches = List.fold_left (fun n i -> n + drawn_of.(i)) 0 shown in
  let things =
    List.map (fun r -> spin m.frames r (rgb 240 200 60) 24.) m.runes
    @ [ spin (m.frames / 4) (add exit_at (0., 12., 0.)) (if m.runes = [] then rgb 120 240 160 else rgb 80 110 90) 40. ]
  in
  let huds =
    List.map hud (view_panel screen m (List.length shown) patches)
    @
    match m.over with
    | Some _ -> [ hud (text yellow 6. "ESCAPED!" |> move_y 250.); hud (text white 3. "space: again" |> move_y 170.) ]
    | None ->
        if m.runes = [] then [ hud (text yellow 4. "TO THE EXIT" |> move_y 300.) ]
        else if m.frames < 240 then
          [ hud (text yellow 4. "FIND THE 3 RUNES" |> move_y 300.);
            hud (text white 2.5 "arrows: look   w/s/a/d: move   space: jump   v: pvs" |> move_y 230.) ]
        else []
  in
  (cam, level @ things @ huds)

let app = game3d view update initial_model

(* the light is baked into the patches' colors: the renderer must not
 * add its own *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = No_lighting } app
