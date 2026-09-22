(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Portal (Valve, 2007), in 3D -- games/TinyPortal2D.ml
 * is the Flash version's. One test chamber: the weighted cube is up on
 * a ledge four metres high, the button that opens the exit is on the
 * floor, and no jump reaches the ledge. W/A/S/D and space to walk and
 * jump, the arrows (or the mouse) to look, z (or a click) for the blue
 * portal, x (or a right click) for the orange one -- on the white
 * panels only -- and f to pick up or drop the cube. (Not q: the native
 * backends quit on it, every game's.)
 *
 * The way up is Portal's first lesson: a portal in the floor, one on
 * the wall above the ledge, and walk into the floor. A portal pair is
 * one rigid motion (Portal3d.mli): what goes in one comes
 * out of the other turned, its speed turned with it and not scaled --
 * the fall into the floor becomes a fling out of the wall, "speedy
 * thing goes in, speedy thing comes out". The fling is kept until you
 * land ([fling]): a player walking in Character3d has no momentum of
 * its own, and this is the one place the game needs it.
 *
 * Everything that crosses goes through the same motion -- you (your
 * place, your fall, which way you look), the cube (its place, velocity,
 * orientation and spin: Portal3d.carry), and the cube while you hold it
 * -- and it crosses when its middle goes through the portal's
 * rectangle, from the front. While both portals are open, their two
 * panels are holes: taken out of the solids, so that you can be half
 * way through, and not drawn, so that you can see through.
 *
 * What you see through a portal is the expensive half, and it is done
 * the way the plan (plan_physics3d_teaching.md, phase 12) said it would
 * be: no stencil buffer, no render-to-texture -- the whole chamber is
 * taken through the pair's motion, so that what is in front of the
 * other portal lands behind this one, and cut to what the eye can see
 * through this one's rectangle (Portal3d.clip: the four planes through
 * the eye and its edges, and its own plane). The pieces are then
 * ordinary polygons behind a hole in the wall, and the z-buffer does
 * the rest. One level: a portal seen through a portal shows its colour,
 * not a view. You can see yourself.
 *
 * The simplifications, said up front, as the 2D game says its own: a
 * portal is a whole panel (2 m square), where Portal's are an oval cut
 * out of a wall; portals go only on the chamber's walls and floor, so
 * that nothing real is ever behind a hole; and the player comes out
 * upright, only which way it looks turned.
 *
 * What it uses: Portal3d (the motion, crossing, the cut), Character3d
 * (you), Physics3d (the cube in a world, held_by, ray-free: the portal
 * gun's ray is against the panels, in [aim]), Scene2d.
 *
 * Exercises: portals seen through portals, one level more (the cut
 * twice, and a second copy of the chamber); a portal on a moving
 * panel; the portal gun's ray passing through a portal; turrets.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The chamber: panels *)
(*****************************************************************************)

type vec = number * number * number

(* A panel: a 2 m square of surface, facing into the chamber. White ones
 * take portals; the metal ones do not. *)
type panel = { centre : vec; normal : vec; up : vec; white : bool; door : bool }

let panel_size = 2.

(* a grid of panels over a rectangle of a surface, from its [corner]:
 * [across] of them along up x normal (the panels' right), [high] of
 * them along [up] *)
let grid ?(white = true) (corner : vec) (normal : vec) (up : vec) (across : int) (high : int) : panel list =
  let r = Vec3.cross up normal in
  List.concat
    (List.init across (fun i ->
         List.init high (fun j ->
             let c =
               Vec3.add corner
                 (Vec3.add (Vec3.scale ((float_of_int i +. 0.5) *. panel_size) r) (Vec3.scale ((float_of_int j +. 0.5) *. panel_size) up))
             in
             { centre = c; normal; up; white; door = false })))

(* The chamber: 16 m square, 8 m high, a ledge 4 m up along its back
 * (z from -8 to -4), the exit's door in the front wall. *)
let panels : panel list =
  let x_ = (1., 0., 0.) and y_ = (0., 1., 0.) and z_ = (0., 0., 1.) in
  let neg (a, b, c) = (-.a, -.b, -.c) in
  (* the floor, in front of the ledge *)
  grid (8., 0., -4.) y_ z_ 8 6
  (* the ledge: its top, and its face *)
  @ grid ~white:false (8., 4., -8.) y_ z_ 8 2
  @ grid ~white:false (-8., 0., -4.) z_ y_ 8 2
  (* the back wall, above the ledge *)
  @ grid (-8., 4., -8.) z_ y_ 8 2
  (* the front wall, with the door in the middle of its bottom row *)
  @ List.map
      (fun p -> let x, y, _ = p.centre in if Float.abs x < 1.5 && y < 2. then { p with white = false; door = true } else p)
      (grid (8., 0., 8.) (neg z_) y_ 8 4)
  (* the side walls, above the ledge where it runs along them *)
  @ List.filter (fun p -> let _, y, z = p.centre in not (z < -4. && y < 4.)) (grid (-8., 0., 8.) x_ y_ 8 4)
  @ List.filter (fun p -> let _, y, z = p.centre in not (z < -4. && y < 4.)) (grid (8., 0., -8.) (neg x_) y_ 8 4)
  (* the ceiling *)
  @ grid ~white:false (8., 8., 8.) (neg y_) (neg z_) 8 8

(* where the portal gun's shot lands: the nearest panel along the ray *)
let aim (eye : vec) (dir : vec) (candidates : panel list) : panel option =
  List.fold_left
    (fun best p ->
      let denom = Vec3.dot dir p.normal in
      if denom >= -1e-9 then best
      else
        let t = Vec3.dot (Vec3.sub p.centre eye) p.normal /. denom in
        let hit = Vec3.add eye (Vec3.scale t dir) in
        let off = Vec3.sub hit p.centre in
        let inside = Float.abs (Vec3.dot off (Vec3.cross p.up p.normal)) <= 1. && Float.abs (Vec3.dot off p.up) <= 1. in
        match best with
        | Some (bt, _) when bt <= t -> best
        | _ -> if t > 0. && inside then Some (t, p) else best)
    None candidates
  |> Option.map snd

let button_at = (4., 3.)
let start = (0., 0., 6.)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type portal = { at : panel; shape : Portal3d.t }

type game = {
  me : Character3d.t;
  fling : number * number; (* speed across the ground kept from a portal, until you land *)
  yaw : number;
  pitch : number;
  blue : portal option;
  orange : portal option;
  world : Physics3d.world; (* the cube first, then the solid panels *)
  holding : bool;
  keys_down : string list;
}

type scene = Title | Playing of game | Complete of game
type model = scene Scene2d.t

let open_pair (g : game) : (portal * portal) option = match (g.blue, g.orange) with Some b, Some o -> Some (b, o) | _ -> None

let pressed_button (cube : Physics3d.body) : bool =
  let bx, bz = button_at in
  cube.y < 0.5 && Float.hypot (cube.x -. bx) (cube.z -. bz) < 0.7

(* the panels that are solid now: all but the two open portals', and
 * the door when the button is down *)
let solid_panels (blue : portal option) (orange : portal option) (door_open : bool) : panel list =
  let holes = match (blue, orange) with Some b, Some o -> [ b.at; o.at ] | _ -> [] in
  List.filter (fun p -> (not (List.memq p holes)) && not (p.door && door_open)) panels

let panel_body (p : panel) : Physics3d.body =
  let r = Vec3.cross p.up p.normal in
  let x, y, z = Vec3.sub p.centre (Vec3.scale 0.2 p.normal) in
  { (Physics3d.body (box white panel_size panel_size 0.4)) with orientation = Portal3d.of_frame r p.up p.normal }
  |> Physics3d.at x y z |> Physics3d.immovable |> Physics3d.rough 0.6

let cube_body = Physics3d.body (cube (rgb 200 200 210) 0.6) |> Physics3d.at 0. 4.3 (-6.) |> Physics3d.heavy 25. |> Physics3d.rough 0.6

let door_open (g : game) : bool = pressed_button (List.hd g.world.bodies)

(* the world again, the cube as it is, when the solids change *)
let rebuild (g : game) : game =
  let cube = List.hd g.world.bodies in
  let solids = solid_panels g.blue g.orange (pressed_button cube) in
  { g with world = Physics3d.world (cube :: List.map panel_body solids) }

let new_game () : game =
  let x, y, z = start in
  rebuild
    { me = Character3d.make x y z; fling = (0., 0.); yaw = 0.; pitch = 0.; blue = None; orange = None;
      world = Physics3d.world [ cube_body ]; holding = false; keys_down = [] }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let eye_height = 1.6
let eye (g : game) : vec = (g.me.x, g.me.y +. eye_height, g.me.z)

let look (g : game) : vec =
  let y = g.yaw *. Float.pi /. 180. and p = g.pitch *. Float.pi /. 180. in
  (cos p *. sin y, sin p, -.(cos p *. cos y))

(* a portal on a panel: on a wall, up is up; on the floor, up is where
 * you were looking, so that you come out of it facing on *)
let portal_on (g : game) (p : panel) : portal =
  let lx, _, lz = look g in
  let up =
    let _, ny, _ = p.normal in
    if Float.abs ny > 0.9 then Vec3.normalize (lx, 0., lz) else p.up
  in
  { at = p; shape = { Portal3d.centre = Vec3.add p.centre (Vec3.scale 0.001 p.normal); normal = p.normal; up; width = 2.; height = 2. } }

let shoot (g : game) (blue : bool) : game =
  match aim (eye g) (look g) (solid_panels g.blue g.orange (door_open g) @ List.filter_map (Option.map (fun p -> p.at)) [ g.blue; g.orange ]) with
  | Some p when p.white ->
      let other = if blue then g.orange else g.blue in
      if (match other with Some o -> o.at == p | None -> false) then g
      else rebuild (if blue then { g with blue = Some (portal_on g p) } else { g with orange = Some (portal_on g p) })
  | _ -> g

(* You through a portal: your middle taken through, your fall and your
 * fling turned with it, which way you look too; out upright *)
let through_me (g : game) (before : Character3d.t) : game =
  match open_pair g with
  | None -> g
  | Some (b, o) ->
      let mid (c : Character3d.t) = (c.x, c.y +. (c.height /. 2.), c.z) in
      let go (from : portal) (into : portal) =
        let x, y, z = Portal3d.point ~from:from.shape ~into:into.shape (mid g.me) in
        let fx, fz = g.fling in
        let vx, vy, vz = Portal3d.direction ~from:from.shape ~into:into.shape (fx, g.me.vy, fz) in
        (* the look taken through too; but you stay upright, so a look
         * that comes out nearly straight up or down -- walking forward
         * into a floor portal, looking ahead, comes out of a wall one
         * looking at the ceiling, which a first version kept, heading
         * and all -- is set level, out of the portal *)
        let lx, ly, lz = Portal3d.direction ~from:from.shape ~into:into.shape (look g) in
        let deg a = a *. 180. /. Float.pi in
        let yaw, pitch =
          if Float.hypot lx lz > 0.5 then (deg (atan2 lx (-.lz)), deg (Float.asin (Float.max (-1.) (Float.min 1. ly))))
          else
            let nx, _, nz = into.shape.normal in
            if Float.hypot nx nz > 0.5 then (deg (atan2 nx (-.nz)), 0.) else (g.yaw, 0.)
        in
        (* out a little way in front, so as not to be in the wall *)
        let nx, ny, nz = into.shape.normal in
        { g with
          me = { g.me with x = x +. (0.35 *. nx); y = y -. (g.me.height /. 2.) +. (0.35 *. ny); z = z +. (0.35 *. nz); vy; grounded = false };
          fling = (vx, vz); yaw; pitch }
      in
      if Portal3d.crossed b.shape ~before:(mid before) ~after:(mid g.me) then go b o
      else if Portal3d.crossed o.shape ~before:(mid before) ~after:(mid g.me) then go o b
      else g

let through_cube (g : game) (before : Physics3d.body) : game =
  match open_pair g with
  | None -> g
  | Some (b, o) ->
      let cube = List.hd g.world.bodies in
      let p (c : Physics3d.body) = (c.x, c.y, c.z) in
      let carried =
        if Portal3d.crossed b.shape ~before:(p before) ~after:(p cube) then Some (Portal3d.carry ~from:b.shape ~into:o.shape cube)
        else if Portal3d.crossed o.shape ~before:(p before) ~after:(p cube) then Some (Portal3d.carry ~from:o.shape ~into:b.shape cube)
        else None
      in
      match carried with None -> g | Some c -> { g with world = { g.world with bodies = c :: List.tl g.world.bodies } }

let walk (computer : computer) (g : game) : game =
  let k = computer.keyboard and m = computer.mouse in
  let axis a b = (if a then 1. else 0.) -. if b then 1. else 0. in
  let yaw = g.yaw +. (0.15 *. m.mdx) +. (2.5 *. axis k.kright k.kleft) in
  let pitch = Float.max (-85.) (Float.min 85. (g.pitch +. (0.15 *. m.mdy) +. (1.5 *. axis k.kup k.kdown))) in
  let ahead = axis k.kw k.ks and aside = axis k.kd k.ka in
  let y = yaw *. Float.pi /. 180. in
  let fx, fz = g.fling in
  let vx = (4. *. ((ahead *. sin y) +. (aside *. cos y))) +. fx and vz = (4. *. ((aside *. sin y) -. (ahead *. cos y))) +. fz in
  let solids = List.tl g.world.bodies in
  let me = Character3d.walk ~jump:(if k.kspace then 4.5 else 0.) solids (vx, vz) g.me in
  (* the fling lasts until the ground takes it *)
  let fling = if me.grounded then (0., 0.) else g.fling in
  { g with yaw; pitch; me; fling }

let cube_and_hands (computer : computer) (g : game) : game =
  let down key = Set_.mem key computer.keyboard.keys in
  let pressed key = down key && not (List.mem key g.keys_down) in
  let cube = List.hd g.world.bodies in
  let ex, ey, ez = eye g and lx, ly, lz = look g in
  let near = Vec3.length (Vec3.sub (cube.x, cube.y, cube.z) (ex, ey, ez)) < 2.5 in
  let holding = if pressed "f" then (not g.holding) && near else g.holding in
  let cube = if holding then Physics3d.held_by (ex +. (1.5 *. lx), ey +. (1.5 *. ly), ez +. (1.5 *. lz)) cube else cube in
  let bodies = (cube |> Physics3d.fall 9.8) :: List.tl g.world.bodies in
  let world = Physics3d.simulate ~sleeping:false { g.world with bodies } in
  { g with holding; world }

let update_game (computer : computer) (g : game) : game =
  let down key = Set_.mem key computer.keyboard.keys in
  let pressed key = down key && not (List.mem key g.keys_down) in
  let m = computer.mouse in
  let keys_down = List.filter down [ "z"; "x"; "f" ] @ (if m.mdown then [ "click" ] else []) @ if m.mrdown then [ "rclick" ] else [] in
  let was_open = door_open g in
  let g = if pressed "z" || (m.mdown && not (List.mem "click" g.keys_down)) then shoot g true else g in
  let g = if pressed "x" || (m.mrdown && not (List.mem "rclick" g.keys_down)) then shoot g false else g in
  let before_me = g.me and before_cube = List.hd g.world.bodies in
  let g = walk computer g in
  let g = through_me g before_me in
  let g = cube_and_hands computer g in
  let g = through_cube g before_cube in
  let g = { g with keys_down } in
  if door_open g <> was_open then rebuild g else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer g in
      if g.me.z > 8.3 then Scene2d.go (Complete g) s else { s with scene = Playing g }
  | Complete _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

type poly = { color : color; points : vec list }

let blue = rgb 60 140 255
let orange = rgb 255 150 40

let panel_poly (p : panel) : poly =
  let r = Vec3.cross p.up p.normal in
  let c sx sy = Vec3.add p.centre (Vec3.add (Vec3.scale sx r) (Vec3.scale sy p.up)) in
  let x, y, z = p.centre in
  let shade = if (int_of_float (Float.round (x +. y +. z)) / 2) mod 2 = 0 then 0 else 12 in
  let color =
    if p.door then rgb 200 60 50 else if p.white then rgb (222 - shade) (222 - shade) (215 - shade) else rgb (95 - shade) (100 - shade) (108 - shade)
  in
  { color; points = [ c (-1.) (-1.); c 1. (-1.); c 1. 1.; c (-1.) 1. ] }

(* a box's six faces, [half] its half sides, turned by [q], at [at] *)
let box_polys (color : color) ((hx, hy, hz) : vec) (q : Quat.t) (at : vec) : poly list =
  let corner sx sy sz = Vec3.add at (Quat.rotate q (sx *. hx, sy *. hy, sz *. hz)) in
  let face a b c d = { color; points = [ a; b; c; d ] } in
  let k = corner in
  [ face (k (-1.) (-1.) 1.) (k 1. (-1.) 1.) (k 1. 1. 1.) (k (-1.) 1. 1.);
    face (k 1. (-1.) (-1.)) (k (-1.) (-1.) (-1.)) (k (-1.) 1. (-1.)) (k 1. 1. (-1.));
    face (k (-1.) 1. 1.) (k 1. 1. 1.) (k 1. 1. (-1.)) (k (-1.) 1. (-1.));
    face (k (-1.) (-1.) (-1.)) (k 1. (-1.) (-1.)) (k 1. (-1.) 1.) (k (-1.) (-1.) 1.);
    face (k 1. (-1.) 1.) (k 1. (-1.) (-1.)) (k 1. 1. (-1.)) (k 1. 1. 1.);
    face (k (-1.) (-1.) (-1.)) (k (-1.) (-1.) 1.) (k (-1.) 1. 1.) (k (-1.) 1. (-1.)) ]

(* a portal's rim: four strips just inside its hole's edge; a portal
 * with no partner yet is a flat colour *)
let rim (color : color) (open_ : bool) (p : Portal3d.t) : poly list =
  let r = Portal3d.right p and u = p.up and c = Vec3.add p.centre (Vec3.scale 0.02 p.normal) in
  let at sx sy = Vec3.add c (Vec3.add (Vec3.scale sx r) (Vec3.scale sy u)) in
  if not open_ then [ { color; points = [ at (-1.) (-1.); at 1. (-1.); at 1. 1.; at (-1.) 1. ] } ]
  else
    let w = 0.12 in
    [ { color; points = [ at (-1.) (-1.); at 1. (-1.); at 1. (w -. 1.); at (-1.) (w -. 1.) ] };
      { color; points = [ at (-1.) (1. -. w); at 1. (1. -. w); at 1. 1.; at (-1.) 1. ] };
      { color; points = [ at (-1.) (-1.); at (w -. 1.) (-1.); at (w -. 1.) 1.; at (-1.) 1. ] };
      { color; points = [ at (1. -. w) (-1.); at 1. (-1.); at 1. 1.; at (1. -. w) 1. ] } ]

(* everything in the chamber, as polygons: what is drawn, and what is
 * taken through the portals to be seen through them *)
let chamber (g : game) : poly list =
  let holes = match open_pair g with Some (b, o) -> [ b.at; o.at ] | None -> [] in
  let opened = door_open g in
  let cube = List.hd g.world.bodies in
  let bx, bz = button_at in
  List.filter_map (fun p -> if List.memq p holes || (p.door && opened) then None else Some (panel_poly p)) panels
  @ box_polys (rgb 200 200 210) (0.3, 0.3, 0.3) cube.orientation (cube.x, cube.y, cube.z)
  @ box_polys (if opened then rgb 80 220 80 else rgb 220 40 40) (0.45, 0.03, 0.45) Quat.identity (bx, 0.03, bz)
  @ (match g.blue with Some b -> rim blue (holes <> []) b.shape | None -> [])
  @ match g.orange with Some o -> rim orange (holes <> []) o.shape | None -> []

(* you, as a box: not drawn where you are, only seen through a portal *)
let me_polys (g : game) : poly list =
  box_polys (rgb 240 240 240) (0.25, 0.9, 0.2) (Quat.of_axis_angle (0., 1., 0.) (-.g.yaw *. Float.pi /. 180.)) (g.me.x, g.me.y +. 0.9, g.me.z)

(* what is seen through [p]: the chamber taken through from its partner
 * [q] -- what is in front of [q] lands behind [p] -- and cut to what the
 * eye sees through [p] *)
let through (g : game) (all : poly list) (p : portal) (q : portal) : shape3d list =
  let e = eye g in
  if Vec3.dot (Vec3.sub e p.shape.centre) p.shape.normal <= 0. then []
  else
    List.filter_map
      (fun poly ->
        let moved = List.map (Portal3d.point ~from:q.shape ~into:p.shape) poly.points in
        match Portal3d.clip ~eye:e p.shape moved with
        | pts when List.length pts >= 3 -> Some (polygon3d poly.color pts)
        | _ -> None)
      all

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let camera_of (g : game) : camera =
  let ex, ey, ez = eye g and lx, ly, lz = look g in
  camera ~eye:(ex, ey, ez) ~target:(ex +. lx, ey +. ly, ez +. lz) ~fov:75. ~near:0.05 ~far:200. ()

let scene_shapes (g : game) : shape3d list =
  let here = chamber g in
  let all = here @ me_polys g in
  List.map (fun poly -> polygon3d poly.color poly.points) here
  @ match open_pair g with Some (b, o) -> through g all b o @ through g all o b | None -> []

let hud_shapes (screen : screen) (g : game) : shape list =
  [ rectangle (if g.blue = None then gray else blue) 12. 3. |> move (-10.) 0.;
    rectangle (if g.orange = None then gray else orange) 12. 3. |> move 10. 0.; rectangle white 2. 2.;
    text (rgb 60 60 70) 1.8 "z: blue portal   x: orange portal   f: the cube" |> move_y (screen.bottom +. 25.) ]

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let g = new_game () in
      let cam = camera ~eye:(6., 6., 7.5) ~target:(-1., 2.5, -3.) ~fov:65. ~far:200. () in
      ( cam,
        scene_shapes g
        @ List.map hud
            ([ text white 6. "TINY PORTAL" |> move_y 330.;
               text white 2.2 "the cube is on the ledge; the button opens the door" |> move_y 270. ]
            @ Scene2d.blink 1. s [ text orange 3.5 "PRESS SPACE" |> move_y 220. ]) )
  | Playing g -> (camera_of g, scene_shapes g @ List.map hud (hud_shapes screen g))
  | Complete g -> (camera_of g, scene_shapes g @ [ hud (text orange 6. "CHAMBER COMPLETE" |> move_y 120.) ])

let app = game3d view update initial_model

let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    ~capture_mouse:true app
