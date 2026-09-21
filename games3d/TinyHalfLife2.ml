(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Half-Life 2 (Valve, 2004): one yard of Ravenholm,
 * two zombies walking at you, and the gravity gun. W/A/S/D to walk,
 * the arrows (or the mouse) to look, z (or a right click) to grab what
 * is under the crosshair or drop it, x (or a click) to launch what you
 * hold, or to punt what you look at. Knock both zombies down before
 * they reach you.
 *
 * Half-Life 2 was built on Havok, and its lesson was that physics is a
 * thing to *use*: every prop is a weapon, a lid a shield, a seesaw a
 * catapult. Each of its party tricks is here, and each is one thing
 * the engine (playground3d/Physics3d, plan_physics3d_teaching.md) does:
 *
 *   - the gravity gun is three calls (notes_3d_physics.md section 13):
 *     Physics3d.ray from the eye picks a body; Physics3d.held_by pulls
 *     it, every step, to a point in front of you; and a launch is one
 *     speed set along where you look;
 *   - the seesaw is one joint, a hinge through the plank's middle, with
 *     limits (physics/3d/Joint3d.mli, phase 11): drop a crate on one
 *     end, and whatever is on the other goes up;
 *   - the crates stand in a pile because the solver answers all their
 *     contacts together, and sleep once they are still (phase 8);
 *   - the barrels float in the tank, Archimedes (Physics3d.floating,
 *     phase 2): a barrel is half as dense as water, and rides half out;
 *   - and a zombie walks as one upright body -- until something hits it
 *     harder than it walks. Then it is a ragdoll (playground3d/
 *     Ragdoll3d: ten boxes, nine joints), going on at the speed it was
 *     knocked, and it falls like a body because it is built like one.
 *     That moment, a character going limp, is what made Half-Life 2
 *     feel like nothing before it.
 *
 * You are playground3d/Character3d, the capsule controller (phase 9):
 * the props are solids to you, and you cannot push them with your body
 * -- that is what the gun is for.
 *
 * What it uses: Physics3d (a world, joints, ray, held_by, floating),
 * Ragdoll3d, Character3d, Scene2d. Not TinyTeardown's voxels: a prop
 * here is a body from the start.
 *
 * Exercises: a lid of a crate as a shield; saw blades, which the real
 * game threw through zombies (a thin fast box, and the sweep of phase
 * 10); the zombies' own ragdolls as ammunition; a zombie getting up
 * again, blending its ragdoll back into a walk.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The yard *)
(*****************************************************************************)

let wall_color = rgb 110 100 95
let crate_color = rgb 170 120 60

let block (color : color) ((x, y, z) : number * number * number) ((w, h, d) : number * number * number) : Physics3d.body =
  Physics3d.body (box color w h d) |> Physics3d.at x y z |> Physics3d.immovable |> Physics3d.rough 0.7

(* the tank: its walls, and the water in it up to [water] *)
let tank_x = 4.
let tank_z = 4.
let tank_half = 1.5
let water = 0.7
let in_tank (b : Physics3d.body) : bool = Float.abs (b.x -. tank_x) < tank_half && Float.abs (b.z -. tank_z) < tank_half

let statics : Physics3d.body list =
  [ block (rgb 90 95 80) (0., -0.25, 0.) (20., 0.5, 20.);
    block wall_color (0., 1.5, -10.) (20., 3., 0.4);
    block wall_color (0., 1.5, 10.) (20., 3., 0.4);
    block wall_color (-10., 1.5, 0.) (0.4, 3., 20.);
    block wall_color (10., 1.5, 0.) (0.4, 3., 20.);
    (* the seesaw's stand *)
    block wall_color (-4., 0.3, 3.) (0.3, 0.6, 0.6);
    (* the tank's four walls *)
    block wall_color (tank_x, 0.45, tank_z -. tank_half) ((2. *. tank_half) +. 0.2, 0.9, 0.2);
    block wall_color (tank_x, 0.45, tank_z +. tank_half) ((2. *. tank_half) +. 0.2, 0.9, 0.2);
    block wall_color (tank_x -. tank_half, 0.45, tank_z) (0.2, 0.9, 2. *. tank_half);
    block wall_color (tank_x +. tank_half, 0.45, tank_z) (0.2, 0.9, 2. *. tank_half) ]

let crate ((x, y, z) : number * number * number) : Physics3d.body =
  Physics3d.body (cube crate_color 0.6) |> Physics3d.at x y z |> Physics3d.heavy 20. |> Physics3d.rough 0.6

(* a pile of six against the back wall: three, two, one *)
let pile : Physics3d.body list =
  List.map crate
    [ (-1.2, 0.3, -8.); (-0.6, 0.3, -8.); (0., 0.3, -8.); (-0.9, 0.9, -8.); (-0.3, 0.9, -8.); (-0.6, 1.5, -8.) ]

let plank = Physics3d.body (box (rgb 150 110 70) 3. 0.1 0.5) |> Physics3d.at (-4.) 0.65 3. |> Physics3d.heavy 15. |> Physics3d.rough 0.7

(* a crate waiting on the seesaw's low end *)
let on_plank = crate (-5.2, 1.0, 3.)

let barrels : Physics3d.body list =
  List.map
    (fun (x, z) ->
      Physics3d.body (box (rgb 150 60 50) 0.5 0.8 0.5) |> Physics3d.at x 1. z |> Physics3d.heavy 80. |> Physics3d.rough 0.4)
    [ (tank_x -. 0.6, tank_z -. 0.5); (tank_x +. 0.6, tank_z); (tank_x, tank_z +. 0.7) ]

(* a zombie walking: one upright body, until something knocks it
 * faster than it walks *)
let zombie ((x, z) : number * number) : Physics3d.body =
  Physics3d.body (box (rgb 100 120 90) 0.45 1.8 0.3) |> Physics3d.at x 0.9 z |> Physics3d.heavy 70. |> Physics3d.upright
  |> Physics3d.rough 0.5

let zombie_starts = [ (-6., -7.); (7., -6.) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* The world's bodies, in order: the statics, the pile, the plank and
 * the crate on it, the barrels, the zombies, and then any ragdolls, ten
 * bodies each, appended as zombies fall. A fallen zombie's own body is
 * left in its place, out of the way, so that no index moves. *)
let first_zombie = List.length statics + List.length pile + 2 + List.length barrels
let plank_index = List.length statics + List.length pile

type zombie_state = Walking | Fallen

type game = {
  world : Physics3d.world;
  zombies : zombie_state list;
  me : Character3d.t;
  yaw : number;
  pitch : number;
  held : int option;
  health : int;
  hurt : int; (* frames the screen stays red *)
  keys_down : string list;
}

type scene = Title | Playing of game | Cleared of game | Dead of game
type model = scene Scene2d.t

let new_game () : game =
  let bodies = statics @ pile @ [ plank; on_plank ] @ barrels @ List.map zombie zombie_starts in
  let world =
    Physics3d.world bodies
    |> Physics3d.hinge ~limits:(-18., 18.) (List.length statics - 5) plank_index ~at:(-4., 0.65, 3.) ~axis:(0., 0., 1.)
  in
  { world; zombies = List.map (fun _ -> Walking) zombie_starts; me = Character3d.make 0. 0. 6.; yaw = 0.; pitch = 0.;
    held = None; health = 100; hurt = 0; keys_down = [] }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let eye (g : game) : number * number * number = (g.me.x, g.me.y +. 1.6, g.me.z)

let look (g : game) : number * number * number =
  let y = g.yaw *. Float.pi /. 180. and p = g.pitch *. Float.pi /. 180. in
  (cos p *. sin y, sin p, -.(cos p *. cos y))

let nth (g : game) (i : int) : Physics3d.body = List.nth g.world.bodies i

let set (i : int) (f : Physics3d.body -> Physics3d.body) (w : Physics3d.world) : Physics3d.world =
  (* and awake: a sleeping body does not move for what is done to it *)
  { w with
    bodies = List.mapi (fun k b -> if k = i then f b else b) w.bodies;
    asleep = List.mapi (fun k a -> if k = i then false else a) w.asleep;
    still = List.mapi (fun k s -> if k = i then 0 else s) w.still }

let movable (b : Physics3d.body) : bool = Float.is_finite b.mass

(* what the crosshair is on, within [reach]: a body that can move, and
 * not a walking zombie (the gun cannot hold what walks) *)
let aimed (g : game) (reach : number) : int option =
  let bodies = List.mapi (fun i b -> (i, b)) g.world.bodies in
  let candidates =
    List.filter
      (fun (i, (b : Physics3d.body)) ->
        movable b && Some i <> g.held
        && not (i >= first_zombie && i < first_zombie + List.length g.zombies && List.nth g.zombies (i - first_zombie) = Walking))
      bodies
  in
  match Physics3d.ray ~from:(eye g) ~direction:(look g) (List.map snd candidates) with
  | Some (b, d) when d < reach -> Option.map fst (List.find_opt (fun (_, b') -> b' == b) candidates)
  | _ -> None

(* the gravity gun: z grabs or drops, x launches what is held or punts
 * what is aimed at *)
let gun (computer : computer) (g : game) : game =
  let k = computer.keyboard and m = computer.mouse in
  let down key = Set_.mem key k.keys in
  let pressed key = down key && not (List.mem key g.keys_down) in
  let grab = pressed "z" || pressed "rclick" and fire = pressed "x" || pressed "click" in
  let keys_down = List.filter down [ "z"; "x" ] @ (if m.mrdown then [ "rclick" ] else []) @ if m.mdown then [ "click" ] else [] in
  let lx, ly, lz = look g in
  let throw speed (b : Physics3d.body) = Physics3d.moving (lx *. speed) (ly *. speed) (lz *. speed) b in
  let g = { g with keys_down } in
  match g.held with
  | Some _ when grab -> { g with held = None }
  | Some i when fire -> { g with held = None; world = set i (throw 12.) g.world }
  | None when grab -> { g with held = aimed g 6. }
  | None when fire -> (
      match aimed g 4. with Some i -> { g with world = set i (fun b -> throw (Float.max 6. (Physics3d.speed b)) b) g.world } | None -> g)
  | _ -> g

(* what is held, pulled to a point 1.8 m in front of the eye *)
let hold (g : game) : game =
  match g.held with
  | None -> g
  | Some i ->
      let ex, ey, ez = eye g and lx, ly, lz = look g in
      { g with world = set i (Physics3d.held_by (ex +. (1.8 *. lx), ey +. (1.8 *. ly), ez +. (1.8 *. lz))) g.world }

(* the zombies walk at you; the one near enough hurts *)
let walk_zombies (g : game) : game =
  let g = ref g in
  List.iteri
    (fun n state ->
      if state = Walking then begin
        let i = first_zombie + n in
        let z = nth !g i in
        let dx = !g.me.x -. z.x and dz = !g.me.z -. z.z in
        let d = Float.hypot dx dz in
        g := { !g with world = set i (fun b -> Physics3d.moving (0.9 *. dx /. d) b.vy (0.9 *. dz /. d) b) !g.world };
        if d < 1.1 && !g.hurt = 0 then g := { !g with health = !g.health - 20; hurt = 40 }
      end)
    !g.zombies;
  !g

(* a zombie going faster than it walks has been hit: its body put away,
 * and a ragdoll where it stood, going on at the speed it was knocked *)
let knock_down (g : game) : game =
  let g = ref g in
  List.iteri
    (fun n state ->
      let i = first_zombie + n in
      let z = nth !g i in
      if state = Walking && Physics3d.speed z > 2.5 then begin
        let first = List.length !g.world.bodies in
        let doll =
          Ragdoll3d.bodies ~color:(rgb 100 120 90) (z.x, 0., z.z) |> List.map (Physics3d.moving z.vx (z.vy +. 1.) z.vz)
        in
        let w = set i (fun b -> b |> Physics3d.at z.x (-50.) z.z |> Physics3d.moving 0. 0. 0. |> Physics3d.immovable) !g.world in
        let w =
          { w with
            bodies = w.bodies @ doll;
            still = w.still @ List.map (fun _ -> 0) doll;
            asleep = w.asleep @ List.map (fun _ -> false) doll }
        in
        g := { !g with world = Ragdoll3d.join first w; zombies = List.mapi (fun k s -> if k = n then Fallen else s) !g.zombies }
      end)
    !g.zombies;
  !g

(* one tick of the world: gravity for everything but what floats in the
 * tank, which Archimedes holds up (Physics3d.floating, gravity
 * included), then every contact and joint solved *)
let physics (g : game) : game =
  let bodies =
    List.map
      (fun (b : Physics3d.body) ->
        if not (movable b) then b
        else if in_tank b && b.y < water +. 1. then Physics3d.floating ~water ~density:0.5 b
        else Physics3d.fall 9.8 b)
      g.world.bodies
  in
  { g with world = Physics3d.simulate { g.world with bodies } }

let walk_me (computer : computer) (g : game) : game =
  let k = computer.keyboard and m = computer.mouse in
  let axis a b = (if a then 1. else 0.) -. if b then 1. else 0. in
  let yaw = g.yaw +. (0.15 *. m.mdx) +. (2.5 *. axis k.kright k.kleft) in
  let pitch = Float.max (-80.) (Float.min 80. (g.pitch +. (0.15 *. m.mdy) +. (1.5 *. axis k.kup k.kdown))) in
  let ahead = axis k.kw k.ks and aside = axis k.kd k.ka in
  let y = yaw *. Float.pi /. 180. in
  let vx = 4. *. ((ahead *. sin y) +. (aside *. cos y)) and vz = 4. *. ((aside *. sin y) -. (ahead *. cos y)) in
  (* the solids: everything but what the gun holds, and the fallen
   * zombies' put-away bodies *)
  let solids = List.filteri (fun i (b : Physics3d.body) -> Some i <> g.held && b.y > -10.) g.world.bodies in
  { g with yaw; pitch; me = Character3d.walk ~jump:(if k.kspace then 4. else 0.) solids (vx, vz) g.me }

let update_game (computer : computer) (g : game) : game =
  let g = walk_me computer g |> gun computer |> hold |> walk_zombies |> physics |> knock_down in
  { g with hurt = max 0 (g.hurt - 1) }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer g in
      if List.for_all (fun z -> z = Fallen) g.zombies then Scene2d.go (Cleared g) s
      else if g.health <= 0 then Scene2d.go (Dead g) s
      else { s with scene = Playing g }
  | Cleared _ | Dead _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let water_shape : shape3d =
  polygon3d (rgb 60 110 150)
    [ (tank_x -. tank_half, water, tank_z +. tank_half); (tank_x +. tank_half, water, tank_z +. tank_half);
      (tank_x +. tank_half, water, tank_z -. tank_half); (tank_x -. tank_half, water, tank_z -. tank_half) ]

let camera_of (g : game) : camera =
  let ex, ey, ez = eye g and lx, ly, lz = look g in
  camera ~eye:(ex, ey, ez) ~target:(ex +. lx, ey +. ly, ez +. lz) ~fov:70. ~near:0.05 ~far:3000. ()

(* the dusk: a box round the whole yard, seen from inside (the back
 * faces are drawn), the same from any eye, where Camera3d.sky, a
 * ceiling and a skirt, left a white band from the title's high eye *)
let dusk : shape3d = box (rgb 65 70 88) 300. 300. 300.

let scene_shapes (_ : camera) (g : game) : shape3d list =
  dusk
  :: (water_shape :: List.filter_map (fun (b : Physics3d.body) -> if b.y > -10. then Some (Physics3d.draw b) else None) g.world.bodies)

let hud_shapes (screen : screen) (g : game) : shape list =
  let fallen = List.length (List.filter (fun z -> z = Fallen) g.zombies) in
  [ rectangle (rgb 255 160 40) 22. 2.; rectangle (rgb 255 160 40) 2. 22.;
    text (rgb 255 200 120) 2.2 (Printf.sprintf "HEALTH %d" g.health) |> move (screen.left +. 130.) (screen.bottom +. 35.);
    text (rgb 255 200 120) 2.2 (Printf.sprintf "ZOMBIES DOWN %d / %d" fallen (List.length g.zombies))
    |> move (screen.right -. 200.) (screen.bottom +. 35.);
    text (rgb 220 220 220) 1.8 (if g.held = None then "z: grab   x: punt" else "z: drop   x: launch") |> move_y (screen.bottom +. 35.) ]
  @ if g.hurt > 20 then [ rectangle red screen.width screen.height |> fade 0.25 ] else []

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  let over g title color =
    let cam = camera_of g in
    (cam, scene_shapes cam g @ List.map hud [ text color 6. title |> move_y 120. ])
  in
  match s.scene with
  | Title ->
      let g = new_game () in
      let cam = camera ~eye:(7., 5., 9.) ~target:(-1., 0.5, -2.) ~fov:60. ~far:3000. () in
      ( cam,
        scene_shapes cam g
        @ List.map hud
            ([ text (rgb 255 160 40) 6. "TINY HALF-LIFE 2" |> move_y 330.;
               text white 2.2 "w/a/s/d: walk   arrows: look   z: grab/drop   x: launch/punt" |> move_y 270. ]
            @ Scene2d.blink 1. s [ text (rgb 255 200 120) 3.5 "PRESS SPACE" |> move_y 220. ]) )
  | Playing g ->
      let cam = camera_of g in
      (cam, scene_shapes cam g @ List.map hud (hud_shapes screen g))
  | Cleared g -> over g "RAVENHOLM, CLEARED" (rgb 255 200 120)
  | Dead g -> over g "YOU DIED" red

let app = game3d view update initial_model

(* flat shading; the back faces drawn, for the sky (Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    ~capture_mouse:true app
