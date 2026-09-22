(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Push one hitbox through four others and watch what the engine sees.
 *
 *   arrows     move it about the floor
 *   w / s      lift it and put it down
 *   1 / 2 / 3  what it is to the engine: a box, a ball, a capsule
 *   r          the ray it casts ahead of it, on and off
 *
 * Nothing here moves by itself: this is phase 4 of
 * plan_physics3d_teaching.md, collision *detection*, and answering a
 * collision is phase 5. What the example is for is the difference
 * between what a thing looks like and what it is:
 *
 *   - every shape wears its hitbox as a green wireframe
 *     (Physics3d.debug). The ball's is a sphere, the capsule's a
 *     segment with a radius, the crates' are boxes that turn with
 *     them, and the one you drive can be any of the three ("1/2/3")
 *     without its drawing changing at all;
 *   - a pair that overlaps turns red, and the arrow between them is
 *     the contact: its direction is the way out, its length the depth
 *     (Contact3d.mli);
 *   - the turned crate is the one worth pushing into. Two boxes at an
 *     angle need fifteen axes to be told apart, not six, and the
 *     corner is where the missing nine show up
 *     (Collide3d.mli's diagram).
 *
 * The ray ("r") is the other half of phase 4: pick, aim, ground check.
 * It is drawn to the first thing it meets, with a marker where it
 * lands.
 *
 * Metres, as everywhere in physics/3d/.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The scene *)
(*****************************************************************************)

let grey = rgb 150 150 160
let hit_color = rgb 225 80 70

type shape_kind = As_box | As_ball | As_capsule

let kind_name = function As_box -> "a box" | As_ball -> "a ball" | As_capsule -> "a capsule"

(* the four to push into: each drawn as itself, each with the hitbox
 * that fits it *)
let scenery : (string * Physics3d.body) list =
  [ ("the ball", Physics3d.body (sphere (rgb 90 170 220) 0.6) |> Physics3d.at (-2.4) 0.6 0. |> Physics3d.ball);
    ("the crate", Physics3d.body (box (rgb 180 140 90) 1.2 1.2 1.2) |> Physics3d.at (-0.8) 0.6 0.);
    ( "the turned crate",
      Physics3d.body (box (rgb 200 120 80) 1.6 0.5 0.5)
      |> Physics3d.at 0.9 0.7 0.
      |> Physics3d.pointing (0.3, 1., 0.5) 50. );
    ("the post", Physics3d.body (box grey 0.5 1.8 0.5) |> Physics3d.at 2.6 0.9 0. |> Physics3d.pill) ]

type model = { probe : Physics3d.body; kind : shape_kind; ray_on : bool }

let probe_shape = box (rgb 120 200 140) 0.7 0.7 0.7

let as_kind (kind : shape_kind) (b : Physics3d.body) : Physics3d.body =
  match kind with
  | As_box -> Physics3d.hitbox (Hitbox3d.Box (0.35, 0.35, 0.35)) b
  | As_ball -> Physics3d.ball b
  | As_capsule -> Physics3d.hitbox (Hitbox3d.Capsule (0.3, 0.35)) b

let start () : model =
  { probe = Physics3d.body probe_shape |> Physics3d.at 0. 0.45 2.6 |> as_kind As_box; kind = As_box; ray_on = true }

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let down key = Set_.mem key k.keys in
  if down "1" then { m with kind = As_box; probe = as_kind As_box m.probe }
  else if down "2" then { m with kind = As_ball; probe = as_kind As_ball m.probe }
  else if down "3" then { m with kind = As_capsule; probe = as_kind As_capsule m.probe }
  else if down "r" then { m with ray_on = not m.ray_on }
  else
    let dx, dz = to_xy k in
    let dy = (if down "w" then 1. else 0.) -. if down "s" then 1. else 0. in
    let speed = 0.04 in
    let p = m.probe in
    { m with
      probe =
        Physics3d.at
          (Float.max (-3.4) (Float.min 3.4 (p.Physics3d.x +. (dx *. speed))))
          (Float.max 0.2 (Float.min 2.5 (p.Physics3d.y +. (dy *. speed))))
          (Float.max (-2.5) (Float.min 3. (p.Physics3d.z -. (dz *. speed))))
          p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size
(* back far enough that the probe can leave the row and still be seen *)
let cam = Camera3d.from_far ~fov:42. ~offset:(1.2, 5., 9.5) (0., 0.4, 0.3)

(* the contact, drawn where it is: a rod along the normal as long as
 * the overlap is deep, with a cube at the point they touch *)
let contact_shapes (c : Contact3d.t) : shape3d list =
  let nx, ny, nz = c.Contact3d.normal and px, py, pz = c.Contact3d.point in
  let len = Float.max 0.25 (c.Contact3d.depth *. 3.) in
  [ cube (rgb 250 230 90) 0.08 |> move3d px py pz;
    box (rgb 250 120 60) 0.05 len 0.05
    |> (fun s ->
         let q = Quat.of_axis_angle (Vec3.cross (0., 1., 0.) (nx, ny, nz)) (acos (Float.max (-1.) (Float.min 1. ny))) in
         let ax, ay, az = Quat.to_euler_xyz q in
         s |> move_y3d (len /. 2.) |> rotate3d ax ay az)
    |> move3d px py pz ]

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let contacts = List.map (fun (name, b) -> (name, b, Physics3d.contact m.probe b)) scenery in
  let touching = List.filter_map (fun (name, _, c) -> match c with Some c -> Some (name, c) | None -> None) contacts in
  let shapes =
    List.concat_map
      (fun (_, b, c) ->
        [ Physics3d.draw b; Physics3d.debug b ]
        @ (match c with Some c -> contact_shapes c | None -> []))
      contacts
  in
  (* what the probe is: its drawing never changes, its hitbox does *)
  let probe = [ Physics3d.draw m.probe; Physics3d.debug m.probe ] in
  let ray_shapes =
    if not m.ray_on then []
    else
      let from = (m.probe.Physics3d.x, m.probe.Physics3d.y, m.probe.Physics3d.z) in
      let direction = (0., 0., -1.) in
      let hit = Physics3d.ray ~from ~direction (List.map snd scenery) in
      let far = match hit with Some (_, d) -> d | None -> 12. in
      let fx, fy, fz = from in
      (box (rgb 120 120 230) 0.03 0.03 far |> move3d fx fy (fz -. (far /. 2.)))
      :: (match hit with Some (_, d) -> [ cube (rgb 60 60 220) 0.12 |> move3d fx fy (fz -. d) ] | None -> [])
  in
  let floor = plane (rgb 120 160 120) 16. 16. in
  let said =
    match touching with
    | [] -> [ text (rgb 60 110 70) 2.2 "touching nothing" ]
    | l ->
        List.map
          (fun (name, c) -> text hit_color 2.2 (Printf.sprintf "in %s by %.2f m" name c.Contact3d.depth))
          l
  in
  ( cam,
    (floor :: shapes) @ probe @ ray_shapes
    @ List.map hud
        ([ text black 2.2 "green: what the engine sees. red arrow: the way out, as long as the overlap is deep"
           |> move_y (screen.top -. 45.) ]
        @ List.mapi (fun i s -> s |> move_y (screen.top -. 95. -. (float_of_int i *. 32.))) said
        @ [ text darkGray 2. (Printf.sprintf "the one you push is %s   (1: box  2: ball  3: capsule)" (kind_name m.kind))
            |> move_y (screen.bottom +. 45.);
            text darkGray 2. "arrows: move    w/s: up and down    r: the ray" |> move_y (screen.bottom +. 15.) ]) )

let app = game3d view update (start ())
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
