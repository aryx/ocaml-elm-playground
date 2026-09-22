(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Five blocks dropped into water, each of a different density, each
 * settling at its own waterline.
 *
 *   space      drop them again
 *   up/down    raise and lower the water
 *
 * Archimedes (c. 250 BC): the upward force is the weight of the fluid
 * displaced. Divide it by the block's own weight and everything but a
 * ratio cancels, so a block floats with exactly as much of itself
 * under the surface as its density relative to the water -- 60% for
 * wood, all of it and then some for stone.
 *
 * Each block wears a stripe painted at that fraction of its height.
 * Nothing lines the stripes up with the water: they are drawn once,
 * from the density, and the simulation puts them there. When the
 * bobbing stops, every stripe is at the surface, and that is the whole
 * demonstration (physics/3d/Force3d.mli does the same check in
 * numbers, against the analytic waterline).
 *
 * Raising the water with the arrows lifts them all, each keeping its
 * stripe on the new surface; the last block, denser than water, stays
 * on the bottom whatever you do.
 *
 * What is *not* modelled, and would be the next thing (the plan's
 * phase 11): the push is applied at the block's centre rather than at
 * the centre of the part under water, so nothing rights itself. Push a
 * real barrel under at an angle and it turns upright as it comes back;
 * these are held upright instead, which is honest rather than
 * accidental.
 *
 * Metres, seconds, kilograms, as everywhere in physics/3d/: the blocks
 * are a metre tall. The water's drag is 4 rather than the default 1.5,
 * so that the bobbing dies down while you are still watching: the drag
 * is proportional to how much of a block is in the water, so the
 * lightest one, with a fifth of itself under, is also the slowest to
 * settle.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The blocks *)
(*****************************************************************************)

let block_w = 0.8
let block_h = 1.
(* just above the water: dropped from higher they plunge right under
 * and shoot back out, which is true and takes a while to calm down *)
let drop_from = 1.3

(* the five, from cork to stone, with the colour each is meant to be *)
let blocks = [ (0.2, "cork", rgb 225 190 120); (0.4, "balsa", rgb 235 215 165); (0.6, "oak", rgb 165 110 60);
               (0.85, "ice", rgb 190 225 240); (1.4, "stone", rgb 140 140 150) ]

(* a block, with its waterline painted on: the stripe sits [density] of
 * the way up from its bottom, which is where the water should cut it *)
let block_shape (density : number) (color : color) : shape3d =
  let stripe_y = (-.block_h /. 2.) +. (Float.min 1. density *. block_h) in
  group3d
    [ box color block_w block_h block_w;
      box (rgb 40 40 45) (block_w +. 0.01) 0.04 (block_w +. 0.01) |> move_y3d stripe_y ]

type model = {
  bodies : Physics3d.body list;
  water : number;
  frames : int;
}

let drop (water : number) : model =
  { bodies =
      List.mapi
        (fun i (density, _, color) ->
          Physics3d.body (block_shape density color)
          |> Physics3d.at ((float_of_int i -. 2.) *. 1.3) (drop_from +. (float_of_int i *. 0.15)) 0.
          (* held upright: the buoyant push is applied at the centre
           * here, so there is no righting torque to show *)
          |> Physics3d.upright)
        blocks;
    water; frames = 0 }

let initial_model = drop 0.

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  if k.kspace then drop m.water
  else
    let water = m.water +. (if k.kup then 0.01 else 0.) -. if k.kdown then 0.01 else 0. in
    let water = Float.max (-1.5) (Float.min 2. water) in
    let bodies =
      List.map2
        (fun (density, _, _) b -> b |> Physics3d.floating ~damping:4. ~water ~density |> Physics3d.step)
        blocks m.bodies
    in
    (* the bottom of the pool, for the stone *)
    let bodies =
      List.map (fun (b : Physics3d.body) -> if b.Physics3d.y < -2. then { b with Physics3d.y = -2.; vy = 0. } else b) bodies
    in
    { bodies; water; frames = m.frames + 1 }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let water_color = rgb 60 130 200
let text color size str = words color str |> scale size

(* far enough back that all five fit, low enough to see the water *)
let cam = camera ~eye:(0., 2.1, 9.5) ~target:(0., -0.1, 0.) ~fov:40. ()

(* the surface, wide enough to be the horizon, and the pool floor under
 * it. Opaque: the software backend has no alpha, so what is under the
 * water is hidden by it -- which is exactly how water looks from just
 * above it, and is why the camera sits there. *)
let pool (water : number) : shape3d list =
  [ plane (rgb 35 60 80) 40. 40. |> move_y3d (-2.6); plane water_color 40. 40. |> move_y3d water ]

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  (* a label over each block: how dense it is, and how much of it the
   * water has taken -- Playground3d.project, which is what puts a 2D
   * word over a 3D thing *)
  let labels =
    List.concat
      (List.map2
         (fun (density, name, _) (b : Physics3d.body) ->
           let under = Force3d.submerged ~water:m.water ~half_height:(block_h /. 2.) b.Physics3d.y in
           match project cam screen (b.Physics3d.x, b.Physics3d.y +. 0.9, b.Physics3d.z) with
           | None -> []
           | Some (x, y) ->
               [ text black 1.6 (Printf.sprintf "%s %.2f" name density) |> move x (y +. 22.);
                 text (if Float.abs (under -. Float.min 1. density) < 0.01 then rgb 20 120 60 else rgb 170 90 30) 1.6
                   (Printf.sprintf "%.0f%% under" (100. *. under))
                 |> move x y ])
         blocks m.bodies)
  in
  ( cam,
    pool m.water
    @ List.map Physics3d.draw m.bodies
    @ List.map hud
        (labels
        @ [ text black 2.2 "each block floats with its own density under water: the stripe is painted there"
            |> move_y (screen.top -. 45.);
            text darkGray 2. (Printf.sprintf "water at %+.2f m    (up/down)     space: drop them again" m.water)
            |> move_y (screen.bottom +. 30.) ]) )

let app = game3d view update initial_model
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
