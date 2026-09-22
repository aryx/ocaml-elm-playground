(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyCrush in 3D, after Crush3D (Zoë Mode, Sega, Nintendo 3DS, 2012),
 * the remake that took the game from the PSP to a stereoscopic screen:
 * the same levels, the same rules, drawn with real cubes and a camera.
 *
 *   left right   run         up down   a step deeper, or nearer
 *   space        jump        c         crush, and uncrush
 *   tab          turn the camera a quarter (front, side)
 *
 * TinyCrush and this game are one game drawn twice, as TinyDoom and
 * TinyDoom3d are: the model is gamekits/crush's Crush, whose interface
 * explains the rules, and only the picture differs. TinyCrush fakes the
 * depth with a cabinet projection; here the blocks are cubes at their
 * depth, and the crush is two movements at once:
 *
 *   - the world: every slice's depth slides, as the crush goes, to
 *     Danny's own ([depth_z]) -- the level folding onto the plane he is
 *     in, which is what the rule says;
 *   - the camera ([camera_for]): from behind and above, three-quarters
 *     on, where the depths can be seen, it swings round to straight on,
 *     where they cannot -- the view in which the crush is true. Uncrushed,
 *     depth is what the picture shows; crushed, what it hides.
 *
 * Crushed, the slices would all be at the same depth, their cubes one
 * inside another, and a z-buffer cannot decide between two faces at the
 * same depth: they flicker, each pixel won by whichever came last
 * ("z-fighting"). So the crushed level is drawn as what it is, the
 * crushed plane, one cube per solid cell of it -- the model's own
 * [Crush.plane], drawn.
 *
 * What it uses: gamekits/crush's Crush (the levels, the rules, the
 * planes), Playground3d (cubes, a camera, the HUD), Scene2d.
 *
 * Left undone, exercises: the camera turning round the level when tab
 * is pressed, rather than cutting (the turn drawn, as the crush is);
 * the stereoscopic picture, two cameras an eye's width apart
 * (Playground3d's split views could put them side by side);
 * TinyCrush's exercises, which are the kit's.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The model: the kit's, and the game's scenes *)
(*****************************************************************************)

type scene = Title | Playing of Crush.play | Done
type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Done -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (Crush.enter 0)) scenes else scenes
  | Playing p -> (
      match Crush.frame (Crush.read_input pressed computer.keyboard) p with
      | Going p -> { scenes with scene = Playing p }
      | Next_level p -> Scene2d.go (Playing p) scenes
      | Finished -> Scene2d.go Done scenes)

(*****************************************************************************)
(* View: cubes at their depth *)
(*****************************************************************************)

let tile = Crush.tile

(* where slice [d] is, in depth: its distance from Danny's, shrinking to
 * nothing as the crush goes (the squash from 1 to 0); nearer slices
 * towards the camera, at +z *)
let depth_z (p : Crush.play) (d : int) : float = -.float_of_int (d - p.depth) *. tile *. p.squash

(* The camera: three-quarters on and from above, uncrushed; straight
 * on, crushed; in between, in between. *)
let camera_for (p : Crush.play) : camera =
  let s = p.squash in
  let mix a b = (a *. s) +. (b *. (1. -. s)) in
  camera
    ~eye:(p.u +. mix (3. *. tile) 0., p.y +. mix (4. *. tile) (1. *. tile), mix (10. *. tile) (14. *. tile))
    ~target:(p.u +. mix (1. *. tile) 0., p.y -. mix (0.5 *. tile) 0., mix (-2. *. tile) 0.)
    ~fov:50. ~far:4000. ()

let orange = rgb 230 140 60
let grey = rgb 130 130 150

(* a cell of the plane (column u, row y) at depth z, in the plane's
 * coordinates, as Tilemap's: centered on the level *)
let at (l : Crush.level) (view : int) (u : int) (y : int) : float * float =
  let w = float_of_int (Crush.across l view) *. tile and h = float_of_int (Crush.ny l) *. tile in
  ((float_of_int u *. tile) -. (w /. 2.) +. (tile /. 2.), (h /. 2.) -. (float_of_int y *. tile) -. (tile /. 2.))

let exit_shape (x : float) (y : float) (z : float) : shape3d =
  group3d [ box (rgb 250 220 80) 30. 44. 6. |> move3d x y z; box (rgb 40 30 10) 18. 32. 8. |> move3d x y z ]

let danny (p : Crush.play) : shape3d =
  group3d
    [ box (rgb 70 90 200) 22. 26. 16. |> move3d p.u (p.y -. 7.) 0.; sphere (rgb 240 210 180) 9. |> move3d p.u (p.y +. 12.) 0.;
      sphere (rgb 40 30 30) 5. |> move3d (p.u -. 3.) (p.y +. 16.) 5. ]

(* the night behind the level: a wall far back, following Danny, for
 * the picture to have a background and the HUD's words to show *)
let night : shape3d = box (rgb 25 22 45) 4000. 4000. 10. |> move_z3d (-1000.)
let backdrop (p : Crush.play) : shape3d = night |> move3d p.u p.y 0.

let level_shapes (p : Crush.play) : shape3d list =
  let l = Crush.levels.(p.level) in
  let cells f = List.concat (List.init (Crush.ny l) (fun y -> List.filter_map (fun u -> f u y) (List.init (Crush.across l p.view) Fun.id))) in
  if p.squash = 0. then
    (* crushed: the crushed plane, one cube per solid cell *)
    let map = Crush.current p in
    cells (fun u y ->
        let x, yy = at l p.view u y in
        match Tilemap.get map u y with
        | Some '#' -> Some (cube grey (tile *. 0.96) |> move3d x yy 0.)
        | Some 'E' -> Some (exit_shape x yy 0.)
        | _ -> None)
  else
    (* every slice, at its depth *)
    List.concat
      (List.init (Crush.deep_n l p.view) (fun d ->
           let z = depth_z p d in
           cells (fun u y ->
               let gx, gz = Crush.at p.view u d in
               let x, yy = at l p.view u y in
               match Crush.cell l gx y gz with
               | '#' -> Some (cube (if d = p.depth && not p.crushed then orange else grey) (tile *. 0.96) |> move3d x yy z)
               | 'E' -> Some (exit_shape x yy z)
               | _ -> None)))

let text (color : color) (size : float) (s : string) : shape = words color s |> scale size

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  let still = camera ~eye:(0., 0., 400.) ~target:(0., 0., 0.) ~far:4000. () in
  match model.scene with
  | Title ->
      ( still,
        [ night; hud (text orange 6. "TINY CRUSH 3D" |> move_y 200.);
          hud (text white 2. "c crushes the level along your line of sight" |> move_y 60.);
          hud (text white 2. "the camera turns to look straight on, and the depths fold into your plane" |> move_y 25.) ]
        @ List.map hud (Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]) )
  | Done -> (still, [ night; hud (text orange 4. "DANNY SLEEPS" |> move_y 60.) ] @ List.map hud (Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]))
  | Playing p ->
      let l = Crush.levels.(p.level) in
      ( camera_for p,
        (backdrop p :: level_shapes p)
        @ [ danny p;
            hud
              (text white 2.2
                 (Printf.sprintf "%s   (%s, %s)" l.name (if p.view = 0 then "front" else "side")
                    (if p.crushed then "crushed" else "depth " ^ string_of_int p.depth))
              |> move_y (screen.top -. 40.));
            hud (text (rgb 200 200 230) 1.6 p.message |> move_y (screen.bottom +. 60.));
            hud (text (rgb 150 150 180) 1.5 "arrows run and step in depth   space jump   c crush   tab turn" |> move_y (screen.bottom +. 30.)) ] )

let help = {|TinyCrush3d
  left right run, up down step in depth, space jump, c crush and uncrush, tab turn the camera
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d app
