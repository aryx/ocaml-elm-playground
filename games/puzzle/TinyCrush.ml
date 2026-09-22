(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Crush (Zoë Mode, Sega, PSP, 2007): a level in three
 * dimensions that you crush flat, to walk across what was far apart.
 *
 *   left right   run         up down   a step deeper, or nearer
 *   space        jump        c         crush, and uncrush
 *   tab          turn the camera a quarter (front, side)
 *
 * Danny, an insomniac, walks through his own mind, and the one trick
 * the game has is its whole design: seen from the side, press a button
 * and the level is crushed along the line of sight -- everything at
 * every depth pulled into one plane, a 2D platformer; uncrush, and the
 * depths come back, Danny standing wherever the platform under him
 * really was. Two platforms far apart in depth are one platform when
 * crushed; a wall far behind you is in your way when crushed. It sits
 * in the family of games whose projection is the puzzle, before Fez
 * and after Echochrome; plan_games_original.md lists them. (Names and
 * dates from memory, to check.)
 *
 * The rules are the crush kit's (gamekits/crush's Crush, whose
 * interface explains them, and TinyCrush3d's too): the level is a stack
 * of 2D slices, played uncrushed on the slice Danny stands in and
 * crushed on their union along the camera's axis -- the same
 * platformer, Tile_move, on either map. A crush is refused where Danny
 * would end up inside a block; an uncrush puts him at the depth of the
 * block he stands on.
 *
 * The picture ([view]): uncrushed, the blocks in a cabinet projection
 * -- each depth drawn a little up and to the right of the one in front,
 * painted far first -- which is the trick of this game, and crushing is
 * that offset shrinking to nothing: the depths slide into one plane
 * before your eyes, the rule drawn. The slice Danny is in is coloured,
 * the others grey.
 *
 * What it uses: gamekits/crush's Crush (the levels and the rules, and
 * through it gamekits/platformer's Tile_move and Tilemap), Scene2d. Not Playground3d: a cabinet projection is a line of
 * arithmetic, and the crush is best seen as a projection losing its
 * depth.
 *
 * Left undone, exercises: the top view, Crush's third crush, which
 * turns the level into a maze seen from above; the marbles to collect
 * before the exit opens, and the enemies (Crush's cockroaches);
 * crushing from behind, where what was hidden behind a wall is in front
 * of it; blocks that Danny pushes, the level's content moving between
 * slices; more levels, which are strings.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model: the kit's, and the game's scenes *)
(*****************************************************************************)

type scene = Title | Playing of Crush.play | Done
type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

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
(* View: the cabinet projection *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the trick of this game, in 21 lines (see the header): depth d drawn up and to
 * the right of the front, by [lean] of a tile per slice, times the
 * [squash] -- 1 uncrushed, 0 crushed, the depths in one plane *)
let lean = 0.45

let shade ((r, g, b) : int * int * int) (k : int) : color =
  let c v = max 0 (min 255 (v +.. k)) in
  rgb (c r) (c g) (c b)

let block_shape (color : int * int * int) (squash : number) : shape =
  let tile = Crush.tile in
  let o = lean * tile * squash in
  let h = tile / 2. in
  group
    ((if o > 0.5 then
        [ (* the top, and the right side: the depth, leaning *)
          polygon (shade color 40) [ (-.h, h); (h, h); (h + o, h + o); (-.h + o, h + o) ];
          polygon (shade color (-40)) [ (h, -.h); (h + o, -.h + o); (h + o, h + o); (h, h) ] ]
      else [])
    @ [ square (shade color 0) tile; square (shade color (-30)) (tile - 8.) |> fade 0.3 ])

let view_play (computer : computer) (p : Crush.play) : shape list =
  let screen = computer.screen in
  let l = Crush.levels.(p.level) in
  let tile = Crush.tile in
  let n_across = Crush.across l p.view and n_deep = Crush.deep_n l p.view in
  let w = float_of_int n_across * tile and h = float_of_int (Crush.ny l) * tile in
  (* where the grid's cell (u, y) at depth d is drawn *)
  let pos u y d =
    let o = float_of_int d * lean * tile * p.squash in
    ((float_of_int u * tile) - (w / 2.) + (tile / 2.) + o - 80., (h / 2.) - (float_of_int y * tile) - (tile / 2.) + o - 40.)
  in
  (* Danny, where the plane puts him, leaning like his depth *)
  let danny =
    let o = float_of_int (if p.crushed then 0 else p.depth) * lean * tile * p.squash in
    group [ rectangle (rgb 70 90 200) 24. 26. |> move_y (-7.); circle (rgb 240 210 180) 9. |> move_y 12.; circle (rgb 40 30 30) 5. |> move (-3.) 16. ]
    |> move (p.u + o - 80.) (p.y + o - 40.)
  in
  let danny_depth = if p.crushed then 0 else p.depth in
  let blocks =
    (* far first; in a depth, left to right and from the bottom up, so
     * that each block's top and side are covered by its neighbours';
     * Danny with his depth, the nearer ones over him *)
    List.concat_map
      (fun d ->
        (fun shapes -> if d = danny_depth then shapes @ [ danny ] else shapes) @@
        List.concat_map
          (fun y ->
            List.filter_map
              (fun u ->
                let x, z = Crush.at p.view u d in
                let x0, y0 = pos u y d in
                match Crush.cell l x y z with
                | '#' ->
                    let mine = (not p.crushed) && d = p.depth in
                    Some (block_shape (if mine then (230, 140, 60) else (120, 120, 140)) p.squash |> move x0 y0)
                | 'E' -> Some (group [ rectangle (rgb 250 230 90) 30. 44. |> fade 0.8; rectangle (rgb 40 30 10) 18. 32. ] |> move x0 y0)
                | _ -> None)
              (List.init n_across Fun.id))
          (List.init (Crush.ny l) (fun r -> Crush.ny l -.. 1 -.. r)))
      (List.init n_deep (fun k -> n_deep -.. 1 -.. k))
  in
  [ rectangle (rgb 25 22 45) screen.width screen.height ] @ blocks
  @ [ text white 2.2 (Printf.sprintf "%s   (%s, %s)" l.name (if p.view = 0 then "front" else "side") (if p.crushed then "crushed" else "depth " ^ string_of_int p.depth))
      |> move_y (screen.top - 40.);
      text (rgb 200 200 230) 1.6 p.message |> move_y (screen.bottom + 60.);
      text (rgb 150 150 180) 1.5 "arrows run and step in depth   space jump   c crush   tab turn" |> move_y (screen.bottom + 30.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let bg = rectangle (rgb 25 22 45) screen.width screen.height in
  match model.scene with
  | Title ->
      [ bg; text (rgb 230 140 60) 6. "TINY CRUSH" |> move_y 200.;
        text white 2. "c crushes the level along your line of sight:" |> move_y 70.;
        text white 2. "what was far apart in depth is one platform" |> move_y 35.;
        text white 2. "c again uncrushes it, and you are where that platform was" |> move_y 0. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_play computer p
  | Done -> [ bg; text (rgb 230 140 60) 4. "DANNY SLEEPS" |> move_y 60. ] @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]

let help = {|TinyCrush
  left right run, up down step in depth, space jump, c crush and uncrush, tab turn the camera
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
