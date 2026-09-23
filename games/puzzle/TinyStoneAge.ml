(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Stone Age (Stonehenge Soft Art, published by Eclipse
 * Software, 1992, on the Amiga, the Atari ST, the C64 and the PC): a
 * little dinosaur crosses stone blocks floating over the void to the
 * cave of each level, against the clock.
 *
 *   arrows   walk, one block at a time
 *   r        start the level again (it costs a life)
 *
 * The blocks, as the level strings write them:
 *
 *   #   stone, walked on as often as you like
 *   c   crumbling stone: it falls into the void when you step off it
 *   > < ^ v   a block that moves, the way its arrow points: step the
 *       arrow's way from it, over the void, and it slides with you on
 *       it until it meets another block (the riding is from memory,
 *       to check)
 *   r g b     a key of that colour, on a stone, picked up by walking
 *       onto it
 *   R G B     a lock: you pass only with its key, which it keeps, and
 *       it is stone from then on
 *   S   where the dinosaur starts, on stone; E the cave
 *   .   the void, where he won't step
 *
 * The lesson is the kind of puzzle it is. Nothing moves unless you do,
 * so a level is a graph: a state is where you stand, what is left of
 * the blocks, and the keys you carry, and a move an edge. Sokoban is
 * the same, with boxes; what Stone Age adds is that most of its moves
 * can't be undone -- a crumbled stone doesn't come back, a moved block
 * doesn't go back the way it came -- so a level is about the *order*
 * of the moves: which stones to spend before you can't reach them,
 * which block to ride while it still points the right way. That makes
 * it small for a computer: [solve] is a breadth-first search over those
 * states, which finds the shortest way through every level here -- 11,
 * 9, 11, 14 and 14 moves -- and the tests prove with it that each can
 * be finished, in time. The five levels are ours, not the original's.
 *
 * The original had a hundred levels, a password after each, two
 * difficulties that differ by the time given, lives, and a jukebox for
 * its music (from descriptions of it; to check).
 *
 * What it uses: Tilemap (the level, and the blocks' strings), Scene2d.
 * Not the puzzle kit's Push (nothing is pushed here) nor Undo: Stone
 * Age had none, and a mistake costs a life -- r starts the level again.
 *
 * Exercises: the slide drawn smoothly between cells; passwords; the
 * hint key, the first move of [solve]'s solution; more block kinds (a
 * block that moves both ways, one that slides by itself, the
 * teleporters of the later levels, if memory serves); a level editor
 * checking with [solve] that its level can be done, as TinySokobanEd
 * does with Sokoban's solver.
 *)
open Playground

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

type level = { name : string; rows : string list; seconds : int }

let levels =
  [ { name = "Crumbling";
      seconds = 60;
      rows =
        [ "..........";
          ".Sc.......";
          "..c.......";
          "..cccc....";
          ".....c....";
          ".cc..cccE.";
          "..cccc....";
          ".........." ] };
    { name = "Across the void";
      seconds = 60;
      rows =
        [ "............";
          ".S#>.....#..";
          ".........#..";
          ".E......<#..";
          "............" ] };
    { name = "The red key";
      seconds = 60;
      rows =
        [ "...........";
          ".S#c#R##E..";
          "..#.c......";
          "..rcc......";
          "..........." ] };
    { name = "Spend them in order";
      seconds = 90;
      rows =
        [ "............";
          ".S#cc.......";
          "..c.c.......";
          "..c.cv......";
          "..ccc.......";
          ".....#c#gG#E";
          "............" ] };
    { name = "Two rides";
      seconds = 120;
      rows =
        [ ".............";
          ".S#>......#..";
          "..........c..";
          "..........c..";
          "..b#.....<c..";
          "..#..........";
          ".EB..........";
          "............." ] } ]

let tile_size = 56.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  level : int;
  map : Tilemap.t; (* the blocks left; the start is stone *)
  col : int;
  row : int;
  keys : char list; (* the keys carried, 'r' 'g' 'b' *)
  riding : (int * int) option; (* on a sliding block, its way *)
  moves : int;
  time : int; (* frames left *)
  lives : int;
}

type scene = Title | Playing of play | Cleared of play | Over of play | Home of play
type model = scene Scene2d.t

let load ?(lives = 3) (n : int) : play =
  let l = List.nth levels n in
  let map = Tilemap.of_strings tile_size l.rows in
  let col, row = match Tilemap.find map 'S' with p :: _ -> p | [] -> (0, 0) in
  { level = n; map = Tilemap.set map col row '#'; col; row; keys = []; riding = None; moves = 0;
    time = l.seconds * 60; lives }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let void (c : char option) : bool = c = Some '.'

let arrow ((dc, dr) : int * int) : char =
  match (dc, dr) with 1, 0 -> '>' | -1, 0 -> '<' | 0, -1 -> '^' | _ -> 'v'

let is_arrow (c : char) : bool = c = '>' || c = '<' || c = '^' || c = 'v'
let is_key (c : char) : bool = c = 'r' || c = 'g' || c = 'b'
let is_lock (c : char) : bool = c = 'R' || c = 'G' || c = 'B'

(* can he step onto [c], carrying [keys]? *)
let walkable (keys : char list) (c : char option) : bool =
  match c with
  | Some ('#' | 'c' | 'E') -> true
  | Some c when is_arrow c || is_key c -> true
  | Some c when is_lock c -> List.mem (Char.lowercase_ascii c) keys
  | _ -> false

(* One press of an arrow: onto a moving block's way over the void, the
 * ride starts; otherwise a step, and what it leaves behind (a crumbling
 * stone falls) and finds (a key taken, a lock opened); None if he can't. *)
let step ((dc, dr) as d : int * int) (p : play) : play option =
  if p.riding <> None then None
  else
    let here = Tilemap.get p.map p.col p.row in
    let c, r = (p.col + dc, p.row + dr) in
    let there = Tilemap.get p.map c r in
    if here = Some (arrow d) && void there then Some { p with riding = Some d; moves = p.moves + 1 }
    else if not (walkable p.keys there) then None
    else
      let map = if here = Some 'c' then Tilemap.set p.map p.col p.row '.' else p.map in
      let map, keys =
        match there with
        | Some k when is_key k -> (Tilemap.set map c r '#', k :: p.keys)
        | Some l when is_lock l ->
            let k = Char.lowercase_ascii l in
            (Tilemap.set map c r '#', List.filter (fun k' -> k' <> k) p.keys)
        | _ -> (map, p.keys)
      in
      Some { p with map; keys; col = c; row = r; moves = p.moves + 1 }

(* the ride, a block a frame... of the slide: on while the void is ahead *)
let slide (p : play) : play =
  match p.riding with
  | None -> p
  | Some (dc, dr) ->
      let c, r = (p.col + dc, p.row + dr) in
      if void (Tilemap.get p.map c r) then
        let block = Option.value (Tilemap.get p.map p.col p.row) ~default:'#' in
        { p with map = Tilemap.set (Tilemap.set p.map p.col p.row '.') c r block; col = c; row = r }
      else { p with riding = None }

(* a move and its ride to the end: what the search explores *)
let rec settle (p : play) : play = if p.riding = None then p else settle (slide p)

let home (p : play) : bool = p.riding = None && Tilemap.get p.map p.col p.row = Some 'E'

let directions = [ (0, -1); (0, 1); (-1, 0); (1, 0) ]

(* The shortest way to the cave, as the arrows to press, by a
 * breadth-first search over the states (where he is, the blocks, the
 * keys); None if there is none. *)
let solve (p : play) : (int * int) list option =
  let key (p : play) = (p.col, p.row, Tilemap.to_strings p.map, List.sort compare p.keys) in
  let seen = Hashtbl.create 1000 in
  let rec bfs frontier =
    match frontier with
    | [] -> None
    | _ -> (
        match List.find_opt (fun (p, _) -> home p) frontier with
        | Some (_, path) -> Some (List.rev path)
        | None ->
            let next =
              List.concat_map
                (fun (p, path) ->
                  List.filter_map
                    (fun d ->
                      match step d p with
                      | None -> None
                      | Some q ->
                          let q = settle q in
                          if Hashtbl.mem seen (key q) then None
                          else (
                            Hashtbl.add seen (key q) ();
                            Some (q, d :: path)))
                    directions)
                frontier
            in
            bfs next)
  in
  Hashtbl.add seen (key p) ();
  bfs [ (p, []) ]

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let slide_frames = 6

let update_play (s : model) (p : play) : play =
  let p = { p with time = p.time - 1 } in
  if p.riding <> None then if s.frames mod slide_frames = 0 then slide p else p
  else
    let pressed f = Scene2d.pressed f s in
    let dir =
      if pressed (fun k -> k.kup) then Some (0, -1)
      else if pressed (fun k -> k.kdown) then Some (0, 1)
      else if pressed (fun k -> k.kleft) then Some (-1, 0)
      else if pressed (fun k -> k.kright) then Some (1, 0)
      else None
    in
    match dir with Some d -> Option.value (step d p) ~default:p | None -> p

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (load 0)) s else s
  | Playing p ->
      let p = update_play s p in
      let lost = p.time <= 0 || (p.riding = None && Scene2d.pressed (fun k -> Set_.mem "r" k.keys) s) in
      if home p then Scene2d.go (if p.level + 1 < List.length levels then Cleared p else Home p) s
      else if lost && p.lives <= 1 then Scene2d.go (Over p) s
      else if lost then { s with scene = Playing (load ~lives:(p.lives - 1) p.level) }
      else { s with scene = Playing p }
  | Cleared p -> if space then Scene2d.go (Playing (load ~lives:p.lives (p.level + 1))) s else s
  | Over _ | Home _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let stone = rgb 150 140 130
let lit = rgb 190 180 170
let key_color (c : char) : color =
  match Char.lowercase_ascii c with 'r' -> rgb 220 50 50 | 'g' -> rgb 60 180 70 | _ -> rgb 60 110 230

(* a block: a square with a lighter top, the cracks of a crumbling one,
 * the arrow of a moving one *)
let block (c : char) : shape =
  let s = tile_size -. 4. in
  let base color = group [ square (rgb 70 60 55) s; square color (s -. 6.) |> move 0. 2.; rectangle lit (s -. 10.) 5. |> move_y ((s /. 2.) -. 6.) ] in
  match c with
  | '#' -> base stone
  | 'c' ->
      group
        [ base (rgb 170 130 90);
          rectangle (rgb 90 60 40) 3. 24. |> rotate 30. |> move (-8.) 4.;
          rectangle (rgb 90 60 40) 3. 18. |> rotate (-40.) |> move 9. (-6.) ]
  | '>' | '<' | '^' | 'v' ->
      let angle = match c with '>' -> -90. | '<' -> 90. | '^' -> 0. | _ -> 180. in
      group [ base (rgb 120 150 170); triangle (rgb 250 240 200) 14. |> rotate angle ]
  | 'r' | 'g' | 'b' ->
      group
        [ base stone;
          circle (key_color c) 8. |> move (-8.) 0.;
          rectangle (key_color c) 20. 5. |> move 8. 0.;
          rectangle (key_color c) 4. 8. |> move 15. (-5.) ]
  | 'R' | 'G' | 'B' ->
      group [ base (key_color c); circle black 6. |> move_y 4.; rectangle black 5. 12. |> move_y (-5.) ]
  | 'E' ->
      group [ base stone; oval (rgb 30 20 20) 36. 40. |> move_y (-4.); rectangle (rgb 30 20 20) 36. 16. |> move_y (-16.) ]
  | _ -> group []

(* the dinosaur, a green one, facing right *)
let dinosaur : shape =
  let green = rgb 90 190 70 and dark = rgb 50 120 40 in
  group
    [ triangle green 14. |> rotate 30. |> move (-18.) (-4.);
      oval green 30. 22. |> move (-2.) (-4.);
      circle green 10. |> move 12. 10.;
      circle white 3.5 |> move 15. 13.;
      circle black 1.8 |> move 16. 13.;
      rectangle dark 5. 9. |> move (-8.) (-17.);
      rectangle dark 5. 9. |> move 4. (-17.);
      triangle dark 4. |> move (-6.) 8.;
      triangle dark 4. |> move 0. 7. ]

let view_play (p : play) : shape list =
  let l = List.nth levels p.level in
  let x, y = Tilemap.center p.map p.col p.row in
  [ Tilemap.view block p.map; dinosaur |> move x (y +. 4.);
    text white 2.5 (Printf.sprintf "LEVEL %d  %s" (p.level + 1) l.name) |> move_y 380.;
    text white 2. (Printf.sprintf "TIME %d   LIVES %d   MOVES %d" ((p.time + 59) / 60) p.lives p.moves) |> move_y 340. ]
  @ List.mapi (fun i k -> block k |> scale 0.7 |> move (-300. +. (float_of_int i *. 45.)) (-340.)) p.keys
  @ [ text (rgb 170 170 190) 1.8 "arrows: walk (and ride an arrow over the void)   r: start again" |> move_y (-390.) ]

let view (computer : computer) (s : model) : shape list =
  let sky = rectangle (rgb 30 25 50) computer.screen.width computer.screen.height in
  let blink words = Scene2d.blink 1. s [ text (rgb 240 200 40) 3. words |> move_y (-250.) ] in
  sky
  ::
  (match s.scene with
  | Title ->
      [ text (rgb 240 200 40) 6. "TINY STONE AGE" |> move_y 200.;
        text white 2.2 "take the dinosaur to the cave" |> move_y 110.;
        text white 2.2 "cracked stones fall when you step off them" |> move_y 70.;
        text white 2.2 "arrow blocks carry you over the void; keys open locks" |> move_y 30.;
        block '#' |> move (-120.) (-60.); block 'c' |> move (-60.) (-60.); block '>' |> move 0. (-60.);
        block 'r' |> move 60. (-60.); block 'E' |> move 120. (-60.); dinosaur |> move (-120.) (-56.) ]
      @ blink "PRESS SPACE"
  | Playing p -> view_play p
  | Cleared p -> view_play p @ [ text (rgb 240 200 40) 5. "IN THE CAVE!" |> move_y 250. ] @ blink "SPACE: NEXT LEVEL"
  | Over p -> view_play p @ [ text (rgb 240 80 60) 5. "GAME OVER" |> move_y 250. ] @ blink "PRESS SPACE"
  | Home p -> view_play p @ [ text (rgb 240 200 40) 5. "EVERY CAVE REACHED" |> move_y 250. ] @ blink "PRESS SPACE")

let app = game view update initial_model

let main = Playground_platform.run_app app
