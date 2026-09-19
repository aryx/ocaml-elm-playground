(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Baba Is You (Arvi Teikari, 2019): a puzzle where the
 * rules are on the board, as words you can push. "BABA IS YOU": you are
 * Baba. "WALL IS STOP": walls stop you -- push the word STOP away, and
 * they don't anymore. "FLAG IS WIN": touch the flag to win. Arrows to
 * move, z to undo, r to start the level over.
 *
 * The rules are sentences on the grid, read left to right or top to
 * bottom, NOUN IS PROPERTY ("ROCK IS PUSH") or NOUN IS NOUN ("ROCK IS
 * FLAG": every rock becomes a flag); remade after every move ([rules]).
 * The properties: YOU (moved by the arrows), WIN (a YOU on it wins),
 * STOP, PUSH (the words always are), DEFEAT (destroys a YOU on it), SINK
 * (destroys anything on it, and itself).
 *
 * The new idea is the one the game is about: the rules as data, in the
 * same world as the things they rule. A program reading its own rules
 * from the board, and the player rewriting them: a game that is its own
 * level editor. The ideas before it: PuzzleScript (Stephen Lavelle,
 * 2013), whose games are a map and a dozen rewriting rules; Sokoban,
 * which is Baba Is You with its rules glued down (BABA IS YOU, BOX IS
 * PUSH, WALL IS STOP; see games/TinySokoban). (Names and dates from
 * memory, to check.)
 *
 * What it uses: the puzzle kit (kits/puzzle/: Push, rows of things
 * pushed, no limit; Undo, the boards before), Scene2d. Not Tilemap: a
 * cell can hold several things (Baba on the flag, a rock on the water),
 * so the board is a list of objects with their cells.
 *
 * Exercises: more words (AND, HOT and MELT, MOVE, OPEN and SHUT, TEXT IS
 * ...), more levels (the original's first world, rebuilt), a solver
 * showing the shortest solution (the tests have one, a breadth-first
 * search).
 *)
open Playground

(*****************************************************************************)
(* Things and words *)
(*****************************************************************************)

type noun = Baba | Wall | Rock | Flag | Water | Skull
type prop = You | Win | Stop | Push | Defeat | Sink
type word = Noun of noun | Is | Prop of prop
type kind = Thing of noun | Text of word

(* an object on the board; [id] to tell two rocks apart *)
type obj = { id : int; kind : kind; c : int; r : int }

type board = { objs : obj list; cols : int; rows : int }

(* the levels: things in lowercase (b Baba, # wall, o rock, f flag, ~
 * water, x skull), words in capitals and signs (B BABA, W WALL, R ROCK,
 * F FLAG, A WATER, K SKULL, = IS, y YOU, v WIN, s STOP, p PUSH, d
 * DEFEAT, k SINK) *)
let levels : string list list =
  [ [ "............."; ".B=y.....F=v."; "............."; "#############"; ".....o......."; ".b...o.....f."; ".....o......."; "#############"; ".W=s.....R=p." ];
    [ ".............."; ".B=y.........."; "......#####..."; ".b....#...#..."; "......#.f.#..."; "......#...#..."; "......#####..."; ".W=s...F=v...."; ".............." ];
    [ ".B=y...~.A=k..."; ".......~......."; ".......~......."; ".b..o..~.....f."; ".......~......."; ".R=p...~..F=v.."; ".......~......." ];
    [ ".B=y......#.f"; "..........#.."; ".....=....#.."; ".....v....#.."; ".......b..#.."; ".W=s..F=v.#.." ] ]

let kind_of (ch : char) : kind option =
  match ch with
  | 'b' -> Some (Thing Baba) | '#' -> Some (Thing Wall) | 'o' -> Some (Thing Rock) | 'f' -> Some (Thing Flag) | '~' -> Some (Thing Water) | 'x' -> Some (Thing Skull)
  | 'B' -> Some (Text (Noun Baba)) | 'W' -> Some (Text (Noun Wall)) | 'R' -> Some (Text (Noun Rock)) | 'F' -> Some (Text (Noun Flag))
  | 'A' -> Some (Text (Noun Water)) | 'K' -> Some (Text (Noun Skull)) | '=' -> Some (Text Is)
  | 'y' -> Some (Text (Prop You)) | 'v' -> Some (Text (Prop Win)) | 's' -> Some (Text (Prop Stop)) | 'p' -> Some (Text (Prop Push))
  | 'd' -> Some (Text (Prop Defeat)) | 'k' -> Some (Text (Prop Sink))
  | _ -> None

let load (level : int) : board =
  let rows = List.nth levels level in
  let objs =
    List.concat (List.mapi (fun r line -> List.filter_map (fun c -> Option.map (fun kind -> { id = (r * 100) + c; kind; c; r }) (kind_of line.[c])) (List.init (String.length line) Fun.id)) rows)
  in
  { objs; cols = String.length (List.hd rows); rows = List.length rows }

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let word_at (b : board) (c : int) (r : int) : word option =
  List.find_map (fun o -> if o.c = c && o.r = r then match o.kind with Text w -> Some w | Thing _ -> None else None) b.objs

(* [rules b]: the sentences on the board, NOUN IS PROPERTY or NOUN IS
 * NOUN, across (the word right of NOUN, then the next) or down (the word
 * under it, then the next). E.g. B = y on a row: (Baba, Prop You). *)
let rules (b : board) : (noun * word) list =
  List.concat_map
    (fun o ->
      match o.kind with
      | Text (Noun n) ->
          List.filter_map
            (fun (dc, dr) ->
              match (word_at b (o.c + dc) (o.r + dr), word_at b (o.c + (2 * dc)) (o.r + (2 * dr))) with
              | Some Is, Some ((Prop _ | Noun _) as w) -> Some (n, w)
              | _ -> None)
            [ (1, 0); (0, 1) ]
      | _ -> [])
    b.objs

(* [is rules o p]: [o] has the property [p]; words are always PUSH *)
let is (rs : (noun * word) list) (o : obj) (p : prop) : bool =
  match o.kind with Thing n -> List.mem (n, Prop p) rs | Text _ -> p = Push

(*****************************************************************************)
(* A move *)
(*****************************************************************************)

let at (b : board) (c : int) (r : int) : obj list = List.filter (fun o -> o.c = c && o.r = r) b.objs

(* one YOU, one step: pushing the rows of PUSH things in front, stopped
 * by a STOP thing or the edge (the puzzle kit's Push.chain, no limit) *)
let step_one (rs : (noun * word) list) ((dc, dr) : int * int) (b : board) (you : obj) : board =
  match List.find_opt (fun o -> o.id = you.id) b.objs with
  | None -> b
  | Some you -> (
      let inside (c, r) = c >= 0 && c < b.cols && r >= 0 && r < b.rows in
      let blocked (c, r) = (not (inside (c, r))) || List.exists (fun o -> is rs o Stop && not (is rs o Push)) (at b c r) in
      let pushable (c, r) = inside (c, r) && List.exists (fun o -> is rs o Push) (at b c r) in
      match Push.chain ~blocked ~pushable (you.c, you.r) (dc, dr) with
      | None -> b
      | Some chain ->
          let moved o = o.id = you.id || (List.mem (o.c, o.r) chain && is rs o Push) in
          { b with objs = List.map (fun o -> if moved o then { o with c = o.c + dc; r = o.r + dr } else o) b.objs })

(* after the moves: NOUN IS NOUN turns things into others; SINK takes
 * everything in its cell with it; DEFEAT destroys the YOUs in its cell *)
let settle (b : board) : board =
  let rs = rules b in
  let objs = List.map (fun o -> match o.kind with Thing n -> (match List.find_opt (fun (n', w) -> n' = n && (match w with Noun m -> m <> n | _ -> false)) rs with Some (_, Noun m) -> { o with kind = Thing m } | _ -> o) | Text _ -> o) b.objs in
  let b = { b with objs } in
  let rs = rules b in
  let sunk o = List.exists (fun s -> s.id <> o.id && (is rs s Sink || is rs o Sink)) (at b o.c o.r) in
  let b = { b with objs = List.filter (fun o -> not (sunk o)) b.objs } in
  let defeated o = is rs o You && List.exists (fun d -> is rs d Defeat) (at b o.c o.r) in
  { b with objs = List.filter (fun o -> not (defeated o)) b.objs }

(* [turn b dir]: every YOU steps (the one ahead first, so a row of YOUs
 * moves together), then the board settles *)
let turn (b : board) ((dc, dr) : int * int) : board =
  let rs = rules b in
  let yous = List.sort (fun a o -> compare ((o.c * dc) + (o.r * dr)) ((a.c * dc) + (a.r * dr))) (List.filter (fun o -> is rs o You) b.objs) in
  settle (List.fold_left (step_one rs (dc, dr)) b yous)

(* a YOU on a WIN (itself too: BABA IS WIN, and BABA IS YOU) *)
let won (b : board) : bool =
  let rs = rules b in
  List.exists (fun y -> is rs y You && List.exists (fun w -> is rs w Win) (at b y.c y.r)) b.objs

let no_you (b : board) : bool = let rs = rules b in not (List.exists (fun o -> is rs o You) b.objs)

(*****************************************************************************)
(* The model, update *)
(*****************************************************************************)

type play = { level : int; boards : board Undo.t }
type scene = Title | Playing of play | Won_level of play | The_end
type model = scene Scene2d.t

let start (level : int) : play = { level; boards = Undo.start (load level) }
let initial_model : model = Scene2d.start Title

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed k = Scene2d.pressed k s in
  let key c = pressed (fun k -> Set_.mem c k.keys) in
  match s.scene with
  | Title -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (start 0)) s else s
  | Playing p ->
      let dir =
        if pressed (fun k -> k.kup) then Some (0, -1) else if pressed (fun k -> k.kdown) then Some (0, 1)
        else if pressed (fun k -> k.kleft) then Some (-1, 0) else if pressed (fun k -> k.kright) then Some (1, 0) else None
      in
      let p =
        match dir with
        | Some d -> { p with boards = Undo.record (turn p.boards.now d) p.boards }
        | None -> if key "z" then { p with boards = Undo.undo p.boards } else if key "r" then start p.level else p
      in
      if won p.boards.now then (Audio.play Audio.coin; Scene2d.go (Won_level p) s) else { s with scene = Playing p }
  | Won_level p ->
      if pressed (fun k -> k.kspace) then if p.level + 1 < List.length levels then Scene2d.go (Playing (start (p.level + 1))) s else Scene2d.go The_end s else s
  | The_end -> if pressed (fun k -> k.kspace) then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let noun_name (n : noun) : string = match n with Baba -> "BABA" | Wall -> "WALL" | Rock -> "ROCK" | Flag -> "FLAG" | Water -> "WATER" | Skull -> "SKULL"
let prop_name (p : prop) : string = match p with You -> "YOU" | Win -> "WIN" | Stop -> "STOP" | Push -> "PUSH" | Defeat -> "DEFEAT" | Sink -> "SINK"
let prop_color (p : prop) : color =
  match p with You -> rgb 230 90 150 | Win -> rgb 240 210 60 | Stop -> rgb 70 160 70 | Push -> rgb 160 110 60 | Defeat -> rgb 200 50 50 | Sink -> rgb 60 110 220

let view_thing (size : number) (frames : int) (n : noun) : shape =
  let s = size in
  match n with
  | Baba ->
      let bob = if frames / 15 mod 2 = 0 then 0. else s *. 0.04 in
      group [ oval white (s *. 0.7) (s *. 0.5) |> move_y bob; circle black (s *. 0.06) |> move (s *. 0.12) (s *. 0.08 +. bob); circle black (s *. 0.06) |> move (s *. 0.25) (s *. 0.08 +. bob) ]
  | Wall -> group [ square (rgb 60 60 80) s; rectangle (rgb 40 40 55) s (s *. 0.06); rectangle (rgb 40 40 55) (s *. 0.06) (s *. 0.5) |> move_y (s *. 0.25) ]
  | Rock -> group [ circle (rgb 150 100 60) (s *. 0.38); circle (rgb 180 130 80) (s *. 0.15) |> move (-.s *. 0.1) (s *. 0.1) ]
  | Flag -> group [ rectangle (rgb 200 200 200) (s *. 0.06) (s *. 0.8) |> move_x (-.s *. 0.2); polygon (rgb 240 210 60) [ (-.s *. 0.17, s *. 0.35); (s *. 0.3, s *. 0.2); (-.s *. 0.17, s *. 0.05) ] ]
  | Water -> group [ square (rgb 50 90 200) s; rectangle (rgb 100 150 240) (s *. 0.4) (s *. 0.05) |> move (-.s *. 0.15) (s *. 0.1) ]
  | Skull -> group [ circle (rgb 200 60 60) (s *. 0.35); circle black (s *. 0.08) |> move (-.s *. 0.12) (s *. 0.05); circle black (s *. 0.08) |> move (s *. 0.12) (s *. 0.05) ]

(* a word: its tile, brighter when it's part of a rule now *)
let view_word (size : number) (active : bool) (w : word) : shape =
  let label, color = match w with Noun n -> (noun_name n, rgb 230 110 160) | Is -> ("IS", white) | Prop p -> (prop_name p, prop_color p) in
  let tile = square (rgb 25 25 35) (size *. 0.9) in
  (* the label within the tile: 80% of its width (a letter of words at
   * scale 1 is about 6 wide), 35% of its height (10 high) *)
  let k = Float.min (0.8 *. size /. (6. *. float_of_int (String.length label))) (0.35 *. size /. 10.) in
  group [ tile; text color k label ] |> fade (if active then 1. else 0.45)

(* the words in a sentence now *)
let active_words (b : board) : int list =
  List.concat_map
    (fun o ->
      match o.kind with
      | Text (Noun _) ->
          List.concat_map
            (fun (dc, dr) ->
              let a = List.find_opt (fun x -> x.c = o.c + dc && x.r = o.r + dr && x.kind = Text Is) b.objs in
              let w = List.find_opt (fun x -> x.c = o.c + (2 * dc) && x.r = o.r + (2 * dr) && (match x.kind with Text (Prop _ | Noun _) -> true | _ -> false)) b.objs in
              match (a, w) with Some a, Some w -> [ o.id; a.id; w.id ] | _ -> [])
            [ (1, 0); (0, 1) ]
      | _ -> [])
    b.objs

let view_board (frames : int) (p : play) : shape list =
  let b = p.boards.now in
  let size = Float.min (900. /. float_of_int b.cols) (700. /. float_of_int b.rows) in
  let x c = (float_of_int c +. 0.5 -. (float_of_int b.cols /. 2.)) *. size and y r = ((float_of_int b.rows /. 2.) -. float_of_int r -. 0.5) *. size in
  let active = active_words b in
  (* the things first, the words over them *)
  let order o = match o.kind with Thing (Water | Wall) -> 0 | Thing _ -> 1 | Text _ -> 2 in
  [ rectangle (rgb 15 15 25) (float_of_int b.cols *. size) (float_of_int b.rows *. size) ]
  @ List.map
      (fun o -> (match o.kind with Thing n -> view_thing size frames n | Text w -> view_word size (List.mem o.id active) w) |> move (x o.c) (y o.r))
      (List.stable_sort (fun a o -> compare (order a) (order o)) b.objs)
  @ [ text white 2.5 (Printf.sprintf "LEVEL %d   arrows move   z undo   r restart" (p.level + 1)) |> move_y 440. ]
  @ if no_you b then [ text (rgb 230 90 150) 3. "NOTHING IS YOU: z TO UNDO" |> move_y (-440.) ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 10 10 15) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text (rgb 230 110 160) 7. "TINY BABA IS YOU" |> move_y 150.; text white 2.5 "the rules are words on the board: push them" |> move_y 60.;
        text white 2.5 "arrows move   z undo   r restart" |> move_y 20. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ]
  | Playing p -> view_board s.frames p
  | Won_level p -> view_board s.frames p @ [ text (rgb 240 210 60) 6. "CONGRATULATIONS" ] @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-80.) ]
  | The_end -> [ text (rgb 240 210 60) 6. "ALL LEVELS DONE" ] @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-80.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
