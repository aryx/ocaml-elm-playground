(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Boulder Dash (Peter Liepa with Chris Gray, First
 * Star Software, 1984, on the Atari 800): Rockford digs through a cave
 * of dirt for diamonds, under boulders that fall when what held them
 * is gone. Arrows to dig and walk; space held with an arrow to reach
 * without stepping (take a diamond, dig a cell, push a boulder).
 * Collect the diamonds the cave asks for, and the exit opens.
 *
 * A boulder at rest hurts nobody, and Rockford can stand under it for
 * ever; a *falling* one explodes what it lands on. Boulders are pushed
 * sideways, and roll off each other, off diamonds and off brick walls.
 * Fireflies and butterflies patrol the empty spaces, a hand on the
 * wall, and explode at Rockford's touch -- and under a falling
 * boulder, when a butterfly leaves nine diamonds.
 *
 * Peter Liepa wrote it in six months from a demo of Chris Gray's; it
 * was the first home-computer game made into an arcade game rather
 * than the reverse, and Repton, Supaplex and Emerald Mine are its
 * children. (Names and dates from memory, to check.)
 *
 * The new idea here is that the cave is a cellular automaton, and its
 * rules are the game's whole physics ([scan]). Seven times a second
 * every cell is visited once, row by row from the top left, and does
 * what its kind does, by looking at its neighbours only:
 *
 *     o        o             . o .       a boulder over nothing falls;
 *              v     o       . o .  =>   over something round, it
 *     .  =>    .     o  =>   . . .       rolls off to a side that is
 *                    .     o . . .       empty, and empty below
 *
 * Nothing knows about piles, chains or avalanches: they are what the
 * rule does when applied everywhere. Two details make it work, both
 * the original's:
 *
 *   - the scan is done in place, so its order shows. A boulder moved
 *     down, or to the right, lands on a cell the scan has not reached
 *     yet, and would move again in the same tick; so what moves is
 *     marked ([moved]) and skipped. Rockford is a cell like the others:
 *     he moves when the scan reaches him;
 *   - at rest and falling are two kinds of cell ('o' and 'O'), since a
 *     cell has no memory: that one bit is the difference between a
 *     ceiling and a death.
 *
 * A firefly keeps its left hand on the wall, a butterfly its right
 * ([fly]): turn that way if the cell is empty, else go on, else turn
 * the other way and wait. Started along a wall, that is a patrol round
 * the room (started in the open, a spin on the spot: the hand finds no
 * wall); in the tunnels Rockford digs, it is a pursuit nobody wrote.
 *
 * The caves are made as the original's were, to fit in a few bytes
 * ([cave], [build]): a seed and odds for a random fill, then a few
 * boxes drawn over it.
 *
 * What it uses: Sprite ([pixels]: the cave's ground in runs, and
 * Rockford), Scene2d, Audio's ready-made sounds, Playground's random
 * (the caves' fill). No kit and no Tilemap: the cave is rows of
 * characters rewritten at every tick, and TinySokoban's Push is one
 * line here.
 *
 * The flag cave=2 starts in the second cave:
 *   dune exec games/arcade/TinyBoulderDash.exe -- cave=2
 * or ?cave=2 on the web.
 *
 * Exercises: the amoeba, which grows a cell at random and turns to
 * diamonds when enclosed, and the magic wall, which turns the boulders
 * falling through it into diamonds -- two more rules for [scan]; caves
 * bigger than the screen (the original's are 40 by 22, scrolled:
 * Camera2d); Rockford moving smoothly between two cells; the
 * original's push, which only works one time in eight; its twenty
 * caves and five difficulties; a cave editor (TinySokobanEd is the
 * model).
 *)
open Playground

(*****************************************************************************)
(* The cave *)
(*****************************************************************************)

(* rows from the top, a character a cell:
 *   ' ' empty    '.' dirt     '#' steel    'w' brick
 *   'o' a boulder at rest, 'O' falling;  '*' a diamond at rest, 'V' falling
 *   'R' Rockford  'E' the exit, shut, 'X' open
 *   '0' to '3' a firefly going up, right, down, left; '4' to '7' a butterfly
 *   '!' an explosion, '$' one that leaves a diamond *)
type cave = string array

let cols = 30
let rows = 19

(* up, right, down, left: a quarter turn right is the next one *)
let dirs = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]

let is_firefly (ch : char) : bool = ch >= '0' && ch <= '3'
let is_butterfly (ch : char) : bool = ch >= '4' && ch <= '7'
let is_fly (ch : char) : bool = is_firefly ch || is_butterfly ch

(* what a boulder rolls off *)
let round (ch : char) : bool = ch = 'o' || ch = '*' || ch = 'w'

(* what a tick did, beyond the cave: diamonds taken, the exit entered *)
type news = { diamonds : int; out : bool }

(* [scan dir grab cave]: the cave one tick later. Rockford goes [dir],
 * if any; with [grab] he reaches that way without moving. See the
 * header. The cave is copied, the copy rewritten in place, and a new
 * cave made of it: the mutation does not leave this function. *)
let scan (dir : (int * int) option) (grab : bool) (cave : cave) : cave * news =
  let b = Array.map Bytes.of_string cave in
  let moved = Array.make_matrix rows cols false in
  let at c r = Bytes.get b.(r) c in
  let set c r ch = Bytes.set b.(r) c ch in
  (* a cell written where the scan may still pass: not to be visited again *)
  let put c r ch = set c r ch; moved.(r).(c) <- true in
  let diamonds = ref 0 and out = ref false in
  (* the nine cells round (c, r), but for the steel: gone, or diamonds
   * if a butterfly was there *)
  let explode c r =
    let left = if is_butterfly (at c r) then '$' else '!' in
    for dr = -1 to 1 do
      for dc = -1 to 1 do
        if at (c + dc) (r + dr) <> '#' then put (c + dc) (r + dr) left
      done
    done
  in
  (* a boulder or a diamond: [rest] and [falling] its two characters *)
  let fall c r rest falling =
    let below = at c (r + 1) in
    let side dc = at (c + dc) r = ' ' && at (c + dc) (r + 1) = ' ' in
    if below = ' ' then (set c r ' '; put c (r + 1) falling)
    else if at c r = falling && (below = 'R' || is_fly below) then explode c (r + 1)
    else if round below && side (-1) then (set c r ' '; put (c - 1) r falling)
    else if round below && side 1 then (set c r ' '; put (c + 1) r falling)
    else set c r rest
  in
  (* a firefly ([base] '0', [turn] 3: to its left) or a butterfly ('4',
   * 1: to its right), by the wall; next to Rockford, it blows up *)
  let fly c r base turn =
    let d = Char.code (at c r) - Char.code base in
    let facing d = Char.chr (Char.code base + (d mod 4)) in
    let go d =
      let dc, dr = dirs.(d mod 4) in
      at (c + dc) (r + dr) = ' ' && (set c r ' '; put (c + dc) (r + dr) (facing d); true)
    in
    if Array.exists (fun (dc, dr) -> at (c + dc) (r + dr) = 'R') dirs then explode c r
    else if not (go (d + turn) || go d) then set c r (facing (d + 4 - turn))
  in
  let rockford c r (dc, dr) =
    let tc = c + dc and tr = r + dr in
    (* into the cell, or with [grab] only emptying it *)
    let enter () = if grab then set tc tr ' ' else (set c r ' '; put tc tr 'R') in
    match at tc tr with
    | ' ' -> if not grab then enter ()
    | '.' -> enter ()
    | '*' | 'V' -> incr diamonds; enter ()
    | 'X' -> if not grab then (set c r ' '; out := true)
    | 'o' when dr = 0 && at (tc + dc) tr = ' ' -> put (tc + dc) tr 'o'; enter ()
    | _ -> ()
  in
  for r = 0 to rows - 1 do
    for c = 0 to cols - 1 do
      if not moved.(r).(c) then
        match at c r with
        | 'o' | 'O' -> fall c r 'o' 'O'
        | '*' | 'V' -> fall c r '*' 'V'
        | '0' .. '3' -> fly c r '0' 3
        | '4' .. '7' -> fly c r '4' 1
        | 'R' -> Option.iter (rockford c r) dir
        | '!' -> set c r ' '
        | '$' -> set c r '*'
        | _ -> ()
    done
  done;
  (Array.map Bytes.to_string b, { diamonds = !diamonds; out = !out })

let has (ch : char) (cave : cave) : bool = Array.exists (fun row -> String.contains row ch) cave

(*****************************************************************************)
(* The caves *)
(*****************************************************************************)

(* [Box (ch, c0, r0, c1, r1)]: those cells, corners included, all [ch] *)
type command = Box of char * int * int * int * int

(* A cave: dirt inside a wall of steel, where each cell is first
 * something else with the [odds] given (in percent, drawn from
 * [seed]), then the [commands] drawn over it, in order. [needed]
 * diamonds open the exit; [seconds] to do it. *)
type level = { name : string; seed : int; odds : (char * int) list; commands : command list; needed : int; seconds : int }

let put ch c r = Box (ch, c, r, c, r)

(* a room 8 by 5 for a firefly, four diamonds in its middle; it starts
 * in a corner, the wall on its left *)
let den c r = [ Box (' ', c, r, c + 7, r + 4); Box ('*', c + 2, r + 2, c + 5, r + 2); put '1' c r ]

(* a room 6 by 4 for a butterfly, under a ceiling of dirt one cell
 * thick and a row of boulders: dig the ceiling, step aside *)
let trap c = [ Box ('o', c, 6, c + 5, 6); Box ('.', c, 7, c + 5, 7); Box (' ', c, 8, c + 5, 11); put '6' c 8 ]

let levels : level array =
  [| { name = "A  INTRO"; seed = 10; odds = [ ('o', 12); ('*', 8); (' ', 5) ]; commands = [ put 'R' 2 2; put 'E' 27 16 ]; needed = 12; seconds = 120 };
     { name = "B  FIREFLIES"; seed = 3; odds = [ ('o', 6) ];
       commands = den 3 3 @ den 18 3 @ den 3 12 @ den 18 12 @ [ put 'R' 14 9; put 'E' 27 9 ]; needed = 10; seconds = 120 };
     { name = "C  BUTTERFLIES"; seed = 7; odds = [ ('o', 4) ];
       commands = trap 3 @ trap 12 @ trap 21 @ [ Box ('w', 2, 12, 27, 12); put ' ' 14 12; put 'R' 2 2; put 'E' 27 16 ]; needed = 9; seconds = 150 } |]

let build (l : level) : cave =
  let seed = ref (initial_seed l.seed) in
  let fill c r =
    if r = 0 || c = 0 || r = rows - 1 || c = cols - 1 then '#'
    else
      let n, s = random_int 0 99 !seed in
      seed := s;
      (* the first of the odds that the number falls in *)
      let rec pick n = function [] -> '.' | (ch, p) :: rest -> if n < p then ch else pick (n - p) rest in
      pick n l.odds
  in
  let b = Array.init rows (fun r -> Bytes.init cols (fun c -> fill c r)) in
  List.iter (fun (Box (ch, c0, r0, c1, r1)) -> for r = r0 to r1 do for c = c0 to c1 do Bytes.set b.(r) c ch done done) l.commands;
  Array.map Bytes.to_string b

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = Digging | Dead of int | Out of int (* frames since *)

type game = {
  cave : cave;
  level : int; (* in [levels] *)
  state : state;
  want : (int * int) option; (* the arrow held since the last tick *)
  facing : int; (* 1 right, -1 left *)
  got : int; (* diamonds, in this cave *)
  time_left : int; (* frames *)
  frames : int;
  score : int;
  lives : int;
}

type scene = Title | Playing of game | Game_over of int | All_done of int
type model = scene Scene2d.t

let tick = 8 (* frames between two scans *)

let enter ~(level : int) ~(score : int) ~(lives : int) : game =
  let l = levels.(level) in
  { cave = build l; level; state = Digging; want = None; facing = 1; got = 0; time_left = l.seconds * 60; frames = 0; score; lives }

let initial_model : model = Scene2d.start Title

(* cave=2: the cave a game starts in, 1 by default *)
let first_cave (flags : flags) : int =
  match Option.bind (List.assoc_opt "cave" flags) int_of_string_opt with
  | Some n when n >= 1 && n <= Array.length levels -> n - 1
  | _ -> 0

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the arrow held, as a step in the cave: rows go down *)
let arrow (k : keyboard) : (int * int) option =
  if k.kleft then Some (-1, 0) else if k.kright then Some (1, 0) else if k.kup then Some (0, -1) else if k.kdown then Some (0, 1) else None

let update_game (keys : keyboard) (g : game) : game =
  let g = { g with frames = g.frames + 1 } in
  match g.state with
  | Dead n -> { g with state = Dead (n + 1); cave = (if g.frames mod tick = 0 then fst (scan None false g.cave) else g.cave) }
  | Out n -> { g with state = Out (n + 1); time_left = max 0 (g.time_left - 60); score = (if g.time_left > 0 then g.score + 1 else g.score) }
  | Digging ->
      (* an arrow tapped between two ticks is not lost *)
      let want = match arrow keys with Some d -> Some d | None -> g.want in
      let g = { g with want; time_left = g.time_left - 1 } in
      if g.frames mod tick <> 0 then g
      else
        let cave, news = scan want keys.kspace g.cave in
        let got = g.got + news.diamonds in
        if news.diamonds > 0 then Audio.play Audio.coin;
        let needed = levels.(g.level).needed in
        if got >= needed && g.got < needed then Audio.play Audio.powerup;
        let cave = if got >= needed then Array.map (String.map (fun ch -> if ch = 'E' then 'X' else ch)) cave else cave in
        let facing = match want with Some (dc, 0) -> dc | _ -> g.facing in
        let g = { g with cave; got; facing; want = None; score = g.score + (10 * news.diamonds) } in
        if news.out then (Audio.play Audio.jump; { g with state = Out 0 })
        else if (not (has 'R' cave)) || g.time_left <= 0 then (Audio.play Audio.explosion; { g with state = Dead 0 })
        else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (enter ~level:(first_cave computer.flags) ~score:0 ~lives:3)) s else s
  | Playing g -> (
      let g = update_game computer.keyboard g in
      match g.state with
      (* the same cave again, as it was, with a life less *)
      | Dead n when n > 100 -> if g.lives <= 1 then Scene2d.go (Game_over g.score) s else { s with scene = Playing (enter ~level:g.level ~score:g.score ~lives:(g.lives - 1)) }
      (* the seconds left counted into the score, then the next cave *)
      | Out n when n > 60 && g.time_left = 0 ->
          if g.level + 1 < Array.length levels then { s with scene = Playing (enter ~level:(g.level + 1) ~score:g.score ~lives:g.lives) }
          else Scene2d.go (All_done g.score) s
      | _ -> { s with scene = Playing g })
  | Game_over _ | All_done _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let size = 32. (* a cell *)
let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

(* the ground, flat: the whole cave as one sprite, a cell a pixel *)
let ground = [ ('.', rgb 120 80 40); ('#', rgb 110 115 130); ('w', rgb 170 70 50); ('E', rgb 110 115 130) ]

let rockford = [ "..HHHH.."; ".HHHHHH."; ".SKSSKS."; ".SSSSSS."; "..BBBB.."; ".SBBBBS."; "..BBBB.."; ".LL..LL." ]
let rockford_colors = [ ('H', rgb 230 180 60); ('S', rgb 240 190 150); ('K', black); ('B', rgb 60 110 220); ('L', rgb 90 60 40) ]

(* what is drawn over the ground in a cell *)
let thing (g : game) (ch : char) : shape option =
  let beat = g.frames / 8 mod 2 = 0 in
  match ch with
  | 'o' | 'O' -> Some (group [ circle (rgb 150 150 160) 15.; circle (rgb 190 190 200) 6. |> move (-4.) 5. ])
  | '*' | 'V' -> Some (group [ polygon (if beat then rgb 120 230 255 else rgb 200 250 255) [ (0., 14.); (13., 0.); (0., -14.); (-13., 0.) ]; polygon white [ (0., 14.); (5., 5.); (-5., 5.) ] ])
  | 'R' -> Some (Sprite.pixels 4. rockford_colors (if g.facing < 0 then Sprite.flip rockford else rockford))
  | '0' .. '3' -> Some (group [ square (rgb 255 120 40) 26.; square black (if beat then 16. else 8.); square (rgb 255 220 80) (if beat then 6. else 3.) ])
  | '4' .. '7' ->
      let w = if beat then 13. else 6. in
      Some (group [ polygon (rgb 230 90 220) [ (0., 0.); (-.w, 13.); (-.w, -13.) ]; polygon (rgb 230 90 220) [ (0., 0.); (w, 13.); (w, -13.) ]; rectangle white 3. 20. ])
  | 'X' -> Some (group [ square (if beat then white else rgb 110 115 130) size; rectangle black 16. 24. |> move_y (-4.) ])
  | '!' -> Some (group [ square orange 30. |> rotate 45.; square yellow 16. ])
  | '$' -> Some (group [ square (rgb 120 230 255) 30. |> rotate 45.; square white 16. ])
  | '.' -> Some (group [ square (rgb 95 62 30) 4. |> move (-7.) 6.; square (rgb 95 62 30) 4. |> move 8. (-5.) ])
  | '#' | 'E' -> Some (group [ square (rgb 80 85 100) 4. |> move (-10.) 10.; square (rgb 80 85 100) 4. |> move 10. (-10.) ])
  | 'w' -> Some (group [ rectangle (rgb 110 40 30) size 2.; rectangle (rgb 110 40 30) 2. 15. |> move (-6.) 8.; rectangle (rgb 110 40 30) 2. 15. |> move 8. (-8.) ])
  | _ -> None

let view_game (screen : screen) (g : game) : shape list =
  let l = levels.(g.level) in
  let cell c r = ((float_of_int c -. (float_of_int (cols - 1) /. 2.)) *. size, ((float_of_int (rows - 1) /. 2.) -. float_of_int r) *. size -. 30.) in
  let things =
    List.concat
      (List.init rows (fun r ->
           List.filter_map (fun c -> Option.map (fun s -> let x, y = cell c r in move x y s) (thing g g.cave.(r).[c])) (List.init cols Fun.id)))
  in
  let seconds = (g.time_left + 59) / 60 in
  [ rectangle black screen.width screen.height; Sprite.pixels size ground (Array.to_list g.cave) |> move_y (-30.) ]
  @ things
  @ [ text white 2.4 (Printf.sprintf "CAVE %s" l.name) |> move (screen.left +. 170.) (screen.top -. 60.);
      text (if g.got >= l.needed then green else rgb 120 230 255) 2.4 (Printf.sprintf "DIAMONDS %d / %d" g.got l.needed) |> move (-40.) (screen.top -. 60.);
      text (if seconds <= 20 then red else white) 2.4 (Printf.sprintf "TIME %d" seconds) |> move (screen.right -. 280.) (screen.top -. 60.);
      text white 2.4 (Printf.sprintf "LIVES %d" g.lives) |> move (screen.right -. 90.) (screen.top -. 60.);
      text yellow 2.4 (Printf.sprintf "%06d" g.score) |> move_y (screen.top -. 110.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let press y = Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y y ] in
  let ended color title score = [ rectangle black screen.width screen.height; text color 6. title; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ press (-160.) in
  match s.scene with
  | Title ->
      view_game screen (enter ~level:0 ~score:0 ~lives:3)
      @ [ rectangle black 820. 280. |> fade 0.85 |> move_y 20.; text (rgb 255 200 60) 7. "TINY BOULDER DASH" |> move_y 110.;
          text white 2.3 "arrows: dig, walk, push a boulder" |> move_y 45.;
          text white 2.3 "space held with an arrow: reach without stepping" |> move_y 10.;
          text white 2.3 "enough diamonds open the exit; never stop under a falling boulder" |> move_y (-25.) ]
      @ press (-85.)
  | Playing g -> view_game screen g
  | Game_over score -> ended red "GAME OVER" score
  | All_done score -> ended (rgb 120 230 255) "EVERY CAVE DONE" score

let app = game view update initial_model

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
