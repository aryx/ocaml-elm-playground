(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Go on a 9x9 board, against a computer that knows nothing about Go
 * (ai/Mcts.mli). You are black, it is white: click a point to put a
 * stone down (or move the cursor with the arrows and press space),
 * "p" to pass; two passes in a row end the game, and the
 * score is counted Chinese style -- your stones plus the empty points
 * only you surround, white's plus komi, 6.5 points for playing second.
 * Space plays again.
 *
 * The point of this game in this repository is what the computer does
 * *not* have. Every other searching game here leans on an evaluation
 * function: AiChess counts material and where the pieces stand,
 * AiOthello has a table of what each square is worth. Nobody has ever
 * written one for Go -- whether a position is good depends on whether
 * groups will live, which is as hard as playing -- and that is why Go
 * programs stayed weak from 1970 to 2005 while chess programs beat the
 * world champion.
 *
 * What broke it open in 2006 was giving up on knowledge: to judge a
 * position, play it out *at random* to the end, hundreds of times, and
 * count the wins. This program does exactly that, and nothing else:
 * search for the sentence "how good is this position" in the code and
 * you will not find it. What you will find is a playout that plays
 * random legal moves until neither side has one worth making.
 *
 * The one piece of knowledge in the playouts is a rule about eyes: a
 * random player that fills in its own eyes kills its own groups, and
 * the playouts then say nothing. Not filling a point surrounded by
 * your own stones is the smallest rule that makes random play mean
 * something -- and it is the same rule every Monte Carlo Go program
 * starts from.
 *
 * It plays like a weak amateur, which is the honest result: pure MCTS
 * on 9x9 in 2006 was about that, and what lifted it to superhuman ten
 * years later was AlphaGo replacing the random playouts and the win
 * counts with a neural network (notes_ai_learning.md section 9).
 *
 * What it uses: ai/'s Mcts (the search) and Minimax (the [game] record
 * it takes), Scene2d (the keys pressed). Not Deepening or Zobrist:
 * there is no depth to deepen and no value to remember.
 *
 * Exercises: the ko rule in full (this is the simple one: a move may
 * not take back the single stone that just took); playouts that answer
 * a capture or an atari instead of playing anywhere (the next thing
 * every Go program did); RAVE, which lets a move's results elsewhere
 * count towards it here.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The board *)
(*****************************************************************************)

let size = 9
let points = size *.. size
let komi = 6.5

type stone = Empty | Black | White

type position = {
  board : stone array;
  turn : stone;
  ko : int option; (* the point a stone may not be put back on *)
  passes : int; (* in a row *)
}

type move = Put of int | Pass

let start : position = { board = Array.make points Empty; turn = Black; ko = None; passes = 0 }
let other (s : stone) : stone = match s with Black -> White | White -> Black | Empty -> Empty
let xy (i : int) : int * int = (i mod size, i /.. size)

let neighbours (i : int) : int list =
  let (x, y) = xy i in
  List.filter_map
    (fun (dx, dy) ->
      let (nx, ny) = (x +.. dx, y +.. dy) in
      if nx < 0 || nx >= size || ny < 0 || ny >= size then None else Some ((ny *.. size) +.. nx))
    [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

(* the same neighbours, worked out once: a playout asks for them tens
 * of thousands of times, and a fresh list each time is most of what a
 * playout costs *)
let around : int array array = Array.init points (fun i -> Array.of_list (neighbours i))

let any_around (i : int) (f : int -> bool) : bool = Array.exists f around.(i)
let all_around (i : int) (f : int -> bool) : bool = Array.for_all f around.(i)
let each_around (i : int) (f : int -> unit) : unit = Array.iter f around.(i)

(* the group of stones [i] belongs to, and how many liberties it has:
 * the flood fill every Go program starts with.
 *
 * The points already seen are stamped in one array kept between calls,
 * rather than a fresh array of flags for each -- this is called tens of
 * thousands of times in a playout, and that array was most of what the
 * playout cost (15 seconds a move became 1.6 with this and the
 * neighbour table above) *)
let seen_by : int array = Array.make points 0
let visit = ref 0

let group (board : stone array) (i : int) : int list * int =
  let colour = board.(i) in
  incr visit;
  let liberties = ref 0 and stones = ref [] in
  let rec fill i =
    if seen_by.(i) <> !visit then begin
      seen_by.(i) <- !visit;
      if board.(i) = colour then begin
        stones := i :: !stones;
        each_around i fill
      end
      else if board.(i) = Empty then incr liberties
    end
  in
  fill i;
  (!stones, !liberties)

(* a stone put down: the captures taken off, and whether it was legal
 * at all (a move that takes its own group's last liberty is not) *)
let put (p : position) (i : int) : position option =
  if p.board.(i) <> Empty || p.ko = Some i then None
  else begin
    let board = Array.copy p.board in
    board.(i) <- p.turn;
    let taken = ref [] in
    each_around i (fun n ->
        if board.(n) = other p.turn then
          let (stones, liberties) = group board n in
          if liberties = 0 then taken := stones @ !taken);
    let taken = !taken in
    List.iter (fun j -> board.(j) <- Empty) taken;
    let (_, mine) = group board i in
    if mine = 0 then None (* suicide *)
    else
      (* the simple ko: a single stone taken by a stone that is itself
       * alone and has that one point for its only liberty -- the shape
       * where taking back would repeat the position for ever *)
      let ko =
        match taken with
        | [ j ] when (match group board i with ([ _ ], 1) -> true | _ -> false) -> Some j
        | _ -> None
      in
      Some { board; turn = other p.turn; ko; passes = 0 }
  end

(* the legal points. A point with an empty neighbour is legal without
 * further ado -- the stone put there has that liberty, so it cannot be
 * suicide -- and only the points hemmed in need the full test, which
 * costs a board and a flood fill or two. Late in a game those are most
 * of the board; early, almost none *)
let legal (p : position) : int list =
  List.filter
    (fun i ->
      p.board.(i) = Empty && p.ko <> Some i
      && (any_around i (fun n -> p.board.(n) = Empty) || put p i <> None))
    (List.init points Fun.id)

let play (p : position) (m : move) : position =
  match m with
  | Pass -> { p with turn = other p.turn; ko = None; passes = p.passes +.. 1 }
  | Put i -> ( match put p i with Some p' -> p' | None -> p)

let over (p : position) : bool = p.passes >= 2

(* Chinese scoring: your stones, plus the empty points that touch only
 * your colour *)
let area (p : position) (who : stone) : number =
  let seen = Array.make points false in
  let stones = ref 0 and territory = ref 0 in
  Array.iteri (fun i s -> if s = who then incr stones else ignore i) p.board;
  Array.iteri
    (fun i s ->
      if s = Empty && not seen.(i) then begin
        (* the empty region [i] is in, and the colours around it *)
        let region = ref [] and touches = ref [] in
        let rec fill j =
          if not seen.(j) then begin
            seen.(j) <- true;
            if p.board.(j) = Empty then begin
              region := j :: !region;
              each_around j fill
            end
            else if not (List.mem p.board.(j) !touches) then touches := p.board.(j) :: !touches
          end
        in
        (* a neighbour of another colour is seen, not filled: undo that
         * so another region can see it too *)
        fill i;
        List.iter (fun j -> each_around j (fun n -> if p.board.(n) <> Empty then seen.(n) <- false)) !region;
        if !touches = [ who ] then territory := !territory +.. List.length !region
      end)
    p.board;
  float_of_int (!stones +.. !territory)

let final_score (p : position) : number = area p Black - area p White - komi

(*****************************************************************************)
(* The rules, as ai/ wants them *)
(*****************************************************************************)

(* MAX is white, the computer; the score is only ever asked for at the
 * end of a playout, and only its sign is used (Mcts.mli) *)
let go : (position, move) Minimax.game =
  {
    moves = (fun p -> if over p then [] else Pass :: List.map (fun i -> Put i) (legal p));
    play;
    score = (fun p -> -.final_score p);
    max_to_play = (fun p -> p.turn = White);
  }

(* the one piece of Go knowledge in the whole file: a point surrounded
 * by your own stones is an eye, and filling it is how a random player
 * kills its own group *)
let own_eye (p : position) (i : int) : bool = p.board.(i) = Empty && all_around i (fun n -> p.board.(n) = p.turn)

(* a playout: random legal moves that are not own eyes, until neither
 * side has one; then both pass and the board is counted.
 *
 * The candidates are tried in a random order and the first legal one
 * is played, rather than [legal] being computed and one of those
 * picked: legality costs a board and two flood fills a point, and a
 * playout asks for it at every step of every one of a thousand games.
 * A first version did it the plain way and took 15 seconds a move
 * where this takes a fifth of one. *)
let playout (st : Random.State.t) (_ : (position, move) Minimax.game) (p : position) : position =
  let rec go p steps =
    if over p || steps > 300 then p
    else begin
      let candidates =
        Array.of_list (List.filter (fun i -> p.board.(i) = Empty && not (own_eye p i)) (List.init points Fun.id))
      in
      let n = Array.length candidates in
      (* Fisher-Yates, as far as the first legal one *)
      let rec try_from k =
        if k >= n then go (play p Pass) (steps +.. 1)
        else begin
          let j = k +.. Random.State.int st (n -.. k) in
          let i = candidates.(j) in
          candidates.(j) <- candidates.(k);
          match put p i with Some p' -> go p' (steps +.. 1) | None -> try_from (k +.. 1)
        end
      in
      try_from 0
    end
  in
  go p 0

(*****************************************************************************)
(* The game *)
(*****************************************************************************)

(* A playout of a 9x9 board costs about 1.2 ms here (tests/games times
 * it), so a dozen of them is a frame at 60 fps and a thousand is the
 * second and a half it takes to answer -- during which the game is
 * never stopped, because the tree is an answer at every moment
 * (Mcts.mli: anytime). A chess engine cannot do this: interrupt its
 * search and it has nothing. *)
let playouts_a_move = 1000
let playouts_a_frame = 12

type game = {
  position : position;
  cursor : int;
  last : int option;
  (* its tree, while it is white's turn: MCTS is anytime, so the game
   * goes on drawing while it grows (Mcts.mli's think) *)
  mind : (position, move) Mcts.thinking option;
  thought : int; (* playouts into this move *)
  said : (int * int * float) option; (* playouts, tree nodes, its win rate *)
  moves_played : int;
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; cursor = (4 *.. size) +.. 4; last = None; mind = None; thought = 0; said = None; moves_played = 0 }
let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell = 80.
let board_left = -.(float_of_int (size -.. 1) * cell / 2.)
let point_at (x : number) (y : number) : int option =
  let c = int_of_float (Float.round ((x - board_left) / cell)) in
  let r = int_of_float (Float.round ((y - board_left) / cell)) in
  if c >= 0 && c < size && r >= 0 && r < size && Float.hypot (x - (board_left + (float_of_int c * cell))) (y - (board_left + (float_of_int r * cell))) < cell / 2.
  then Some ((r *.. size) +.. c)
  else None

(* a frame's worth of thinking; when it has had enough, it plays the
 * move its tree believes in *)
let machine_thinks (g : game) : game =
  let t =
    match g.mind with
    | Some t -> t
    | None -> Mcts.start ~seed:g.moves_played ~playout go g.position
  in
  let t = Mcts.think ~playouts:playouts_a_frame t in
  let thought = g.thought +.. playouts_a_frame in
  if thought < playouts_a_move then { g with mind = Some t; thought }
  else
    let r = Mcts.plan t in
    let rate =
      match r.best with
      | Some m -> ( match List.find_opt (fun (x, _, _) -> x = m) r.tried with Some (_, _, share) -> share | None -> 0.5)
      | None -> 0.5
    in
    let move = match r.best with Some m -> m | None -> Pass in
    { g with
      position = play g.position move;
      last = (match move with Put i -> Some i | Pass -> None);
      mind = None;
      thought = 0;
      said = Some (r.playouts, r.nodes, rate);
      moves_played = g.moves_played +.. 1 }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let m = computer.mouse in
  if over g.position then if Scene2d.pressed (fun k -> k.kspace) scenes then new_game () else g
  else if g.position.turn = White then machine_thinks g
  else if Scene2d.pressed (fun k -> Set_.mem "p" k.keys) scenes then
    { g with position = play g.position Pass; last = None; moves_played = g.moves_played +.. 1 }
  else if Scene2d.pressed (fun k -> k.kleft || k.kright || k.kup || k.kdown) scenes then begin
    let (x, y) = xy g.cursor in
    let k = computer.keyboard in
    let x = if k.kleft then max 0 (x -.. 1) else if k.kright then min (size -.. 1) (x +.. 1) else x in
    let y = if k.kdown then max 0 (y -.. 1) else if k.kup then min (size -.. 1) (y +.. 1) else y in
    { g with cursor = (y *.. size) +.. x }
  end
  else if Scene2d.pressed (fun k -> k.kspace) scenes && put g.position g.cursor <> None then
    { g with position = play g.position (Put g.cursor); last = Some g.cursor; moves_played = g.moves_played +.. 1 }
  else if m.mclick then
    match point_at m.mx m.my with
    | Some i when put g.position i <> None ->
        { g with position = play g.position (Put i); last = Some i; moves_played = g.moves_played +.. 1 }
    | _ -> g
  else g

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  { scenes with scene = update_game computer scenes scenes.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let centre (i : int) : number * number =
  let (x, y) = xy i in
  (board_left + (float_of_int x * cell), board_left + (float_of_int y * cell))

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let board = float_of_int (size -.. 1) * cell in
  let grid =
    List.concat_map
      (fun i ->
        let at = board_left + (float_of_int i * cell) in
        [ rectangle (rgb 60 40 20) board 2. |> move_y at; rectangle (rgb 60 40 20) 2. board |> move_x at ])
      (List.init size Fun.id)
  in
  let stones =
    List.filter_map
      (fun i ->
        let (x, y) = centre i in
        match g.position.board.(i) with
        | Empty -> None
        | Black -> Some (circle (rgb 20 20 25) (cell * 0.45) |> move x y)
        | White -> Some (circle (rgb 240 240 235) (cell * 0.45) |> move x y))
      (List.init points Fun.id)
  in
  let last = match g.last with Some i -> let (x, y) = centre i in [ circle (rgb 220 70 60) 8. |> move x y ] | None -> [] in
  let cursor =
    if over g.position || g.position.turn <> Black then []
    else
      let (x, y) = centre g.cursor in
      [ circle (rgb 20 20 25) (cell * 0.45) |> fade 0.35 |> move x y ]
  in
  let said =
    match g.said with
    | None -> []
    | Some (playouts, nodes, rate) ->
        [ text (rgb 170 170 190) 1.5 (Printf.sprintf "%d random games, a tree of %d positions" playouts nodes) |> move_y (-425.);
          text (rgb 170 170 190) 1.5 (Printf.sprintf "it expects to win %.0f%% of them" (100. * rate)) |> move_y (-455.) ]
  in
  let ended =
    if not (over g.position) then []
    else
      let s = final_score g.position in
      [ text white 3. (if s > 0. then Printf.sprintf "YOU WIN BY %.1f" s else Printf.sprintf "WHITE WINS BY %.1f" (-.s))
        |> move_y 430. ]
  in
  [ rectangle (rgb 25 28 45) screen.width screen.height;
    rectangle (rgb 200 160 90) (board + cell) (board + cell) ]
  @ grid @ stones @ cursor @ last @ said @ ended
  @ [ text white 2.2 "GO  9x9" |> move_y 460.;
      text (rgb 150 150 170) 1.5
        (if over g.position then "space: play again"
         else if g.position.turn = Black then "click a point, or the arrows and space;  p: pass"
         else Printf.sprintf "white is playing out random games... %d" g.thought)
      |> move_y (-390.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
