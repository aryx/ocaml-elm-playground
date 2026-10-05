(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Go9.mli *)
open Basics (* float arithmetics *)

type number = float

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
let playout (st : Lehmer.state) (_ : (position, move) Minimax.game) (p : position) : position =
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
          let j = k +.. Lehmer.int st (n -.. k) in
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
(* As a network reads it *)
(*****************************************************************************)

(* the same rules without the one move no player should make: filling
 * an eye of one's own. A search guided by a network that knows nothing
 * yet would otherwise spend its games on it *)
let sensible : (position, move) Minimax.game =
  {
    go with
    moves =
      (fun p -> if over p then [] else Pass :: List.filter_map (fun i -> if own_eye p i then None else Some (Put i)) (legal p));
  }

(* three planes of 81: the stones of whoever is to play, the other's,
 * and the point of the ko if there is one *)
let encode (p : position) : float array =
  Array.init (3 *.. points) (fun k ->
      let plane = k /.. points and i = k mod points in
      match plane with
      | 0 -> if p.board.(i) = p.turn then 1. else 0.
      | 1 -> if p.board.(i) = other p.turn then 1. else 0.
      | _ -> if p.ko = Some i then 1. else 0.)

(* a point its own number, the pass after them all *)
let index (m : move) : int = match m with Put i -> i | Pass -> points

let board : (position, move) Selfplay.board =
  { game = sensible; start; inputs = 3 *.. points; moves = points +.. 1; encode; index }

(* the same, for games that must end: a position with the moves played
 * so far, and no move left after [longest] of them. Two players who
 * know nothing do not pass, and would go on capturing each other for
 * ever *)
let capped ~(longest : int) : (position * int, move) Selfplay.board =
  {
    game =
      {
        moves = (fun (p, n) -> if n >= longest then [] else sensible.moves p);
        play = (fun (p, n) m -> (sensible.play p m, n +.. 1));
        score = (fun (p, _) -> sensible.score p);
        max_to_play = (fun (p, _) -> sensible.max_to_play p);
      };
    start = (start, 0);
    inputs = 3 *.. points;
    moves = points +.. 1;
    encode = (fun (p, _) -> encode p);
    index;
  }

(* the board turned and flipped: its eight symmetries, 0 the board as
 * it is. A point's place under one of them *)
let turned (symmetry : int) (i : int) : int =
  let (x, y) = xy i in
  let (x, y) = if symmetry land 1 = 1 then (size -.. 1 -.. x, y) else (x, y) in
  let (x, y) = if symmetry land 2 = 2 then (x, size -.. 1 -.. y) else (x, y) in
  let (x, y) = if symmetry land 4 = 4 then (y, x) else (x, y) in
  (y *.. size) +.. x

(* a lesson seen under a symmetry: the planes and the policy turned
 * the same way, the pass and the value as they were *)
let lesson_turned (symmetry : int) (l : Policy_value.lesson) : Policy_value.lesson =
  let input = Array.make (Array.length l.input) 0. and policy = Array.make (Array.length l.policy) 0. in
  for plane = 0 to 2 do
    for i = 0 to points -.. 1 do
      input.((plane *.. points) +.. turned symmetry i) <- l.input.((plane *.. points) +.. i)
    done
  done;
  for i = 0 to points -.. 1 do
    policy.(turned symmetry i) <- l.policy.(i)
  done;
  policy.(points) <- l.policy.(points);
  { l with input; policy }
