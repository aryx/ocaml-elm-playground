(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tui_snake.mli *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the board, inside its frame *)
let height = 20
let width = 40

(* seconds between two moves *)
let period = 0.1

type model = {
  body : (int * int) list; (* head first *)
  dir : int * int; (* rows, columns a move *)
  turn : int * int; (* the direction asked for, taken at the next move *)
  food : int * int;
  seed : Lehmer.t;
  clock : float; (* seconds since the last move *)
  score : int;
  crashed : bool;
  quit : bool;
}

(* a cell the snake isn't on, from the generator *)
let rec place (seed : Lehmer.t) (body : (int * int) list) : (int * int) * Lehmer.t =
  let seed = Lehmer.next seed in
  let i = int_of_float (Lehmer.to_unit seed *. float_of_int (height * width)) in
  let cell = (i / width, i mod width) in
  if List.mem cell body then place seed body else (cell, seed)

let start (seed : Lehmer.t) : model =
  let body = [ (10, 12); (10, 11); (10, 10) ] in
  let food, seed = place seed body in
  { body; dir = (0, 1); turn = (0, 1); food; seed; clock = 0.; score = 0; crashed = false; quit = false }

let init = start (Lehmer.of_int 42)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a move: the head a cell further; the tail follows unless it ate *)
let move (m : model) : model =
  (* no turning back into oneself *)
  let dir = if fst m.turn = -fst m.dir && snd m.turn = -snd m.dir then m.dir else m.turn in
  let r, c = List.hd m.body in
  let head = (r + fst dir, c + snd dir) in
  let ate = head = m.food in
  let body = head :: (if ate then m.body else List.filteri (fun i _ -> i < List.length m.body - 1) m.body) in
  let off = fst head < 0 || fst head >= height || snd head < 0 || snd head >= width in
  if off || List.mem head (List.tl body) then { m with crashed = true }
  else if ate then
    let food, seed = place m.seed body in
    { m with body; dir; food; seed; score = m.score + 1 }
  else { m with body; dir }

let update (e : Tui.event) (m : model) : model =
  match e with
  | Key "q" -> { m with quit = true }
  | Key "r" when m.crashed -> { (start m.seed) with score = 0 }
  | Key ("\x1b[A" | "k") -> { m with turn = (-1, 0) }
  | Key ("\x1b[B" | "j") -> { m with turn = (1, 0) }
  | Key ("\x1b[C" | "l") -> { m with turn = (0, 1) }
  | Key ("\x1b[D" | "h") -> { m with turn = (0, -1) }
  | Key _ -> m
  | Tick _ when m.crashed -> m
  | Tick dt ->
      (* as many moves as periods passed, a slow frame catching up; with
         a hair of tolerance, since 0.3 - 0.1 - 0.1 is 0.0999...98 in
         floating point, and three periods must be three moves *)
      let rec steps (m : model) =
        if m.clock >= period -. 1e-9 && not m.crashed then steps (move { m with clock = m.clock -. period }) else m
      in
      steps { m with clock = m.clock +. dt }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let green = { Vt.plain with fg = Vt.Green; bold = true }
let red = { Vt.plain with fg = Vt.Red; bold = true }

let view (m : model) : Curses.t =
  let cell (r, c) = (r + 2, c + 1) in
  let screen =
    Curses.create ~rows:24 ~cols:80
    |> Curses.put 0 0 "SNAKE"
    |> Curses.box 1 0 (height + 2) (width + 2)
    |> Curses.put 3 46 "arrows, or h j k l: turn"
    |> Curses.put 4 46 "q: quit"
    |> Curses.put 6 46 (Printf.sprintf "score: %d" m.score)
  in
  let fr, fc = cell m.food in
  let screen = Curses.put ~attrs:red fr fc "*" screen in
  let screen =
    List.fold_left
      (fun s (i, rc) ->
        let r, c = cell rc in
        Curses.put ~attrs:green r c (if i = 0 then "@" else "o") s)
      screen
      (List.mapi (fun i rc -> (i, rc)) m.body)
  in
  if m.crashed then Curses.put ~attrs:{ Vt.plain with reverse = true } 8 46 " CRASHED -- r: again " screen else screen

let program : model Tui.program = { init; update; view; over = (fun m -> m.quit) }
let body (m : model) = m.body
let crashed (m : model) = m.crashed
let score (m : model) = m.score
