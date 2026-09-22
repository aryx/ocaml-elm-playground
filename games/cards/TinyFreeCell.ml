(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of FreeCell (Paul Alfille, on the PLATO system, 1978;
 * Jim Horne's port shipped with Windows from 1995): all 52 cards dealt
 * face up onto 8 columns, 4 free cells to park a card each, and the
 * four foundations to build up by suit, ace to king. A card goes onto a
 * column's last card if it is one lower and of the other color (a red 6
 * on a black 7), and any card into an empty free cell or column.
 *
 *   click a card     pick it up, with the cards below it in its column
 *   click a place    put them there: a column, a free cell, a foundation
 *   n                the next deal       deal=N (a flag): deal N
 *
 * Everything is face up: no luck once dealt, only thinking, which is why
 * it became the patience of the programmers. Windows' version numbered
 * its deals 1 to 32000 and made every one the same on every computer
 * (Cards.deal: a known random generator, seeded with the number), so
 * that players could compare their play -- and the Internet FreeCell
 * Project (Dave Ring, 1994) played all 32000: every one can be won but
 * number 11982. (Names and dates from memory, to check.)
 *
 * The trick of the game's rules is the supermove. The rules move one
 * card at a time, but moving a run of cards (a 9 8 7 of alternating
 * colors) is just a sequence of single moves through the empty free
 * cells and columns -- so the game lets you move the run at once, as
 * long as there are enough of them:
 *
 *   the longest run  =  (1 + empty free cells) * 2 ^ (empty columns)
 *
 * with 2 free cells and 1 empty column, (1 + 2) * 2 = 6 cards: park 2
 * in the cells, 3 in the empty column (with its own 2 through the
 * cells), and so on ([capacity]).
 *
 * What it uses: the cards kit (gamekits/cards: the cards, Microsoft's
 * deals, a card drawn), Scene2d (the keys pressed). Not the puzzle kit:
 * nothing is pushed on a grid.
 *
 * Exercises: undo (the model is a value: keep the past ones, as
 * TinySokoban does); the moves to the foundations the game can make by
 * itself, the "safe" ones Windows makes (a card no other card can need
 * any more); a double click to send a card home; a solver (a search
 * over the positions, the free cells as a set) showing a solution, or
 * proving deal 11982 lost.
 *)
open Playground

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type card = Cards.card

(* a column is its cards from the last one (the one you can take) down *)
type game = {
  number : int;
  columns : card list array;
  cells : card option array;
  (* the rank on each foundation, by suit: clubs, diamonds, hearts,
   * spades; 0 when empty *)
  foundations : int array;
  moves : int;
}

type place = Column of int | Cell of int | Foundation of int

let suit_index (c : card) = match c.suit with Clubs -> 0 | Diamonds -> 1 | Hearts -> 2 | Spades -> 3

(* the 52 cards of deal [n], row by row onto the 8 columns *)
let new_game (n : int) : game =
  let columns = Array.make 8 [] in
  List.iteri (fun i c -> columns.(i mod 8) <- c :: columns.(i mod 8)) (Cards.deal n);
  { number = n; columns; cells = Array.make 4 None; foundations = Array.make 4 0; moves = 0 }

(* a red on a black one higher, or a black on a red *)
let goes_on (c : card) (onto : card) = onto.rank = c.rank + 1 && Cards.red c <> Cards.red onto

(* the first [k] cards of a column, if they are a run *)
let rec run k (cards : card list) =
  match (k, cards) with
  | 1, c :: _ -> Some [ c ]
  | k, c :: (next :: _ as rest) when k > 1 && goes_on c next -> Option.map (fun r -> c :: r) (run (k - 1) rest)
  | _ -> None

(* how many cards can move at once onto [dest] (see the header) *)
let capacity (g : game) (dest : place) : int =
  let free = Array.fold_left (fun n c -> if c = None then n + 1 else n) 0 g.cells in
  let empty = ref 0 in
  Array.iteri (fun i col -> if col = [] && dest <> Column i then incr empty) g.columns;
  (free + 1) * (1 lsl !empty)

(* the cards picked up at [from]: the first [k] of a column, or a free
 * cell's *)
let taken (g : game) (from : place) (k : int) : card list option =
  match from with
  | Column i -> run k g.columns.(i)
  | Cell i -> ( match g.cells.(i) with Some c when k = 1 -> Some [ c ] | _ -> None)
  | Foundation _ -> None

(* [move_cards g from k dest]: the game after moving [k] cards from
 * [from] to [dest], if the rules allow it *)
let move_cards (g : game) (from : place) (k : int) (dest : place) : game option =
  match taken g from k with
  | None -> None
  | Some cards ->
      let bottom = List.nth cards (k - 1) in
      let ok =
        from <> dest
        &&
        match dest with
        | Column j -> (
            k <= capacity g dest && match g.columns.(j) with [] -> true | top :: _ -> goes_on bottom top)
        | Cell j -> k = 1 && g.cells.(j) = None
        | Foundation j -> k = 1 && suit_index bottom = j && g.foundations.(j) = bottom.rank - 1
      in
      if not ok then None
      else begin
        let columns = Array.copy g.columns and cells = Array.copy g.cells and foundations = Array.copy g.foundations in
        (match from with
        | Column i -> columns.(i) <- List.filteri (fun n _ -> n >= k) columns.(i)
        | Cell i -> cells.(i) <- None
        | Foundation _ -> ());
        (match dest with
        | Column j -> columns.(j) <- cards @ columns.(j)
        | Cell j -> cells.(j) <- Some bottom
        | Foundation j -> foundations.(j) <- bottom.rank);
        Some { g with columns; cells; foundations; moves = g.moves + 1 }
      end

let won (g : game) = Array.for_all (fun r -> r = 13) g.foundations

(*****************************************************************************)
(* Where things are on the screen *)
(*****************************************************************************)

let column_x i = -420. +. (120. *. float_of_int i)
let top_y = 380.
let columns_y = 200.

(* the place under the mouse, and for a column how many cards from its
 * last one the click picked (0 below them: the column itself) *)
let at (g : game) (mx, my) : (place * int) option =
  let top = List.init 4 (fun i -> (Cell i, column_x i)) @ List.init 4 (fun i -> (Foundation i, column_x (4 + i))) in
  match List.find_opt (fun (_, x) -> Cards.under (x, top_y) (mx, my)) top with
  | Some (p, _) -> Some (p, 1)
  | None ->
      List.init 8 Fun.id
      |> List.find_opt (fun i -> Float.abs (mx -. column_x i) <= Cards.width /. 2. && my < top_y -. (Cards.height /. 2.))
      |> Option.map (fun i ->
             let n = List.length g.columns.(i) in
             (* from the last card up, the first whose face is under *)
             let rec find k =
               if k > n then 0
               else if Cards.under (column_x i, columns_y -. (Cards.fanned *. float_of_int (n - k))) (mx, my) then k
               else find (k + 1)
             in
             (Column i, find 1))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = { g : game; picked : (place * int) option }
type model = play Scene2d.t

let deal_of_flags (computer : computer) =
  match List.assoc_opt "deal" computer.flags with Some n -> ( try int_of_string n with _ -> 1) | None -> 1

let initial_model : model = Scene2d.start { g = new_game 1; picked = None }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let p = s.scene in
  (* the flag, read at the first frame (the flags are the computer's) *)
  let p = if s.frames = 1 && p.g.moves = 0 then { g = new_game (deal_of_flags computer); picked = None } else p in
  let p =
    if Scene2d.pressed (fun k -> k.keys |> Set_.mem "n") s then { g = new_game (p.g.number + 1); picked = None }
    else if not computer.mouse.mclick then p
    else
      match (p.picked, at p.g (computer.mouse.mx, computer.mouse.my)) with
      | None, Some ((Column _ | Cell _) as from, k) when k > 0 && taken p.g from k <> None -> { p with picked = Some (from, k) }
      | Some (from, k), Some (dest, _) -> (
          match move_cards p.g from k dest with Some g -> { g; picked = None } | None -> { p with picked = None })
      | _ -> { p with picked = None }
  in
  { s with scene = p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let felt = rgb 30 120 55
let glow = rectangle (rgb 250 220 60) (Cards.width +. 10.) (Cards.height +. 10.)

let view (_computer : computer) (s : model) : shape list =
  let p = s.scene and g = s.scene.g in
  let picked place n = match p.picked with Some (pl, k) -> pl = place && n < k | None -> false in
  let at_top place x card_opt =
    move x top_y
      (group
         ((if picked place 0 then [ glow ] else [])
         @ [ (match card_opt with Some c -> Cards.face c | None -> Cards.slot) ]))
  in
  let top_row =
    List.init 4 (fun i -> at_top (Cell i) (column_x i) g.cells.(i))
    @ List.init 4 (fun i ->
          let r = g.foundations.(i) in
          let suit = [| Cards.Clubs; Diamonds; Hearts; Spades |].(i) in
          if r = 0 then
            move (column_x (4 + i)) top_y (group [ Cards.slot; Cards.suit_shape ~color:(rgb 20 90 40) suit 40. ])
          else at_top (Foundation i) (column_x (4 + i)) (Some { Cards.rank = r; suit }))
  in
  let columns =
    List.init 8 (fun i ->
        let col = List.rev g.columns.(i) and n = List.length g.columns.(i) in
        move (column_x i) columns_y Cards.slot
        :: List.mapi
             (fun j c ->
               let from_last = n - 1 - j in
               move (column_x i)
                 (columns_y -. (Cards.fanned *. float_of_int j))
                 (group ((if picked (Column i) from_last then [ glow ] else []) @ [ Cards.face c ])))
             col)
    |> List.concat
  in
  let line =
    if won g then Printf.sprintf "DEAL %d WON IN %d MOVES    n: the next deal" g.number g.moves
    else Printf.sprintf "DEAL %d    moves %d    n: the next deal" g.number g.moves
  in
  [ rectangle felt 1000. 1000. ] @ top_row @ columns @ [ move 0. (-470.) (words white line) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = game view update initial_model
let main = Playground_platform.run_app app
