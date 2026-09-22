(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Solitaire, the Klondike patience of Windows (Wes
 * Cherry, an intern at Microsoft, for Windows 3.0 in 1990): seven
 * columns dealt with their last card face up, the rest of the deck in
 * the stock, and the four foundations to build up by suit, ace to king.
 * A card goes onto a column's last card if it is one lower and of the
 * other color, a king into an empty column; a column's face-down card
 * turns over when nothing is left on it.
 *
 *   click the stock   turn its top card onto the waste (empty: the
 *                     waste goes back into the stock)
 *   click a card      pick it up, with the cards on it
 *   click a place     put them there: a column, a foundation
 *   n                 the next deal        deal=N (a flag): deal N
 *
 * Windows shipped it to teach the mouse: pointing, clicking and
 * dragging were new to most of its users, and a game of patience made
 * them practise all afternoon (here it is click and click; dragging is
 * an exercise). The game itself is older than computers -- patience
 * games are from the 18th century, and this one is named after the
 * Klondike gold rush of 1896 -- and unlike FreeCell (TinyFreeCell) it
 * hides most of its cards, so luck decides many deals before you play
 * a card. (Names and dates from memory, to check.)
 *
 * The deals are Microsoft's FreeCell deals (Cards.deal), dealt the
 * Klondike way -- row by row, one card fewer each row, the first card
 * of each row face up -- so that a deal has a number, and deal 1 is
 * the same on every computer.
 *
 * What it uses: the cards kit (gamekits/cards: the cards, Microsoft's
 * deals, a card drawn), Scene2d (the keys pressed).
 *
 * Exercises: drag and drop (computer.mouse's mdown and its motion: the
 * card follows the pointer, and lands where it is let go); undo; turning
 * three cards at a time from the stock, as the harder game does;
 * Windows' score (5 points a card turned over, 10 a card home) and the
 * cards bouncing across the screen when the game is won.
 *)
open Playground

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type card = Cards.card

(* the lists begin with the top card: the one you can take *)
type game = {
  number : int;
  stock : card list;
  waste : card list;
  (* a column's cards, each face up (true) or down *)
  columns : (card * bool) list array;
  (* the rank on each foundation, by suit: clubs, diamonds, hearts,
   * spades; 0 when empty *)
  foundations : int array;
  moves : int;
}

type place = Stock | Waste | Column of int | Foundation of int

let suit_index (c : card) = match c.suit with Clubs -> 0 | Diamonds -> 1 | Hearts -> 2 | Spades -> 3

(* deal [n] the Klondike way: row r gives a card to columns r to 6, the
 * one on column r face up; the other 24 cards are the stock *)
let new_game (n : int) : game =
  let columns = Array.make 7 [] in
  let rest = ref (Cards.deal n) in
  for r = 0 to 6 do
    for c = r to 6 do
      match !rest with
      | card :: tl ->
          columns.(c) <- (card, c = r) :: columns.(c);
          rest := tl
      | [] -> ()
    done
  done;
  { number = n; stock = !rest; waste = []; columns; foundations = Array.make 4 0; moves = 0 }

let goes_on (c : card) (onto : card) = onto.rank = c.rank + 1 && Cards.red c <> Cards.red onto

(* the first [k] cards of a column, if they are all face up and a run *)
let rec run k (cards : (card * bool) list) =
  match (k, cards) with
  | 1, (c, true) :: _ -> Some [ c ]
  | k, (c, true) :: ((next, true) :: _ as rest) when k > 1 && goes_on c next ->
      Option.map (fun r -> c :: r) (run (k - 1) rest)
  | _ -> None

(* the stock clicked: its top card turned onto the waste, or, empty,
 * the waste turned back over into it *)
let turn (g : game) : game =
  match g.stock with
  | c :: rest -> { g with stock = rest; waste = c :: g.waste; moves = g.moves + 1 }
  | [] when g.waste <> [] -> { g with stock = List.rev g.waste; waste = []; moves = g.moves + 1 }
  | [] -> g

let taken (g : game) (from : place) (k : int) : card list option =
  match from with
  | Waste -> ( match g.waste with c :: _ when k = 1 -> Some [ c ] | _ -> None)
  | Column i -> run k g.columns.(i)
  | Foundation i when k = 1 && g.foundations.(i) > 0 ->
      Some [ { Cards.rank = g.foundations.(i); suit = [| Cards.Clubs; Diamonds; Hearts; Spades |].(i) } ]
  | _ -> None

(* a column's last card turns face up once it is uncovered *)
let uncover = function (c, false) :: rest -> (c, true) :: rest | col -> col

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
        | Column j -> ( match g.columns.(j) with [] -> bottom.rank = 13 | (top, _) :: _ -> goes_on bottom top)
        | Foundation j -> k = 1 && suit_index bottom = j && g.foundations.(j) = bottom.rank - 1
        | Stock | Waste -> false
      in
      if not ok then None
      else begin
        let columns = Array.copy g.columns and foundations = Array.copy g.foundations in
        let waste = if from = Waste then List.tl g.waste else g.waste in
        (match from with
        | Column i -> columns.(i) <- uncover (List.filteri (fun n _ -> n >= k) columns.(i))
        | Foundation i -> foundations.(i) <- foundations.(i) - 1
        | Stock | Waste -> ());
        (match dest with
        | Column j -> columns.(j) <- List.map (fun c -> (c, true)) cards @ columns.(j)
        | Foundation j -> foundations.(j) <- bottom.rank
        | Stock | Waste -> ());
        Some { g with waste; columns; foundations; moves = g.moves + 1 }
      end

let won (g : game) = Array.for_all (fun r -> r = 13) g.foundations

(*****************************************************************************)
(* Where things are on the screen *)
(*****************************************************************************)

let column_x i = -360. +. (120. *. float_of_int i)
let top_y = 380.
let columns_y = 200.

(* the cards of a column from the first dealt, each with its height on
 * the screen: face-down cards stacked tight, face-up ones fanned *)
let placed (col : (card * bool) list) : ((card * bool) * float) list =
  let _, acc =
    List.fold_left
      (fun (y, acc) ((_, up) as c) -> (y -. if up then Cards.fanned else Cards.stacked), (c, y) :: acc)
      (columns_y, []) (List.rev col)
  in
  List.rev acc

(* the place under the mouse, and for a column how many cards from its
 * last one the click picked (0 below them: the column itself) *)
let at (g : game) (mx, my) : (place * int) option =
  let top =
    [ (Stock, column_x 0); (Waste, column_x 1) ] @ List.init 4 (fun i -> (Foundation i, column_x (3 + i)))
  in
  match List.find_opt (fun (_, x) -> Cards.under (x, top_y) (mx, my)) top with
  | Some (p, _) -> Some (p, 1)
  | None ->
      List.init 7 Fun.id
      |> List.find_opt (fun i -> Float.abs (mx -. column_x i) <= Cards.width /. 2. && my < top_y -. (Cards.height /. 2.))
      |> Option.map (fun i ->
             let ys = List.rev (placed g.columns.(i)) in
             (* from the last card up, the first whose face is under *)
             let rec find k = function
               | [] -> 0
               | (_, y) :: rest -> if Cards.under (column_x i, y) (mx, my) then k else find (k + 1) rest
             in
             (Column i, find 1 ys))

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
    if Scene2d.pressed (fun k -> Set_.mem "n" k.keys) s then { g = new_game (p.g.number + 1); picked = None }
    else if not computer.mouse.mclick then p
    else
      match (p.picked, at p.g (computer.mouse.mx, computer.mouse.my)) with
      | None, Some (Stock, _) -> { g = turn p.g; picked = None }
      | None, Some (from, k) when k > 0 && taken p.g from k <> None -> { p with picked = Some (from, k) }
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
  let top place x shape = move x top_y (group ((if picked place 0 then [ glow ] else []) @ [ shape ])) in
  let top_row =
    [ top Stock (column_x 0) (if g.stock = [] then Cards.slot else Cards.back);
      top Waste (column_x 1) (match g.waste with c :: _ -> Cards.face c | [] -> Cards.slot) ]
    @ List.init 4 (fun i ->
          let r = g.foundations.(i) in
          let suit = [| Cards.Clubs; Diamonds; Hearts; Spades |].(i) in
          if r = 0 then
            move (column_x (3 + i)) top_y (group [ Cards.slot; Cards.suit_shape ~color:(rgb 20 90 40) suit 40. ])
          else top (Foundation i) (column_x (3 + i)) (Cards.face { rank = r; suit }))
  in
  let columns =
    List.init 7 (fun i ->
        let n = List.length g.columns.(i) in
        move (column_x i) columns_y Cards.slot
        :: List.mapi
             (fun j ((c, up), y) ->
               move (column_x i) y
                 (group
                    ((if picked (Column i) (n - 1 - j) then [ glow ] else [])
                    @ [ (if up then Cards.face c else Cards.back) ])))
             (placed g.columns.(i)))
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
