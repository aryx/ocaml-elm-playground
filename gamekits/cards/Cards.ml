(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cards.mli *)

open Playground

type suit = Clubs | Diamonds | Hearts | Spades
type card = { rank : int; suit : suit }

let red c = c.suit = Hearts || c.suit = Diamonds
let suits = [| Clubs; Diamonds; Hearts; Spades |]
let of_index i = { rank = (i / 4) + 1; suit = suits.(i mod 4) }

let name c =
  String.make 1 "A23456789TJQK".[c.rank - 1]
  ^ match c.suit with Clubs -> "C" | Diamonds -> "D" | Hearts -> "H" | Spades -> "S"

let deck = List.init 52 of_index

(*****************************************************************************)
(* Microsoft's deals *)
(*****************************************************************************)

let deal (n : int) : card list =
  let state = ref n in
  let rand () =
    state := ((!state * 214013) + 2531011) land ((1 lsl 31) - 1);
    !state lsr 16
  in
  let cards = Array.init 52 Fun.id in
  let rec go left acc =
    if left = 0 then List.rev acc
    else begin
      let j = rand () mod left in
      let last = cards.(left - 1) in
      cards.(left - 1) <- cards.(j);
      cards.(j) <- last;
      go (left - 1) (of_index cards.(left - 1) :: acc)
    end
  in
  go 52 []

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let width = 100.
let height = 140.
let fanned = 32.
let stacked = 12.

let ink c = if red c then rgb 200 30 30 else black

let suit_shape ?color (s : suit) (size : number) : shape =
  let color =
    match color with Some c -> c | None -> if s = Hearts || s = Diamonds then rgb 200 30 30 else black
  in
  let r = size /. 4. in
  match s with
  | Diamonds -> rotate 45. (square color (size *. 0.7))
  | Hearts ->
      group
        [ move (-.r) (r *. 0.6) (circle color r);
          move r (r *. 0.6) (circle color r);
          move 0. (-.r *. 0.4) (rotate 180. (triangle color (size *. 0.62))) ]
  | Spades ->
      group
        [ move (-.r) (-.r *. 0.3) (circle color r);
          move r (-.r *. 0.3) (circle color r);
          move 0. (r *. 0.7) (triangle color (size *. 0.62));
          move 0. (-.r *. 1.4) (triangle color (size *. 0.3)) ]
  | Clubs ->
      group
        [ move 0. (r *. 0.9) (circle color r);
          move (-.r) (-.r *. 0.3) (circle color r);
          move r (-.r *. 0.3) (circle color r);
          move 0. (-.r *. 1.4) (triangle color (size *. 0.3)) ]

let rank_text c =
  match c.rank with 1 -> "A" | 10 -> "10" | 11 -> "J" | 12 -> "Q" | 13 -> "K" | r -> string_of_int r

(* a card's paper: a thin dark border round a white rectangle *)
let paper color = group [ rectangle (rgb 60 60 60) width height; rectangle color (width -. 4.) (height -. 4.) ]

let face (c : card) : shape =
  group
    [ paper white;
      move (-.width /. 2. +. 18.) (height /. 2. -. 16.) (scale 0.8 (words (ink c) (rank_text c)));
      move (-.width /. 2. +. 42.) (height /. 2. -. 15.) (suit_shape c.suit 16.);
      suit_shape c.suit 44. ]

let back =
  group
    [ paper (rgb 30 70 150);
      rectangle (rgb 60 110 200) (width -. 20.) (height -. 20.);
      rotate 45. (square (rgb 30 70 150) 30.) ]

let slot = group [ rectangle (rgb 20 90 40) width height; rectangle (rgb 30 120 55) (width -. 6.) (height -. 6.) ]

let under (x, y) (mx, my) = Float.abs (mx -. x) <= width /. 2. && Float.abs (my -. y) <= height /. 2.
