(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Linebreak.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type word = { text : string; width : float }
type params = { measure : float; space : float; stretch : float; shrink : float }
type line = { first : int; last : int; ratio : float; demerits : float }

(* TeX's "infinitely bad": a line no amount of glue can set properly *)
let infinitely_bad = 10000.

(*****************************************************************************)
(* Scoring one line *)
(*****************************************************************************)

(* how far the spaces of words i..j must give to fill the measure *)
let ratio p (words : word array) i j ~last =
  let w = ref 0. in
  for k = i to j do
    w := !w +. words.(k).width
  done;
  let spaces = float_of_int (j - i) in
  let natural = !w +. (spaces *. p.space) in
  (* the last line is set at its natural width: its glue stretches as
   * far as it likes (TeX's \parfillskip) *)
  if last && natural <= p.measure then 0.
  else if natural < p.measure then
    if spaces = 0. then infinity else (p.measure -. natural) /. (spaces *. p.stretch)
  else if natural > p.measure then
    if spaces = 0. then neg_infinity else (p.measure -. natural) /. (spaces *. p.shrink)
  else 0.

let badness_of r =
  if Float.abs r = infinity then infinitely_bad else min infinitely_bad (100. *. (Float.abs r ** 3.))

(* the 10 is TeX's \linepenalty: even a perfect line costs something,
 * so that a paragraph with fewer lines wins a tie *)
let demerits_of r = (10. +. badness_of r) ** 2.

let line_of p words i j ~last =
  let r = ratio p words i j ~last in
  { first = i; last = j; ratio = r; demerits = demerits_of r }

(*****************************************************************************)
(* Greedy: the best for this line, blind to the next *)
(*****************************************************************************)

let greedy p (words : word array) =
  let n = Array.length words in
  let fits i j =
    let w = ref 0. in
    for k = i to j do
      w := !w +. words.(k).width
    done;
    !w +. (float_of_int (j - i) *. p.space) <= p.measure
  in
  let rec go start acc =
    if start >= n then List.rev acc
    else
      (* take the next word while the line still fits at its natural
       * spacing, and not a word more *)
      let rec extend j = if j + 1 < n && fits start (j + 1) then extend (j + 1) else j in
      let j = extend start in
      go (j + 1) (line_of p words start j ~last:(j = n - 1) :: acc)
  in
  go 0 []

(*****************************************************************************)
(* Optimal: the whole paragraph, by dynamic programming *)
(*****************************************************************************)

(* best.(j): the least total demerits of a paragraph whose last line
 * ends just before word j; from.(j): where that last line began *)
let optimal p (words : word array) =
  let n = Array.length words in
  if n = 0 then []
  else begin
    let best = Array.make (n + 1) infinity in
    let from = Array.make (n + 1) 0 in
    best.(0) <- 0.;
    for j = 1 to n do
      let last = j = n in
      (* every place the line ending at word j-1 could have started,
       * nearest first: the line only gets longer as it starts
       * earlier, so once it is too long to shrink into the measure,
       * every earlier start is too *)
      let rec try_from i =
        if i >= 0 then begin
          let r = ratio p words i (j - 1) ~last in
          (* a single word is always allowed, even too wide -- there
           * is nothing else to do with it (TeX's overfull box) *)
          if r >= -1. || i = j - 1 then begin
            let cost = best.(i) +. demerits_of r in
            if cost < best.(j) then begin
              best.(j) <- cost;
              from.(j) <- i
            end;
            try_from (i - 1)
          end
        end
      in
      try_from (j - 1)
    done;
    (* and back from the end, along the choices that were kept *)
    let rec back j acc =
      if j = 0 then acc else
        let i = from.(j) in
        back i (line_of p words i (j - 1) ~last:(j = n) :: acc)
    in
    back n []
  end

(*****************************************************************************)
(* What a view needs *)
(*****************************************************************************)

let total lines = List.fold_left (fun acc l -> acc +. l.demerits) 0. lines

let spacing p l =
  if Float.abs l.ratio = infinity then p.space
  else if l.ratio >= 0. then p.space +. (l.ratio *. p.stretch)
  else p.space +. (l.ratio *. p.shrink)

let badness l = badness_of l.ratio
