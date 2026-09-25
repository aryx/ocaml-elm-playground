(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Teletype

(* See Tty_guess.mli *)

(* how many times [limit] can be halved before 1 is left, plus one *)
let best (limit : int) : int =
  let rec halvings n = if n <= 1 then 0 else 1 + halvings (n / 2) in
  halvings limit + 1

(* a number, asked again until it is one *)
let rec number (question : string) : int talk =
  let* answer = ask question in
  match int_of_string_opt (String.trim answer) with
  | Some n when n > 0 -> return n
  | _ ->
      let* () = print "A NUMBER, PLEASE.\n" in
      number question

let rec guess (secret : int) (limit : int) (tries : int) : unit talk =
  let* g = number "YOUR GUESS? " in
  if g < secret then
    let* () = print "TOO LOW.\n" in
    guess secret limit (tries + 1)
  else if g > secret then
    let* () = print "TOO HIGH.\n" in
    guess secret limit (tries + 1)
  else
    print
      (Printf.sprintf "THAT'S IT! YOU GOT IT IN %d %s.\n%s\n" tries
         (if tries = 1 then "TRY" else "TRIES")
         (if tries <= best limit then "GOOD: HALVING WOULDN'T HAVE DONE BETTER."
          else Printf.sprintf "HALVING WHAT IS LEFT NEVER TAKES MORE THAN %d." (best limit)))

let rec game (limit : int) : unit talk =
  let* n = random limit in
  let* () = print (Printf.sprintf "\nI'M THINKING OF A NUMBER FROM 1 TO %d.\n" limit) in
  let* () = guess (n + 1) limit 1 in
  let* again = ask "\nPLAY AGAIN? " in
  if String.length again > 0 && Char.uppercase_ascii again.[0] = 'Y' then game limit else print "BYE!\n"

let program =
  let* () = print "GUESS THE NUMBER\n\n" in
  let* limit = number "WHAT LIMIT DO YOU WANT? " in
  game limit
