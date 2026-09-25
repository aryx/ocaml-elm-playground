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

(* See Tty_hangman.mli *)

let words =
  [| "APPLE"; "BRIDGE"; "CANDLE"; "DRAGON"; "ELEPHANT"; "FOREST"; "GARDEN"; "HAMMER"; "ISLAND"; "JUNGLE";
     "KETTLE"; "LANTERN"; "MONKEY"; "NAPKIN"; "ORANGE"; "PENCIL"; "QUARTER"; "RABBIT"; "SADDLE"; "TURTLE" |]

(* the man, a part per miss: head, body, arms, legs *)
let gallows (misses : int) : string =
  let part n s = if misses >= n then s else "" in
  String.concat "\n"
    [ "  +---+";
      "  |   |";
      "  |   " ^ part 1 "O";
      "  |  " ^ (if misses >= 4 then "/|\\" else if misses >= 3 then "/|" else if misses >= 2 then " |" else "");
      "  |  " ^ part 5 "/" ^ part 6 " \\";
      "  |";
      "=====";
      "" ]

let shown (word : string) (found : char list) : string =
  String.concat " " (List.map (fun c -> if List.mem c found then String.make 1 c else "-") (List.of_seq (String.to_seq word)))

(* the game's state is the arguments, as a loop's variables would be
   in BASIC *)
let rec turn (word : string) (found : char list) (misses : int) : unit talk =
  if String.for_all (fun c -> List.mem c found) word then print ("\n" ^ shown word found ^ "\nYOU FOUND THE WORD!\n")
  else if misses = 6 then print (gallows 6 ^ "SORRY, YOU LOSE. THE WORD WAS " ^ word ^ ".\n")
  else
    let* () = print ("\n" ^ gallows misses ^ shown word found ^ "\n") in
    let* guess = ask "WHAT IS YOUR GUESS? " in
    let guess = String.uppercase_ascii (String.trim guess) in
    if String.length guess <> 1 || guess.[0] < 'A' || guess.[0] > 'Z' then
      let* () = print "ONE LETTER, PLEASE.\n" in
      turn word found misses
    else
      let g = guess.[0] in
      if List.mem g found then
        let* () = print "YOU GUESSED THAT LETTER BEFORE!\n" in
        turn word found misses
      else if String.contains word g then turn word (g :: found) misses
      else
        let* () = print "SORRY, THAT LETTER ISN'T IN THE WORD.\n" in
        (* a miss is remembered in [found] too, so it can't be guessed
           twice, without showing: the word doesn't contain it *)
        turn word (g :: found) (misses + 1)

let rec hangman () : unit talk =
  let* i = random (Array.length words) in
  let* () = turn words.(i) [] 0 in
  let* again = ask "\nWANT ANOTHER WORD? " in
  if String.length again > 0 && Char.uppercase_ascii again.[0] = 'Y' then hangman () else print "BYE!\n"

let program =
  let* () = print "HANGMAN\n\n" in
  hangman ()
