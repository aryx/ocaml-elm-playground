(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tty.mli *)

let contains (s : string) (sub : string) : bool =
  let n = String.length sub in
  let rec at i = i + n <= String.length s && (String.sub s i n = sub || at (i + 1)) in
  at 0

(* the guesses halving [lo..hi] makes, reading the answers off the
   transcript as a player would: the game is played by a program *)
let halve (limit : int) : string =
  (* each run replays from the start with one more answer, until the
     game says it's over *)
  let rec play answers lo hi =
    let out = Teletype.run ~seed:3 Tty_guess.program answers in
    if contains out "THAT'S IT" then out
    else
      let lo, hi =
        if String.ends_with ~suffix:"TOO LOW.\nYOUR GUESS? " out then (int_of_string (List.nth answers (List.length answers - 1)) + 1, hi)
        else if String.ends_with ~suffix:"TOO HIGH.\nYOUR GUESS? " out then (lo, int_of_string (List.nth answers (List.length answers - 1)) - 1)
        else (lo, hi)
      in
      play (answers @ [ string_of_int ((lo + hi) / 2) ]) lo hi
  in
  play [ string_of_int limit ] 1 limit

let tests =
  Testo.categorize "Tty"
    [
      Testo.create "Guess: the bound halving never exceeds" (fun () ->
          Alcotest.(check (list int)) "1, 2, 7, 10" [ 1; 2; 7; 10 ] (List.map Tty_guess.best [ 1; 2; 100; 1000 ]));
      Testo.create "Guess: played by halving, always within the bound" (fun () ->
          let out = halve 100 in
          Alcotest.(check bool) "good" true (contains out "GOOD: HALVING WOULDN'T HAVE DONE BETTER."));
      Testo.create "Guess: not a number, asked again" (fun () ->
          let out = Teletype.run Tty_guess.program [ "lots" ] in
          Alcotest.(check bool) "asked again" true (String.ends_with ~suffix:"A NUMBER, PLEASE.\nWHAT LIMIT DO YOU WANT? " out));
      Testo.create "Hangman: the word's letters, won; six others, lost" (fun () ->
          let letters w = List.of_seq (String.to_seq w) |> List.map (String.make 1) in
          (* the seed's word: the first of the list whose letters win *)
          let wins w = contains (Teletype.run Tty_hangman.program (letters w)) "YOU FOUND THE WORD!" in
          let word = List.find wins (Array.to_list Tty_hangman.words) in
          let absent = List.filter (fun l -> not (String.contains word l.[0])) (letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") in
          let lost = Teletype.run Tty_hangman.program ("1" :: List.hd absent :: List.filteri (fun i _ -> i < 6) absent) in
          Alcotest.(check bool) "a mistyped guess" true (contains lost "ONE LETTER, PLEASE.");
          Alcotest.(check bool) "a letter twice" true (contains lost "YOU GUESSED THAT LETTER BEFORE!");
          Alcotest.(check bool) "lost" true (contains lost ("SORRY, YOU LOSE. THE WORD WAS " ^ word ^ ".")));
    ]
