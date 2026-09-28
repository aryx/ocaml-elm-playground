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
    let out = Talk.run ~seed:3 Tty_guess.program answers in
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
          let out = Talk.run Tty_guess.program [ "lots" ] in
          Alcotest.(check bool) "asked again" true (String.ends_with ~suffix:"A NUMBER, PLEASE.\nWHAT LIMIT DO YOU WANT? " out));
      Testo.create "Hangman: the word's letters, won; six others, lost" (fun () ->
          let letters w = List.of_seq (String.to_seq w) |> List.map (String.make 1) in
          (* the seed's word: the first of the list whose letters win *)
          let wins w = contains (Talk.run Tty_hangman.program (letters w)) "YOU FOUND THE WORD!" in
          let word = List.find wins (Array.to_list Tty_hangman.words) in
          let absent = List.filter (fun l -> not (String.contains word l.[0])) (letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") in
          let lost = Talk.run Tty_hangman.program ("1" :: List.hd absent :: List.filteri (fun i _ -> i < 6) absent) in
          Alcotest.(check bool) "a mistyped guess" true (contains lost "ONE LETTER, PLEASE.");
          Alcotest.(check bool) "a letter twice" true (contains lost "YOU GUESSED THAT LETTER BEFORE!");
          Alcotest.(check bool) "lost" true (contains lost ("SORRY, YOU LOSE. THE WORD WAS " ^ word ^ ".")));
      Testo.create "Wumpus: the cave is the .mli's three rings, 30 tunnels both ways" (fun () ->
          let t = Tty_wumpus.tunnels in
          let joined a b = Array.mem b t.(a) in
          for r = 1 to 20 do
            Alcotest.(check int) (Printf.sprintf "room %d: three tunnels" r) 3 (Array.length t.(r));
            Array.iter (fun n -> Alcotest.(check bool) (Printf.sprintf "%d-%d both ways" r n) true (n <> r && joined n r)) t.(r)
          done;
          let ring l = List.iteri (fun i a -> let b = List.nth l ((i + 1) mod List.length l) in Alcotest.(check bool) (Printf.sprintf "%d-%d" a b) true (joined a b)) l in
          ring [ 1; 2; 3; 4; 5 ];
          ring [ 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 ];
          ring [ 16; 17; 18; 19; 20 ];
          List.iter (fun (a, b) -> Alcotest.(check bool) (Printf.sprintf "spoke %d-%d" a b) true (joined a b))
            [ (1, 8); (2, 10); (3, 12); (4, 14); (5, 6); (7, 17); (9, 18); (11, 19); (13, 20); (15, 16) ]);
      Testo.create "Wumpus: smelt next door, shot through one room, won" (fun () ->
          let cave = { Tty_wumpus.you = 1; wumpus = 2; pits = [ 19; 20 ]; bats = [ 17; 18 ]; arrows = 5 } in
          let game = Talk.( let* ) (Tty_wumpus.play cave) (fun o -> Talk.print (if o = Tty_wumpus.Won then "=WON" else "=LOST")) in
          let out = Talk.run game [ "s"; "1"; "2" ] in
          Alcotest.(check bool) "smelt" true (contains out "I SMELL A WUMPUS!");
          Alcotest.(check bool) "tunnels" true (contains out "TUNNELS LEAD TO 2 5 8.");
          Alcotest.(check bool) "won" true (String.ends_with ~suffix:"AHA! YOU GOT THE WUMPUS!\n=WON" out));
      Testo.create "Wumpus: a draft, no tunnel to 7, then into the pit" (fun () ->
          let cave = { Tty_wumpus.you = 1; wumpus = 13; pits = [ 8; 20 ]; bats = [ 17; 18 ]; arrows = 5 } in
          let game = Talk.( let* ) (Tty_wumpus.play cave) (fun o -> Talk.print (if o = Tty_wumpus.Won then "=WON" else "=LOST")) in
          let out = Talk.run game [ "m"; "7"; "8" ] in
          Alcotest.(check bool) "draft" true (contains out "I FEEL A DRAFT.");
          Alcotest.(check bool) "no tunnel" true (contains out "NO TUNNEL GOES THERE.");
          Alcotest.(check bool) "lost" true (String.ends_with ~suffix:"YOU FELL IN A PIT.\n=LOST" out));
      Testo.create "Wumpus: an arrow can't go back the way it came" (fun () ->
          let cave = { Tty_wumpus.you = 1; wumpus = 13; pits = [ 19; 20 ]; bats = [ 17; 18 ]; arrows = 5 } in
          let out = Talk.run (Tty_wumpus.play cave) [ "s"; "3"; "2"; "1"; "2" ] in
          Alcotest.(check bool) "refused" true (contains out "ARROWS AREN'T THAT CROOKED"));
      Testo.create "Wumpus: a whole program from a seed, instructions and all" (fun () ->
          let out = Talk.run ~seed:5 Tty_wumpus.program [ "y" ] in
          Alcotest.(check bool) "instructions" true (contains out "DODECAHEDRON");
          Alcotest.(check bool) "a first turn" true (String.ends_with ~suffix:"SHOOT OR MOVE (S-M)? " out));
    ]
