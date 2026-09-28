(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Doremi: the .mli's example, Au clair de la lune, voices *)

let t = Testo.create
let events = Alcotest.(list (pair (list int) (float 1e-9)))

let voices (text : string) : (int list * float) list list =
  match Doremi.parse text with
  | Ok tune -> List.map (List.map (fun (e : Abc.event) -> (e.notes, e.length))) tune.voices
  | Error e -> Alcotest.fail e

let test_notation () =
  Alcotest.check (Alcotest.list events) "Doremi.mli's example"
    [ [ ([ 60 ], 0.5); ([ 62 ], 0.5); ([ 64 ], 1.); ([ 72 ], 0.5); ([ 71 ], 0.25) ] ]
    (voices "tempo 120  do re mi:2 | do5 si4:1/2");
  Alcotest.check (Alcotest.list events) "sharps, flats, ré, ut, sol, a rest"
    [ [ ([ 61 ], 0.5); ([ 70 ], 0.5); ([ 62 ], 0.5); ([ 60 ], 0.5); ([ 67 ], 0.5); ([], 1.) ] ]
    (voices "do# sib ré ut sol -:2");
  Alcotest.check (Alcotest.list events) "the octave stays, tempo 60: a second a beat"
    [ [ ([ 72 ], 1.); ([ 74 ], 1.) ] ]
    (voices "tempo 60\ndo5 re % the octave 5 goes on");
  Alcotest.(check int) "two voices" 2 (List.length (voices "do re mi\nvoix\ndo3 sol3"));
  Alcotest.(check bool) "not a note" true (Result.is_error (Doremi.parse "do ze mi"))

(* Au clair de la lune (French, traditional): its first line, 16 beats
 * (4 bars of 4) at 120 a minute: 8 s *)
let test_lune () =
  match Doremi.parse "do do do re mi:2 re:2 | do mi re re do:4" with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      Alcotest.(check (float 1e-9)) "8 seconds" 8. (Abc.duration tune);
      Alcotest.(check (list int)) "do do do re mi re do mi re re do"
        [ 60; 60; 60; 62; 64; 62; 60; 64; 62; 62; 60 ]
        (List.concat_map (fun (e : Abc.event) -> e.notes) (List.hd tune.voices))

let tests = Testo.categorize "Doremi" [ t "the notation" test_notation; t "Au clair de la lune" test_lune ]
