(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Abc and Music's tunes: the notation's pieces, one by one *)

let t = Testo.create

(* a one-voice tune's (notes, length) *)
let notes (body : string) : (int list * float) list =
  match Abc.parse ("X:1\nL:1/8\nQ:1/4=120\n" ^ body) with
  | Ok { voices = [ events ]; _ } -> List.map (fun (e : Abc.event) -> (e.notes, e.length)) events
  | Ok _ -> Alcotest.fail "one voice expected"
  | Error e -> Alcotest.fail e

let check name expected body = Alcotest.(check (list (pair (list int) (float 1e-9)))) name expected (notes body)

let test_notation () =
  check "Abc.mli's example: F# (the key), F (=), C5 halved" [ ([ 66 ], 0.5); ([ 65 ], 0.5); ([ 72 ], 0.125) ] "K:G\nF2 =F2 c/2";
  check "an accidental lasts until the bar" [ ([ 65 ], 0.25); ([ 65 ], 0.25); ([ 66 ], 0.25) ] "K:G\n=F F | F";
  check "octaves" [ ([ 48 ], 0.25); ([ 72 ], 0.25); ([ 84 ], 0.25) ] "K:C\nC, c c'";
  check "sharps and flats" [ ([ 61 ], 0.25); ([ 70 ], 0.25) ] "K:C\n^C _B";
  check "a chord" [ ([ 60; 64; 67 ], 0.5) ] "K:C\n[CEG]2";
  check "a triplet: three in the time of two" [ ([ 60 ], 0.25 *. 2. /. 3.); ([ 62 ], 0.25 *. 2. /. 3.); ([ 64 ], 0.25 *. 2. /. 3.); ([ 65 ], 0.25) ]
    "K:C\n(3CDE F";
  check "a dotted pair" [ ([ 60 ], 0.375); ([ 62 ], 0.125) ] "K:C\nC>D";
  check "a rest" [ ([], 0.5); ([ 60 ], 0.25) ] "K:C\nz2 C";
  check "E minor: one sharp, F#" [ ([ 66 ], 0.25) ] "K:Em\nF";
  check "decorations, chord names, comments skipped" [ ([ 60 ], 0.25); ([ 62 ], 0.25) ] "K:C\n\"Am\"!trill!C ~D % a comment E"

let frere_jacques =
  {|X:1
T:Frere Jacques (a round, traditional)
L:1/8
Q:1/4=120
K:C
V:1
C2 D2 E2 C2 | C2 D2 E2 C2 | E2 F2 G4 | E2 F2 G4 |
GA GF E2 C2 | GA GF E2 C2 | C2 G,2 C4 | C2 G,2 C4 |
V:2
z8 | z8 | C2 D2 E2 C2 | C2 D2 E2 C2 | E2 F2 G4 | E2 F2 G4 |
GA GF E2 C2 | GA GF E2 C2 |
|}

let test_tune () =
  match Abc.parse frere_jacques with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      Alcotest.(check string) "the title" "Frere Jacques (a round, traditional)" tune.title;
      Alcotest.(check int) "two voices" 2 (List.length tune.voices);
      (* 8 bars of 4 quarters at 120 a minute: 16 s *)
      Alcotest.(check (float 1e-9)) "16 seconds" 16. (Abc.duration tune);
      let second = List.nth tune.voices 1 in
      Alcotest.(check (float 1e-9)) "the round's second voice comes in after 2 bars" 4. (List.find (fun (e : Abc.event) -> e.notes <> []) second).start;
      let s = Music.to_sound tune in
      Alcotest.(check (float 1e-9)) "the sound lasts as long" 16. (Synth.duration s)

let tests = Testo.categorize "ABC" [ t "the notation, piece by piece" test_notation; t "Frere Jacques, a round in two voices" test_tune ]
