(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Karel: the worked examples of Karel.mli *)

open Karel

let t = Testo.create

(* the world of Karel.mli's header *)
let example = [ ". . . ."; "  -    "; ".|> . 2" ]

let test_world () =
  let w = world example in
  Alcotest.(check (triple int int bool)) "Karel on 2nd Avenue, 1st Street, facing east" (2, 1, true)
    (let a, s, d = karel w in
     (a, s, d = East));
  Alcotest.(check int) "two beepers on 4th Avenue" 2 (beepers w 4 1);
  Alcotest.(check (list string)) "and back to strings" example (to_strings w)

(* A wall behind (west), a wall on its left (north), and on its right
 * (south) the city's edge: only its front is clear; moving into a wall
 * is an error. A condition is
 * checked by putting down the bag's one beeper if it holds. *)
let holds (c : condition) : bool = beepers (current (execute (world ~bag:1 example) [ if_ c [ put_beeper ] ])) 2 1 = 1

let test_conditions () =
  Alcotest.(check (list bool)) "front, left, right; facing east, north; a beeper here"
    [ true; false; false; true; false; false ]
    (List.map holds [ front_is_clear; left_is_clear; right_is_clear; facing East; facing North; next_to_a_beeper ]);
  Alcotest.(check bool) "not_" true (holds (not_ left_is_clear));
  let into_wall program = match status (execute (world example) program) with Error _ -> true | _ -> false in
  Alcotest.(check bool) "into the wall behind" true (into_wall [ turn_left; turn_left; move ]);
  Alcotest.(check bool) "into the wall on its left" true (into_wall [ turn_left; move ]);
  Alcotest.(check bool) "a beeper from an empty bag" true (into_wall [ put_beeper ])

(* the header's turn_right, taught *)
let turn_right = block [ turn_left; turn_left; turn_left ]

let test_turn_right () =
  let r = execute (world example) [ turn_right ] in
  let _, _, d = karel (current r) in
  Alcotest.(check bool) "facing south" true (d = South);
  Alcotest.(check int) "three primitives" 3 (steps r)

(* to the beepers, picking them up, until there are none *)
let test_while () =
  let r = execute (world example) [ while_ front_is_clear [ move ]; while_ next_to_a_beeper [ pick_beeper ]; turn_off ] in
  Alcotest.(check bool) "finished" true (status r = Finished);
  Alcotest.(check (pair int int)) "none left, two in the bag" (0, 2) (beepers_left (current r), bag (current r))

(* an instruction by name, recursive: to the wall *)
let test_call () =
  let definitions = [ ("to-the-wall", [ if_ front_is_clear [ move; call "to-the-wall" ] ]) ] in
  let r = execute ~definitions (world example) [ call "to-the-wall" ] in
  let a, _, _ = karel (current r) in
  Alcotest.(check int) "at 4th Avenue" 4 a;
  let r = execute (world example) [ call "nowhere" ] in
  Alcotest.(check bool) "an unknown one: an error" true (match status r with Error _ -> true | _ -> false)

(* a loop that never ends ends, with an error *)
let test_forever () =
  let spin = execute (world example) [ while_ front_is_clear [ turn_left; turn_left; turn_left; turn_left ] ] in
  Alcotest.(check bool) "too many steps" true (match status spin with Error _ -> true | _ -> false);
  let nothing = execute (world example) [ while_ front_is_clear [] ] in
  Alcotest.(check bool) "an empty body: too much work" true (match status nothing with Error _ -> true | _ -> false)

let tests =
  Testo.categorize "Karel"
    [ t "the header's world" test_world; t "walls" test_conditions; t "turn_right, taught" test_turn_right;
      t "while" test_while; t "call, recursive" test_call; t "loops that never end" test_forever ]
