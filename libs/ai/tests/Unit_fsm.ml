(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_fsm.mli *)

type light = Green | Orange | Red

let lights : (light, unit) Fsm.machine =
  [ { from = Green; label = "change"; guard = Fsm.after 3; target = Orange };
    { from = Orange; label = "stop"; guard = Fsm.after 1; target = Red };
    { from = Red; label = "go"; guard = Fsm.after 2; target = Green } ]

(* Fsm.mli's: green 3 steps, orange 1, red 2 *)
let test_traffic_light () =
  let runs = List.rev (snd (List.fold_left (fun (r, acc) _ -> let r = Fsm.step lights () r in (r, r :: acc)) (Fsm.start Green, []) (List.init 6 Fun.id))) in
  let states = List.map (fun (r : light Fsm.run) -> r.state) runs in
  Alcotest.(check bool) "green, green, orange, red, red, green" true (states = [ Green; Green; Orange; Red; Red; Green ]);
  Alcotest.(check (list int)) "the steps spent" [ 1; 2; 0; 0; 1; 0 ] (List.map (fun (r : light Fsm.run) -> r.since) runs);
  Alcotest.(check (list (option string))) "what fired" [ None; None; Some "change"; Some "stop"; None; Some "go" ]
    (List.map (fun (r : light Fsm.run) -> r.fired) runs);
  Alcotest.(check bool) "its states, in order" true (Fsm.states lights = [ Green; Orange; Red ])

(* the first rule that holds wins: the order of the rules is priority *)
let test_priority () =
  let m : (light, unit) Fsm.machine =
    [ { from = Green; label = "first"; guard = (fun () _ -> true); target = Red };
      { from = Green; label = "second"; guard = (fun () _ -> true); target = Orange } ]
  in
  Alcotest.(check bool) "the first" true ((Fsm.step m () (Fsm.start Green)).state = Red)

let tests = [ Testo.create "Fsm: a traffic light" test_traffic_light; Testo.create "Fsm: priority" test_priority ]
