(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_decide.mli *)

type act = Flee | Attack | Patrol
type dog = { health : float; closeness : float (* of the intruder, 0 far to 1 at the gate *) }

let act = Alcotest.testable (fun ppf a -> Fmt.string ppf (match a with Flee -> "Flee" | Attack -> "Attack" | Patrol -> "Patrol")) ( = )

(* the rules' thresholds *)
let hurt d = d.health < 0.5
let intruder d = d.closeness > 0.5

(*****************************************************************************)
(* The three dogs *)
(*****************************************************************************)

(* as a behavior tree: its priorities, top to bottom *)
let tree : (dog, act) Behavior.t =
  Selector
    [ Sequence [ Condition ("hurt?", hurt); Action ("Flee", Flee) ];
      Sequence [ Condition ("intruder?", intruder); Action ("Attack", Attack) ];
      Action ("Patrol", Patrol) ]

(* as scores *)
let options : (dog, act) Utility.option_ list =
  [ { action = Flee; label = "flee"; score = (fun d -> (1. -. d.health) ** 2.) };
    { action = Attack; label = "attack"; score = (fun d -> d.closeness *. d.health) };
    { action = Patrol; label = "patrol"; score = (fun _ -> 0.2) } ]

(* as a machine: fleeing until healed well past the threshold it fled
 * at (0.8, not 0.5): hysteresis *)
let machine : (act, dog) Fsm.machine =
  [ { from = Patrol; label = "hurt"; guard = (fun d _ -> hurt d); target = Flee };
    { from = Patrol; label = "an intruder"; guard = (fun d _ -> intruder d); target = Attack };
    { from = Attack; label = "hurt"; guard = (fun d _ -> hurt d); target = Flee };
    { from = Attack; label = "gone"; guard = (fun d _ -> not (intruder d)); target = Patrol };
    { from = Flee; label = "healed"; guard = (fun d _ -> d.health > 0.8); target = Patrol } ]

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_tree () =
  Alcotest.(check (option act)) "hurt, intruder near: flee" (Some Flee) (Behavior.decide tree { health = 0.3; closeness = 0.8 });
  Alcotest.(check (option act)) "healthy, intruder near: attack" (Some Attack) (Behavior.decide tree { health = 1.; closeness = 0.8 });
  Alcotest.(check (option act)) "healthy, alone: patrol" (Some Patrol) (Behavior.decide tree { health = 1.; closeness = 0. });
  Alcotest.(check (list (pair string bool)))
    "the way it thought"
    [ ("hurt?", false); ("intruder?", true); ("Attack", true) ]
    (Behavior.path tree { health = 1.; closeness = 0.8 })

(* Utility.mli's numbers *)
let test_utility () =
  let s d = List.map snd (Utility.scores options d) in
  Alcotest.(check (list (float 0.001))) "healthy, near" [ 0.; 0.8; 0.2 ] (s { health = 1.; closeness = 0.8 });
  Alcotest.(check (list (float 0.001))) "hurt, near" [ 0.49; 0.24; 0.2 ] (s { health = 0.3; closeness = 0.8 });
  let torn = { health = 0.4; closeness = 0.95 } in
  Alcotest.(check (option act)) "torn: attack, by a hair" (Some Attack) (Utility.choose options torn);
  Alcotest.(check (option act)) "but already fleeing, flee on" (Some Flee) (Utility.choose ~current:Flee ~inertia:0.1 options torn)

(* where the situation is clear-cut, the three agree; the machine, one
 * step at a time, gets there from patrolling *)
let test_agree () =
  List.iter
    (fun (d, expected) ->
      let from_machine = (Fsm.step machine d (Fsm.start Patrol)).state in
      Alcotest.(check (option act)) "the tree" (Some expected) (Behavior.decide tree d);
      Alcotest.(check (option act)) "the scores" (Some expected) (Utility.choose options d);
      Alcotest.(check act) "the machine" expected from_machine)
    [ ({ health = 0.2; closeness = 0.9 }, Flee); ({ health = 1.; closeness = 0.9 }, Attack); ({ health = 1.; closeness = 0.1 }, Patrol) ]

(* the machine remembers, the others don't: healed to 0.6, the tree and
 * the scores stop fleeing at once; the machine flees on until 0.8 *)
let test_memory () =
  let d = { health = 0.6; closeness = 0. } in
  let fleeing = { (Fsm.start Flee) with since = 10 } in
  Alcotest.(check act) "the machine: still fleeing" Flee (Fsm.step machine d fleeing).state;
  Alcotest.(check (option act)) "the tree: patrol" (Some Patrol) (Behavior.decide tree d)

let tests =
  [ Testo.create "Behavior: the guard dog" test_tree;
    Testo.create "Utility: the guard dog's scores" test_utility;
    Testo.create "Fsm, Behavior, Utility agree where it's clear-cut" test_agree;
    Testo.create "Fsm remembers, with hysteresis" test_memory ]
