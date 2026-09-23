(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Ai: the four families that wrap ai/ (steering, the
 * fifth, is drawn by examples/AiSteering and examples/AiFlock) *)

let t = Testo.create
let cell = Alcotest.(pair int int)

(*****************************************************************************)
(* Ways through a map *)
(*****************************************************************************)

(* a wall across the middle, with one gap:
 *
 *     y=0  . . . . .
 *     y=1  # # # . #
 *     y=2  . . . . .
 *)
let rows = [| "....."; "###.#"; "....." |]

let walkable ((x, y) : int * int) : bool =
  y >= 0 && y < Array.length rows && x >= 0 && x < String.length rows.(y) && rows.(y).[x] = '.'

let test_way () =
  let path = Ai.way ~walkable (0, 0) (0, 2) in
  (* through the gap at x = 3, which is the only way down *)
  Alcotest.(check int) "eight steps around the wall" 8 (List.length path);
  Alcotest.check cell "and it ends where it was going" (0, 2) (List.nth path 7);
  Alcotest.(check bool) "the tile it stands on is not a step" true (not (List.mem (0, 0) path));
  Alcotest.(check bool) "it never walks into the wall" true (List.for_all walkable path);
  (* the gap closed: no way at all, and the same for a goal in a wall *)
  let shut (x, y) = walkable (x, y) && not (x = 3 && y = 1) in
  Alcotest.(check (list cell)) "no way through" [] (Ai.way ~walkable:shut (0, 0) (0, 2));
  Alcotest.(check (list cell)) "nor into a wall" [] (Ai.way ~walkable (0, 0) (0, 1))

let test_way_diagonal () =
  let open_ (x, y) = x >= 0 && x < 3 && y >= 0 && y < 3 in
  Alcotest.(check int) "four steps, four ways" 4 (List.length (Ai.way ~walkable:open_ (0, 0) (2, 2)));
  Alcotest.(check int) "two, with the corners" 2 (List.length (Ai.way ~walkable:open_ ~diagonal:true (0, 0) (2, 2)))

(* mud in the middle of the road: the cheapest way is the long one *)
let test_way_over () =
  let cost (x, y) = if x < 0 || x > 4 || y < 0 || y > 2 then Float.infinity else if (x, y) = (2, 1) then 10. else 1. in
  let path = Ai.way_over ~cost (0, 1) (4, 1) in
  Alcotest.(check bool) "it goes round the mud" true (not (List.mem (2, 1) path));
  Alcotest.(check int) "six steps instead of four" 6 (List.length path);
  (* and straight through it when the mud is only a little slower *)
  let shallow (c : int * int) = if cost c = 10. then 1.5 else cost c in
  Alcotest.(check bool) "but not round a puddle" true (List.mem (2, 1) (Ai.way_over ~cost:shallow (0, 1) (4, 1)))

let test_flow () =
  let f = Ai.flow ~walkable (0, 2) in
  Alcotest.(check (option (float 1e-9))) "eight steps away" (Some 8.) (Ai.steps_to_go f (0, 0));
  Alcotest.(check (option (float 1e-9))) "nothing to do here" (Some 0.) (Ai.steps_to_go f (0, 2));
  Alcotest.(check (option cell)) "the only way out is right" (Some (1, 0)) (Ai.next_step f (0, 0));
  Alcotest.(check (option cell)) "down through the gap" (Some (3, 1)) (Ai.next_step f (3, 0));
  Alcotest.(check (option cell)) "on the goal there is nowhere better" None (Ai.next_step f (0, 2));
  Alcotest.(check (option cell)) "and a wall is not on the field" None (Ai.next_step f (0, 1));
  (* every tile of the map leads to the goal, which is what a flow
     field is for: one search, and the whole crowd knows the way *)
  let reachable = List.filter walkable (List.concat_map (fun y -> List.init 5 (fun x -> (x, y))) [ 0; 1; 2 ]) in
  Alcotest.(check bool) "every tile knows where to go" true
    (List.for_all (fun c -> c = (0, 2) || Ai.next_step f c <> None) reachable)

(*****************************************************************************)
(* An opponent *)
(*****************************************************************************)

(* Nim, the simplest game with a right answer: sticks on a table, one
 * to three taken a turn, and whoever takes the last one wins. Leave a
 * multiple of four and you cannot lose. The state is the sticks left
 * and whether it is the machine's turn. *)
let nim : (int * bool, int) Ai.rules =
  {
    moves = (fun (n, _) -> List.filter (fun k -> k <= n) [ 1; 2; 3 ]);
    play = (fun (n, mine) k -> (n - k, not mine));
    (* nothing left on your turn means the other one took the last *)
    score = (fun (n, mine) -> if n > 0 then 0. else if mine then -1. else 1.);
    my_turn = (fun (_, mine) -> mine);
  }

let test_opponent () =
  let o = Ai.thinking_ahead 6 nim in
  Alcotest.(check (option int)) "from five it leaves four" (Some 1) (Ai.best_move o (5, true));
  Alcotest.(check (option int)) "from three it takes them all" (Some 3) (Ai.best_move o (3, true));
  (* what it makes of each move, which is what a game draws *)
  let thoughts = List.sort compare (Ai.thoughts o (5, true)) in
  Alcotest.(check (list (pair int (float 1e-9)))) "one move wins, two lose"
    [ (1, 1.); (2, -1.); (3, -1.) ] thoughts

(* the same game played out at random instead of searched: no [score]
 * worth the name, only who won *)
let test_playing_out () =
  let o = Ai.playing_out 400 nim in
  Alcotest.(check (option int)) "it sees a win in front of it" (Some 3) (Ai.best_move o (3, true));
  Alcotest.(check (option int)) "and finds the multiple of four" (Some 1) (Ai.best_move o (5, true));
  let shares = Ai.thoughts o (3, true) in
  Alcotest.(check int) "an opinion of every move" 3 (List.length shares);
  Alcotest.(check bool) "winning outright is worth 1" true (List.assoc 3 shares = 1.)

(* thinking across frames ends where thinking all at once does *)
let test_pondering () =
  let check_same (o : (int * bool, int) Ai.opponent) (name : string) =
    let rec frames n p = if Ai.settled p || n > 500 then (p, n) else frames (n + 1) (Ai.ponder p) in
    let (p, n) = frames 0 (Ai.pondering o (5, true)) in
    Alcotest.(check bool) (name ^ ": it stops") true (Ai.settled p);
    Alcotest.(check bool) (name ^ ": and not on the first frame it is asked") true (n >= 1);
    Alcotest.(check (option int)) (name ^ ": same answer") (Ai.best_move o (5, true)) (Ai.answer p)
  in
  check_same (Ai.thinking_ahead 6 nim |> Ai.a_frame_of 40) "ahead";
  check_same (Ai.playing_out 400 nim |> Ai.a_frame_of 25) "playouts";
  (* and it has an answer from the very first frame: that is the whole
     point of thinking a frame at a time *)
  let p = Ai.ponder (Ai.pondering (Ai.thinking_ahead 6 nim |> Ai.a_frame_of 40) (5, true)) in
  Alcotest.(check bool) "an answer straight away" true (Ai.answer p <> None);
  Alcotest.(check bool) "and something to draw" true (Ai.so_far p <> [])

(*****************************************************************************)
(* What a character is doing *)
(*****************************************************************************)

type mode = Home | Chase | Flee

let test_mind () =
  (* a ghost: it chases while it can see you, gives up after a while,
     and runs when you are big *)
  let changes =
    [
      Ai.on ~why:"you ate the pill" Chase (fun (_seen, big) -> big) Flee;
      Ai.on ~why:"saw you" Home (fun (seen, _) -> seen) Chase;
      Ai.after ~why:"lost you" 3 Chase Home;
    ]
  in
  let m = Ai.mind Home in
  Alcotest.(check bool) "it starts at home" true (Ai.doing m = Home);
  let m = Ai.deciding changes (true, false) m in
  Alcotest.(check bool) "and gives chase" true (Ai.doing m = Chase);
  Alcotest.(check (option string)) "saying why" (Some "saw you") (Ai.changed m);
  Alcotest.(check int) "the frame it changed counts as none" 0 (Ai.doing_for m);
  (* three frames of chasing, and the chase runs out *)
  let m = List.fold_left (fun m _ -> Ai.deciding changes (false, false) m) m [ 1; 2; 3 ] in
  Alcotest.(check bool) "back home" true (Ai.doing m = Home);
  Alcotest.(check (option string)) "because it lost you" (Some "lost you") (Ai.changed m);
  (* the order of the list is the priority: fleeing comes first *)
  let m = Ai.deciding changes (true, true) (Ai.mind Chase) in
  Alcotest.(check bool) "the pill beats the chase" true (Ai.doing m = Flee);
  let m = Ai.deciding changes (false, false) m in
  Alcotest.(check (option string)) "and nothing happened this frame" None (Ai.changed m)

(*****************************************************************************)
(* A bot *)
(*****************************************************************************)

(* the world is a number, what it senses is that number, and what it
 * wants is to say it: enough to see the handicaps at work *)
let counter : (int, int, int) Ai.bot = Ai.bot ~senses:(fun _ w -> w) (fun s -> s)

(* the intents it gives over [frames] frames of a world counting up *)
let run (b : (int, int, int) Ai.bot) (frames : int) : int list =
  let (intents, _) =
    List.fold_left
      (fun (acc, p) world -> let (i, p) = Ai.thinks b world p in (i :: acc, p))
      ([], Ai.playing 0) (List.init frames Fun.id)
  in
  List.rev intents

let test_bot () =
  Alcotest.(check (list int)) "a machine answers the frame it sees" [ 0; 1; 2; 3 ] (run counter 4);
  Alcotest.(check (list int)) "reacting in two frames, it is two behind" [ 0; 0; 0; 1 ]
    (run (Ai.reacting_in 2 counter) 4);
  Alcotest.(check (list int)) "deciding every three, it repeats itself" [ 0; 0; 0; 3; 3; 3 ]
    (run (Ai.deciding_every 3 counter) 6);
  (* what it saw last, which is what a game draws to show it is not
     cheating *)
  let (_, p) = Ai.thinks counter 7 (Ai.playing 0) in
  Alcotest.(check (option int)) "and it says what it noticed" (Some 7) (Ai.noticed p)

let test_skill () =
  (* one number for both handicaps, and no more knowledge either way.
     A beginner reacts in 12 frames and decides every 6, so its first
     word about the world comes on frame 18 and is six frames stale; at
     half skill that is 6 and 4, and the words come on frames 8, 12,
     16, each six frames behind. *)
  Alcotest.(check (list int)) "a machine sees every frame" [ 0; 1; 2; 3; 4; 5 ] (run (Ai.skill 1. counter) 6);
  Alcotest.(check (list int)) "a beginner says nothing until frame 18"
    [ 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 6; 6 ]
    (run (Ai.skill 0. counter) 20);
  Alcotest.(check (list int)) "and half way is half way"
    [ 0; 0; 0; 0; 0; 0; 0; 0; 2; 2; 2; 2; 6; 6; 6; 6 ]
    (run (Ai.skill 0.5 counter) 16)

let test_aim_error () =
  (* it wanders, it starts inside the spread, and it settles as the
     target stays in sight *)
  let miss seen_for = Ai.aim_error ~spread:10. ~seen_for ~seed:3 () in
  Alcotest.(check bool) "within the spread at once" true (Float.abs (miss 0) <= 10.);
  Alcotest.(check bool) "smaller after a second of looking" true (Float.abs (miss 60) < Float.abs (miss 0));
  Alcotest.(check bool) "nearly gone after four" true (Float.abs (miss 240) < 1.);
  Alcotest.(check bool) "two bots miss differently" true (miss 10 <> Ai.aim_error ~spread:10. ~seen_for:10 ~seed:4 ())

let tests =
  [
    t "Ai.way, around a wall" test_way;
    t "Ai.way, the corners" test_way_diagonal;
    t "Ai.way_over, round the mud" test_way_over;
    t "Ai.flow, one search for a crowd" test_flow;
    t "Ai.thinking_ahead, Nim" test_opponent;
    t "Ai.playing_out, Nim at random" test_playing_out;
    t "Ai.pondering, a frame at a time" test_pondering;
    t "Ai.deciding, what it is doing" test_mind;
    t "Ai.bot, the handicaps" test_bot;
    t "Ai.skill, both from one number" test_skill;
    t "Ai.aim_error, the wobble that settles" test_aim_error;
  ]
