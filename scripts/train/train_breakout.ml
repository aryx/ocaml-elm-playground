(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Teaches a network TinyBreakout from its score (Dqn.mli).
 *
 *   dune exec scripts/train/train_breakout.exe -- numbers 100000                 (steps: the check, three minutes)
 *   dune exec scripts/train/train_breakout.exe -- pixels 150 out.weights          (iterations: from the screen)
 *   dune exec scripts/train/train_breakout.exe -- pixels 50 out.weights out.weights   (go on from a file)
 *   RATE=0.0003 dune exec ... pixels 300 more.weights out.weights.best            (with smaller steps)
 *   dune exec scripts/train/train_breakout.exe -- screen 40 seen.pgm               (what it sees, after 40 steps)
 *   dune exec scripts/train/train_breakout.exe -- measure out.weights [juice]      (40 games; on the game with its effects)
 *   dune exec scripts/train/train_breakout.exe -- follow 20 out.weights            (can it see the ball? a check)
 *
 * [numbers]: the check of the learner before it is given the screen.
 * The game is six numbers (Breakout_env.numbers), the network small,
 * and a few minutes are enough to see whether it learns to meet the
 * ball at all. If it does not here, it will not from pixels, and the
 * reason is not the pixels.
 *
 * It plays with some chance in its moves, less as it goes (from all
 * of them at random to one in twenty), keeps every step lived, and
 * takes a step of learning for each step of play, on thirty-two
 * drawn from what it remembers; the frozen copy is brought up to date
 * every thousand steps. A ball lost ends an episode: nothing after it
 * is owed to what was done before.
 *
 * Every so often it is measured without learning, a game of three
 * balls played with one move in twenty left to chance, beside a
 * player that moves at random. *)

(* a game played to its end by a policy: its score *)
let play (seed : int) (choose : Breakout_env.t -> int) : int =
  let draws = Lehmer.make seed in
  let rec go (e : Breakout_env.t) (steps : int) (best : int) : int =
    if Breakout_env.over e || steps > 5000 then max best (Breakout_env.score e)
    else
      let action = if Lehmer.float draws 1. < 0.05 then Lehmer.int draws Breakout_env.actions else choose e in
      let (e, _, _) = Breakout_env.step e action in
      go e (steps + 1) (max best (Breakout_env.score e))
  in
  go (Breakout_env.start ()) 0 0

let mean (scores : int list) : float = float_of_int (List.fold_left ( + ) 0 scores) /. float_of_int (List.length scores)

let numbers (steps : int) : unit =
  let draws = Lehmer.make 1 in
  let memory = Dqn.memory 100_000 in
  let net = ref (Dqn.make ~seed:1 ~rate:0.0005 ~shape:(Dqn.Numbers 64) ~inputs:6 ~actions:Breakout_env.actions ()) in
  let target = ref !net in
  let e = ref (Breakout_env.start ()) in
  let t0 = Unix.gettimeofday () in
  let random = mean (List.init 10 (fun g -> play g (fun _ -> Lehmer.int draws Breakout_env.actions))) in
  Printf.printf "a player moving at random: %.1f points a game\n%!" random;
  for step = 1 to steps do
    (* all chance at first, one move in ten after the first third *)
    let chance = Float.max 0.1 (1. -. (float_of_int step /. (float_of_int steps /. 3.))) in
    let state = Breakout_env.numbers !e in
    let action = if Lehmer.float draws 1. < chance then Lehmer.int draws Breakout_env.actions else Dqn.best !net state in
    let (e', points, lost) = Breakout_env.step !e action in
    (* a brick is a point whatever its colour, as in the paper: the
       learner's rewards between 0 and 1 *)
    let reward = if points > 0 then 1. else 0. in
    Dqn.remember memory { state; action; reward; next = (if lost then None else Some (Breakout_env.numbers e')) };
    e := if Breakout_env.over e' then Breakout_env.start () else e';
    if Dqn.remembered memory >= 1000 then net := fst (Dqn.step ~target:!target !net (Dqn.recall draws memory 32));
    if step mod 1000 = 0 then target := !net;
    if step mod (max 1 (steps / 10)) = 0 then
      let now = !net in
      Printf.printf "step %6d  %4.0f s  chance %.2f  %.1f points a game\n%!" step (Unix.gettimeofday () -. t0) chance
        (mean (List.init 5 (fun g -> play (100 + g) (fun e -> Dqn.best now (Breakout_env.numbers e)))))
  done

(*****************************************************************************)
(* From the screen *)
(*****************************************************************************)
(* What the paper did: the learner is given the last four screens (64
 * by 64 greys each here, 84 in the paper) and the score. The loop is the one above, spread
 * over processes as the trainers of the board games are
 * (Alphazero_trainer): an iteration is
 *
 *   - [actors] processes each playing [lived_each] steps with the
 *     network as it is, some of their moves left to chance, and
 *     sending back what they lived;
 *   - [steps_each] steps of learning, each on a batch drawn from
 *     everything remembered, its slopes worked out by [slicers]
 *     processes at once, a few steps lived each; the copy taken as
 *     the target is the network the iteration started with.
 *
 * One thing is not the paper's: a ball lost costs a point. In the
 * paper it only ends the episode. In this game nearly every point a
 * beginner scores comes from the serve, whatever the paddle does, so
 * the part of the score that says anything about the paddle is small
 * and arrives a second late; three runs learned nothing from it. A
 * miss, said when it happens, is the same signal made plain. It asks
 * nothing that is not on the screen (the balls left are drawn there).
 *
 * A screen is 4,096 bytes and a state four of them, so a step lived
 * is kept as the screens themselves, shared between the steps that
 * have them in common, and made into numbers only when drawn. *)

let pixels_of = Breakout_env.side * Breakout_env.side

(* a step lived: the four screens before, oldest first, what was done,
 * what it paid, and the screen after; None if the ball was lost *)
type lived = { before : Bytes.t array; action : int; reward : float; after : Bytes.t option }

(* four screens as the network's input: a frame after the other, each
 * grey between 0 and 1 *)
let numbers_of (screens : Bytes.t array) : float array =
  Array.init (4 * pixels_of) (fun i -> float_of_int (Bytes.get_uint8 screens.(i / pixels_of) (i mod pixels_of)) /. 255.)

let after_of (l : lived) : float array option =
  Option.map (fun s -> numbers_of [| l.before.(1); l.before.(2); l.before.(3); s |]) l.after

(* the paper's network on a smaller screen: 64 by 64, windows of 8
 * every 4 (15 by 15, 16 channels), of 4 every 2 (6 by 6, 16
 * channels), a layer of 128. About 82,000 numbers where the paper's
 * has 680,000, and a third of its arithmetic *)
let shape : Dqn.screen =
  { width = Breakout_env.side; height = Breakout_env.side; frames = 4; first = (8, 4, 16); second = (4, 2, 16); hidden = 128 }

(* [steps] of the game played by [net] with [chance] of a random move,
 * from the title: what was lived. The game drawn a little larger or
 * smaller each time, by [zoom] *)
(* the move that takes the paddle towards the ball: for [follow],
 * the check below *)
let towards_the_ball (e : Breakout_env.t) : int =
  let n = Breakout_env.numbers e in
  if n.(5) = 0. then 1 else if n.(1) < n.(0) -. 0.06 then 0 else if n.(1) > n.(0) +. 0.06 then 2 else 1

(* [follow]: instead of the score, a point for each move towards the
 * ball, and nothing after it to wait for. Not learning the game: a
 * check that the screen says where the ball and the paddle are, and
 * that this network can read it, apart from the harder question of
 * learning from rewards that come late *)
let act ?(follow = false) (net : Dqn.t) ~(seed : int) ~(chance : float) ~(steps : int) : lived list =
  let draws = Lehmer.make seed in
  let zoom = 0.97 +. Lehmer.float draws 0.06 in
  let fresh () =
    let e = Breakout_env.start () in
    let s = Breakout_env.screen ~zoom e in
    (e, [| s; s; s; s |])
  in
  let rec go (e : Breakout_env.t) (before : Bytes.t array) (n : int) (lived : lived list) : lived list =
    if n = 0 then lived
    else
      let action = if Lehmer.float draws 1. < chance then Lehmer.int draws Breakout_env.actions else Dqn.best net (numbers_of before) in
      let wanted = towards_the_ball e in
      let (e, points, lost) = Breakout_env.step e action in
      let s = Breakout_env.screen ~zoom e in
      let l =
        if follow then { before; action; reward = (if action = wanted then 1. else 0.); after = None }
        else
          (* a brick is a point, whatever its colour; a ball lost
             takes one away. The paper's learner is told only that
             the episode ended there, and has to find out by itself
             that this is bad, from the points that then do not come:
             with millions of frames it does. Here the miss is said,
             at the step it happens (see the header) *)
          { before; action; reward = (if lost then -1. else if points > 0 then 1. else 0.); after = (if lost then None else Some s) }
      in
      if Breakout_env.over e then
        let (e, before) = fresh () in
        go e before (n - 1) (l :: lived)
      else go e [| before.(1); before.(2); before.(3); s |] (n - 1) (l :: lived)
  in
  let (e, before) = fresh () in
  go e before steps []

(* a game of three balls by [net], one move in twenty left to chance:
 * its score *)
let game_of (net : Dqn.t) (seed : int) : int =
  let draws = Lehmer.make seed in
  let s0 = Breakout_env.screen (Breakout_env.start ()) in
  let rec go (e : Breakout_env.t) (before : Bytes.t array) (steps : int) (best : int) : int =
    if Breakout_env.over e || steps > 4000 then max best (Breakout_env.score e)
    else
      let action = if Lehmer.float draws 1. < 0.05 then Lehmer.int draws Breakout_env.actions else Dqn.best net (numbers_of before) in
      let (e, _, _) = Breakout_env.step e action in
      go e [| before.(1); before.(2); before.(3); Breakout_env.screen e |] (steps + 1) (max best (Breakout_env.score e))
  in
  go (Breakout_env.start ()) [| s0; s0; s0; s0 |] 0 0

(* few actors and long runs, not many and short: a run always starts
 * at the title, and what comes late in a game (the ball faster after
 * 4 hits and 12, the paddle halved once the wall is pierced) has to
 * be lived to be learned. With 200 steps each, thirteen seconds of
 * game, the score stopped at what thirteen seconds can hold *)
let actors = 24
let lived_each = 500
(* a step's batch is [slicers] slices of [slice] steps lived *)
let slicers = 16
let slice = 2
let steps_each = 800
let remembered = 400_000

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write (path : string) (net : Dqn.t) (notes : (string * string) list) : unit =
  let oc = open_out_bin path in
  output_string oc (Weights.to_string (Dqn.to_weights ~notes net));
  close_out oc

let pixels ?(follow = false) ~(iterations : int) ~(out : string) ~(from : string option) () : unit =
  Gc.set { (Gc.get ()) with minor_heap_size = 4 * 1024 * 1024 };
  Printexc.record_backtrace true;
  let (start, done_before) =
    match from with
    | None -> (Dqn.make ~seed:1 ~rate:0.001 ~shape:(Dqn.Screen shape) ~inputs:(4 * pixels_of) ~actions:Breakout_env.actions (), 0)
    | Some path -> (
        match Result.bind (Weights.of_string (read path)) (fun w -> Result.map (fun n -> (n, w)) (Dqn.of_weights w)) with
        | Error why -> failwith why
        | Ok (net, w) -> (net, Option.value ~default:0 (Option.bind (Weights.note w "iterations") int_of_string_opt)))
  in
  Printf.printf "%d numbers; from iteration %d; %d actors, %d steps an iteration on batches of %d\n%!" (Dqn.parameters start) done_before actors steps_each (slicers * slice);
  (* RATE: a smaller step than the network was made with, for going on
     from a file where the first run's would undo what it found *)
  let rate = Option.bind (Sys.getenv_opt "RATE") float_of_string_opt in
  let net = ref start in
  let memory : lived option array = Array.make remembered None in
  let next = ref 0 and count = ref 0 in
  let best = ref (-1.) in
  let t0 = Unix.gettimeofday () in
  for iteration = done_before + 1 to done_before + iterations do
    let began = Unix.gettimeofday () in
    (* all chance at first, one move in ten from the sixtieth iteration *)
    let chance = if follow then 0.5 else Float.max 0.1 (1. -. (float_of_int iteration /. 60.)) in
    let now = !net in
    let lived =
      List.concat
        (Processes.those_that_finish "actors"
           (List.init actors (fun a () -> act ~follow now ~seed:((iteration * 1000) + a) ~chance ~steps:lived_each)))
    in
    List.iter
      (fun l ->
        memory.(!next) <- Some l;
        next := (!next + 1) mod remembered;
        count := min (!count + 1) remembered)
      lived;
    let played = Unix.gettimeofday () in
    let points = List.fold_left (fun sum (l : lived) -> sum +. l.reward) 0. lived in
    (* the steps: one network, each step's batch shared among helpers
       that stay for the whole iteration. Each is sent the network's
       numbers, works out the slopes of a few steps lived that it
       draws itself, and sends them back; their mean is the step. The
       target is the network this iteration started with *)
    let helpers =
      Processes.helpers slicers (fun number ->
          let draws = Lehmer.make ((iteration * 100) + number) in
          fun (numbers : float array) ->
            let batch =
              Array.init slice (fun _ ->
                  let l = Option.get memory.(Lehmer.int draws !count) in
                  ({ state = numbers_of l.before; action = l.action; reward = l.reward; next = after_of l } : Dqn.lived))
            in
            Dqn.gradient ~target:now (Dqn.with_numbers now numbers) batch)
    in
    let share = 1. /. float_of_int slicers in
    let last = ref 0. in
    for _ = 1 to steps_each do
      let parts = Processes.ask_all helpers (Dqn.numbers_of !net) in
      let slopes = Array.make (Dqn.parameters !net) 0. in
      List.iter (fun (part, _) -> Array.iteri (fun i g -> slopes.(i) <- slopes.(i) +. (g *. share)) part) parts;
      last := List.fold_left (fun sum (_, l) -> sum +. l) 0. parts *. share;
      net := Dqn.apply ?rate !net slopes
    done;
    Processes.dismiss helpers;
    let loss = !last in
    Printf.printf "iteration %3d  %5.0f s  (playing %.0f s, learning %.0f s)  chance %.2f  %.0f bricks in %d steps lived  loss %.4f\n%!"
      iteration (Unix.gettimeofday () -. t0) (played -. began)
      (Unix.gettimeofday () -. played)
      chance points (List.length lived) loss;
    let notes score =
      [ ("model", "Dqn, the Atari paper's at half its screen: the last 4 screens of 64 by 64 greys, windows of 8 every 4 (16), of 4 every 2 (16), 128, 3 actions");
        ("game", "TinyBreakout (games/arcade), four frames a step, juice off");
        ("trainer", "scripts/train/train_breakout pixels");
        ("iterations", string_of_int iteration);
        ("measured", score) ]
    in
    write out !net (notes "not measured at this iteration");
    (* measured every fifth iteration, and the best seen kept beside:
       the last network of a run is not its best (notes_ai_dark_arts.md) *)
    if iteration mod 5 = 0 then (
      let now = !net in
      let scores = Processes.those_that_finish "games" (List.init 12 (fun g () -> game_of now (500 + g))) in
      let score = mean scores in
      Printf.printf "  a game of three balls: %.1f points (the best of the twelve %d)\n%!" score (List.fold_left max 0 scores);
      if score > !best then (
        best := score;
        write (out ^ ".best") now (notes (Printf.sprintf "%.1f points a game, the mean of 12" score))))
  done

(* what the learner sees, to look at: the screen after so many steps
 * of a player moving at random, as a PGM picture (greys, any viewer
 * opens it) *)
let screen_to (path : string) (steps : int) : unit =
  let draws = Lehmer.make 3 in
  let rec go (e : Breakout_env.t) (n : int) : Breakout_env.t =
    if n = 0 then e
    else
      let (e, _, _) = Breakout_env.step e (Lehmer.int draws Breakout_env.actions) in
      go e (n - 1)
  in
  let s = Breakout_env.screen (go (Breakout_env.start ()) steps) in
  let oc = open_out_bin path in
  Printf.fprintf oc "P5\n%d %d\n255\n" Breakout_env.side Breakout_env.side;
  output_bytes oc s;
  close_out oc

(* a network from a file, measured: the mean of [games] games of three
 * balls, each in a process of its own *)
let measure (path : string) (games : int) : unit =
  match Result.bind (Weights.of_string (read path)) Dqn.of_weights with
  | Error why -> failwith why
  | Ok net ->
      let scores = Processes.together (List.init games (fun g () -> game_of net (900 + g))) in
      Printf.printf "%s, the game %s: %.1f points a game over %d (from %d to %d)\n" (Filename.basename path)
        (if !Breakout_env.juice then "with its effects" else "dry")
        (mean scores) games
        (List.fold_left min max_int scores)
        (List.fold_left max 0 scores)

let () =
  (* no one is listening *)
  Audio.silently (fun () ->
      match Array.to_list Sys.argv with
      | [ _; "numbers"; steps ] -> numbers (int_of_string steps)
      | [ _; "measure"; path ] -> measure path 40
      | [ _; "measure"; path; "juice" ] ->
          Breakout_env.juice := true;
          measure path 40
      | [ _; "screen"; steps; out ] -> screen_to out (int_of_string steps)
      | [ _; "pixels"; iterations; out ] -> pixels ~iterations:(int_of_string iterations) ~out ~from:None ()
      | [ _; "pixels"; iterations; out; from ] -> pixels ~iterations:(int_of_string iterations) ~out ~from:(Some from) ()
      | [ _; "follow"; iterations; out ] -> pixels ~follow:true ~iterations:(int_of_string iterations) ~out ~from:None ()
      | _ -> prerr_endline "usage: train_breakout numbers <steps> | pixels <iterations> <out.weights> [from.weights]")
