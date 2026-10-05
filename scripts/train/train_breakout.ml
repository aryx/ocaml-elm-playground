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
(* What the paper did: the learner is given the last four screens (42
 * by 42 greys each here, 84 in the paper) and the score. The loop is the one above, spread
 * over processes as the trainers of the board games are
 * (Alphazero_trainer): an iteration is
 *
 *   - [actors] processes each playing [lived_each] steps with the
 *     network as it is, some of their moves left to chance, and
 *     sending back what they lived;
 *   - [learners] processes each taking [steps_each] steps of learning
 *     on batches drawn from everything remembered, apart, their
 *     networks then averaged; the copy they all take as the target is
 *     the network the iteration started with.
 *
 * A screen is 1,764 bytes and a state four of them, so a step lived
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

(* the paper's network at half its screen: 42 by 42, windows of 4
 * every 2 (20 by 20, 16 channels), again (9 by 9, 16 channels), a
 * layer of 128. About 170,000 numbers where the paper's has 680,000,
 * and a third of its arithmetic *)
let shape : Dqn.screen =
  { width = Breakout_env.side; height = Breakout_env.side; frames = 4; first = (4, 2, 16); second = (4, 2, 16); hidden = 128 }

(* [steps] of the game played by [net] with [chance] of a random move,
 * from the title: what was lived. The game drawn a little larger or
 * smaller each time, by [zoom] *)
let act (net : Dqn.t) ~(seed : int) ~(chance : float) ~(steps : int) : lived list =
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
      let (e, points, lost) = Breakout_env.step e action in
      let s = Breakout_env.screen ~zoom e in
      let l = { before; action; reward = (if points > 0 then 1. else 0.); after = (if lost then None else Some s) } in
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

let actors = 48
let lived_each = 200
let learners = 16
let steps_each = 150
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

let pixels ~(iterations : int) ~(out : string) ~(from : string option) : unit =
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
  Printf.printf "%d numbers; from iteration %d; %d actors, %d learners\n%!" (Dqn.parameters start) done_before actors learners;
  let net = ref start in
  let memory : lived option array = Array.make remembered None in
  let next = ref 0 and count = ref 0 in
  let best = ref (-1.) in
  let t0 = Unix.gettimeofday () in
  for iteration = done_before + 1 to done_before + iterations do
    let began = Unix.gettimeofday () in
    (* all chance at first, one move in ten from the sixtieth iteration *)
    let chance = Float.max 0.1 (1. -. (float_of_int iteration /. 60.)) in
    let now = !net in
    let lived =
      List.concat
        (Processes.those_that_finish "actors"
           (List.init actors (fun a () -> act now ~seed:((iteration * 1000) + a) ~chance ~steps:lived_each)))
    in
    List.iter
      (fun l ->
        memory.(!next) <- Some l;
        next := (!next + 1) mod remembered;
        count := min (!count + 1) remembered)
      lived;
    let played = Unix.gettimeofday () in
    let points = List.fold_left (fun sum (l : lived) -> sum +. l.reward) 0. lived in
    (* the learners, apart, the network of this iteration's start their
       target *)
    let arrived =
      Processes.those_that_finish "learners"
        (List.init learners (fun w () ->
             let draws = Lehmer.make ((iteration * 100) + w) in
             let rec go (n : Dqn.t) (loss : float) (k : int) : Dqn.t * float =
               if k = 0 then (n, loss)
               else
                 let batch =
                   Array.init 32 (fun _ ->
                       let l = Option.get memory.(Lehmer.int draws !count) in
                       ({ state = numbers_of l.before; action = l.action; reward = l.reward; next = after_of l } : Dqn.lived))
                 in
                 let (n, loss) = Dqn.step ~target:now n batch in
                 go n loss (k - 1)
             in
             go now 0. steps_each))
    in
    let (first, _) = List.hd arrived in
    let share = 1. /. float_of_int (List.length arrived) in
    let matrices =
      List.map
        (fun (name, (x : Matrix.t)) ->
          let data = Array.make (Array.length x.data) 0. in
          List.iter
            (fun ((n : Dqn.t), _) ->
              let (m : Matrix.t) = List.assoc name n.matrices in
              Array.iteri (fun i v -> data.(i) <- data.(i) +. (v *. share)) m.data)
            arrived;
          (name, { x with data }))
        first.matrices
    in
    net := { first with matrices };
    let loss = List.fold_left (fun sum (_, l) -> sum +. l) 0. arrived *. share in
    Printf.printf "iteration %3d  %5.0f s  (playing %.0f s, learning %.0f s)  chance %.2f  %.0f bricks in %d steps lived  loss %.4f\n%!"
      iteration (Unix.gettimeofday () -. t0) (played -. began)
      (Unix.gettimeofday () -. played)
      chance points (List.length lived) loss;
    let notes score =
      [ ("model", "Dqn, the Atari paper's at half its screen: the last 4 screens of 42 by 42 greys, windows of 4 every 2 (16), again (16), 128, 3 actions");
        ("game", "TinyBreakout (games/arcade), four frames a step, juice on");
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

let () =
  (* no one is listening *)
  Audio.silently (fun () ->
      match Array.to_list Sys.argv with
      | [ _; "numbers"; steps ] -> numbers (int_of_string steps)
      | [ _; "pixels"; iterations; out ] -> pixels ~iterations:(int_of_string iterations) ~out ~from:None
      | [ _; "pixels"; iterations; out; from ] -> pixels ~iterations:(int_of_string iterations) ~out ~from:(Some from)
      | _ -> prerr_endline "usage: train_breakout numbers <steps> | pixels <iterations> <out.weights> [from.weights]")
