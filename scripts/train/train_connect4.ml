(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Teaches a network Connect 4 by self-play (Selfplay.mli), and writes
 * what it learned: AiConnect4's network.
 *
 *   dune exec scripts/train/train_connect4.exe -- data/weights/connect4/connect4.weights
 *   dune exec scripts/train/train_connect4.exe -- out.weights 10               (iterations)
 *   dune exec scripts/train/train_connect4.exe -- out.weights 10 out.weights   (go on from a file)
 *   WORKERS=8 dune exec ...                                                    (processes playing at once)
 *   NET=board dune exec ...                                                    (convolutions: the board as a board)
 *
 * An iteration is 480 games against itself at 100 playouts a move,
 * then 600 steps on batches of 64 drawn from the last 100,000
 * lessons. The file is written after every iteration, so the training
 * can be stopped at any time and gone on with from the file (the
 * lessons remembered are not kept: it plays new ones).
 *
 * With NET=board the network reads the board as a board (two
 * convolutions of 16 channels, Policy_value.mli) and its steps are
 * taken another way, each of 32 processes its own 300, their networks
 * averaged ([learn_apart] below): 85 minutes for 100 iterations. The
 * weights in data/weights/connect4 are the flat network's; the board
 * network's result is in notes_ai_learning.md, section 16.
 *
 * Two things a trainer adds to the loop of Selfplay.mli, and both are
 * about getting more games out of the same hour:
 *
 *  - the games of an iteration are played by many processes at once
 *    (48 by default: games against oneself do not depend on each
 *    other, so they need no more than a fork each and a pipe to send
 *    their lessons back);
 *  - every lesson is learned twice, as it was and as in a mirror: a
 *    Connect 4 board turned left to right is as good a lesson, with
 *    the policy turned too.
 *
 * After AlphaZero.jl's Connect Four tutorial, which measures as it
 * goes against players that learn nothing, and so does this, every
 * fifth iteration:
 *
 *  - the same search with no network: Monte Carlo tree search with
 *    random playouts, as many of them (Mcts.mli);
 *  - the game's own computer: alpha-beta with its evaluation, at
 *    depths 1, 3, 5 and 7 (AiConnect4 plays at 7);
 *
 * both with the search guided by the network and with the network
 * alone, its policy's first choice. Won-drawn-lost over 20 games, the
 * network moving first in half, each game from its own two random
 * opening moves -- or two players without dice play the same game
 * twenty times. The last of these lines goes in the file's notes. *)

let playouts = 100
let settings : Selfplay.settings = { Selfplay.default with playouts; exploring = 8 }
let schedule : Selfplay.schedule = { games = 480; steps = 600; batch = 64; remembered = 100_000 }
let board = Connect4.board
let shown (s : Arena.score) : string = Printf.sprintf "%d-%d-%d" s.won s.drawn s.lost

let workers : int =
  match Option.bind (Sys.getenv_opt "WORKERS") int_of_string_opt with Some n when n > 0 -> n | _ -> 48

(*****************************************************************************)
(* Many processes at once *)
(*****************************************************************************)

(* [jobs] run each in a process of its own, their results in order: a
 * fork and a pipe a job, the result marshalled back *)
let together (jobs : (unit -> 'a) list) : 'a list =
  let started =
    List.map
      (fun job ->
        let (from_child, to_parent) = Unix.pipe () in
        match Unix.fork () with
        | 0 ->
            Unix.close from_child;
            let oc = Unix.out_channel_of_descr to_parent in
            Marshal.to_channel oc (job ()) [];
            close_out oc;
            exit 0
        | pid ->
            Unix.close to_parent;
            (pid, Unix.in_channel_of_descr from_child))
      jobs
  in
  List.map
    (fun (pid, ic) ->
      let result = Marshal.from_channel ic in
      close_in ic;
      ignore (Unix.waitpid [] pid);
      result)
    started

(* a lesson as in a mirror: the columns turned left to right, in the
 * position and in the policy *)
let mirrored (l : Policy_value.lesson) : Policy_value.lesson =
  let squares = Connect4.columns * Connect4.rows in
  let input =
    Array.init (2 * squares) (fun i ->
        let plane = i / squares and at = i mod squares in
        let column = at / Connect4.rows and row = at mod Connect4.rows in
        l.input.((plane * squares) + ((Connect4.columns - 1 - column) * Connect4.rows) + row))
  in
  { l with input; policy = Array.init Connect4.columns (fun c -> l.policy.(Connect4.columns - 1 - c)) }

(* an iteration's games, shared out among the processes *)
let games (net : Policy_value.t) (iteration : int) : Policy_value.lesson list =
  let each = schedule.games / workers in
  let played =
    together
      (List.init workers (fun w () ->
           List.concat
             (List.init each (fun g -> fst (Selfplay.play ~settings ~seed:((iteration * 100_000) + (w * 1000) + g) board net)))))
  in
  let lessons = List.concat played in
  lessons @ List.map mirrored lessons

(* the steps of an iteration, for the board network, whose steps are
 * the trainer's time and not its games: [learners] processes each
 * take the network as it is and their own [steps] on their own
 * batches, apart; then the networks they arrive at are averaged,
 * number by number. A few hundred steps from the same start do not
 * go far, and the middle of where thirty-two of them went is a better
 * place than where any one did (McMahan et al., 2017). Better, not
 * further: averaging thirty-two learners takes the noise out of
 * their steps, it does not make them thirty-two times as many.
 *
 * One fork a learner an iteration. The first version forked sixteen
 * processes at every *step*, to share one large batch: a step took
 * 0.3 s, most of it the forks (notes_ai_dark_arts.md) *)
let learners = 32

let learn_apart ~(steps : int) ~(batch : int) (l : Selfplay.learner) (fresh : Policy_value.lesson list) :
    Selfplay.learner * float =
  let all = Array.append (Array.of_list fresh) l.lessons in
  let lessons = Array.sub all 0 (min schedule.remembered (Array.length all)) in
  let arrived =
    together
      (List.init learners (fun w () ->
           let draws = Lehmer.make ((l.iteration * 100) + w) in
           let rec go (net : Policy_value.t) (loss : float) (n : int) : Policy_value.t * float =
             if n = 0 then (net, loss)
             else
               let (net, loss) =
                 Policy_value.step net (Array.init batch (fun _ -> lessons.(Lehmer.int draws (Array.length lessons))))
               in
               go net loss (n - 1)
           in
           go l.net 0. steps))
  in
  let (first, _) = List.hd arrived in
  let matrices =
    List.map
      (fun (name, (x : Matrix.t)) ->
        let data = Array.make (Array.length x.data) 0. in
        List.iter
          (fun ((net : Policy_value.t), _) ->
            let (m : Matrix.t) = List.assoc name net.matrices in
            Array.iteri (fun i v -> data.(i) <- data.(i) +. (v /. float_of_int learners)) m.data)
          arrived;
        (name, { x with data }))
      first.matrices
  in
  let loss = List.fold_left (fun sum (_, loss) -> sum +. loss) 0. arrived /. float_of_int learners in
  (* the first learner's memory of its slopes (Adam's) is kept with the
   * averaged numbers: near enough, the learners having gone the same
   * way *)
  ({ l with net = { first with matrices }; lessons; iteration = l.iteration + 1 }, loss)

(*****************************************************************************)
(* Measuring *)
(*****************************************************************************)

(* a player whose first move of a game is any legal one: with the
 * other player's, two random opening moves a game *)
let opening (player : (Connect4.position, int) Arena.player) : (Connect4.position, int) Arena.player =
 fun ~seed p ->
  let pieces = Array.fold_left (fun n piece -> if piece = Connect4.Empty then n else n + 1) 0 p.board in
  if pieces < 2 then Arena.random Connect4.connect4 ~seed p else player ~seed p

let plain_mcts : (Connect4.position, int) Arena.player =
 fun ~seed p -> Option.get (Mcts.search ~seed Connect4.connect4 ~playouts p).best

let measure (net : Policy_value.t) : string =
  let searching : (Connect4.position, int) Arena.player = fun ~seed p -> Option.get (Selfplay.choose ~playouts ~seed board net p) in
  let alone : (Connect4.position, int) Arena.player = fun ~seed:_ p -> Option.get (Selfplay.instinct board net p) in
  let against a b () = shown (Arena.play Connect4.connect4 Connect4.start ~a:(opening a) ~b:(opening b) ~games:20) in
  match
    together
      [ against searching plain_mcts; against searching (Connect4.alphabeta ~depth:1);
        against searching (Connect4.alphabeta ~depth:3); against searching (Connect4.alphabeta ~depth:5);
        against searching (Connect4.alphabeta ~depth:7); against alone plain_mcts;
        against alone (Connect4.alphabeta ~depth:1) ]
  with
  | [ m; a1; a3; a5; a7; im; i1 ] ->
      Printf.sprintf "with the search: mcts %s, alpha-beta 1 %s, 3 %s, 5 %s, 7 %s; alone: mcts %s, alpha-beta 1 %s" m a1 a3
        a5 a7 im i1
  | _ -> assert false

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  let out =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else failwith "usage: train_connect4 <out.weights> [iterations] [from.weights]"
  in
  let iterations = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 60 in
  (* room in the minor heap for a step's graph, which dies at its end
   * (notes_opti_ocaml.md, section 20). Not more room than that: every
   * process forked gets this heap to write in, and copies what it
   * writes *)
  Gc.set { (Gc.get ()) with minor_heap_size = 4 * 1024 * 1024 };
  let seed = 1 in
  let (net, done_before) =
    if Array.length Sys.argv > 3 then
      match Weights.of_string (read Sys.argv.(3)) with
      | Error why -> failwith why
      | Ok w -> (
          match Policy_value.of_weights w with
          | Error why -> failwith why
          | Ok net -> (net, Option.value ~default:0 (Option.bind (Weights.note w "iterations") int_of_string_opt)))
    else
      ( (match Sys.getenv_opt "NET" with
        | Some "board" ->
            (* the board as a board: 7 columns of 6, two planes *)
            let shape : Policy_value.board =
              { planes = 2; height = Connect4.columns; width = Connect4.rows; channels = 16; layers = 2 }
            in
            Policy_value.make ~seed ~rate:0.003 ~board:shape ~inputs:board.inputs ~moves:board.moves ()
        | _ -> Policy_value.make ~seed ~hidden:128 ~rate:0.003 ~inputs:board.inputs ~moves:board.moves ()),
        0 )
  in
  Printf.printf "%d numbers; from iteration %d; %d processes\n%!" (Policy_value.parameters net) done_before workers;
  let t0 = Unix.gettimeofday () in
  let learner = ref { (Selfplay.learner ~seed:5 net) with iteration = done_before } in
  let measured = ref (measure net) in
  Printf.printf "before: %s\n%!" !measured;
  for _ = 1 to iterations do
    let began = Unix.gettimeofday () in
    let fresh = games !learner.net (!learner.iteration + 1) in
    let played = Unix.gettimeofday () in
    let (l, loss) =
      match !learner.net.shape with
      | Flat -> Selfplay.learn ~schedule !learner fresh
      | Board _ -> learn_apart ~steps:300 ~batch:64 !learner fresh
    in
    learner := l;
    Printf.printf "iteration %3d  %5.0f s  (its games %.0f s, its steps %.0f s)  loss %.3f  %d lessons\n%!" l.iteration
      (Unix.gettimeofday () -. t0) (played -. began)
      (Unix.gettimeofday () -. played)
      loss (Array.length l.lessons);
    if l.iteration mod 5 = 0 then (
      measured := measure l.net;
      Printf.printf "  %s\n%!" !measured);
    let notes =
      [
        ("model", "Policy_value: 84 numbers in, two layers of 128, a policy over 7 columns and a value");
        ("game", "Connect 4 (gamekits/boards/Connect4)");
        ("trainer", "scripts/train/train_connect4");
        ("seed", string_of_int seed);
        ("iterations", string_of_int l.iteration);
        ( "an-iteration",
          Printf.sprintf "%d games against itself at %d playouts, each lesson and its mirror, then %d steps of %d"
            schedule.games playouts schedule.steps schedule.batch );
        ("measured", !measured);
      ]
    in
    let oc = open_out_bin out in
    output_string oc (Weights.to_string (Policy_value.to_weights ~notes l.net));
    close_out oc
  done
