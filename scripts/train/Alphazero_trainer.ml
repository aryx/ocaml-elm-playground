(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Alphazero_trainer.mli *)

let workers : int =
  match Option.bind (Sys.getenv_opt "WORKERS") int_of_string_opt with Some n when n > 0 -> n | _ -> 48

(*****************************************************************************)
(* Many processes at once (Processes) *)
(*****************************************************************************)

let together = Processes.together
let those_that_finish = Processes.those_that_finish

let score ~(games : int) (play : int -> float) : string =
  let shares = together (List.init games (fun n () -> play n)) in
  let count f = List.length (List.filter f shares) in
  Printf.sprintf "%d-%d-%d" (count (fun s -> s > 0.5)) (count (fun s -> s = 0.5)) (count (fun s -> s < 0.5))

(*****************************************************************************)
(* An iteration *)
(*****************************************************************************)

type ('state, 'move) setup = {
  board : ('state, 'move) Alphazero.board;
  settings : Alphazero.settings;
  games : int;
  source : (seed:int -> Policy_value.t -> Policy_value.lesson list) option;
  remembered : int;
  also : Policy_value.lesson -> Policy_value.lesson list;
  steps : int;
  batch : int;
  learners : int;
  measure : Policy_value.t -> string;
  every : int;
  notes : (string * string) list;
}

(* A lesson as it is remembered: of its numbers, only those that are
 * not zero, each with its place. A position of chess is 1,088 numbers
 * and a policy 4,096, of which some thirty pieces and some thirty
 * moves are not zero: remembered whole, the lessons of three
 * iterations were 2.7 GB, every process forked had them, and the
 * machine ran out of memory at the fourth (notes_ai_dark_arts.md) *)
type sparse = { size : int; at : int array; is : float array }
type kept = { input : sparse; policy : sparse; value : float }

let sparse (a : float array) : sparse =
  let at = List.filter (fun i -> a.(i) <> 0.) (List.init (Array.length a) Fun.id) |> Array.of_list in
  { size = Array.length a; at; is = Array.map (fun i -> a.(i)) at }

let whole (s : sparse) : float array =
  let a = Array.make s.size 0. in
  Array.iteri (fun k i -> a.(i) <- s.is.(k)) s.at;
  a

let keep (l : Policy_value.lesson) : kept = { input = sparse l.input; policy = sparse l.policy; value = l.value }
let lesson (k : kept) : Policy_value.lesson = { input = whole k.input; policy = whole k.policy; value = k.value }

(* an iteration's games, shared out among the processes, and each
 * lesson with the other ways it is a lesson *)
let games (s : ('state, 'move) setup) (net : Policy_value.t) (iteration : int) : kept list =
  let each = max 1 (s.games / workers) in
  (* a process lost is some games fewer, not the end of the run *)
  let played =
    those_that_finish "games' processes"
      (List.init workers (fun w () ->
           let lessons =
             List.concat
               (List.init each (fun g ->
                    let seed = (iteration * 100_000) + (w * 1000) + g in
                    match s.source with
                    | Some lessons -> lessons ~seed net
                    | None -> fst (Alphazero.play ~settings:s.settings ~seed s.board net)))
           in
           List.map keep (lessons @ List.concat_map s.also lessons)))
  in
  List.concat played

(* the steps of an iteration by [learners] processes: each takes the
 * network as it is and its own [steps] on its own batches, apart;
 * then the networks they arrive at are averaged, number by number. A
 * few hundred steps from the same start do not go far, and the middle
 * of where they all went is a better place than where any one did
 * (McMahan et al., 2017). Better, not further: averaging takes the
 * noise out of their steps, it does not make them as many times as
 * many.
 *
 * One fork a learner an iteration. The first version forked sixteen
 * processes at every *step*, to share one large batch: a step took
 * 0.3 s, most of it the forks (notes_ai_dark_arts.md) *)
let learn_apart (s : ('state, 'move) setup) (l : Alphazero.learner) (lessons : kept array) : Alphazero.learner * float =
  let arrived =
    those_that_finish "learners"
      (* no more of them than the processes allowed (WORKERS) *)
      (List.init (min s.learners workers) (fun w () ->
           let draws = Lehmer.make ((l.iteration * 100) + w) in
           let rec go (net : Policy_value.t) (loss : float) (n : int) : Policy_value.t * float =
             if n = 0 then (net, loss)
             else
               let (net, loss) =
                 Policy_value.step net
                   (Array.init s.batch (fun _ -> lesson lessons.(Lehmer.int draws (Array.length lessons))))
               in
               go net loss (n - 1)
           in
           go l.net 0. s.steps))
  in
  let (first, _) = List.hd arrived in
  let share = 1. /. float_of_int (List.length arrived) in
  let matrices =
    List.map
      (fun (name, (x : Matrix.t)) ->
        let data = Array.make (Array.length x.data) 0. in
        List.iter
          (fun ((net : Policy_value.t), _) ->
            let (m : Matrix.t) = List.assoc name net.matrices in
            Array.iteri (fun i v -> data.(i) <- data.(i) +. (v *. share)) m.data)
          arrived;
        (name, { x with data }))
      first.matrices
  in
  let loss = List.fold_left (fun sum (_, loss) -> sum +. loss) 0. arrived *. share in
  (* the first learner's memory of its slopes (Adam's) is kept with the
   * averaged numbers: near enough, the learners having gone the same
   * way *)
  ({ l with net = { first with matrices }; iteration = l.iteration + 1 }, loss)

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let run (s : ('state, 'move) setup) ~(fresh : unit -> Policy_value.t) ~(out : string) ~(iterations : int)
    ~(from : string option) : unit =
  (* room in the minor heap for a step's graph, which dies at its end
   * (notes_opti_ocaml.md, section 20). Not more room than that: every
   * process forked gets this heap to write in, and copies what it
   * writes *)
  Gc.set { (Gc.get ()) with minor_heap_size = 4 * 1024 * 1024 };
  Printexc.record_backtrace true;
  let (net, done_before) =
    match from with
    | None -> (fresh (), 0)
    | Some path -> (
        match Weights.of_string (read path) with
        | Error why -> failwith why
        | Ok w -> (
            match Policy_value.of_weights w with
            | Error why -> failwith why
            | Ok net -> (net, Option.value ~default:0 (Option.bind (Weights.note w "iterations") int_of_string_opt))))
  in
  Printf.printf "%d numbers; from iteration %d; %d processes\n%!" (Policy_value.parameters net) done_before workers;
  let t0 = Unix.gettimeofday () in
  let learner = ref { (Alphazero.learner ~seed:5 net) with iteration = done_before } in
  (* the newest lessons, newest first *)
  let remembered = ref [||] in
  let measured = ref (s.measure net) in
  Printf.printf "before: %s\n%!" !measured;
  for _ = 1 to iterations do
    let began = Unix.gettimeofday () in
    let fresh = games s !learner.net (!learner.iteration + 1) in
    let played = Unix.gettimeofday () in
    let all = Array.append (Array.of_list fresh) !remembered in
    remembered := Array.sub all 0 (min s.remembered (Array.length all));
    let (l, loss) =
      if s.learners > 1 then learn_apart s !learner !remembered
      else
        (* one learner, in this process: the loop's own, which
         * remembers for itself, and whole *)
        Alphazero.learn
          ~schedule:{ games = s.games; steps = s.steps; batch = s.batch; remembered = s.remembered }
          !learner (List.map lesson fresh)
    in
    learner := l;
    Printf.printf "iteration %3d  %5.0f s  (its games %.0f s, its steps %.0f s)  loss %.3f  %d lessons\n%!" l.iteration
      (Unix.gettimeofday () -. t0) (played -. began)
      (Unix.gettimeofday () -. played)
      loss (Array.length !remembered);
    if l.iteration mod s.every = 0 then (
      measured := s.measure l.net;
      Printf.printf "  %s\n%!" !measured);
    let notes =
      s.notes
      @ [
          ("iterations", string_of_int l.iteration);
          ( "an-iteration",
            Printf.sprintf "%d games against itself at %d playouts, then %d steps of %d by %d learners" s.games
              s.settings.playouts s.steps s.batch s.learners );
          ("measured", !measured);
        ]
    in
    let oc = open_out_bin out in
    output_string oc (Weights.to_string (Policy_value.to_weights ~notes l.net));
    close_out oc
  done
