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
(* Many processes at once *)
(*****************************************************************************)

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
  remembered : int;
  also : Policy_value.lesson -> Policy_value.lesson list;
  steps : int;
  batch : int;
  learners : int;
  measure : Policy_value.t -> string;
  every : int;
  notes : (string * string) list;
}

(* an iteration's games, shared out among the processes, and each
 * lesson with the other ways it is a lesson *)
let games (s : ('state, 'move) setup) (net : Policy_value.t) (iteration : int) : Policy_value.lesson list =
  let each = max 1 (s.games / workers) in
  let played =
    together
      (List.init workers (fun w () ->
           List.concat
             (List.init each (fun g ->
                  fst (Alphazero.play ~settings:s.settings ~seed:((iteration * 100_000) + (w * 1000) + g) s.board net)))))
  in
  let lessons = List.concat played in
  lessons @ List.concat_map s.also lessons

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
let learn_apart (s : ('state, 'move) setup) (l : Alphazero.learner) (fresh : Policy_value.lesson list) :
    Alphazero.learner * float =
  let all = Array.append (Array.of_list fresh) l.lessons in
  let lessons = Array.sub all 0 (min s.remembered (Array.length all)) in
  let arrived =
    together
      (List.init s.learners (fun w () ->
           let draws = Lehmer.make ((l.iteration * 100) + w) in
           let rec go (net : Policy_value.t) (loss : float) (n : int) : Policy_value.t * float =
             if n = 0 then (net, loss)
             else
               let (net, loss) =
                 Policy_value.step net (Array.init s.batch (fun _ -> lessons.(Lehmer.int draws (Array.length lessons))))
               in
               go net loss (n - 1)
           in
           go l.net 0. s.steps))
  in
  let (first, _) = List.hd arrived in
  let share = 1. /. float_of_int s.learners in
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
  ({ l with net = { first with matrices }; lessons; iteration = l.iteration + 1 }, loss)

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
  let measured = ref (s.measure net) in
  Printf.printf "before: %s\n%!" !measured;
  for _ = 1 to iterations do
    let began = Unix.gettimeofday () in
    let fresh = games s !learner.net (!learner.iteration + 1) in
    let played = Unix.gettimeofday () in
    let (l, loss) =
      if s.learners > 1 then learn_apart s !learner fresh
      else
        Alphazero.learn
          ~schedule:{ games = s.games; steps = s.steps; batch = s.batch; remembered = s.remembered }
          !learner fresh
    in
    learner := l;
    Printf.printf "iteration %3d  %5.0f s  (its games %.0f s, its steps %.0f s)  loss %.3f  %d lessons\n%!" l.iteration
      (Unix.gettimeofday () -. t0) (played -. began)
      (Unix.gettimeofday () -. played)
      loss (Array.length l.lessons);
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
