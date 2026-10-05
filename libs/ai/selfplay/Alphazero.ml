(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Alphazero.mli *)

type ('state, 'move) board = {
  game : ('state, 'move) Minimax.game;
  start : 'state;
  inputs : int;
  moves : int;
  encode : 'state -> float array;
  index : 'move -> int;
}

(*****************************************************************************)
(* The network as the search's two guesses *)
(*****************************************************************************)

let guides (b : ('state, 'move) board) (net : Policy_value.t) :
    ('state -> ('move * float) list) * ('state -> float) =
  (* the search asks for the policy of a position once per move it
   * expands there, and for its value besides: the last position's
   * opinion is kept, and one forward pass answers them all *)
  let last = ref None in
  let opinion (state : 'state) : float array * float =
    match !last with
    | Some (s, o) when s == state -> o
    | _ ->
        let o = Policy_value.opinion net (b.encode state) in
        last := Some (state, o);
        o
  in
  let prior (state : 'state) : ('move * float) list =
    let (p, _) = opinion state in
    (* only the legal moves, their shares made to sum to 1 again: the
     * network is never asked to learn the rules *)
    let legal = b.game.moves state in
    let total = List.fold_left (fun sum m -> sum +. p.(b.index m)) 0. legal in
    List.map (fun m -> (m, if total > 0. then p.(b.index m) /. total else 1. /. float_of_int (List.length legal))) legal
  in
  let evaluate (state : 'state) : float =
    let (_, v) = opinion state in
    (* the network speaks for whoever is to play, between -1 and 1;
     * the search counts MAX's share, between 0 and 1 *)
    if b.game.max_to_play state then (v +. 1.) /. 2. else (1. -. v) /. 2.
  in
  (prior, evaluate)

(*****************************************************************************)
(* A move *)
(*****************************************************************************)

type settings = {
  playouts : int;
  exploring : int; (* the first moves of a game drawn in proportion to their visits *)
  noise : float; (* how much of the root's policy is replaced by chance *)
}

let default : settings = { playouts = 50; exploring = 4; noise = 0.25 }

(* shares drawn at random, any split as likely as any other
 * (Dirichlet's distribution with all its parameters 1): each a number
 * from an exponential law, divided by their sum *)
let random_shares (state : Lehmer.state) (n : int) : float array =
  let xs = Array.init n (fun _ -> -.log (1. -. Lehmer.float state 1.)) in
  let total = Array.fold_left ( +. ) 0. xs in
  Array.map (fun x -> x /. total) xs

(* the search from a position, guided by the network: each legal
 * move's visits. [noise]: at the root only, that much of the policy
 * is replaced by random shares, so that a move the network has
 * written off still gets tried sometimes *)
let visits ?(noise = 0.) ~(seed : int) ~(playouts : int) (b : ('state, 'move) board) (net : Policy_value.t)
    (state : 'state) : ('move * int) list =
  let (prior, evaluate) = guides b net in
  let prior =
    if noise <= 0. then prior
    else
      let shaken =
        let p = prior state in
        let chance = random_shares (Lehmer.make seed) (List.length p) in
        List.mapi (fun i (m, share) -> (m, ((1. -. noise) *. share) +. (noise *. chance.(i)))) p
      in
      fun s -> if s == state then shaken else prior s
  in
  let r = Mcts.search ~seed ~prior ~evaluate b.game ~playouts state in
  List.map (fun (m, n, _) -> (m, n)) r.tried

let most_visited (tried : ('move * int) list) : 'move option =
  List.fold_left (fun best (m, n) -> match best with Some (_, k) when k >= n -> best | _ -> Some (m, n)) None tried
  |> Option.map fst

let choose ?(playouts = default.playouts) ~(seed : int) (b : ('state, 'move) board) (net : Policy_value.t)
    (state : 'state) : 'move option =
  most_visited (visits ~seed ~playouts b net state)

(* the network alone, no search: its policy's first choice among the
 * legal moves *)
let instinct (b : ('state, 'move) board) (net : Policy_value.t) (state : 'state) : 'move option =
  let (prior, _) = guides b net in
  List.fold_left (fun best (m, p) -> match best with Some (_, q) when q >= p -> best | _ -> Some (m, p)) None (prior state)
  |> Option.map fst

(*****************************************************************************)
(* A game against itself *)
(*****************************************************************************)

let play ?(settings = default) ~(seed : int) (b : ('state, 'move) board) (net : Policy_value.t) :
    Policy_value.lesson list * float =
  let draws = Lehmer.make seed in
  (* each position met, with the search's visits there as shares, and
   * who was to play: the value is filled in when the game is over *)
  let rec go (state : 'state) (n : int) (met : (float array * float array * bool) list) =
    match b.game.moves state with
    | [] -> (state, met)
    | _ ->
        let tried = visits ~noise:settings.noise ~seed:((seed * 1000) + n) ~playouts:settings.playouts b net state in
        let total = float_of_int (List.fold_left (fun sum (_, k) -> sum + k) 0 tried) in
        let policy = Array.make b.moves 0. in
        List.iter (fun (m, k) -> policy.(b.index m) <- float_of_int k /. total) tried;
        let move =
          if n < settings.exploring then
            (* early on, any move the search spent time on, as often as
               it did: the games must not all be the same game *)
            let x = Lehmer.float draws total in
            let rec pick tried sum =
              match tried with
              | [ (m, _) ] -> m
              | (m, k) :: rest -> if x < sum +. float_of_int k then m else pick rest (sum +. float_of_int k)
              | [] -> assert false
            in
            pick tried 0.
          else Option.get (most_visited tried)
        in
        go (b.game.play state move) (n + 1) ((b.encode state, policy, b.game.max_to_play state) :: met)
  in
  let (final, met) = go b.start 0 [] in
  let score = b.game.score final in
  let share = if score > 0. then 1. else if score < 0. then 0. else 0.5 in
  (* MAX's result, turned to whoever was to play at each position *)
  let for_max = (2. *. share) -. 1. in
  let lessons =
    List.rev_map
      (fun (input, policy, max_to_play) : Policy_value.lesson ->
        { input; policy; value = (if max_to_play then for_max else -.for_max) })
      met
  in
  (lessons, share)

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

type schedule = {
  games : int; (* against itself, an iteration *)
  steps : int; (* downhill, after them *)
  batch : int; (* lessons a step *)
  remembered : int; (* the newest lessons kept *)
}

let usual : schedule = { games = 20; steps = 200; batch = 32; remembered = 3000 }

type learner = {
  net : Policy_value.t;
  lessons : Policy_value.lesson array; (* newest first *)
  iteration : int;
  draws : Lehmer.state; (* the batches' dice *)
}

let learner ~(seed : int) (net : Policy_value.t) : learner = { net; lessons = [||]; iteration = 0; draws = Lehmer.make seed }

(* the second half of an iteration: these lessons remembered, then
 * steps on lessons drawn from all it remembers, not only the newest:
 * games of one evening are all alike, and learning only from them
 * forgets the rest *)
let learn ?(schedule = usual) ?(step = fun net batch -> Policy_value.step net batch) (l : learner)
    (fresh : Policy_value.lesson list) : learner * float =
  let all = Array.append (Array.of_list fresh) l.lessons in
  let lessons = Array.sub all 0 (min schedule.remembered (Array.length all)) in
  let rec steps (net : Policy_value.t) (loss : float) (n : int) : Policy_value.t * float =
    if n = 0 then (net, loss)
    else
      let batch = Array.init schedule.batch (fun _ -> lessons.(Lehmer.int l.draws (Array.length lessons))) in
      let (net, loss) = step net batch in
      steps net loss (n - 1)
  in
  let (net, loss) = steps l.net 0. schedule.steps in
  ({ l with net; lessons; iteration = l.iteration + 1 }, loss)

let iterate ?(settings = default) ?(schedule = usual) (b : ('state, 'move) board) (l : learner) : learner * float =
  let iteration = l.iteration + 1 in
  (* games against itself, by the network as it is now *)
  let fresh =
    List.concat (List.init schedule.games (fun g -> fst (play ~settings ~seed:((iteration * 1000) + g) b l.net)))
  in
  learn ~schedule l fresh
