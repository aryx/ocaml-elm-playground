(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bot.mli *)

type ('world, 'senses, 'intent) t = {
  sense : 'senses option -> 'world -> 'senses;
  decide : 'senses -> 'intent;
  delay : int;
  rate : int;
  reflex : 'world -> 'intent -> 'intent;
}

let make ?(delay = 0) ?(rate = 1) ?(reflex = fun _ intent -> intent) ~(sense : 'senses option -> 'world -> 'senses)
    ~(decide : 'senses -> 'intent) () : ('world, 'senses, 'intent) t =
  { sense; decide; delay; rate = max 1 rate; reflex }

(* [memory] holds the last [delay] + 1 senses, the newest first *)
type ('senses, 'intent) running = { memory : 'senses list; last : 'intent; frame : int }

let start (intent : 'intent) : ('senses, 'intent) running = { memory = []; last = intent; frame = 0 }

let rec nth_or_last (l : 'a list) (n : int) : 'a option =
  match l with [] -> None | [ x ] -> Some x | x :: rest -> if n <= 0 then Some x else nth_or_last rest (n - 1)

let step (bot : ('world, 'senses, 'intent) t) (world : 'world) (r : ('senses, 'intent) running) :
    'intent * ('senses, 'intent) running =
  let memory = bot.sense (match r.memory with s :: _ -> Some s | [] -> None) world :: r.memory in
  (* keep what the delay needs, no more *)
  let memory = List.filteri (fun i _ -> i <= bot.delay) memory in
  let intent =
    if r.frame mod bot.rate <> 0 then r.last
    else match nth_or_last memory bot.delay with Some s -> bot.decide s | None -> r.last
  in
  (* claude: the decision is what is kept and repeated; the reflex
   * adjusts it to the world of this frame, every frame *)
  (bot.reflex world intent, { memory; last = intent; frame = r.frame + 1 })

let last_senses (r : ('senses, 'intent) running) : 'senses option =
  match r.memory with s :: _ -> Some s | [] -> None

(* a smooth wobble in [-1, 1]: two sines that don't share a period, so
 * it doesn't repeat visibly *)
let wobble (t : float) (seed : int) : float =
  let s = float_of_int seed in
  (sin ((t *. 0.37) +. (s *. 1.7)) +. sin ((t *. 0.11) +. (s *. 0.9))) /. 2.

let aim_error ~(spread : float) ~(settle : float) ~(seen_for : int) ~(seed : int) () : float =
  let decay = if settle <= 0. then 0. else 0.5 ** (float_of_int seen_for /. settle) in
  spread *. decay *. wobble (float_of_int seen_for) seed
