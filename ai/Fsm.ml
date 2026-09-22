(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type ('state, 'context) rule = { from : 'state; label : string; guard : 'context -> int -> bool; target : 'state }
type ('state, 'context) machine = ('state, 'context) rule list
type 'state run = { state : 'state; since : int; fired : string option }

let start (s : 'state) : 'state run = { state = s; since = 0; fired = None }

let step (machine : ('state, 'context) machine) (context : 'context) (run : 'state run) : 'state run =
  (* [since + 1]: the steps spent in the state, this one included *)
  let since = run.since + 1 in
  match List.find_opt (fun r -> r.from = run.state && r.guard context since) machine with
  | Some r -> { state = r.target; since = 0; fired = Some r.label }
  | None -> { run with since; fired = None }

let after (n : int) (_ : 'context) (since : int) : bool = since >= n

let states (machine : ('state, 'context) machine) : 'state list =
  List.fold_left
    (fun acc r -> List.fold_left (fun acc s -> if List.mem s acc then acc else acc @ [ s ]) acc [ r.from; r.target ])
    [] machine
