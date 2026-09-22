(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type ('context, 'action) t =
  | Action of string * 'action
  | Condition of string * ('context -> bool)
  | Sequence of ('context, 'action) t list
  | Selector of ('context, 'action) t list
  | Not of ('context, 'action) t

(* one evaluation: success or not, the action decided (the last one on
 * the successful way), and the nodes visited, most recent first *)
let rec eval (tree : ('context, 'action) t) (c : 'context) (seen : (string * bool) list) :
    bool * 'action option * (string * bool) list =
  match tree with
  | Action (label, a) -> (true, Some a, (label, true) :: seen)
  | Condition (label, q) ->
      let ok = q c in
      (ok, None, (label, ok) :: seen)
  | Sequence children ->
      let rec go decided seen = function
        | [] -> (true, decided, seen)
        | child :: rest ->
            let ok, a, seen = eval child c seen in
            if not ok then (false, None, seen) else go (if a = None then decided else a) seen rest
      in
      go None seen children
  | Selector children ->
      let rec go seen = function
        | [] -> (false, None, seen)
        | child :: rest ->
            let ok, a, seen = eval child c seen in
            if ok then (true, a, seen) else go seen rest
      in
      go seen children
  | Not child ->
      let ok, _, seen = eval child c seen in
      (not ok, None, seen)

let decide tree c = match eval tree c [] with true, a, _ -> a | false, _, _ -> None
let path tree c = let _, _, seen = eval tree c [] in List.rev seen
