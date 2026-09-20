(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Focus.mli *)

type t = {
  who : Widget.id option;
  (* this frame's widgets, newest first *)
  order : Widget.id list;
  (* and the last frame's, in the order they were asked for: what Tab
     walks, since this frame's is not built yet when Tab arrives *)
  last : Widget.id list;
}

let none = { who = None; order = []; last = [] }
let frame t = { t with last = List.rev t.order; order = [] }
let saw id t = { t with order = id :: t.order }
let has id t = t.who = Some id
let give id t = { t with who = Some id }
let clear t = { t with who = None }

(* the one after [who] in [order], wrapping round; the first if nobody
 * has it, or if whoever has it is not in the order any more (a widget
 * that went away while focused) *)
let after order who =
  let rec go = function
    | [] -> None
    | [ last ] -> if Some last = who then List.nth_opt order 0 else go []
    | a :: (b :: _ as rest) -> if Some a = who then Some b else go rest
  in
  match order with [] -> None | first :: _ -> ( match go order with Some id -> Some id | None -> Some first)

let next t = { t with who = after t.last t.who }
let previous t = { t with who = after (List.rev t.last) t.who }
