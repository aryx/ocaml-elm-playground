(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Push.mli *)

let chain ~(blocked : int * int -> bool) ~(pushable : int * int -> bool) ?(limit = max_int) ((c, r) : int * int) ((dc, dr) : int * int) : (int * int) list option =
  let rec walk (c, r) acc =
    let next = (c + dc, r + dr) in
    if pushable next then walk next (next :: acc)
    else if blocked next || List.length acc > limit then None
    else Some (List.rev acc)
  in
  walk (c, r) []
