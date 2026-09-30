(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_history.mli *)

type 'a t = { behind : 'a list; ahead : 'a list }

let empty = { behind = []; ahead = [] }
let visit (current : 'a) (h : 'a t) : 'a t = { behind = current :: h.behind; ahead = [] }

let back (current : 'a) (h : 'a t) : ('a * 'a t) option =
  match h.behind with e :: rest -> Some (e, { behind = rest; ahead = current :: h.ahead }) | [] -> None

let forward (current : 'a) (h : 'a t) : ('a * 'a t) option =
  match h.ahead with e :: rest -> Some (e, { ahead = rest; behind = current :: h.behind }) | [] -> None
