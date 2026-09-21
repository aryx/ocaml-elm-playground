(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Undo.mli *)

type 'a t = { now : 'a; past : 'a list }

let start (a : 'a) : 'a t = { now = a; past = [] }
let record (a : 'a) (h : 'a t) : 'a t = { now = a; past = h.now :: h.past }
let undo (h : 'a t) : 'a t = match h.past with before :: rest -> { now = before; past = rest } | [] -> h
