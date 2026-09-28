(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Style.mli *)

type t = { bold : bool; italic : bool; underline : bool; strike : bool; size : float }

let plain = { bold = false; italic = false; underline = false; strike = false; size = 16. }
let toggle_bold s = { s with bold = not s.bold }
let toggle_italic s = { s with italic = not s.italic }
let toggle_underline s = { s with underline = not s.underline }
let toggle_strike s = { s with strike = not s.strike }
