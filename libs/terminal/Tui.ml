(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tui.mli *)

type event = Key of string | Tick of float

type 'model program = {
  init : 'model;
  update : event -> 'model -> 'model;
  view : 'model -> Curses.t;
  over : 'model -> bool;
}
