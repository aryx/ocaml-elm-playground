(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Which files are a program's code, found by the modules its code
   names, from its main file (Codemap.mli says how). Pure, over the
   repository's sources given as (path, contents): the code map uses it,
   and so does the build-time generator that counts every program's
   lines (launcher/codegen). *)

(* the modules [src] names: M in M.x, open M, include M *)
val modules_used : string -> string list

val count_lines : string -> int

(* [own program_path p]: [p] is the program's own code -- its folder's
   and the kits' (gamekits/, appkits/); not the Playground's nor the
   from-scratch libraries' (libs/). What a program's budget counts (at
   most 5,000 lines, tests/catalog) *)
val own : string -> string -> bool

(* [closure ~keep sources path]: [path] and the modules it names that
   pass [keep], and the modules they name, transitively, in reading
   order: [path] first, then breadth first, each .mli before its .ml *)
val closure : ?keep:(string -> bool) -> (string * string) list -> string -> string list

(* [own_size sources path]: the files of the program's own code and
   their lines *)
val own_size : (string * string) list -> string -> int * int

(* a program's budget: its own code (own_size) at most 5,000 lines --
   the libraries (libs/) and the Playground not counted. Checked by
   tests/catalog, README's "A budget" *)
val budget : int

(* [repository_sources ~root]: the repository's sources under [root]
   (games/, apps/, gamekits/, appkits/, playground/, libs/, and
   tinybox's own, launcher/), not the
   build's copies of them (web/, software/, svg/, tests/) nor generated
   modules: what tinybox embeds, and what budgets are counted over *)
val repository_sources : root:string -> (string * string) list
