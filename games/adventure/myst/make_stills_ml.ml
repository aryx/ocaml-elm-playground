(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A build-time script (run by `ocaml` from ./dune): TinyMyst's stills
 * as one OCaml list of their names and bytes,
 *
 *   let all = [ ("dock-down", "\255\216..."); ... ]
 *
 * which the game looks up by Myst_island.still_name. Arguments that are
 * not .jpg files (the rule's own script) are skipped. *)

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  print_string "(* generated from stills/*.jpg by games/adventure/myst/make_stills_ml.ml *)\nlet all = [\n";
  Array.iteri
    (fun i path ->
      if i > 0 && Filename.check_suffix path ".jpg" then
        Printf.printf "  (%S, \"%s\");\n" (Filename.remove_extension (Filename.basename path)) (String.escaped (read path)))
    Sys.argv;
  print_string "]\n"
