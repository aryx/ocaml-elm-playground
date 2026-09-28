(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* TinyMyst's stills, ray traced once, as Cyan's were overnight on
 * their Macintoshes (StrataVision 3D): every card in every state it can
 * show (Myst_island.stills), written as JPEGs into a directory:
 *
 *   dune exec games/adventure/myst/make_stills.exe -- games/adventure/myst/stills
 *
 * The game embeds them (the dune rule making Myst_stills.ml), and a test
 * checks they are still the scene's (tests/Unit_myst.ml). Run it again
 * after changing the island. *)

let () =
  Cap.main (fun caps ->
      let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
      List.iter
        (fun ((card : Myst_island.card), fields) ->
          let name = Myst_island.still_name card fields in
          let t0 = Unix.gettimeofday () in
          let picture =
            Raytrace.render ~options:Myst_island.options
              (Povray.raytrace_scene (Myst_island.scene card fields))
              ~width:Myst_island.width ~height:Myst_island.height
          in
          let path = Filename.concat dir (name ^ ".jpg") in
          let (_ : Cap.FS_.open_out) = caps#open_out path in
          let oc = open_out_bin path in
          output_string oc (Jpeg_encode.encode ~quality:85 picture);
          close_out oc;
          Printf.printf "%s (%.1f s)\n%!" path (Unix.gettimeofday () -. t0))
        Myst_island.stills)
