(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A build-time program (run by ../dune, from the build's root, where
 * CATALOG.md and tests/*/golden/ are): what tinybox's menu shows, as one
 * OCaml module on stdout,
 *
 *   let catalogue = "# Catalogue of the games and apps\n..."
 *   let thumbnails = [ ("TinyMario", "\137PNG..."); ... ]
 *
 * a thumbnail being one of the program's golden frames, 1000 by 1000,
 * halved twice: 250 by 250. Which one: the game being played, not its
 * title screen -- its first scripted scene (Scenes_2d's and Scenes_3d's
 * [scripted], a few seconds of play: TinyMario's "run"); else, for the
 * programs without one, its first frame (CATALOG.md's screenshot). Twice, and not once to a quarter: at exactly half, bilinear's
 * point falls between four pixels, so each new pixel is their average,
 * and nothing is skipped (a quarter at once would read only 4 pixels of
 * each 16, and the thin lines of the vector games would flicker away).
 * A program whose frame is missing gets no thumbnail (the catalogue's
 * test, tests/catalog, requires one anyway).
 *
 * claude: a thumbnail takes 0.3 s (0.15 of it our PNG decoder), 55 s
 * for all of them, and the rule runs again after any golden frame is
 * approved: so the thumbnails are made in shards, by as many rules,
 * which dune runs side by side --
 *
 *   make_tinybox_data thumbnails k n   Tinybox_thumbs_k.ml, every n-th
 *                                      program from the k-th
 *   make_tinybox_data catalogue n      Tinybox_data.ml, the catalogue
 *                                      and the n shards' lists joined
 *   make_tinybox_data sources          Tinybox_sources.ml, the sources of
 *                                      the games, apps, kits, playground
 *                                      and libs, for the code map
 *                                      (codemap/) *)

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let halve (img : Rgba_image.t) : Rgba_image.t =
  Scale.resize Bilinear ~width:(img.width / 2) ~height:(img.height / 2) img

let thumbnail (png : string) : string = Png.encode (halve (halve (Png.decode png)))

(* the program's frame: its first scripted scene's, else its first *)
let frame (p : Catalogue.program) : string =
  let played (dir : string) (scenes : Golden_scene.scripted list) =
    List.find_map
      (fun ((exe, label, _, _) : Golden_scene.scripted) ->
        if Filename.basename exe = p.name then Some (Printf.sprintf "tests/%s/golden/%s_%s.png" dir p.name label) else None)
      scenes
  in
  match (played "2d" Scenes_2d.scripted, played "3d" Scenes_3d.scripted) with
  | Some f, _ | None, Some f when Sys.file_exists f -> f
  | _ -> Catalogue.golden_frame p

(* claude: the repository's own sources, as they are in _build: its
 * source files, and not the build's copies of them (a genre's web/ and
 * software/) nor the modules rules make (dune's alias modules, the
 * embedded pictures and pages, ocamllex's output), which say so on their
 * first line *)
let source_roots = [ "games"; "apps"; "gamekits"; "appkits"; "playground"; "libs" ]
let skipped_dirs = [ "web"; "software"; "svg"; "tests" ]

let generated (path : string) : bool =
  let text = read path in
  let first = match String.index_opt text '\n' with Some i -> String.sub text 0 i | None -> text in
  let starts p = String.length first >= String.length p && String.sub first 0 (String.length p) = p in
  starts "(* Auto-generated" || starts "(* generated" || starts "# " || Filename.basename path = "Hud_render.ml"
  || Filename.basename path = "Hud_render.mli"

let rec walk (dir : string) : string list =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.concat_map (fun f ->
         let path = Filename.concat dir f in
         if Sys.is_directory path then if f.[0] = '.' || List.mem f skipped_dirs then [] else walk path
         else if (Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli") && not (generated path) then [ path ]
         else [])

let sources () : string list = List.concat_map walk (List.filter Sys.file_exists source_roots)

let () =
  let catalogue = read "CATALOG.md" in
  print_string "(* generated from CATALOG.md and the golden frames by launcher/codegen/make_tinybox_data.ml *)\n";
  match Array.to_list Sys.argv with
  | [ _; "catalogue"; n ] ->
      Printf.printf "let catalogue = %S\n\n" catalogue;
      Printf.printf "let thumbnails = List.concat [ %s ]\n"
        (String.concat "; " (List.init (int_of_string n) (Printf.sprintf "Tinybox_thumbs_%d.thumbnails")))
  | [ _; "thumbnails"; k; n ] ->
      let k = int_of_string k and n = int_of_string n in
      print_string "let thumbnails = [\n";
      Catalogue.parse catalogue
      |> List.concat_map (fun (s : Catalogue.section) -> s.programs)
      |> List.iteri (fun i (p : Catalogue.program) ->
             let frame = frame p in
             if i mod n = k && Sys.file_exists frame then Printf.printf "  (%S, %S);\n" p.name (thumbnail (read frame)));
      print_string "]\n"
  | [ _; "sources" ] ->
      print_string "let sources = [\n";
      List.iter (fun path -> Printf.printf "  (%S, %S);\n" path (read path)) (sources ());
      print_string "]\n"
  | _ -> failwith "usage: make_tinybox_data (catalogue n | thumbnails k n | sources)"
