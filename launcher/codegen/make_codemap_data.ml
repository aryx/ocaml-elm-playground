(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A directory's code as one file, for its code map on a web page
 * (launcher/codemap/web, Codemap_web; make codemap-web):
 *
 *   make_codemap_data ~/github/ix > codemap_data.txt
 *   make_codemap_data ~/github/ix ix > codemap_data.txt   (its name)
 *
 * the files tinybox codemap <dir> would show and its configs, read the
 * same way (Code_walk), written in Code_bundle's format, with the
 * definitions' uses counted (Code_rank). The name is the directory's
 * own by default.
 *
 *   make_codemap_data -tinybox > tinybox_sources.txt   (from the root)
 *
 * claude: tinybox's sources for the web's menu (Tinybox_web), what
 * make_tinybox_data sources-file writes, with their "#rank" first: the
 * first a on a program's map lexed the whole repository in the browser
 * and froze the page. Here and not in make_tinybox_data, whose other
 * uses (the native menu's data, thumbnails) would be rebuilt by any
 * change of the code map. *)

(* claude: counted as the menu's maps count them (Code_map_base, the
 * map's files and those beyond it: every source, no roots) *)
let tinybox () =
  let sources = Code_deps.repository_sources ~root:"." in
  let files = List.map (fun (p, src) -> (p, lazy (Code_file.make p src))) sources in
  let rank = Code_rank.to_string (Code_rank.compute ~roots:[] files) in
  set_binary_mode_out stdout true;
  List.iter (fun (path, text) -> Printf.printf "%s\n%d\n%s" path (String.length text) text)
    ((("#rank", rank) :: sources) @ Code_deps.repository_configs ~root:".")

let () =
  if Array.to_list Sys.argv = [ Sys.argv.(0); "-tinybox" ] then begin
    tinybox ();
    exit 0
  end;
  let dir, name =
    match Array.to_list Sys.argv with
    | [ _; dir ] ->
        let abs = if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir in
        let rec name p = match Filename.basename p with "." | "" -> name (Filename.dirname p) | n -> n in
        (dir, name abs)
    | [ _; dir; name ] -> (dir, name)
    | _ ->
        prerr_endline "usage: make_codemap_data <dir> [name]";
        exit 2
  in
  let w = Code_walk.walk dir in
  if w.sources = [] then begin
    prerr_endline ("make_codemap_data: no OCaml nor C file under " ^ dir);
    exit 1
  end;
  (* claude: and what lexing every file tells, counted here, natively,
   * once: in a browser the first a froze the page (Code_rank) *)
  let files = List.map (fun (p, src) -> (p, lazy (Code_file.make p src))) w.sources in
  let rank = Code_rank.to_string (Code_rank.compute ~roots:w.roots files) in
  set_binary_mode_out stdout true;
  print_string (Code_bundle.to_string { name; roots = w.roots; sources = w.sources; configs = w.configs; jsonnet = w.jsonnet; rank = Some rank })
