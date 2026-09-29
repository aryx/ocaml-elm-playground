(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* A directory's code map on a web page: tinybox codemap <dir> in a
 * browser, for a project's site (ix's, xix's, principia's), where a link
 * to a part of the code opens the map there instead of GitHub's file
 * view (the author). Its page, codemap.html, loads codemap.bc.js (this
 * program, the same for every project) and it fetches the project's
 * code, codemap_data.txt beside the page, one file written by
 * make_codemap_data <dir> (Code_bundle's format): its sources, its
 * configs and what they import. make codemap-web DIR=... OUT=... makes
 * the three.
 *
 * The page's URL says where the map opens (Codemap.opened_at):
 *
 *   codemap.html?focus=version_control              a folder
 *   codemap.html?focus=version_control/Commands.ml  a file, flown to
 *   codemap.html?focus=kernel/proc.c&line=120       its definition there
 *   codemap.html?def=diff                           a definition by name
 *   codemap.html?data=other.txt                     another bundle
 *
 * The page may name its bundle (data_url): ix's site keeps only its page,
 * the bundle and this program in the assets repository (make
 * codemap-web). *
 * the other flags as the map's own (style=streets). *)

(* claude: where the bundle is: the URL's data=, else the page's own
 * choice (a line <script>var codemap_data = "..."</script> before this
 * program's: a project's site keeps its page, the bundle and this
 * program sit in the assets repository), else beside the page *)
let data_url () =
  match List.assoc_opt "data" (Playground_platform.flags ()) with
  | Some u -> u
  | None -> (
      match Ojs.get_prop_ascii Ojs.global "codemap_data" with
      | v when Ojs.type_of v = "string" -> Ojs.string_of_js v
      | _ -> "codemap_data.txt")

(* the bundle, fetched once, read into a directory the first time the
 * map asks: None while on its way *)
let fetched : (string, string) result option ref = ref None
let directory : (Codemap.directory, string) result option ref = ref None

let get () : (Codemap.directory, string) result option =
  match (!directory, !fetched) with
  | Some d, _ -> Some d
  | None, None -> None
  | None, Some (Error why) -> Some (Error why)
  | None, Some (Ok bytes) ->
      let d =
        match Code_bundle.of_string bytes with
        | exception Failure _ -> Error "not read"
        | b ->
            let read p = List.assoc_opt p b.jsonnet in
            (* claude: a config's mistake said in the console, the map
             * drawn without it, as tinybox codemap <dir> *)
            let guide, mistakes = Code_guide.load ~read b.configs in
            List.iter (fun m -> prerr_endline ("codemap: " ^ m)) mistakes;
            Ok { Codemap.guide = Some guide; colours = Some (Code_guide.colours guide); roots = Some b.roots; name = b.name; sources = b.sources }
      in
      directory := Some d;
      Some d

let main =
  Program.main __MODULE__ (fun () ->
      Fetch_bytes.get (data_url ()) ~ok:(fun s -> fetched := Some (Ok s)) ~failed:(fun why -> fetched := Some (Error why));
      Codemap.run_loading ~get)
