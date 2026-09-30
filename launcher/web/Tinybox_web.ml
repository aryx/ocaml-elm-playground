(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* tinybox's menu in a browser (plan_tinybox_web.md): the same menu as
 * natively (Tinybox_menu), over a host that links no program. On the
 * web, the child process is the page: a program chosen is started by
 * loading its own page, the website's <dir>/<Name>.html beside this
 * one's (docs/tinybox.html, made by the Makefile's website target),
 * the browser's Back returning to the menu. The thumbnails are the
 * website's too, fetched by URL from the assets repository, where make
 * website puts them; and so are the sources, for the code map (see "The
 * sources"). No previews.
 *
 * What it uses: Tinybox_menu and its data (the catalogue, the sizes),
 * Ojs for the page's location. *)

open Playground

(* where make website puts the thumbnails (pngs/<Name>.png) and the
 * sources; the flag assets=<url> for another (a local server, to try) *)
let assets_default = "https://aryx.github.io/assets"
let assets_flag : string option ref = ref None
let assets () = Option.value ~default:assets_default !assets_flag

let programs : Catalogue.program list =
  Catalogue.parse Tinybox_data.catalogue |> List.concat_map (fun (s : Catalogue.section) -> s.programs)

(* games/arcade/TinyPong.ml -> games/arcade/TinyPong.html, relative to
 * this page, at the website's root *)
let page (p : Catalogue.program) : string = Filename.dirname p.source ^ "/" ^ p.name ^ ".html"

(* the browser leaves this page for [url] *)
let go (url : string) : unit = Ojs.set_prop_ascii (Ojs.get_prop_ascii Ojs.global "location") "href" (Ojs.string_to_js url)

(*****************************************************************************)
(* The sources *)
(*****************************************************************************)

(* claude: the repository's sources, for the code map: 10 MB, too many
 * for this program, so a file beside it in the assets (make website,
 * make_tinybox_data sources-file), fetched the first time the menu asks
 * for them -- on the same site as this page, and gzipped on the way *)
let sources_url () = assets () ^ "/js/launcher/tinybox_sources.txt"

(* each file its path, a newline, its length, a newline, its text
 * (Code_bundle's format) *)
let sources : Tinybox_menu.sources option ref = ref None

let fetch_sources () : unit =
  sources := Some Tinybox_menu.Loading;
  Fetch_bytes.get (sources_url ())
    ~ok:(fun s ->
      sources :=
        Some
          (match Code_bundle.entries s with
          | files ->
              (* claude: the uses counted when the file was made
               * (make_codemap_data -tinybox): counting them here lexes
               * every file, and the first a froze the page *)
              Option.iter (fun r -> Codemap.use_rank (Code_rank.of_string r)) (List.assoc_opt "#rank" files);
              Tinybox_menu.Sources (List.filter (fun (p, _) -> p <> "#rank") files)
          | exception Failure _ -> Tinybox_menu.No_sources "not read"))
    ~failed:(fun why -> sources := Some (Tinybox_menu.No_sources why))

let get_sources () : Tinybox_menu.sources =
  match !sources with
  | Some s -> s
  | None ->
      fetch_sources ();
      Tinybox_menu.Loading

let host : Tinybox_menu.host =
  {
    runnable = List.map (fun (p : Catalogue.program) -> p.name) programs;
    thumbnail = (fun p size -> Some (image size size (Printf.sprintf "%s/pngs/%s.png" (assets ()) p.name)));
    play =
      (fun p ->
        (* claude: this page's URL first made ?chosen=<Name> (replaced, not
         * a new entry in the history): Back from the program comes back
         * to the menu on it (Tinybox_menu.initial) *)
        ignore
          (Ojs.call (Ojs.get_prop_ascii Ojs.global "history") "replaceState"
             [| Ojs.null; Ojs.string_to_js ""; Ojs.string_to_js ("?chosen=" ^ p.name) |]);
        go (page p);
        "");
    running = (fun () -> None);
    ended = (fun () -> None);
    sources = get_sources;
    preview = None;
  }

let main =
  Program.main __MODULE__ (fun () ->
      assets_flag := List.assoc_opt "assets" (Playground_platform.flags ());
      Cap.main (fun caps -> Tinybox_menu.run ~network:caps host))
