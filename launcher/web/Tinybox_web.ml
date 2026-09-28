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
 * website puts them. No code map yet (its sources are 10 MB), nor
 * previews.
 *
 * What it uses: Tinybox_menu and its data (the catalogue, the sizes),
 * Ojs for the page's location. *)

open Playground

(* where make website puts the thumbnails: pngs/<Name>.png *)
let assets = "https://aryx.github.io/assets"

let programs : Catalogue.program list =
  Catalogue.parse Tinybox_data.catalogue |> List.concat_map (fun (s : Catalogue.section) -> s.programs)

(* games/arcade/TinyPong.ml -> games/arcade/TinyPong.html, relative to
 * this page, at the website's root *)
let page (p : Catalogue.program) : string = Filename.dirname p.source ^ "/" ^ p.name ^ ".html"

(* the browser leaves this page for [url] *)
let go (url : string) : unit = Ojs.set_prop_ascii (Ojs.get_prop_ascii Ojs.global "location") "href" (Ojs.string_to_js url)

let host : Tinybox_menu.host =
  {
    runnable = List.map (fun (p : Catalogue.program) -> p.name) programs;
    thumbnail = (fun p size -> Some (image size size (Printf.sprintf "%s/pngs/%s.png" assets p.name)));
    play =
      (fun p ->
        go (page p);
        "");
    running = (fun () -> None);
    ended = (fun () -> None);
    sources = None;
    preview = None;
  }

let main = Program.main __MODULE__ (fun () -> Cap.main (fun caps -> Tinybox_menu.run ~network:caps host))
