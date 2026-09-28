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

(* each file its path, a newline, its length, a newline, its text *)
let parse (s : string) : (string * string) list =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let nl1 = String.index_from s i '\n' in
      let nl2 = String.index_from s (nl1 + 1) '\n' in
      let path = String.sub s i (nl1 - i) in
      let n = int_of_string (String.sub s (nl1 + 1) (nl2 - nl1 - 1)) in
      go (nl2 + 1 + n) ((path, String.sub s (nl2 + 1) n) :: acc)
  in
  go 0 []

let sources : Tinybox_menu.sources option ref = ref None

(* claude: the response's bytes as an OCaml string, built by the browser
 * a slice at a time (String.fromCharCode of 32 KB), then taken as it is
 * (Js.to_bytestring, a byte a character). The platform's fetch_web does
 * it a byte at a time (String.init over the Uint8Array): for 10 MB, 1.4 s
 * of the page frozen, most of it the garbage collector; this, a few
 * dozen ms.
 *
 *   old: String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i)))
 *)
let bytes_of_response (response : Ojs.t) : string =
  let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| response |] in
  let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
  let from_char_code = Ojs.get_prop_ascii (Ojs.get_prop_ascii Ojs.global "String") "fromCharCode" in
  let chunk = 32768 in
  let parts =
    List.init ((n + chunk - 1) / chunk) (fun k ->
        let slice = Ojs.call bytes "subarray" [| Ojs.int_to_js (k * chunk); Ojs.int_to_js (min n ((k + 1) * chunk)) |] in
        Ojs.call from_char_code "apply" [| Ojs.null; slice |])
  in
  let whole = Ojs.call (Ojs.list_to_js (fun x -> x) parts) "join" [| Ojs.string_to_js "" |] in
  (* claude: an Ojs.t is a JavaScript value as it is, as a Js.t is:
   * the two libraries' types for the same thing *)
  Js_of_ocaml.Js.to_bytestring (Obj.magic whole : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t)

(* an XMLHttpRequest for bytes, as the web platform's fetch_web *)
let fetch_sources () : unit =
  sources := Some Tinybox_menu.Loading;
  let xhr = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "XMLHttpRequest") [||] in
  ignore (Ojs.call xhr "open" [| Ojs.string_to_js "GET"; Ojs.string_to_js (sources_url ()) |]);
  Ojs.set_prop_ascii xhr "responseType" (Ojs.string_to_js "arraybuffer");
  let failed why = sources := Some (Tinybox_menu.No_sources why) in
  Ojs.set_prop_ascii xhr "onload"
    (Ojs.fun_to_js 1 (fun _ ->
         let status = Ojs.int_of_js (Ojs.get_prop_ascii xhr "status") in
         if status >= 200 && status < 300 then
           match parse (bytes_of_response (Ojs.get_prop_ascii xhr "response")) with
           | files -> sources := Some (Tinybox_menu.Sources files)
           | exception _ -> failed "not read"
         else failed (Printf.sprintf "not found (%d)" status)));
  Ojs.set_prop_ascii xhr "onerror" (Ojs.fun_to_js 1 (fun _ -> failed "no answer"));
  ignore (Ojs.call xhr "send" [||])

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
