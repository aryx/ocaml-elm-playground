(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The website's index pages (run by the Makefile's website target, from
 * the repository's root):
 *
 *   make_website https://aryx.github.io/assets
 *
 * writes docs/games/index.html and docs/apps/index.html, from CATALOG.md:
 * a section per genre or category, its definition, and a row per
 * program -- its thumbnail, what it is after, a link to play it and one
 * to its source. The programs and their thumbnails are in the assets
 * repository (the argument), too many to commit here; the pages that
 * run them, docs/games/<genre>/<Name>.html, are here.
 *
 * And docs/examples/index.html, from the pages in examples/web/ and
 * examples/svg/ that have an examples/<Name>.ml (the examples are not in
 * the catalogue): the 2D and 3D
 * examples side by side, a 3D one on WebGL and drawn as SVG.
 *
 * A stopgap: the web's launcher, tinybox's menu in the browser, is to
 * come.
 *)

let github = "https://github.com/aryx/ocaml-elm-playground/blob/master/"

let read (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write (path : string) (s : string) : unit =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

let escape (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let page ~(title : string) (body : string) : string =
  Printf.sprintf
    {|<!DOCTYPE html>
<html>
  <head>
    <title>%s</title>
    <link rel="stylesheet" href="../odoc.support/odoc.css"/>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <style>
      td { vertical-align: top; padding: 4px 8px; }
      td img { width: 125px; height: 125px; }
    </style>
  </head>
  <body>
    <main class="content">
      <p><a href="../index.html">up</a></p>
      <h1>%s</h1>
%s
    </main>
  </body>
</html>
|}
    title title body

(*****************************************************************************)
(* Games and apps *)
(*****************************************************************************)

(* games/platform/TinyMario.ml -> "platform", its page's directory
 * under docs/games/ (its program's is js/games/platform/ in the assets) *)
let subdir (p : Catalogue.program) : string = Filename.basename (Filename.dirname p.source)

let row ~(assets : string) ~(kind : string) (p : Catalogue.program) : string =
  let play = Printf.sprintf "%s/%s.html" (subdir p) p.name in
  Printf.sprintf
    {|<tr><td><a href="%s"><img loading="lazy" src="%s/pngs/%s.png" alt="%s"/></a></td>
<td><a href="%s"><b>%s</b></a> (%s, %d)<br/>%s<br/><i>after %s</i><br/><a href="%s%s">source</a></td></tr>
|}
    play assets p.name p.name play p.name
    (if kind = "games" then p.look else "app")
    p.year (escape p.one_line) (escape p.after) github p.source

let index ~(assets : string) ~(games : bool) (sections : Catalogue.section list) : string =
  let kind = if games then "games" else "apps" in
  sections
  |> List.filter (fun (s : Catalogue.section) -> s.games = games)
  |> List.map (fun (s : Catalogue.section) ->
         Printf.sprintf "<h2>%s</h2>\n<p>%s</p>\n<table>\n%s</table>\n" (escape s.title)
           (escape (Catalogue.plain s.intro))
           (String.concat "" (List.map (row ~assets ~kind) s.programs)))
  |> String.concat ""
  |> page ~title:(String.capitalize_ascii kind)

(*****************************************************************************)
(* Examples *)
(*****************************************************************************)

let pages (dir : string) : string list =
  Sys.readdir dir |> Array.to_list
  |> List.filter_map (fun f -> if Filename.check_suffix f ".html" then Some (Filename.chop_suffix f ".html") else None)

let examples () : string =
  let web = pages "examples/web" and svg = pages "examples/svg" in
  List.sort_uniq compare (web @ svg)
  (* not the pages without a program (Template.html) *)
  |> List.filter (fun name -> Sys.file_exists ("examples/" ^ name ^ ".ml"))
  |> List.map (fun name ->
         let link dir label = Printf.sprintf {| <a href="%s%s.html">%s</a>|} dir name label in
         Printf.sprintf {|<li>%s:%s%s <a href="%sexamples/%s.ml">source</a></li>|} name
           (if List.mem name web then link "" (if List.mem name svg then "webgl" else "run") else "")
           (if List.mem name svg then link "svg/" "svg" else "")
           github name)
  |> String.concat "\n"
  |> Printf.sprintf "<ul>\n%s\n</ul>\n"
  |> page ~title:"Examples"

let () =
  match Sys.argv with
  | [| _; assets |] ->
      let sections = Catalogue.parse (read "CATALOG.md") in
      write "docs/games/index.html" (index ~assets ~games:true sections);
      write "docs/apps/index.html" (index ~assets ~games:false sections);
      write "docs/examples/index.html" (examples ())
  | _ -> failwith "usage: make_website <assets url>"
