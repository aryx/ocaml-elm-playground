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
 * a section per genre or category, its definition, and a card per
 * program -- its thumbnail, what it is after, a link to play it, one to
 * its source, and one to its code map (tinybox's, on the web:
 * tinybox.html?code=<Name>). Each section and card an anchor, a link
 * to give: games/#shoot-em-up, games/#TinyPong. The programs and their thumbnails are in the assets
 * repository (the argument), too many to commit here; the pages that
 * run them, docs/games/<genre>/<Name>.html, are here.
 *
 * docs/by-size/index.html: the same programs in one list, the smallest
 * first, as tinybox's menu can sort them.
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
      .grid { display: grid; gap: 16px;
              grid-template-columns: repeat(auto-fill, minmax(160px, 1fr)); }
      .card { font-size: 0.85em; line-height: 1.3; }
      .card img { width: 100%%; aspect-ratio: 1; display: block; }
      .card .name { font-weight: bold; }
      .card .meta, .card .src { color: #777; }
      h2 a { color: inherit; text-decoration: none; }
      :target { scroll-margin-top: 12px; }
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

(* a card, as in tinybox's menu: several side by side, as many as the
 * window has room for; what it is after in its tooltip *)
let card ~(assets : string) ~(base : string) ~(meta : string) (p : Catalogue.program) : string =
  let play = Printf.sprintf "%s%s/%s.html" base (subdir p) p.name in
  Printf.sprintf
    {|<div class="card" id="%s" title="after %s"><a href="%s"><img loading="lazy" src="%s/pngs/%s.png" alt="%s"/>
<span class="name">%s</span></a> <span class="meta">%s</span><br/>%s <a class="src" href="%s%s">source</a> &middot;
<a class="src" href="../tinybox.html?code=%s">code map</a></div>
|}
    p.name (escape p.after) play assets p.name p.name p.name meta (escape p.one_line) github p.source p.name

(* "2D, 1978", "app, 1983" *)
let look_year ~(games : bool) (p : Catalogue.program) : string =
  Printf.sprintf "%s, %d" (if games then p.look else "app") p.year

(* claude: a section's anchor, from its title (several apps' sections
 * share a directory, apps/office/): "Shoot 'em up" -> "shoot-em-up",
 * games/#shoot-em-up a link to give *)
let anchor (title : string) : string =
  let b = Buffer.create (String.length title) in
  String.iter
    (fun c ->
      match Char.lowercase_ascii c with
      | ('a' .. 'z' | '0' .. '9') as c -> Buffer.add_char b c
      | ' ' | '-' | '/' -> if Buffer.length b > 0 && Buffer.nth b (Buffer.length b - 1) <> '-' then Buffer.add_char b '-'
      | _ -> ())
    title;
  Buffer.contents b

let index ~(assets : string) ~(games : bool) (sections : Catalogue.section list) : string =
  let kind = if games then "games" else "apps" in
  let sections = List.filter (fun (s : Catalogue.section) -> s.games = games) sections in
  (* the sections first, each a link to its own *)
  let contents =
    sections
    |> List.map (fun (s : Catalogue.section) -> Printf.sprintf {|<a href="#%s">%s</a>|} (anchor s.title) (escape s.title))
    |> String.concat " &middot;\n"
    |> Printf.sprintf "<p>%s</p>\n"
  in
  sections
  |> List.map (fun (s : Catalogue.section) ->
         Printf.sprintf "<h2 id=\"%s\"><a href=\"#%s\">%s</a></h2>\n<p>%s</p>\n<div class=\"grid\">\n%s</div>\n" (anchor s.title)
           (anchor s.title) (escape s.title)
           (escape (Catalogue.plain s.intro))
           (String.concat ""
              (List.map (fun p -> card ~assets ~base:"" ~meta:(look_year ~games p) p) s.programs)))
  |> String.concat ""
  |> ( ^ ) contents
  |> page ~title:(String.capitalize_ascii kind)

(*****************************************************************************)
(* By size *)
(*****************************************************************************)

(* docs/by-size/: the games and apps together, the smallest first, as
 * tinybox's menu sorts them: a program's own code, its folder's and
 * its kits' and languages' (Code_deps.own_size), what its budget counts *)
let by_size ~(assets : string) (sections : Catalogue.section list) : string =
  let sources = Code_deps.repository_sources ~root:"." in
  sections
  |> List.concat_map (fun (s : Catalogue.section) ->
         List.map (fun (p : Catalogue.program) -> (s.games, p, Code_deps.own_size sources p.source)) s.programs)
  |> List.stable_sort (fun (_, _, (_, a)) (_, _, (_, b)) -> compare a b)
  |> List.map (fun (games, (p : Catalogue.program), (files, lines)) ->
         let base = if games then "../games/" else "../apps/" in
         let meta = Printf.sprintf "%s, %d lines, %d file%s" (look_year ~games p) lines files (if files = 1 then "" else "s") in
         card ~assets ~base ~meta p)
  |> String.concat ""
  |> Printf.sprintf
       "<p>Every game and app, the smallest first: its own code's lines -- its file, its folder's and its kits' \
        modules; not the Playground's nor the libraries'. At most %d each, but for the few that carry a whole \
        language.</p>\n<div class=\"grid\">\n%s</div>\n"
       Code_deps.budget
  |> page ~title:"By size"

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
      if not (Sys.file_exists "docs/by-size") then Sys.mkdir "docs/by-size" 0o755;
      write "docs/by-size/index.html" (by_size ~assets sections);
      write "docs/examples/index.html" (examples ())
  | _ -> failwith "usage: make_website <assets url>"
