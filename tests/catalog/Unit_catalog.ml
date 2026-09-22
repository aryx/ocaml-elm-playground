(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_catalog.mli *)

(* the test runs in _build/default/tests/catalog *)
let root = "../.."
let path file = Filename.concat root file
let read file = In_channel.with_open_bin (path file) In_channel.input_all

(* a directory of programs, where its golden frames are, where its web
 * pages are. claude: a genre's directory has 2D and 3D games side by
 * side, so its golden frames are where the catalogue's Dir column says
 * (None): tests/3d for a 3D game, tests/2d for the others *)
let genres =
  [ "shmup"; "fighting"; "platform"; "arcade"; "puzzle"; "cards"; "adventure"; "rpg";
    "fps"; "flight"; "racing"; "sports"; "strategy"; "rhythm"; "programming" ]

(* claude: and the apps' categories, most still empty *)
let categories = [ "office"; "music"; "internet"; "devtools"; "graphics"; "system"; "gamedev" ]

let dirs =
  List.map (fun g -> ("games/" ^ g, None, "games/" ^ g ^ "/web")) genres
  @ List.map (fun c -> ("apps/" ^ c, Some "tests/2d", "apps/" ^ c ^ "/web")) categories

(* the position of [sub] in [s] from [from], if any *)
let rec find (s : string) (sub : string) (from : int) : int option =
  if from + String.length sub > String.length s then None
  else if String.sub s from (String.length sub) = sub then Some from
  else find s sub (from + 1)

(* the executables of a dune file: the words of each (names ...), the
 * comments (from ';' to the end of the line) taken out first, since a
 * program commented out is not a program *)
let names_of_dune (text : string) : string list =
  let text =
    String.split_on_char '\n' text
    |> List.map (fun l -> match String.index_opt l ';' with Some i -> String.sub l 0 i | None -> l)
    |> String.concat " "
  in
  let rec stanzas from =
    match find text "(names" from with
    | None -> []
    | Some i ->
        let start = i + String.length "(names" in
        let stop = String.index_from text start ')' in
        let words =
          String.sub text start (stop - start)
          |> String.split_on_char ' ' |> List.concat_map (String.split_on_char '\t')
          |> List.filter (fun w -> w <> "")
        in
        words @ stanzas stop
  in
  stanzas 0

(* the rows of the catalogue: "| [Name](dir/Name.ml) | ..." gives
 * "dir/Name.ml" *)
let rows (text : string) : string list =
  String.split_on_char '\n' text
  |> List.filter_map (fun line ->
         if not (String.starts_with ~prefix:"| [" line) then None
         else
           match (find line "](" 0, String.index_opt line ')') with
           | Some i, Some j when j > i -> Some (String.sub line (i + 2) (j - i - 2))
           | _ -> None)

let catalogue = lazy (rows (read "CATALOG.md"))

(* claude: the Dir column of the row linking to [source] ("2D", "2.5D",
 * "3D", "app"), the second cell of "| [Name](dir/Name.ml) | 3D | ..." *)
let dim (source : string) : string option =
  String.split_on_char '\n' (read "CATALOG.md")
  |> List.find_map (fun line ->
         match String.split_on_char '|' line with
         | _ :: name :: dir :: _ when find name ("(" ^ source ^ ")") 0 <> None -> Some (String.trim dir)
         | _ -> None)

(* (dir, name, goldens, web) of every program *)
let programs : (string * string * string option * string) list Lazy.t =
  lazy
    (List.concat_map
       (fun (dir, goldens, web) ->
         names_of_dune (read (Filename.concat dir "dune"))
         |> List.map (fun name -> (dir, name, goldens, web)))
       dirs)

let program_test (dir, name, goldens, web) =
  Testo.create name (fun () ->
      let source = Printf.sprintf "%s/%s.ml" dir name in
      if not (List.mem source (Lazy.force catalogue)) then
        Alcotest.failf "%s has no row in CATALOG.md (a link to %s)" name source;
      let goldens =
        match goldens with
        | Some goldens -> goldens
        | None -> if dim source = Some "3D" then "tests/3d" else "tests/2d"
      in
      let golden = Printf.sprintf "%s/golden/%s.png" goldens name in
      if not (Sys.file_exists (path golden)) then
        Alcotest.failf "%s has no screenshot: no golden frame %s" name golden;
      let page = Printf.sprintf "%s/%s.html" web name in
      if not (Sys.file_exists (path page)) then Alcotest.failf "%s has no web page %s" name page)

let rows_test =
  Testo.create "every row is a program" (fun () ->
      let sources =
        Lazy.force programs |> List.map (fun (dir, name, _, _) -> Printf.sprintf "%s/%s.ml" dir name)
      in
      Lazy.force catalogue
      |> List.iter (fun row ->
             if not (List.mem row sources) then
               Alcotest.failf "CATALOG.md has a row for %s, which no dune file of %s builds" row
                 (String.concat ", " (List.map (fun (d, _, _) -> d) dirs))))

let tests = rows_test :: List.map program_test (Lazy.force programs)
