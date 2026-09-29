(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_facts.mli *)

let contains (s : string) (sub : string) : bool =
  let n = String.length s and m = String.length sub in
  let rec go i = i <= n - m && (String.sub s i m = sub || go (i + 1)) in
  m > 0 && go 0

(* the comments in a source's first 60 lines, each its lines, code
 * between them skipped; OCaml's (* *) and C's / * * / *)
let header ?(max = 24) (src : string) : string list =
  let lines = List.filteri (fun i _ -> i < 60) (String.split_on_char '\n' src) in
  let opens t = String.length t >= 2 && (String.sub t 0 2 = "(*" || String.sub t 0 2 = "/*") in
  let closes t = contains t "*)" || contains t "*/" in
  let rec comments acc cur = function
    | [] -> List.rev (match cur with Some c -> List.rev c :: acc | None -> acc)
    | l :: rest -> (
        let t = String.trim l in
        match cur with
        | None -> if opens t then if closes t then comments ([ l ] :: acc) None rest else comments acc (Some [ l ]) rest else comments acc None rest
        | Some c -> if closes t then comments (List.rev (l :: c) :: acc) None rest else comments acc (Some (l :: c)) rest)
  in
  (* a rule of stars, a banner's title alone, a copyright: not a header *)
  let words l = List.length (List.filter (fun w -> String.length w > 1 && String.exists (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) w) (String.split_on_char ' ' l)) in
  let telling b = (not (List.exists (fun l -> contains l "Copyright") b)) && List.fold_left (fun n l -> n + words l) 0 b >= 4 in
  match List.filter telling (comments [] None lines) with
  | b :: _ -> List.filteri (fun i _ -> i < max) b
  | [] -> []

let buf_add = Buffer.add_string

let brief ~(guide : Code_guide.t) ~(sources : (string * string) list) ~(dir : string) : string =
  let b = Buffer.create 4096 in
  let pr fmt = Printf.ksprintf (buf_add b) fmt in
  let prefix = if dir = "" then "" else dir ^ "/" in
  let in_dir p = String.length p > String.length prefix && String.sub p 0 (String.length prefix) = prefix && not (String.contains (String.sub p (String.length prefix) (String.length p - String.length prefix)) '/') in
  let under p = String.length p > String.length prefix && String.sub p 0 (String.length prefix) = prefix in
  let mine = List.filter (fun (p, _) -> in_dir p) sources in
  let files = List.map (fun (p, s) -> (p, lazy (Code_file.make p s))) sources in
  let rank = Code_rank.compute files in
  let links = Code_rank.links rank in
  let count_lines s = List.length (String.split_on_char '\n' s) in
  (* claude: how central each file is: the files naming its module
   * (Code_deps.fan_in); what size does not say (the author: games and
   * apps are like a kernel's device drivers, not its core) *)
  let fans = Code_deps.fan_in sources in
  let fan p = Option.value (Hashtbl.find_opt fans (String.capitalize_ascii (Filename.remove_extension (Filename.basename p)))) ~default:0 in
  let total = List.length (List.filter (fun (p, _) -> Filename.check_suffix p ".ml") sources) in
  let hub p = fan p >= max 30 (total / 20) in
  pr "# %s\n\n" (if dir = "" then "(the root)" else dir);
  pr "%d files here, %d lines; %d under it in all.\n\n" (List.length mine)
    (List.fold_left (fun n (_, s) -> n + count_lines s) 0 mine)
    (List.length (List.filter (fun (p, _) -> under p) sources));
  (* the central files first: what the project stands on *)
  let central = List.filter (fun (p, _) -> fan p > 0) mine |> List.map fst |> List.filter (fun p -> Filename.check_suffix p ".mli" || not (List.mem_assoc (Filename.remove_extension p ^ ".mli") mine)) in
  let central = List.sort (fun p q -> compare (fan q) (fan p)) central in
  if central <> [] then begin
    pr "Named by other files (open, include, a qualified name; of %d .ml files in all), the most central first:\n\n" total;
    List.iter (fun p -> pr "- %s: %d files%s\n" (Filename.basename p) (fan p) (if hub p then "  <- A HUB: part of the project's core" else "")) (List.filteri (fun i _ -> i < 12) central);
    pr "\n";
    if List.exists hub central then
      pr "A hub is what the project is written with: its main types and functions are capitals of the whole map, whatever its size (guidelines, Capitals: centrality). A program nobody names (a game, an app) is a driver: capitals for it, but it is not the core.\n\n"
  end;
  (* the configs' words already *)
  (match Code_guide.dir_summary guide dir with Some s -> pr "Said of it already: %s\n\n" s | None -> pr "Nothing said of it yet (no summary in it or its parent's dirs:).\n\n");
  let own = List.find_opt (fun (d : Code_guide.dir_note) -> d.dir = dir) (Code_guide.dirs guide) in
  pr "Its .codemapconfig: %s.\n\n"
    (match own with
    | Some d -> Printf.sprintf "there, %d files described, %d skeletons, %d tours" (List.length d.notes) (List.length d.skeletons) (List.length d.tours)
    | None -> "none yet");
  (* the subdirectories *)
  let subdirs =
    List.sort_uniq compare
      (List.filter_map
         (fun (p, _) ->
           if under p && not (in_dir p) then
             let rest = String.sub p (String.length prefix) (String.length p - String.length prefix) in
             Some (String.sub rest 0 (String.index rest '/'))
           else None)
         sources)
  in
  if subdirs <> [] then begin
    pr "## Subdirectories\n\n";
    List.iter
      (fun sd ->
        let path = prefix ^ sd in
        let n = List.length (List.filter (fun (p, _) -> under p && String.length p > String.length path && String.sub p 0 (String.length path + 1) = path ^ "/") sources) in
        pr "- %s/ (%d files)%s\n" sd n (match Code_guide.dir_summary guide path with Some s -> ": " ^ s | None -> ""))
      subdirs;
    pr "\n"
  end;
  (* each file *)
  List.iter
    (fun (p, src) ->
      let f = Lazy.force (List.assoc p files) in
      pr "## %s\n\n" (Filename.basename p);
      pr "%d lines, digest %s, named by %d files%s%s%s.\n\n" (count_lines src) (Code_guide.digest src) (fan p) (if hub p then " (A HUB)" else "")
        (if fan p = 0 && List.exists (fun (_, n, _) -> n = "main") f.defs then ", a program nobody names (a driver)" else "")
        (match Code_guide.file_note guide p with
        | Some n -> Printf.sprintf "; described (%s)" (match n.summary with Some s -> s | None -> "no summary")
        | None -> "");
      let head = header src in
      if head <> [] then begin
        pr "Its header:\n\n";
        List.iter (fun l -> pr "    %s\n" l) head;
        pr "\n"
      end;
      (* a section: a short title between rules of stars (the lexer marks
       * every line of a banner's comment) *)
      let sections =
        List.filter_map
          (fun (_, n, (c : Highlight_code.category)) ->
            if c = Comment_section && String.length n < 40 && not (contains n "http" || contains n "TODO" || (n <> "" && (n.[0] = '-' || n.[0] = '*'))) then Some n
            else None)
          f.defs
      in
      if sections <> [] then pr "Sections: %s.\n\n" (String.concat "; " sections);
      if f.marks <> [] then pr "Marked \"%s\" at lines %s.\n\n" Code_file.trick (String.concat ", " (List.map (fun l -> string_of_int (l + 1)) f.marks));
      (* its definitions, with their uses, the most used marked *)
      let defs =
        List.filter_map
          (fun (l, n, (c : Highlight_code.category)) ->
            let kind = match c with Def_function -> Some "def" | Def_value -> Some "def" | Def_type -> Some "type" | Def_module -> Some "module" | _ -> None in
            (* claude: an .mli's declaration counted as its .ml's definition:
             * the uses are the .ml's (Playground.mli's game: 219 files,
             * not 0) *)
            let impl = Filename.remove_extension p ^ ".ml" in
            let q, l' =
              if Filename.check_suffix p ".mli" && List.mem_assoc impl files then
                let kind_of (c : Highlight_code.category) = match c with Def_type -> 1 | Def_module -> 2 | _ -> 0 in
                match List.find_opt (fun (_, m, c') -> m = n && kind_of c' = kind_of c) (Lazy.force (List.assoc impl files)).defs with
                | Some (l', _, _) -> (impl, l')
                | None -> (p, l)
              else (p, l)
            in
            Option.map (fun k -> (l, n, k, Code_rank.uses rank q l' n, Code_rank.score rank q l' n c)) kind)
          f.defs
      in
      if defs <> [] then begin
        let scores = List.sort (fun a b -> compare b a) (List.map (fun (_, _, _, _, s) -> s) defs) in
        let top = match List.nth_opt scores (min 4 (List.length scores - 1)) with Some s -> s | None -> infinity in
        (* claude: which top-level definitions of the file call each one
         * (its uses' enclosing definitions): whether a heart is reached
         * from update or from view, the skeleton's joint (the agents'
         * lesson: a capital is not always on update's path) *)
        let tops = List.filter_map (fun (l, _, (c : Highlight_code.category)) -> match c with Def_function | Def_value | Def_type | Def_module -> Some l | _ -> None) f.defs |> List.sort_uniq compare in
        let name_of l = List.find_map (fun (l', n, _) -> if l' = l then Some n else None) f.defs in
        let enclosing line = List.fold_left (fun acc l -> if l <= line then Some l else acc) None tops in
        let callers (l : int) (n : string) : string list =
          let own = List.find_opt (fun (o : Highlight_code.occurrence) -> o.bound_at = (o.line, o.col) && o.len = String.length n) (if l < Array.length f.names then f.names.(l) else []) in
          match own with
          | None -> []
          | Some o ->
              Code_file.uses f o
              |> List.filter_map (fun (u : Highlight_code.occurrence) -> match enclosing u.line with Some e when e <> l -> name_of e | _ -> None)
              |> List.sort_uniq compare
        in
        (* the names defined twice: def: finds the first *)
        let twice = List.filter (fun (_, n, _, _, _) -> List.length (List.filter (fun (_, m, _, _, _) -> m = n) defs) > 1) defs |> List.map (fun (_, n, _, _, _) -> n) |> List.sort_uniq compare in
        if twice <> [] then pr "Defined twice (def: finds the first; the second needs another anchor): %s.\n\n" (String.concat ", " twice);
        (* a program's Model-View-Update names, those the template assumes *)
        if List.exists (fun (_, n, _) -> n = "main") f.defs then begin
          let has k n = List.exists (fun (_, m, kind, _, _) -> m = n && kind = k) defs in
          let say k n = Printf.sprintf "%s:%s %s" k n (if has k n then "yes" else "NO") in
          pr "The template's names (skeletons.libsonnet): %s, %s, %s, %s.\n\n" (say "type" "model") (say "def" "initial_model") (say "def" "update") (say "def" "view")
        end;
        pr "Definitions (anchor, line, uses here / from other files in N files; * the most used; called by: the file's definitions using it):\n\n";
        List.iter
          (fun (l, n, k, (u : Code_rank.use), s) ->
            pr "- %s%s:%s, line %d, %d / %d in %d%s%s\n" (if s >= top then "* " else "") k n (l + 1) u.own u.others u.files
              (match callers l n with [] -> "" | cs -> "; called by " ^ String.concat ", " (List.filteri (fun i _ -> i < 6) cs))
              (match Code_rank.users rank p l n with [] -> "" | us -> " (from " ^ String.concat ", " (List.map (fun (q, k) -> Printf.sprintf "%s %d" (Filename.basename q) k) (List.filteri (fun i _ -> i < 3) us)) ^ ")"))
          defs;
        pr "\n"
      end;
      let uses = List.filter_map (fun (a, b, n) -> if a = p then Some (b, n) else None) links |> List.sort (fun (_, x) (_, y) -> compare y x) in
      let users = List.filter_map (fun (a, b, n) -> if b = p then Some (a, n) else None) links |> List.sort (fun (_, x) (_, y) -> compare y x) in
      let show l = String.concat ", " (List.map (fun (q, n) -> Printf.sprintf "%s (%d)" q n) (List.filteri (fun i _ -> i < 8) l)) in
      if uses <> [] then pr "Uses: %s.\n\n" (show uses);
      if users <> [] then pr "Used by: %s.\n\n" (show users);
      if List.exists (fun (_, n, _) -> n = "main") f.defs then pr "A program: it has a main.\n\n")
    mine;
  Buffer.contents b

(* claude: what the configs miss, as -check says it (the author, after the
 * first pass left the Playground's core without a capital and most games
 * without a skeleton: "lessons learned from those missings?"): a program
 * with no skeleton; a hub (named by a twentieth of the project or more)
 * with no capital, in its .ml or its .mli *)
let coverage ~(guide : Code_guide.t) ~(sources : (string * string) list) : string list =
  let fans = Code_deps.fan_in sources in
  let fan p = Option.value (Hashtbl.find_opt fans (String.capitalize_ascii (Filename.remove_extension (Filename.basename p)))) ~default:0 in
  let total = List.length (List.filter (fun (p, _) -> Filename.check_suffix p ".ml") sources) in
  let hub p = fan p >= max 30 (total / 20) in
  let described p = Code_guide.file_note guide p <> None in
  let capitals p = match Code_guide.file_note guide p with Some n -> n.capitals <> [] | None -> false in
  let twin p = if Filename.check_suffix p ".mli" then Filename.remove_extension p ^ ".ml" else Filename.remove_extension p ^ ".mli" in
  (* only where configs are written: a directory whose config describes files *)
  let in_scope p = List.exists (fun (d : Code_guide.dir_note) -> d.dir = (match Filename.dirname p with "." -> "" | x -> x) && d.notes <> []) (Code_guide.dirs guide) in
  let programs =
    List.filter_map
      (fun (p, src) ->
        if Filename.check_suffix p ".ml" && in_scope p && (contains src "\nlet main " || contains src "\nlet main=") && described p && Code_guide.skeletons_of guide p = [] then
          Some (p ^ ": a program with no skeleton (skeletons.libsonnet: game, drawn or mvu, one line)")
        else None)
      sources
  in
  let hubs =
    List.filter_map
      (fun (p, _) ->
        let main = Filename.check_suffix p ".mli" || not (List.mem_assoc (twin p) sources) in
        if main && in_scope p && hub p && not (capitals p || capitals (twin p)) then
          Some (Printf.sprintf "%s: a hub (named by %d files) with no capital: its main types and functions are the map's" p (fan p))
        else None)
      sources
  in
  programs @ hubs
