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
  (* claude: C's // lines, a run of them one comment (~/ix's tiny-os) *)
  let slashes t = (String.length t >= 2 && String.sub t 0 2 = "//") || (String.length t >= 1 && t.[0] = '|') in
  let closes t = contains t "*)" || contains t "*/" in
  let rec comments acc cur = function
    | [] -> List.rev (match cur with Some c -> List.rev c :: acc | None -> acc)
    | l :: rest -> (
        let t = String.trim l in
        match cur with
        | None ->
            if t = "//" || t = "|" then comments acc None rest
            else if slashes t then
              (* a bare // ends a paragraph: the copyright's apart from the design's *)
              let rec run acc' = function r :: rest' when slashes (String.trim r) && String.trim r <> "//" && String.trim r <> "|" -> run (r :: acc') rest' | _ :: rest' when acc' <> [] && false -> (List.rev acc', rest') | rest' -> (List.rev acc', rest') in
              let block, rest = run [ l ] rest in
              comments (block :: acc) None rest
            else if opens t then if closes t then comments ([ l ] :: acc) None rest else comments acc (Some [ l ]) rest
            else comments acc None rest
        | Some c -> if closes t then comments (List.rev (l :: c) :: acc) None rest else comments acc (Some (l :: c)) rest)
  in
  (* a rule of stars, a banner's title alone, a copyright: not a header *)
  let words l = List.length (List.filter (fun w -> String.length w > 1 && String.exists (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) w) (String.split_on_char ' ' l)) in
  let telling b = (not (List.exists (fun l -> contains l "Copyright") b)) && List.fold_left (fun n l -> n + words l) 0 b >= 4 in
  match List.filter telling (comments [] None lines) with
  | b :: _ -> List.filteri (fun i _ -> i < max) b
  | [] -> []

(* claude: a program: a top-level main (the Playground's Program.main),
 * or Cap.main run at the top (~/ix's Main.ml: let () = Cap.main ...) *)
let is_program (src : string) : bool =
  contains src "\nlet main =" || contains src "\nlet main () =" || contains src "\nlet main=" || (contains src "\nlet () =" && (contains src "Cap.main" || contains src "Sys.argv" || contains src "Callback.register"))
  (* claude: a kernel's C entry *)
  || contains src "\nkmain(" || contains src " kmain(void)\n{" 
  (* claude: C's main, Plan 9's way (its type on the line above) or not *)
  || contains src "\nmain(" || contains src "\nint main(" || contains src "\nvoid main("
  (* claude: a libthread program's (rio, acme: its main is libthread's) *)
  || contains src "\nthreadmain(" || contains src "\nvoid threadmain("

(* a source the brief reads: OCaml's and C's *)
let is_source (p : string) : bool = List.exists (Filename.check_suffix p) [ ".ml"; ".mli"; ".c"; ".h"; ".s"; ".S"; ".asm" ]

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
  let fan p = Code_deps.fan fans p in
  let total = List.length (List.filter (fun (p, _) -> Filename.check_suffix p ".ml" || Filename.check_suffix p ".c") sources) in
  (* claude: a hub, of the project (a twentieth of its files, eight at
   * least: ~/ix's Memory.mli, 20 of 291) or of its own directory (half
   * its other modules, three at least: mini-rc's Ast) *)
  let hub p = fan p >= max 8 (total / 20) in
  let dir_hub p =
    let d = Filename.dirname p in
    let mods = List.sort_uniq compare (List.filter_map (fun (q, _) -> if Filename.dirname q = d && is_source q then Some (Filename.remove_extension q) else None) sources) in
    let named_here =
      List.length
        (List.sort_uniq compare
           (List.filter_map
              (fun (q, src) ->
                if Filename.dirname q = d && Filename.remove_extension q <> Filename.remove_extension p && is_source q && List.mem (Code_deps.module_name p) (Code_deps.modules_used src) then
                  Some (Filename.remove_extension q)
                else None)
              sources))
    in
    List.length mods >= 4 && named_here >= max 3 ((List.length mods - 1) / 2)
  in
  pr "# %s\n\n" (if dir = "" then "(the root)" else dir);
  pr "%d files here, %d lines; %d under it in all.\n\n" (List.length mine)
    (List.fold_left (fun n (_, s) -> n + count_lines s) 0 mine)
    (List.length (List.filter (fun (p, _) -> under p) sources));
  (* the central files first: what the project stands on *)
  let central = List.filter (fun (p, _) -> fan p > 0) mine |> List.map fst |> List.filter (fun p -> Filename.check_suffix p ".mli" || not (List.mem_assoc (Filename.remove_extension p ^ ".mli") mine)) in
  let central = List.sort (fun p q -> compare (fan q) (fan p)) central in
  if central <> [] then begin
    pr "Named by other files (open, include, a qualified name, #include; of %d .ml and .c files in all), the most central first:\n\n" total;
    List.iter (fun p -> pr "- %s: %d files%s\n" (Filename.basename p) (fan p) (if hub p then "  <- A HUB: part of the project's core" else if dir_hub p then "  <- this directory's hub" else "")) (List.filteri (fun i _ -> i < 12) central);
    pr "\n";
    if List.exists (fun p -> hub p || dir_hub p) central then
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
        (if fan p = 0 && (List.exists (fun (_, n, _) -> n = "main") f.defs || is_program src) then ", a program nobody names (a driver)" else "")
        (match Code_guide.file_note guide p with
        | Some n -> Printf.sprintf "; described (%s)" (match n.summary with Some s -> s | None -> "no summary")
        | None -> "");
      (* claude: an .ml whose header is its .mli's ("(* See X.mli *)",
       * ~/ix's way): no header here, not the next comment's words *)
      let first = header ~max:3 src in
      let in_mli = Filename.check_suffix p ".ml" && (match first with l :: _ -> contains l "See " && contains l ".mli" | [] -> false) in
      let head = if in_mli then [] else first @ [] in
      let head = if in_mli then [] else if head = [] then [] else header src in
      if in_mli then pr "Its header: in its .mli (the file says so).\n\n";
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
      if f.tricks <> [] then pr "Marked \"%s\" at lines %s.\n\n" Code_file.trick (String.concat ", " (List.map (fun l -> string_of_int (l + 1)) f.tricks));
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
        let twice = List.filter (fun (_, n, k, _, _) -> List.length (List.filter (fun (_, m, k', _, _) -> m = n && k' = k) defs) > 1) defs |> List.map (fun (_, n, k, _, _) -> k ^ ":" ^ n) |> List.sort_uniq compare in
        if twice <> [] then pr "%s, defined twice (def: finds the first; the second needs another anchor): %s.\n\n" (Filename.basename p) (String.concat ", " twice);
        (* a program's Model-View-Update names, those the template assumes *)
        (* only a Playground program: the template is its architecture *)
        if List.exists (fun (_, n, _) -> n = "main") f.defs && contains src "Playground" then begin
          let has k n = List.exists (fun (_, m, kind, _, _) -> m = n && kind = k) defs in
          let say k n = Printf.sprintf "%s:%s %s" k n (if has k n then "yes" else "NO") in
          pr "%s, the template's names (skeletons.libsonnet): %s, %s, %s, %s.\n\n" (Filename.basename p) (say "type" "model") (say "def" "initial_model") (say "def" "update") (say "def" "view")
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
      if is_program src then pr "A program: its entry (a main, Cap.main).\n\n")
    mine;
  Buffer.contents b

(* claude: what the configs miss, as -check says it (the author, after the
 * first pass left the Playground's core without a capital and most games
 * without a skeleton: "lessons learned from those missings?"): a program
 * with no skeleton; a hub (named by a twentieth of the project or more)
 * with no capital, in its .ml or its .mli *)
let coverage ~(guide : Code_guide.t) ~(sources : (string * string) list) : string list =
  let fans = Code_deps.fan_in sources in
  let fan p = Code_deps.fan fans p in
  let total = List.length (List.filter (fun (p, _) -> Filename.check_suffix p ".ml" || Filename.check_suffix p ".c") sources) in
  (* claude: a hub, of the project (a twentieth of its files, eight at
   * least: ~/ix's Memory.mli, 20 of 291) or of its own directory (half
   * its other modules, three at least: mini-rc's Ast) *)
  let hub p = fan p >= max 8 (total / 20) in
  let dir_hub p =
    let d = Filename.dirname p in
    let mods = List.sort_uniq compare (List.filter_map (fun (q, _) -> if Filename.dirname q = d && is_source q then Some (Filename.remove_extension q) else None) sources) in
    let named_here =
      List.length
        (List.sort_uniq compare
           (List.filter_map
              (fun (q, src) ->
                if Filename.dirname q = d && Filename.remove_extension q <> Filename.remove_extension p && is_source q && List.mem (Code_deps.module_name p) (Code_deps.modules_used src) then
                  Some (Filename.remove_extension q)
                else None)
              sources))
    in
    List.length mods >= 4 && named_here >= max 3 ((List.length mods - 1) / 2)
  in
  let described p = Code_guide.file_note guide p <> None in
  let capitals p = match Code_guide.file_note guide p with Some n -> n.capitals <> [] | None -> false in
  let twin p = if Filename.check_suffix p ".mli" then Filename.remove_extension p ^ ".ml" else Filename.remove_extension p ^ ".mli" in
  (* only where configs are written: a directory whose config describes files *)
  (* every directory: one with no config yet is the first thing missing
   * (the agents' lesson: a bare area looked finished) *)
  let in_scope _ = true in
  let programs =
    List.filter_map
      (fun (p, src) ->
        if (Filename.check_suffix p ".ml" || Filename.check_suffix p ".c") && in_scope p && is_program src && Code_guide.skeletons_of guide p = [] then
          Some (p ^ (if described p then ": a program with no skeleton (its entry to its core; skeletons.libsonnet has the shapes)" else ": a program in a file no config describes"))
        else None)
      sources
  in
  let hubs =
    List.filter_map
      (fun (p, _) ->
        let main = Filename.check_suffix p ".mli" || not (List.mem_assoc (twin p) sources) in
        if main && in_scope p && (hub p || dir_hub p) && not (capitals p || capitals (twin p)) then
          Some (Printf.sprintf "%s: a hub (named by %d files%s) with no capital: its main types and functions are the map's" p (fan p) (if hub p then "" else ", its directory's"))
        else None)
      sources
  in
  (* claude: every module its skeleton, and every folder (the author:
   * "ideally every file, every folder"): the map derives one where none
   * is written (Map_v2.derived_file), but a written one is the judgement
   * the derived one lacks. A module: its .ml or .c, 150 lines or more
   * (below, the derived one is honest enough: the author agreed 40 was
   * too many); a folder: two sources or more *)
  let lines src = List.length (String.split_on_char '\n' src) in
  let mine p (s : Code_guide.skeleton) =
    let n = List.length s.bones and here = List.length (List.filter (fun (b : Code_guide.bone) -> b.bpath = p) s.bones) in
    here > 0 && 2 * here >= n
  in
  let skeletons = List.concat_map (fun (d : Code_guide.dir_note) -> d.skeletons) (Code_guide.dirs guide) in
  let modules =
    List.filter_map
      (fun (p, src) ->
        if (Filename.check_suffix p ".ml" || Filename.check_suffix p ".c" || Filename.check_suffix p ".s") && lines src >= 150 && (not (is_program src)) && not (List.exists (mine p) skeletons) then
          Some (p ^ ": a module with no skeleton of its own (the map derives one; write it: its parts, who uses whom)")
        else None)
      sources
  in
  let dirs =
    List.sort_uniq compare (List.filter_map (fun (p, _) -> if is_source p then Some (match Filename.dirname p with "." -> "" | d -> d) else None) sources)
    |> List.filter (fun d -> List.length (List.filter (fun (p, _) -> is_source p && (match Filename.dirname p with "." -> "" | x -> x) = d) sources) >= 2)
    |> List.filter (fun d -> not (List.exists (fun (s : Code_guide.skeleton) -> s.sdir = d) skeletons))
    |> List.map (fun d -> (if d = "" then "(the root)" else d) ^ ": a folder with no skeleton (its parts and how they connect)")
  in
  (* claude: every source described (the author found launcher/codemap's
   * files without a summary: its config, written early, had none) *)
  let undescribed =
    List.filter_map
      (fun (p, _) ->
        if is_source p && (match Code_guide.file_note guide p with Some { summary = Some _; _ } -> false | _ -> true) then Some (p ^ ": a file no config describes (its summary)")
        else None)
      sources
  in
  (* claude: the project's senses, once: its root config's anatomy rules
   * (the X-ray's nerves and lungs; guessed from words without them) *)
  let senses =
    match List.find_opt (fun (d : Code_guide.dir_note) -> d.dir = "") (Code_guide.dirs guide) with
    | Some d when d.nerves = [] || d.lungs = [] -> [ ".codemapconfig: no anatomy rules (nerves:, lungs:): what this project's inputs and I/O are" ]
    | _ -> []
  in
  senses @ programs @ hubs @ dirs @ modules @ undescribed
