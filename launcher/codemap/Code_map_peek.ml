(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map_peek.mli *)

open Code_map_base


(* claude: the definition a line is in: its header, to the line before
 * the next top-level one *)
let def_extent (f : Code_file.t) (line : int) : int * int =
  let heads =
    List.filter_map (fun (l, _, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some l | _ -> None) f.defs
    |> List.sort_uniq compare
  in
  let first = List.fold_left (fun acc l -> if l <= line then l else acc) (match heads with l :: _ when l <= line -> l | _ -> line) heads in
  let last = match List.find_opt (fun l -> l > first) heads with Some n -> n - 1 | None -> Code_file.nlines f - 1 in
  (* claude: in a literate program's source (principia's), the chunk's
   * markers at the left margin bound it: the body ends before the next
   * one (not the next definition's name, its return type and marker
   * with it), and starts just under its own /*s: ... */ when only its
   * lines lie between (a comment, a Plan 9 return type on its own
   * line); the markers inside a body are indented *)
  let chunk l = Code_file.syncweb_marker f l && Code_file.at f l 0 <> None in
  let last = let rec go l = if l > last then last else if chunk l then l - 1 else go (l + 1) in go (first + 1) in
  let first =
    let blank l = let rec any c = c < Code_file.cols && (Code_file.at f l c <> None || any (c + 1)) in not (any 0) in
    let rec go l k = if l < 0 || k > 4 || blank l then first else if chunk l then l + 1 else go (l - 1) (k + 1) in
    go (first - 1) 0
  in
  (* not the next section's banner and comment: up to its last line of code *)
  let trailer l =
    let rec first_cat c = if c >= Code_file.cols then None else match Code_file.at f l c with Some cat -> Some cat | None -> first_cat (c + 1) in
    match first_cat 0 with None -> true | Some (Comment | Comment_section) -> true | Some _ -> false
  in
  let rec trim l = if l > first && trailer l then trim (l - 1) else l in
  (first, max first (trim last))

(* claude: a section: its title's line, to the line before the next
 * section's banner *)
let section_extent (f : Code_file.t) (line : int) : int * int =
  let titles =
    List.filter_map (fun (l, _, (cat : Highlight_code.category)) -> if cat = Comment_section && l > line + 1 then Some l else None) f.defs
    |> List.sort_uniq compare
  in
  let next = match titles with l :: _ -> l - 2 | [] -> Code_file.nlines f - 1 in
  (line, max line next)

(* claude: the definition a click at a line and column of [path]'s file
 * peeks at: the name's there (its binding), or, defined elsewhere, found
 * among the sources (the map's, and those beyond it); else the line's
 * own definition *)
let peek_where (t : t) (f : Code_file.t) (path : string) (line : int) (col : int) : (string * int) option =
  match Code_file.name_at f line col with
  | Some o -> Some (path, fst o.bound_at)
  | None -> (
      match Code_file.ref_at f line col with
      | Some r -> (
          match Code_names.find_in ~roots:t.roots (index_of t) ~from:path f r with
          | c :: _, _ -> Some (c.path, c.line)
          | [], _ ->
              t.note <- r.rname ^ ": not found";
              None)
      | None -> Some (path, line))

(* the peeks: one on top of the others, four at most, each its scroll *)
(* claude: the lines a peek at [l] of [p]'s file [g] shows: a top-level
 * comment whole, else the definition with the comment just above it *)
let peek_extent (g : Code_file.t) (p : string) (l : int) : int * int =
  (* claude: a line's text, whole (its spans at their columns) *)
  let text l =
    let b = Buffer.create 80 in
    List.iter (fun (sp : Highlight_code.span) -> while Buffer.length b < sp.col do Buffer.add_char b ' ' done; Buffer.add_string b sp.text) g.lines.(l);
    Buffer.contents b
  in
  let count sub str =
    let n = String.length str and m = String.length sub in
    let k = ref 0 in
    for i = 0 to n - m do if String.sub str i m = sub then incr k done;
    !k
  in
  (* claude: each language its own markers: an OCaml file writing
   * libc/*.s in a comment is no C comment opened (the author, at
   * ~/ix's TinyAssembler.ml: every peek grew back to the header) *)
  let ml = List.exists (Filename.check_suffix p) [ ".ml"; ".mli"; ".mll"; ".mly" ] in
  let opens l = let t = text l in if ml then count "(*" t else count "/*" t
  and closes l = let t = text l in if ml then count "*)" t else count "*/" t in
  (* the comments' depth at each line's start, the file read once *)
  let n = Code_file.nlines g in
  let depth = Array.make (n + 1) 0 in
  for i = 0 to n - 1 do depth.(i + 1) <- max 0 (depth.(i) + opens i - closes i) done;
  (* claude: a top-level comment clicked (a game's header): the whole
   * comment, from where it opens to where it closes (the author: "not
   * just what started at the line clicked"), scrolled if long *)
  let rec opening_of i = if i > 0 && depth.(i) > 0 then opening_of (i - 1) else i in
  let rec closing_of i = if i + 1 < n && depth.(i + 1) > 0 then closing_of (i + 1) else i in
  let top_comment =
    if l < n && (depth.(l) > 0 || opens l > 0) then
      let o = opening_of l in
      let starts_line = String.length (text o) >= 2 && (String.sub (text o) 0 2 = "(*" || String.sub (text o) 0 2 = "/*") in
      if starts_line then Some (o, closing_of l) else None
    else None
  in
  let first, last =
    match top_comment with
    | Some ext -> ext
    | None ->
        let first, last = def_extent g l in
        (* claude: with the comment just above it, which likely says
         * what it is (the author), all of it, its blank lines too;
         * comments stacked right above one another with it; not a
         * section's banner *)
        (* claude: not a syncweb marker, /*s: function [[f]] */ above
         * it, or the previous chunk's /*e: ... */ (the author:
         * boilerplate, at principia) *)
        let comment_end l =
          let rec first_cat c = if c >= Code_file.cols then None else match Code_file.at g l c with Some cat -> Some cat | None -> first_cat (c + 1) in
          l >= 0 && (match first_cat 0 with Some Comment -> true | _ -> false) && not (Code_file.syncweb_marker g l)
        in
        (* a banner's rule, (*----*): where a section begins, not a comment *)
        let rule l =
          let t = String.trim (text l) in
          String.length t >= 8 && String.for_all (fun c -> String.contains "(*)-=/ " c) t
        in
        let rec up l =
          if comment_end (l - 1) then
            let o = opening_of (l - 1) in
            if List.exists rule (List.init (l - o) (fun k -> o + k)) then l else up o
          else l
        in
        (up first, last)
  in
  (first, last)

let open_peek (t : t) (file_of : string -> Code_file.t option) ((p, l) : string * int) : unit =
  match file_of p with
  | Some g ->
      let first, last = peek_extent g p l in
      (match t.peek with Some top -> t.peek_stack <- (top, t.peek_scroll) :: t.peek_stack | None -> ());
      t.peek <- Some (p, first, last);
      t.peek_scroll <- 0
  | None -> ()

let close_peek (t : t) : unit =
  match t.peek_stack with
  | (top, scroll) :: rest ->
      t.peek <- Some top;
      t.peek_scroll <- scroll;
      t.peek_stack <- rest
  | [] -> t.peek <- None
