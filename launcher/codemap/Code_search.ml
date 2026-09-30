(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_search.mli *)

(* claude: @ in constant stack, as Code_map_base's (a browser's stack is small) *)
let ( @ ) (a : 'a list) (b : 'a list) : 'a list = match b with [] -> a | _ -> List.rev_append (List.rev a) b

type kind = Dir | File | Def | Text | View | Tour
type hit = { kind : kind; path : string; line : int; name : string }

let basename (p : string) : string = match String.rindex_opt p '/' with Some i -> String.sub p (i + 1) (String.length p - i - 1) | None -> p

let candidates ?(views = []) ?(tours = []) ~(dirs : string list) ~(files : string list) ~(defs : (string * int * string) list) () : hit array =
  Array.of_list
    (List.mapi (fun i n -> { kind = View; path = n; line = i; name = n }) views
    @ List.mapi (fun i n -> { kind = Tour; path = n; line = i; name = n }) tours
    @ List.map (fun p -> { kind = Dir; path = p; line = 0; name = basename p }) dirs
    @ List.map (fun p -> { kind = File; path = p; line = 0; name = basename p }) files
    @ List.map (fun (p, l, n) -> { kind = Def; path = p; line = l; name = n }) defs)

let parse (q : string) : string * int =
  let n = String.length q in
  let rec k i = if i > 0 && q.[i - 1] = '/' then k (i - 1) else i in
  let e = k n in
  (String.sub q 0 e, n - e)

let starts (s : string) (p : string) = String.length s >= String.length p && String.sub s 0 (String.length p) = p

let contains (s : string) (sub : string) : bool =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

(* the name's words' starts: after _ . - or where a capital follows a
 * small letter *)
let word_start (name : string) (p : string) : bool =
  let low = String.lowercase_ascii name in
  let n = String.length name in
  let rec go i =
    i < n
    && ((i > 0
        && (match name.[i - 1] with '_' | '.' | '-' | ' ' -> true | c -> (c >= 'a' && c <= 'z') && name.[i] >= 'A' && name.[i] <= 'Z')
        && starts (String.sub low i (n - i)) p)
       || go (i + 1))
  in
  go 1

(* the path a hit is under: a directory's parent, a file's or a
 * definition's directory; and its file itself for a definition *)
let under (h : hit) : string =
  let dir p = match String.rindex_opt p '/' with Some i -> String.sub p 0 i | None -> "" in
  String.lowercase_ascii (match h.kind with Dir -> dir h.path | File -> dir h.path | Def | Text -> h.path | View | Tour -> "")

(* the query's path part and name part: "shmup/step" -> "shmup", "step" *)
let split (text : string) : string * string =
  match String.rindex_opt text '/' with Some i -> (String.sub text 0 i, String.sub text (i + 1) (String.length text - i - 1)) | None -> ("", text)

let score (h : hit) (name : string) : int option =
  let n = String.lowercase_ascii h.name in
  let n' = if h.kind = File then Filename.remove_extension n else n in
  if n = name || n' = name then Some 0
  else if starts n name then Some 1
  else if word_start h.name name then Some 2
  else if contains n name then Some 3
  else None

let depth (p : string) = List.length (String.split_on_char '/' p)
let rank_kind = function View | Tour -> 0 | Dir -> 1 | File -> 2 | Def -> 3 | Text -> 4

let matches ?(near = fun _ -> false) (all : hit array) (q : string) : hit list =
  let text, slashes = parse (String.lowercase_ascii (String.trim q)) in
  let pre, name = split text in
  if name = "" then []
  else
    (* claude: a definition in an .mli whose .ml defines it too: the .ml's
     * only, one hit for one definition *)
    let in_ml = Hashtbl.create 64 in
    Array.iter (fun h -> if h.kind = Def && Filename.check_suffix h.path ".ml" then Hashtbl.replace in_ml (h.path, h.name) ()) all;
    let twin h = h.kind = Def && Filename.check_suffix h.path ".mli" && Hashtbl.mem in_ml (Filename.chop_suffix h.path ".mli" ^ ".ml", h.name) in
    Array.to_list all
    |> List.filter (fun h -> not (twin h))
    |> List.filter_map (fun h ->
           if slashes > 0 && h.kind <> Dir then None
           else if pre <> "" && not (contains (under h) pre) then None
           else
             match score h name with
             (* two slashes or more: the directories of that very name *)
             | Some s when slashes < 2 || s = 0 -> Some (s, h)
             | _ -> None)
    (* claude: as good a match, the one near (in the unit looked at) first *)
    |> List.stable_sort (fun (s, h) (s', h') ->
           compare (s, not (near h.path), rank_kind h.kind, depth h.path, h.path, h.line) (s', not (near h'.path), rank_kind h'.kind, depth h'.path, h'.path, h'.line))
    |> List.map snd

let all_named (all : hit array) (q : string) : string list =
  let _, slashes = parse q in
  if slashes < 2 then [] else List.map (fun h -> h.path) (matches all q)

let complete (hits : hit list) (q : string) : string =
  let text, slashes = parse q in
  let pre, name = split text in
  let low = String.lowercase_ascii name in
  (* the best hits' names that start with what was typed *)
  let names = List.filteri (fun i _ -> i < 50) hits |> List.filter_map (fun h -> if starts (String.lowercase_ascii h.name) low then Some h.name else None) in
  match names with
  | [] -> q
  | first :: rest ->
      let common a b =
        let n = min (String.length a) (String.length b) in
        let rec go i = if i < n && Char.lowercase_ascii a.[i] = Char.lowercase_ascii b.[i] then go (i + 1) else i in
        String.sub a 0 (go 0)
      in
      let c = List.fold_left common first rest in
      if String.length c <= String.length name then q
      else
        (* what was typed kept, the rest in the name's own case *)
        let c = name ^ String.sub c (String.length name) (String.length c - String.length name) in
        let one_dir = List.for_all (fun n -> String.lowercase_ascii n = String.lowercase_ascii c) names && List.exists (fun h -> h.kind = Dir && String.lowercase_ascii h.name = String.lowercase_ascii c) hits in
        (if pre = "" then "" else pre ^ "/") ^ c ^ (if slashes > 0 then String.make slashes '/' else if one_dir then "/" else "")

(*****************************************************************************)
(* The text *)
(*****************************************************************************)

let text_query (q : string) : string option =
  if String.length q >= 1 && q.[0] = '"' then
    let t = String.sub q 1 (String.length q - 1) in
    let t = if String.length t > 0 && t.[String.length t - 1] = '"' then String.sub t 0 (String.length t - 1) else t in
    if String.length t >= 2 then Some t else None
  else None

let text_matches ?(limit = 5000) (files : (string * string array) list) (text : string) : hit list =
  let smart = String.exists (fun c -> c >= 'A' && c <= 'Z') text in
  let norm = if smart then Fun.id else String.lowercase_ascii in
  let text = norm text in
  let found = ref [] and n = ref 0 in
  (try
     List.iter
       (fun (path, lines) ->
         Array.iteri
           (fun l line ->
             if contains (norm line) text then begin
               found := { kind = Text; path; line = l; name = String.trim line } :: !found;
               incr n;
               if !n >= limit then raise Exit
             end)
           lines)
       files
   with Exit -> ());
  List.rev !found

let ref_query (q : string) : string option =
  if String.length q >= 3 && q.[0] = '@' then Some (String.sub q 1 (String.length q - 1)) else None

let ref_matches ?(limit = 5000) (files : (string * (int * string) list * string array) list) (name : string) : hit list =
  let ends s suf = let n = String.length s and m = String.length suf in n >= m && String.sub s (n - m) m = suf in
  let is n = n = name || ends n ("." ^ name) in
  let found = ref [] and k = ref 0 in
  (try
     List.iter
       (fun (path, refs, lines) ->
         let seen = Hashtbl.create 8 in
         List.iter
           (fun (l, n) ->
             if is n && not (Hashtbl.mem seen l) then begin
               Hashtbl.replace seen l ();
               found := { kind = Text; path; line = l; name = (if l < Array.length lines then String.trim lines.(l) else n) } :: !found;
               incr k;
               if !k >= limit then raise Exit
             end)
           refs)
       files
   with Exit -> ());
  List.rev !found
