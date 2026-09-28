(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_rank.mli *)

type use = { own : int; others : int; files : int }

(* a definition by its file, its line and its (last) name *)
type t = { uses : (string * int * string, use) Hashtbl.t }

let last_name (s : string) : string = match String.rindex_opt s '.' with Some i -> String.sub s (i + 1) (String.length s - i - 1) | None -> s

let compute ?roots (files : (string * Code_file.t Lazy.t) list) : t =
  let ix = Code_names.index files in
  let h = Hashtbl.create 4096 in
  let get k = Option.value (Hashtbl.find_opt h k) ~default:{ own = 0; others = 0; files = 0 } in
  List.iter
    (fun (path, lf) ->
      let (f : Code_file.t) = Lazy.force lf in
      (* its own uses: the names bound to one of its top-level
       * definitions, but the definition itself *)
      List.iter
        (fun (d : Highlight_code.definition) ->
          match Hashtbl.find_opt f.uses (d.dline, d.dcol) with
          | Some occs ->
              let k = (path, d.dline, last_name d.dname) in
              let u = get k in
              Hashtbl.replace h k { u with own = u.own + List.length occs - 1 }
          | None -> ())
        f.definitions;
      (* the others': every name defined elsewhere, resolved, if sure;
       * a file counted once a definition *)
      let seen = Hashtbl.create 64 in
      Array.iter
        (List.iter (fun (r : Highlight_code.reference) ->
             match Code_names.find_in ?roots ix ~from:path f r with
             | c :: _, true ->
                 let k = (c.path, c.line, r.rname) in
                 let u = get k in
                 let first = not (Hashtbl.mem seen k) in
                 if first then Hashtbl.replace seen k ();
                 Hashtbl.replace h k { u with others = u.others + 1; files = (u.files + if first then 1 else 0) }
             | _ -> ()))
        f.refs)
    files;
  { uses = h }

let uses (t : t) (path : string) (line : int) (name : string) : use =
  Option.value (Hashtbl.find_opt t.uses (path, line, last_name name)) ~default:{ own = 0; others = 0; files = 0 }

(* codemap's multiplier_use: NoUse to HugeUse *)
let bucket (n : int) : float = if n <= 0 then 0.9 else if n = 1 then 1.3 else if n < 5 then 1.7 else if n < 20 then 2.1 else if n < 100 then 2.7 else 3.3

(* codemap's size_font_multiplier_of_categ, by kind *)
let weight (c : Highlight_code.category) : float =
  match c with Def_module | Def_type -> 5. | Def_function -> 3.5 | Def_value -> 3. | Constructor -> 1.2 | Field -> 1.7 | _ -> 1.

let score (t : t) (path : string) (line : int) (name : string) (c : Highlight_code.category) : float =
  let u = uses t path line name in
  weight c *. bucket (u.others + (u.own / 3))
