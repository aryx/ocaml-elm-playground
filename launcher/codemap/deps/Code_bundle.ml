(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_bundle.mli *)

type t = { name : string; roots : string list; sources : (string * string) list; configs : string list; jsonnet : (string * string) list }

let entries (s : string) : (string * string) list =
  let rec go i acc =
    if i >= String.length s then List.rev acc
    else
      let nl1 = String.index_from s i '\n' in
      let nl2 = String.index_from s (nl1 + 1) '\n' in
      let path = String.sub s i (nl1 - i) in
      let n = int_of_string (String.sub s (nl1 + 1) (nl2 - nl1 - 1)) in
      go (nl2 + 1 + n) ((path, String.sub s (nl2 + 1) n) :: acc)
  in
  try go 0 [] with Not_found | Invalid_argument _ -> failwith "Code_bundle: not a bundle"

let to_string (t : t) : string =
  let b = Buffer.create (1 lsl 20) in
  let add (path, text) = Buffer.add_string b (Printf.sprintf "%s\n%d\n%s" path (String.length text) text) in
  add ("#name", t.name);
  add ("#roots", String.concat "\n" t.roots);
  List.iter add t.sources;
  List.iter add t.jsonnet;
  Buffer.contents b

let is_config p = Filename.basename p = ".codemapconfig"
let is_jsonnet p = is_config p || Filename.check_suffix p ".libsonnet" || Filename.check_suffix p ".jsonnet"

let of_string (s : string) : t =
  let es = entries s in
  let meta k = Option.value (List.assoc_opt k es) ~default:"" in
  let files = List.filter (fun (p, _) -> p <> "#name" && p <> "#roots") es in
  let jsonnet, sources = List.partition (fun (p, _) -> is_jsonnet p) files in
  (* claude: the root "" is an empty line; String.split_on_char keeps it *)
  let roots = match meta "#roots" with "" when not (List.mem_assoc "#roots" es) -> [] | r -> String.split_on_char '\n' r in
  { name = meta "#name"; roots; sources; configs = List.filter is_config (List.map fst jsonnet); jsonnet }
