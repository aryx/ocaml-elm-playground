(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_walk.mli *)

let source_extensions = [ ".ml"; ".mli"; ".mll"; ".mly"; ".c"; ".h"; ".s"; ".S"; ".asm" ]

type t = { roots : string list; sources : (string * string) list; configs : string list; jsonnet : (string * string) list }

let read path = match In_channel.with_open_bin path In_channel.input_all with s -> Some s | exception Sys_error _ -> None

let walk (dir : string) : t =
  let config = Code_config.make ~ignore:(read (Filename.concat dir ".codemapignore")) in
  let out = ref [] and roots = ref [] and configs = ref [] and jsonnet = ref [] in
  let rec go (rel : string) =
    let entries = try Sys.readdir (if rel = "" then dir else Filename.concat dir rel) with Sys_error _ -> [||] in
    Array.sort compare entries;
    if Array.exists (fun e -> e = ".git" || e = "dune-project") entries then roots := rel :: !roots;
    if Array.mem ".codemapconfig" entries then begin
      let c = if rel = "" then ".codemapconfig" else Filename.concat rel ".codemapconfig" in
      configs := c :: !configs;
      Option.iter (fun s -> jsonnet := (c, s) :: !jsonnet) (read (Filename.concat dir c))
    end;
    Array.iter
      (fun e ->
        if e <> "" && e.[0] <> '.' then begin
          let r = if rel = "" then e else Filename.concat rel e in
          let path = Filename.concat dir r in
          match (Unix.lstat path).st_kind with
          | S_DIR -> if e.[0] <> '_' && not (Code_config.ignored config r ~dir:true) then go r
          | S_REG when List.exists (Filename.check_suffix e) source_extensions && not (Code_config.ignored config r ~dir:false) -> (
              match read path with Some src -> out := (r, src) :: !out | None -> ())
          | S_REG when Filename.check_suffix e ".libsonnet" || Filename.check_suffix e ".jsonnet" -> (
              match read path with Some src -> jsonnet := (r, src) :: !jsonnet | None -> ())
          | _ -> ()
          | exception Unix.Unix_error _ -> ()
        end)
      entries
  in
  go "";
  { roots = List.rev !roots; sources = List.rev !out; configs = List.rev !configs; jsonnet = List.rev !jsonnet }
