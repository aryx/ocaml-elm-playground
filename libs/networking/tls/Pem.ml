(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pem.mli *)

let certificates (text : string) : string list =
  let lines = String.split_on_char '\n' text |> List.map String.trim in
  let rec go inside acc lines =
    match lines with
    | [] -> List.rev acc
    | "-----BEGIN CERTIFICATE-----" :: rest -> go (Some []) acc rest
    | "-----END CERTIFICATE-----" :: rest -> (
        match inside with Some b64 -> go None (Base64.decode (String.concat "" (List.rev b64)) :: acc) rest | None -> go None acc rest)
    | l :: rest -> go (Option.map (fun b -> l :: b) inside) acc rest
  in
  go None [] lines
