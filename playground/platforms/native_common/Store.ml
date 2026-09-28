(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Store.mli *)

let rec mkdir_p d =
  if not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    Sys.mkdir d 0o755
  end

let dir () =
  let d =
    match Sys.getenv_opt "ELM_PLAYGROUND_STORE" with
    | Some d when d <> "" -> d
    | _ -> Filename.concat (Filename.concat (Option.value (Sys.getenv_opt "HOME") ~default:".") ".elm-playground") "documents"
  in
  mkdir_p d;
  d

(* a name is a file's name in the directory, never a path: no '/', and
   no leading '.' (no "..", no hidden file) *)
let file_name name =
  let s = String.map (fun c -> if c = '/' || c = '\\' then '_' else c) name in
  if s = "" || s.[0] = '.' then "_" ^ s else s

let write path bytes = Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc bytes)
let store name bytes = write (Filename.concat (dir ()) (file_name name)) bytes

let fetch name =
  let path = Filename.concat (dir ()) (file_name name) in
  if Sys.file_exists path then Some (In_channel.with_open_bin path In_channel.input_all) else None

let stored () = List.sort compare (Array.to_list (Sys.readdir (dir ())))
let export name bytes = write (file_name name) bytes
