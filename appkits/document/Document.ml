(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Document.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a t = {
  content : 'a;
  path : string option;
  (* the version last saved, kept so that "is it dirty?" is a
     comparison and not a flag somebody has to remember to set *)
  saved : 'a;
  equal : 'a -> 'a -> bool;
}

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

let create ?path ?(equal = ( == )) content = { content; path; saved = content; equal }
let content t = t.content
let path t = t.path
let with_path path t = { t with path = Some path }
let put content t = { t with content }
let edit f t = { t with content = f t.content }
let dirty t = not (t.equal t.content t.saved)
let mark_saved t = { t with saved = t.content }

let title t =
  let name =
    match t.path with
    | None -> "untitled"
    | Some p -> ( match String.rindex_opt p '/' with
                  | None -> p
                  | Some i -> String.sub p (i + 1) (String.length p - i - 1))
  in
  if dirty t then name ^ " *" else name
