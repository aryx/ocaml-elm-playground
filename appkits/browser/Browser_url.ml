(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_url.mli *)

let resolve (base : string) (href : string) : string =
  match (Url.parse base, Url.parse href) with
  | Ok base, Ok href -> Url.to_string (Url.resolve base href)
  | _ -> href

let split_at (c : char) (url : string) : string * string option =
  match String.index_opt url c with
  | Some i -> (String.sub url 0 i, Some (String.sub url (i + 1) (String.length url - i - 1)))
  | None -> (url, None)

let split_fragment (url : string) : string * string option = split_at '#' url
let split_query (url : string) : string * string option = split_at '?' url

let starts_with (prefix : string) (s : string) : bool =
  String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix
