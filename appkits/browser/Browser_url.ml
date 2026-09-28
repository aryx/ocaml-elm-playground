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
  (* a data: URL is whole already (and its payload is not a path) *)
  if String.length href >= 5 && String.lowercase_ascii (String.sub href 0 5) = "data:" then href
  else
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

(* %XX as its byte *)
let percent_decode (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match s.[i] with
      | '%' when i + 2 < n + 0 && i + 2 <= n - 1 -> (
          match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with
          | Some c -> Buffer.add_char b (Char.chr c); go (i + 3)
          | None -> Buffer.add_char b '%'; go (i + 1))
      | c -> Buffer.add_char b c; go (i + 1)
  in
  go 0;
  Buffer.contents b

let data_url (url : string) : string option =
  if not (starts_with "data:" url) then None
  else
    match String.index_opt url ',' with
    | None -> None
    | Some i ->
        let meta = String.sub url 5 (i - 5) and payload = String.sub url (i + 1) (String.length url - i - 1) in
        if String.ends_with ~suffix:";base64" meta then try Some (Base64.decode payload) with _ -> None else Some (percent_decode payload)
