(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Urlencoded.mli *)

let kept (c : char) : bool =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || String.contains "*-._" c

let escape (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if kept c then Buffer.add_char b c
      else if c = ' ' then Buffer.add_char b '+'
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let hex (c : char) : int option =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let unescape (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      match s.[i] with
      | '+' ->
          Buffer.add_char b ' ';
          go (i + 1)
      | '%' when i + 2 < n -> (
          match (hex s.[i + 1], hex s.[i + 2]) with
          | Some h, Some l ->
              Buffer.add_char b (Char.chr ((h * 16) + l));
              go (i + 3)
          | _ ->
              Buffer.add_char b '%';
              go (i + 1))
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

let encode (fields : (string * string) list) : string =
  String.concat "&" (List.map (fun (name, value) -> escape name ^ "=" ^ escape value) fields)

let decode (s : string) : (string * string) list =
  String.split_on_char '&' s
  |> List.filter (fun field -> field <> "")
  |> List.map (fun field ->
         match String.index_opt field '=' with
         | Some i -> (unescape (String.sub field 0 i), unescape (String.sub field (i + 1) (String.length field - i - 1)))
         | None -> (unescape field, ""))
