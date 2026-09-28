(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Url.mli *)

type authority = { userinfo : string option; host : string; port : int option }

type t = {
  scheme : string option;
  authority : authority option;
  path : string;
  query : string option;
  fragment : string option;
}

(*****************************************************************************)
(* Parsing: appendix B's regexp, by hand *)
(*****************************************************************************)

(* the index of the first character of [s] at or after [from] that is
 * in [stops], or the length of [s] *)
let index_of_any (s : string) (from : int) (stops : string) : int =
  let rec go i = if i >= String.length s || String.contains stops s.[i] then i else go (i + 1) in
  go from

let starts_with ~prefix s = String.starts_with ~prefix s
let rest (s : string) (from : int) : string = String.sub s from (String.length s - from)

(* userinfo@host:port, section 3.2; the host an IP literal in brackets
 * ("[::1]") may hold ':'s, so the port is after the ']' *)
let parse_authority (s : string) : (authority, string) result =
  let userinfo, hostport =
    match String.index_opt s '@' with
    | Some i -> (Some (String.sub s 0 i), rest s (i + 1))
    | None -> (None, s)
  in
  let colon =
    if starts_with ~prefix:"[" hostport then
      match String.index_opt hostport ']' with
      | Some j when j + 1 < String.length hostport && hostport.[j + 1] = ':' -> Some (j + 1)
      | _ -> None
    else String.rindex_opt hostport ':'
  in
  match colon with
  | None -> Ok { userinfo; host = String.lowercase_ascii hostport; port = None }
  | Some i -> (
      let host = String.lowercase_ascii (String.sub hostport 0 i) in
      match rest hostport (i + 1) with
      | "" -> Ok { userinfo; host; port = None } (* "http://a:/", allowed, the default port *)
      | digits when String.for_all (fun c -> c >= '0' && c <= '9') digits && String.length digits <= 5 ->
          Ok { userinfo; host; port = Some (int_of_string digits) }
      | bad -> Error (Printf.sprintf "Url.parse: bad port %S" bad))

let parse (s : string) : (t, string) result =
  (* {|^(([^:/?#]+):)?|} *)
  let scheme, i =
    let j = index_of_any s 0 ":/?#" in
    if j > 0 && j < String.length s && s.[j] = ':' then (Some (String.lowercase_ascii (String.sub s 0 j)), j + 1)
    else (None, 0)
  in
  (* {|(//([^/?#]*))?|} *)
  let authority, i =
    if starts_with ~prefix:"//" (rest s i) then
      let j = index_of_any s (i + 2) "/?#" in
      (Some (String.sub s (i + 2) (j - i - 2)), j)
    else (None, i)
  in
  (* {|([^?#]*)|} *)
  let j = index_of_any s i "?#" in
  let path = String.sub s i (j - i) in
  (* {|(\?([^#]*))?|} *)
  let query, i =
    if j < String.length s && s.[j] = '?' then
      let k = index_of_any s (j + 1) "#" in
      (Some (String.sub s (j + 1) (k - j - 1)), k)
    else (None, j)
  in
  (* {|(#(.*))?|} *)
  let fragment = if i < String.length s then Some (rest s (i + 1)) else None in
  match authority with
  | None -> Ok { scheme; authority = None; path; query; fragment }
  | Some a -> (
      match parse_authority a with
      | Ok a -> Ok { scheme; authority = Some a; path; query; fragment }
      | Error e -> Error e)

(*****************************************************************************)
(* Recomposition, section 5.3 *)
(*****************************************************************************)

let authority_to_string (a : authority) : string =
  (match a.userinfo with Some u -> u ^ "@" | None -> "")
  ^ a.host
  ^ match a.port with Some p -> ":" ^ string_of_int p | None -> ""

let to_string (u : t) : string =
  let b = Buffer.create 64 in
  Option.iter (fun s -> Buffer.add_string b (s ^ ":")) u.scheme;
  Option.iter (fun a -> Buffer.add_string b ("//" ^ authority_to_string a)) u.authority;
  Buffer.add_string b u.path;
  Option.iter (fun q -> Buffer.add_string b ("?" ^ q)) u.query;
  Option.iter (fun f -> Buffer.add_string b ("#" ^ f)) u.fragment;
  Buffer.contents b

(*****************************************************************************)
(* Resolving a relative reference, section 5.2 *)
(*****************************************************************************)

(* the output's last segment and the '/' before it removed: "/a/b" -> "/a" *)
let drop_last_segment (out : string) : string =
  match String.rindex_opt out '/' with Some i -> String.sub out 0 i | None -> ""

(* section 5.2.4, its steps A to E: the input consumed from the left,
 * a segment at a time, into the output *)
let remove_dot_segments (path : string) : string =
  let rec go input out =
    if input = "" then out
    (* A: a leading "../" or "./" dropped *)
    else if starts_with ~prefix:"../" input then go (rest input 3) out
    else if starts_with ~prefix:"./" input then go (rest input 2) out
    (* B: "/./" or a final "/." becomes "/" *)
    else if starts_with ~prefix:"/./" input then go (rest input 2) out
    else if input = "/." then go "/" out
    (* C: "/../" or a final "/.." becomes "/", and the output's last
     * segment goes *)
    else if starts_with ~prefix:"/../" input then go (rest input 3) (drop_last_segment out)
    else if input = "/.." then go "/" (drop_last_segment out)
    (* D: a lone "." or ".." dropped *)
    else if input = "." || input = ".." then out
    (* E: the first segment, with its leading '/' if any, moved to the
     * output *)
    else
      let j = index_of_any input (if input.[0] = '/' then 1 else 0) "/" in
      go (rest input j) (out ^ String.sub input 0 j)
  in
  go path ""

(* section 5.2.3: the base's path up to its last '/', then the
 * reference's (and "/" for a base with an authority and no path) *)
let merge (base : t) (ref_path : string) : string =
  if base.authority <> None && base.path = "" then "/" ^ ref_path
  else
    match String.rindex_opt base.path '/' with
    | Some i -> String.sub base.path 0 (i + 1) ^ ref_path
    | None -> ref_path

(* section 5.2.2, its pseudo-code transcribed branch by branch *)
let resolve (base : t) (r : t) : t =
  if r.scheme <> None then { r with path = remove_dot_segments r.path }
  else if r.authority <> None then { r with scheme = base.scheme; path = remove_dot_segments r.path }
  else
    let path, query =
      if r.path = "" then (base.path, if r.query <> None then r.query else base.query)
      else if starts_with ~prefix:"/" r.path then (remove_dot_segments r.path, r.query)
      else (remove_dot_segments (merge base r.path), r.query)
    in
    { scheme = base.scheme; authority = base.authority; path; query; fragment = r.fragment }

(*****************************************************************************)
(* For HTTP *)
(*****************************************************************************)

let port (u : t) : int option =
  match u.authority with
  | Some { port = Some p; _ } -> Some p
  | _ -> ( match u.scheme with Some "http" -> Some 80 | Some "https" -> Some 443 | _ -> None)

let request_target (u : t) : string =
  (if u.path = "" then "/" else u.path) ^ match u.query with Some q -> "?" ^ q | None -> ""
