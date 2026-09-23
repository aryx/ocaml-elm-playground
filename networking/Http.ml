(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Http.mli *)

type header = string * string

let header (name : string) (headers : header list) : string option =
  let name = String.lowercase_ascii name in
  List.find_map (fun (n, v) -> if String.lowercase_ascii n = name then Some v else None) headers

let ( let* ) = Result.bind

(*****************************************************************************)
(* The request *)
(*****************************************************************************)

type request = { meth : string; target : string; headers : header list }

let get ~(host : string) (target : string) : request =
  { meth = "GET"; target; headers = [ ("Host", host); ("User-Agent", "elm_playground"); ("Connection", "close") ] }

let request_to_string (r : request) : string =
  let b = Buffer.create 128 in
  Buffer.add_string b (Printf.sprintf "%s %s HTTP/1.1\r\n" r.meth r.target);
  List.iter (fun (n, v) -> Buffer.add_string b (Printf.sprintf "%s: %s\r\n" n v)) r.headers;
  Buffer.add_string b "\r\n";
  Buffer.contents b

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

(* the line of [s] starting at [pos], without its CR LF (or lone LF),
 * and where the next one starts; None if the line has no end *)
let line_at (s : string) (pos : int) : (string * int) option =
  if pos > String.length s then None
  else
    match String.index_from_opt s pos '\n' with
    | None -> None
    | Some i ->
        let stop = if i > pos && s.[i - 1] = '\r' then i - 1 else i in
        Some (String.sub s pos (stop - pos), i + 1)

(* "Content-Length: 1523" -> ("Content-Length", "1523"); no space is
 * allowed before the ':' (RFC 9112 section 5.1), and a line starting
 * with a space, HTTP/1.0's continuation of the previous header
 * ("obsolete line folding"), is refused *)
let parse_header (line : string) : (header, string) result =
  match String.index_opt line ':' with
  | Some i when i > 0 && line.[0] <> ' ' && line.[0] <> '\t' && line.[i - 1] <> ' ' ->
      Ok (String.sub line 0 i, String.trim (String.sub line (i + 1) (String.length line - i - 1)))
  | _ -> Error (Printf.sprintf "Http: bad header line %S" line)

(* the header lines from [pos] up to the empty line, and where the body
 * starts *)
let parse_headers (s : string) (pos : int) : (header list * int, string) result =
  let rec go pos acc =
    match line_at s pos with
    | None -> Error "Http: the headers have no end (no empty line)"
    | Some ("", pos) -> Ok (List.rev acc, pos)
    | Some (line, pos) ->
        let* h = parse_header line in
        go pos (h :: acc)
  in
  go pos []

(*****************************************************************************)
(* The response *)
(*****************************************************************************)

type response = { version : string; status : int; reason : string; headers : header list; body : string }

let parse_status_line (line : string) : (string * int * string, string) result =
  let bad () = Error (Printf.sprintf "Http: bad status line %S" line) in
  match String.index_opt line ' ' with
  | Some i
    when String.starts_with ~prefix:"HTTP/" line
         && String.length line >= i + 4
         && (String.length line = i + 4 || line.[i + 4] = ' ') -> (
      let version = String.sub line 0 i in
      let code = String.sub line (i + 1) 3 in
      let reason = String.trim (String.sub line (i + 4) (String.length line - i - 4)) in
      match int_of_string_opt code with
      | Some status when String.for_all (fun c -> c >= '0' && c <= '9') code -> Ok (version, status, reason)
      | _ -> bad ())
  | _ -> bad ()

(* "1a" -> 26, hexadecimal digits only (OCaml's int_of_string "0x..."
 * would accept '_' too) *)
let parse_hex (s : string) : int option =
  let digit c =
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
    | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
    | _ -> None
  in
  if s = "" || String.length s > 15 then None
  else
    String.fold_left (fun acc c -> match (acc, digit c) with Some n, Some d -> Some ((n * 16) + d) | _ -> None) (Some 0) s

(* size [;extensions] CRLF data CRLF, again, until a size of 0, then
 * the trailer headers (ignored) up to an empty line *)
let dechunk (s : string) : (string, string) result =
  let b = Buffer.create (String.length s) in
  let rec chunk pos =
    match line_at s pos with
    | None -> Error "Http: chunked body ends before its last chunk"
    | Some (line, pos) -> (
        let size = String.trim (match String.index_opt line ';' with Some i -> String.sub line 0 i | None -> line) in
        match parse_hex size with
        | None -> Error (Printf.sprintf "Http: bad chunk size %S" line)
        | Some 0 -> trailer pos
        | Some n when pos + n > String.length s -> Error "Http: chunked body ends inside a chunk"
        | Some n -> (
            Buffer.add_string b (String.sub s pos n);
            match line_at s (pos + n) with
            | Some ("", pos) -> chunk pos
            | _ -> Error "Http: a chunk's data isn't followed by CR LF"))
  and trailer pos =
    match line_at s pos with
    | None | Some ("", _) -> Ok (Buffer.contents b) (* a missing final CR LF forgiven *)
    | Some (_, pos) -> trailer pos
  in
  chunk 0

(* RFC 9112 section 6.3, the rules 1 to 4 of Http.mli, in order *)
let body ~(status : int) (headers : header list) (rest : string) : (string, string) result =
  if status / 100 = 1 || status = 204 || status = 304 then Ok ""
  else
    match header "Transfer-Encoding" headers with
    | Some te when String.lowercase_ascii te = "chunked" -> dechunk rest
    | Some te -> Error (Printf.sprintf "Http: transfer coding %S not supported" te)
    | None -> (
        match header "Content-Length" headers with
        | None -> Ok rest
        | Some n when n <> "" && String.length n <= 15 && String.for_all (fun c -> c >= '0' && c <= '9') n ->
            let n = int_of_string n in
            if String.length rest < n then
              Error (Printf.sprintf "Http: body truncated, %d bytes of %d" (String.length rest) n)
            else Ok (String.sub rest 0 n)
        | Some n -> Error (Printf.sprintf "Http: bad Content-Length %S" n))

let parse_response (s : string) : (response, string) result =
  let* line, pos = Option.to_result ~none:"Http: no status line" (line_at s 0) in
  let* version, status, reason = parse_status_line line in
  let* headers, pos = parse_headers s pos in
  let* () =
    match header "Content-Encoding" headers with
    | None -> Ok ()
    | Some ce when String.lowercase_ascii ce = "identity" -> Ok ()
    | Some ce -> Error (Printf.sprintf "Http: content coding %S not supported" ce)
  in
  let* body = body ~status headers (String.sub s pos (String.length s - pos)) in
  Ok { version; status; reason; headers; body }

let is_redirect (status : int) : bool = List.mem status [ 301; 302; 303; 307; 308 ]
