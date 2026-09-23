(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Http_client.mli *)

let ( let* ) = Result.bind

let prepare (url : Url.t) : (string * int * string, string) result =
  match (url.scheme, url.authority, Url.port url) with
  | Some "http", Some (a : Url.authority), Some port ->
      (* the Host header says the port only when it isn't the default *)
      let host_header = match a.port with Some p -> Printf.sprintf "%s:%d" a.host p | None -> a.host in
      (* "[::1]" in a URL, "::1" for the resolver *)
      let host =
        if String.starts_with ~prefix:"[" a.host then String.sub a.host 1 (String.length a.host - 2) else a.host
      in
      Ok (host, port, Http.request_to_string (Http.get ~host:host_header (Url.request_target url)))
  | Some "http", _, _ -> Error (Printf.sprintf "%s: no host" (Url.to_string url))
  | Some "https", _, _ ->
      Error (Printf.sprintf "%s: https (HTTP inside TLS) is not ours yet, only http://" (Url.to_string url))
  | _ -> Error (Printf.sprintf "%s: not an http:// URL" (Url.to_string url))

(* one request, no redirection followed *)
let get_once ?timeout (caps : < Cap.network ; .. >) (url : Url.t) : (Http.response, string) result =
  let* host, port, request = prepare url in
  match Tcp.exchange ?timeout caps ~host ~port request with
  | answer -> Http.parse_response answer
  | exception Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "%s: %s" (Url.to_string url) (Unix.error_message e))
  | exception Failure msg -> Error msg

let get ?(max_redirects = 5) ?timeout (caps : < Cap.network ; .. >) (s : string) : (Http.response, string) result =
  let rec follow (url : Url.t) (left : int) =
    let* (response : Http.response) = get_once ?timeout caps url in
    match (Http.is_redirect response.status, Http.header "Location" response.headers) with
    | true, Some location ->
        if left = 0 then Error (Printf.sprintf "%s: too many redirections" s)
        else
          let* next = Url.parse location in
          follow (Url.resolve url next) (left - 1)
    | _ -> Ok response
  in
  let* url = Url.parse s in
  follow url max_redirects
