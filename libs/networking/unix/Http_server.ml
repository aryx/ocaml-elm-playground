(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Http_server.mli *)

type connection = {
  fd : Unix.file_descr;
  peer : string;
  mutable inbox : string; (* bytes read, not yet a whole request *)
  mutable outbox : string; (* bytes to write, when the socket takes them *)
  mutable answered : bool; (* closed once its outbox is written *)
}

type t = { listener : Unix.file_descr; mutable connections : connection list }
type handler = peer:string -> Http.request -> string -> Http.response

let listen (caps : < Cap.network ; .. >) ~(bind : string) ~(port : int) : t * int =
  let (_ : Cap.Network.t) = caps#network bind in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_of_string bind, port));
  Unix.listen fd 16;
  Unix.set_nonblock fd;
  let port = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
  ({ listener = fd; connections = [] }, port)

let connections (t : t) : int = List.length t.connections

(*****************************************************************************)
(* One step of each connection *)
(*****************************************************************************)

let accept_all (t : t) : unit =
  let rec go () =
    match Unix.accept t.listener with
    | fd, addr ->
        Unix.set_nonblock fd;
        let peer = match addr with Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a | _ -> "?" in
        t.connections <- t.connections @ [ { fd; peer; inbox = ""; outbox = ""; answered = false } ];
        go ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
  in
  go ()

(* what arrived; a client gone before its answer is answered nothing *)
let read (c : connection) : unit =
  let buf = Bytes.create 65536 in
  let rec go () =
    match Unix.read c.fd buf 0 (Bytes.length buf) with
    | 0 -> c.answered <- true
    | n ->
        c.inbox <- c.inbox ^ Bytes.sub_string buf 0 n;
        go ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ -> c.answered <- true
  in
  if not c.answered then go ()

(* a whole request answered by the handler; garbage answered 400 *)
let answer (handler : handler) (c : connection) : unit =
  if not c.answered then
    match Http.parse_request c.inbox with
    | Incomplete -> ()
    | Bad why ->
        c.outbox <- Http.response_to_string (Http.response 400 ~content_type:"text/plain" (why ^ "\n"));
        c.answered <- true
    | Request (request, body, _) ->
        let response =
          try handler ~peer:c.peer request body
          with e -> Http.response 500 ~content_type:"text/plain" (Printexc.to_string e ^ "\n")
        in
        c.outbox <- Http.response_to_string response;
        c.answered <- true

(* as much of the outbox as the socket takes now *)
let write (c : connection) : unit =
  if c.outbox <> "" then
    match Unix.write_substring c.fd c.outbox 0 (String.length c.outbox) with
    | n -> c.outbox <- String.sub c.outbox n (String.length c.outbox - n)
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ -> c.outbox <- ""

let step (t : t) (handler : handler) : unit =
  accept_all t;
  List.iter
    (fun c ->
      read c;
      answer handler c;
      write c)
    t.connections;
  (* the connections answered, once their answer is written *)
  let done_, kept = List.partition (fun c -> c.answered && c.outbox = "") t.connections in
  List.iter (fun c -> try Unix.close c.fd with Unix.Unix_error _ -> ()) done_;
  t.connections <- kept

let wait (t : t) (timeout : float) : unit =
  let reads = t.listener :: List.filter_map (fun c -> if c.answered then None else Some c.fd) t.connections in
  let writes = List.filter_map (fun c -> if c.outbox <> "" then Some c.fd else None) t.connections in
  try ignore (Unix.select reads writes [] timeout) with Unix.Unix_error (Unix.EINTR, _, _) -> ()
