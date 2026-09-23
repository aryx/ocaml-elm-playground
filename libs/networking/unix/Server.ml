(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Server.mli *)

type client = {
  id : int;
  fd : Unix.file_descr;
  mutable inbox : string; (* bytes read, not yet understood *)
  mutable outbox : string; (* bytes to write, when the socket takes them *)
  mutable upgraded : bool; (* past the WebSocket handshake *)
  mutable closing : bool; (* closed once its outbox is written *)
}

type event = Joined of int | Message of int * string | Left of int
type t = { listener : Unix.file_descr; mutable clients : client list; mutable next_id : int }

let listen (caps : < Cap.network ; .. >) ~(bind : string) ~(port : int) : t * int =
  let (_ : Cap.Network.t) = caps#network bind in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_of_string bind, port));
  Unix.listen fd 16;
  Unix.set_nonblock fd;
  let port = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
  ({ listener = fd; clients = []; next_id = 0 }, port)

let clients (t : t) : int list = List.filter_map (fun c -> if c.upgraded && not c.closing then Some c.id else None) t.clients
let frame (c : client) (f : Websocket.frame) : unit = c.outbox <- c.outbox ^ Websocket.encode f

let send (t : t) (id : int) (payload : string) : unit =
  List.iter (fun c -> if c.id = id && not c.closing then frame c { fin = true; opcode = Binary; payload }) t.clients

let close (t : t) (id : int) : unit =
  List.iter
    (fun c ->
      if c.id = id && not c.closing then begin
        frame c { fin = true; opcode = Close; payload = "" };
        c.closing <- true
      end)
    t.clients

(*****************************************************************************)
(* One step of each connection *)
(*****************************************************************************)

let accept_all (t : t) : unit =
  let rec go () =
    match Unix.accept t.listener with
    | fd, _ ->
        Unix.set_nonblock fd;
        t.clients <- t.clients @ [ { id = t.next_id; fd; inbox = ""; outbox = ""; upgraded = false; closing = false } ];
        t.next_id <- t.next_id + 1;
        go ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
  in
  go ()

(* what arrived; a connection ended or broken is closing *)
let read (c : client) : unit =
  let buf = Bytes.create 65536 in
  let rec go () =
    match Unix.read c.fd buf 0 (Bytes.length buf) with
    | 0 -> c.closing <- true
    | n ->
        c.inbox <- c.inbox ^ Bytes.sub_string buf 0 n;
        go ()
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ -> c.closing <- true
  in
  if not c.closing then go ()

(* the handshake: the connection becomes a client *)
let upgrade (c : client) : event list =
  match Websocket.handshake c.inbox with
  | None -> []
  | Some (headers, stop) -> (
      c.inbox <- String.sub c.inbox stop (String.length c.inbox - stop);
      match List.assoc_opt "sec-websocket-key" headers with
      | None ->
          c.closing <- true;
          []
      | Some key ->
          c.outbox <- c.outbox ^ Websocket.response ~key;
          c.upgraded <- true;
          [ Joined c.id ])

(* every whole frame: a message, a ping answered, a close *)
let rec frames (c : client) (acc : event list) : event list =
  match Websocket.decode c.inbox with
  | Incomplete -> List.rev acc
  | Bad _ ->
      c.closing <- true;
      List.rev acc
  | Frame (f, n) -> (
      c.inbox <- String.sub c.inbox n (String.length c.inbox - n);
      match f.opcode with
      | Binary -> frames c (Message (c.id, f.payload) :: acc)
      | Ping ->
          frame c { fin = true; opcode = Pong; payload = f.payload };
          frames c acc
      | Close ->
          c.closing <- true;
          List.rev acc
      | _ -> frames c acc)

(* as much of the outbox as the socket takes now *)
let write (c : client) : unit =
  if c.outbox <> "" then
    match Unix.write_substring c.fd c.outbox 0 (String.length c.outbox) with
    | n -> c.outbox <- String.sub c.outbox n (String.length c.outbox - n)
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ ->
        c.outbox <- "";
        c.closing <- true

let flush (t : t) : unit = List.iter write t.clients

let step (t : t) : event list =
  accept_all t;
  let events =
    List.concat_map
      (fun c ->
        read c;
        let joined = if c.upgraded then [] else upgrade c in
        joined @ if c.upgraded then frames c [] else [])
      t.clients
  in
  List.iter write t.clients;
  (* the connections ended, once they have said what they had to *)
  let gone, kept = List.partition (fun c -> c.closing && c.outbox = "") t.clients in
  List.iter (fun c -> try Unix.close c.fd with Unix.Unix_error _ -> ()) gone;
  t.clients <- kept;
  events @ List.filter_map (fun c -> if c.upgraded then Some (Left c.id) else None) gone

let wait (t : t) (timeout : float) : unit =
  let reads = t.listener :: List.map (fun c -> c.fd) t.clients in
  let writes = List.filter_map (fun c -> if c.outbox <> "" then Some c.fd else None) t.clients in
  try ignore (Unix.select reads writes [] timeout) with Unix.Unix_error (Unix.EINTR, _, _) -> ()
