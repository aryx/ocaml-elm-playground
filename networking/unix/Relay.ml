(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Relay.mli *)

type client = {
  fd : Unix.file_descr;
  mutable inbox : string; (* bytes read, not yet understood *)
  mutable outbox : string; (* bytes to write, when the socket takes them *)
  mutable upgraded : bool; (* past the WebSocket handshake *)
  mutable player : int option;
  mutable closing : bool; (* closed once its outbox is written *)
}

type t = { listener : Unix.file_descr; seats : int; mutable clients : client list; mutable forwarded : int }

let listen (caps : < Cap.network ; .. >) ~(bind : string) ~(port : int) ~(players : int) : t * int =
  let (_ : Cap.Network.t) = caps#network bind in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_of_string bind, port));
  Unix.listen fd 16;
  Unix.set_nonblock fd;
  let port = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
  ({ listener = fd; seats = players; clients = []; forwarded = 0 }, port)

let players (t : t) : int list = List.filter_map (fun c -> c.player) t.clients
let forwarded (t : t) : int = t.forwarded
let send (c : client) (f : Websocket.frame) : unit = c.outbox <- c.outbox ^ Websocket.encode f

(* the lowest number no connected player has *)
let free_seat (t : t) : int option =
  let taken = players t in
  List.find_opt (fun n -> not (List.mem n taken)) (List.init t.seats Fun.id)

(*****************************************************************************)
(* One step of each connection *)
(*****************************************************************************)

let accept_all (t : t) : unit =
  let rec go () =
    match Unix.accept t.listener with
    | fd, _ ->
        Unix.set_nonblock fd;
        t.clients <- t.clients @ [ { fd; inbox = ""; outbox = ""; upgraded = false; player = None; closing = false } ];
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
  go ()

(* the handshake, then a seat: the player's number, or no *)
let upgrade (t : t) (c : client) : unit =
  match Websocket.handshake c.inbox with
  | None -> ()
  | Some (headers, stop) -> (
      c.inbox <- String.sub c.inbox stop (String.length c.inbox - stop);
      match List.assoc_opt "sec-websocket-key" headers with
      | None -> c.closing <- true
      | Some key -> (
          c.outbox <- c.outbox ^ Websocket.response ~key;
          c.upgraded <- true;
          match free_seat t with
          | Some n ->
              c.player <- Some n;
              send c { fin = true; opcode = Binary; payload = String.init 2 (fun i -> Char.chr (if i = 0 then 2 else n)) }
          | None ->
              send c { fin = true; opcode = Close; payload = "" };
              c.closing <- true))

(* every whole frame: a packet copied to the others, a ping answered *)
let rec frames (t : t) (c : client) : unit =
  match Websocket.decode c.inbox with
  | Incomplete -> ()
  | Bad _ -> c.closing <- true
  | Frame (f, n) ->
      c.inbox <- String.sub c.inbox n (String.length c.inbox - n);
      (match f.opcode with
      | Binary ->
          List.iter
            (fun o ->
              if o != c && o.player <> None then begin
                send o { fin = true; opcode = Binary; payload = f.payload };
                t.forwarded <- t.forwarded + 1
              end)
            t.clients
      | Ping -> send c { fin = true; opcode = Pong; payload = f.payload }
      | Close -> c.closing <- true
      | _ -> ());
      frames t c

(* as much of the outbox as the socket takes now *)
let write (c : client) : unit =
  if c.outbox <> "" then
    match Unix.write_substring c.fd c.outbox 0 (String.length c.outbox) with
    | n -> c.outbox <- String.sub c.outbox n (String.length c.outbox - n)
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    | exception Unix.Unix_error _ ->
        c.outbox <- "";
        c.closing <- true

let step (t : t) : unit =
  accept_all t;
  List.iter
    (fun c ->
      read c;
      if not c.upgraded then upgrade t c;
      if c.upgraded then frames t c)
    t.clients;
  List.iter write t.clients;
  (* the connections ended, once they have said what they had to *)
  let gone, kept = List.partition (fun c -> c.closing && c.outbox = "") t.clients in
  List.iter (fun c -> try Unix.close c.fd with Unix.Unix_error _ -> ()) gone;
  t.clients <- kept

let wait (t : t) (timeout : float) : unit =
  let reads = t.listener :: List.map (fun c -> c.fd) t.clients in
  let writes = List.filter_map (fun c -> if c.outbox <> "" then Some c.fd else None) t.clients in
  try ignore (Unix.select reads writes [] timeout) with Unix.Unix_error (Unix.EINTR, _, _) -> ()
