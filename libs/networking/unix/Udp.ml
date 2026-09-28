(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Udp.mli *)

let address (host : string) (port : int) : Unix.sockaddr =
  match Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_DGRAM; Unix.AI_FAMILY Unix.PF_INET ] with
  | a :: _ -> a.ai_addr
  | [] -> failwith (Printf.sprintf "Udp: can't resolve %S" host)

let show (a : Unix.sockaddr) : string =
  match a with
  | Unix.ADDR_INET (ip, port) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
  | Unix.ADDR_UNIX path -> path

let socket () : Unix.file_descr =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.set_nonblock fd;
  fd

(* every datagram waiting, and who sent it; never waits *)
let drain (fd : Unix.file_descr) : (Unix.sockaddr * string) list =
  let buf = Bytes.create 65536 in
  let rec go acc =
    match Unix.recvfrom fd buf 0 (Bytes.length buf) [] with
    | n, from -> go ((from, Bytes.sub_string buf 0 n) :: acc)
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.ECONNREFUSED), _, _) -> List.rev acc
  in
  go []

(* a lost send is a lost packet, which lockstep survives: never raise *)
let send_to (fd : Unix.file_descr) (a : Unix.sockaddr) (s : string) : unit =
  try ignore (Unix.sendto_substring fd s 0 (String.length s) [] a) with Unix.Unix_error _ -> ()

let host (caps : < Cap.network ; .. >) ~(bind : string) ~(port : int) : Transport.t * int =
  let (_ : Cap.Network.t) = caps#network bind in
  let fd = socket () in
  Unix.setsockopt fd Unix.SO_REUSEADDR true;
  Unix.bind fd (address bind port);
  let port = match Unix.getsockname fd with Unix.ADDR_INET (_, p) -> p | _ -> port in
  (* the player: whoever speaks first *)
  let player = ref None in
  let transport : Transport.t =
    {
      send = (fun s -> Option.iter (fun a -> send_to fd a s) !player);
      receive =
        (fun () ->
          drain fd
          |> List.filter_map (fun (from, s) ->
                 if !player = None then player := Some from;
                 if !player = Some from then Some s else None));
      status =
        (fun () ->
          match !player with
          | None -> Printf.sprintf "hosting on %s:%d, waiting for a player" bind port
          | Some a -> Printf.sprintf "hosting on %s:%d, playing with %s" bind port (show a));
      player = (fun () -> Some 0);
    }
  in
  (transport, port)

let join (caps : < Cap.network ; .. >) ~(host : string) ~(port : int) : Transport.t =
  let (_ : Cap.Network.t) = caps#network host in
  let fd = socket () in
  let there = address host port in
  let heard = ref false in
  {
    send = (fun s -> send_to fd there s);
    receive =
      (fun () ->
        drain fd
        |> List.filter_map (fun (from, s) ->
               if from = there then (
                 heard := true;
                 Some s)
               else None));
    status =
      (fun () ->
        if !heard then Printf.sprintf "playing with %s" (show there)
        else Printf.sprintf "joining %s, waiting for an answer" (show there));
    player = (fun () -> Some 1);
  }

let connect (caps : < Cap.network ; .. >) (role : Transport.role) : (Transport.t, string) result =
  try
    match role with
    | Host { bind; port } -> Ok (fst (host caps ~bind ~port))
    | Join { host = h; port } -> Ok (join caps ~host:h ~port)
    | Relay _ -> Error "a relay is a WebSocket, not UDP (Relay_client.mli)"
  with
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  | Failure why -> Error why
