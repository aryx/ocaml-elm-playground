(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tcp.mli *)

let default_timeout = 30.

let connect ?(timeout = default_timeout) ~(host : string) ~(port : int) () : Unix.file_descr =
  let addresses = Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] in
  if addresses = [] then failwith (Printf.sprintf "Tcp.connect: can't resolve %S" host);
  (* each address in turn, the last one's error raised *)
  let rec try_ = function
    | [] -> assert false
    | (a : Unix.addr_info) :: others -> (
        let fd = Unix.socket a.ai_family a.ai_socktype a.ai_protocol in
        try
          Unix.setsockopt_float fd Unix.SO_RCVTIMEO timeout;
          Unix.setsockopt_float fd Unix.SO_SNDTIMEO timeout;
          Unix.connect fd a.ai_addr;
          fd
        with Unix.Unix_error _ as e ->
          Unix.close fd;
          if others = [] then raise e else try_ others)
  in
  try_ addresses

let send_all (fd : Unix.file_descr) (s : string) : unit =
  let rec go pos = if pos < String.length s then go (pos + Unix.write_substring fd s pos (String.length s - pos)) in
  go 0

let receive_all (fd : Unix.file_descr) : string =
  let b = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec go () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents b
    | n ->
        Buffer.add_subbytes b chunk 0 n;
        go ()
  in
  go ()

let exchange ?timeout ~(host : string) ~(port : int) (s : string) : string =
  let fd = connect ?timeout ~host ~port () in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      send_all fd s;
      receive_all fd)
