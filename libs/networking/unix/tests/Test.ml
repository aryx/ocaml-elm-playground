(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* claude: whether a socket may listen on localhost here. Not in opam's
 * sandbox on macOS, which forbids the network to a build: bind fails
 * with EPERM, and every test below with it (opam's CI, 0.3.0). The
 * tests are then skipped, saying why, rather than failed *)
let sockets_allowed () : bool =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close sock)
    (fun () ->
      match Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) with
      | () -> true
      | exception Unix.Unix_error ((Unix.EPERM | Unix.EACCES), _, _) -> false)

(* the tests reach the network (localhost): the capability from here *)
let () =
  Cap.main (fun caps ->
      Testo.interpret_argv ~project_name:"networking_unix" (fun _env ->
          let with_sockets =
            Unit_http_client.tests caps @ Unit_http_request.tests caps @ Unit_udp.tests caps @ Unit_relay.tests caps @ Unit_universe.tests caps @ Unit_irc_server.tests caps @ Unit_mail_server.tests caps @ Unit_tls_tunnel.tests caps @ Unit_tls_client.tests caps @ Unit_http_server.tests caps
          in
          let with_sockets =
            if sockets_allowed () then with_sockets
            else List.map (fun (t : Testo.t) -> if t.skipped = None then Testo.update ~skipped:(Some "no sockets here: a sandbox without network") t else t) with_sockets
          in
          with_sockets @ Unit_worker.tests))
