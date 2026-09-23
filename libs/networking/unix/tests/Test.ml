(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* the tests reach the network (localhost): the capability from here *)
let () =
  Cap.main (fun caps ->
      Testo.interpret_argv ~project_name:"networking_unix" (fun _env ->
          Unit_http_client.tests caps @ Unit_http_request.tests caps @ Unit_udp.tests caps @ Unit_relay.tests caps @ Unit_universe.tests caps @ Unit_irc_server.tests caps))
