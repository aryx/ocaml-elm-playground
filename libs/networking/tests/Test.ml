(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"networking" (fun _env -> Unit_url.tests @ Unit_urlencoded.tests @ Unit_http.tests @ Unit_checksum.tests @ Unit_wire.tests @ Unit_sim_net.tests @ Unit_lockstep.tests @ Unit_rollback.tests @ Unit_websocket.tests @ Unit_irc.tests @ Unit_client_server.tests)
