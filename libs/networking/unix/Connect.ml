(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Connect.mli *)

let connect (caps : < Cap.network ; .. >) (role : Transport.role) : (Transport.t, string) result =
  match role with
  | Host _ | Join _ -> Udp.connect caps role
  | Relay { host; port } -> (
      try Ok (Relay_client.connect caps ~host ~port) with
      | Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "can't reach %s:%d: %s" host port (Unix.error_message e))
      | Failure why -> Error why)
