(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Transport.mli *)

type t = { send : string -> unit; receive : unit -> string list; status : unit -> string; player : unit -> int option }

type role =
  | Host of { bind : string; port : int }
  | Join of { host : string; port : int }
  | Relay of { host : string; port : int }

let installed : (Cap.network -> role -> (t, string) result) ref = ref (fun _ _ -> Error "no network on this platform")
let set_connect f = installed := f
let connect (caps : < Cap.network ; .. >) (role : role) : (t, string) result = !installed (caps :> Cap.network) role
