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

let tunnel_installed : (Cap.exec -> host:string -> port:int -> (t, string) result) ref =
  ref (fun _ ~host:_ ~port:_ -> Error "no TLS here: its tunnel runs a program, which a browser cannot")

let set_tunnel f = tunnel_installed := f
let tunnel (caps : < Cap.exec ; .. >) ~(host : string) ~(port : int) : (t, string) result = !tunnel_installed (caps :> Cap.exec) ~host ~port

let tls_installed : (Cap.network -> host:string -> port:int -> (t, string) result) ref = ref (fun _ ~host:_ ~port:_ -> Error "no TLS on this platform")
let set_tls f = tls_installed := f
let tls (caps : < Cap.network ; .. >) ~(host : string) ~(port : int) : (t, string) result = !tls_installed (caps :> Cap.network) ~host ~port
