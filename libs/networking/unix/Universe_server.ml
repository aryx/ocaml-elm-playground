(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Universe_server.mli *)

type iworld = int
type 'u bundle = 'u * (iworld * string) list * iworld list

type 'u t = {
  server : Server.t;
  mutable state : 'u;
  on_new : 'u -> iworld -> 'u bundle;
  on_msg : 'u -> iworld -> string -> 'u bundle;
  on_disconnect : 'u -> iworld -> 'u bundle;
}

(* the default handlers change nothing *)
let nothing (u : 'u) : 'u bundle = (u, [], [])

let create (caps : < Cap.network ; .. >) ?(bind = "127.0.0.1") ?(port = 4567) (state : 'u)
    ?(on_new = fun u _ -> nothing u) ?(on_msg = fun u _ _ -> nothing u) ?(on_disconnect = fun u _ -> nothing u) () :
    'u t * int =
  let server, port = Server.listen caps ~bind ~port () in
  ({ server; state; on_new; on_msg; on_disconnect }, port)

let state (t : 'u t) : 'u = t.state

(* a bundle: the new state, its mails posted, its worlds dropped *)
let deliver (t : 'u t) ((state, mails, drops) : 'u bundle) : unit =
  t.state <- state;
  List.iter (fun (w, message) -> Server.send t.server w message) mails;
  List.iter (Server.close t.server) drops

let step (t : 'u t) : unit =
  List.iter
    (fun (e : Server.event) ->
      match e with
      | Joined w ->
          (* the world told it is in (as the relay tells a player its
           * number: a transport is connected once it knows) *)
          Server.send t.server w (String.init 2 (fun i -> Char.chr (if i = 0 then 2 else w land 255)));
          deliver t (t.on_new t.state w)
      | Message (w, message) -> deliver t (t.on_msg t.state w message)
      | Left w -> deliver t (t.on_disconnect t.state w))
    (Server.step t.server);
  Server.flush t.server

let universe (caps : < Cap.network ; .. >) ?bind ?port (state : 'u) ?on_new ?on_msg ?on_disconnect () : unit =
  let t, _ = create caps ?bind ?port state ?on_new ?on_msg ?on_disconnect () in
  while true do
    Server.wait t.server 1.0;
    step t
  done
