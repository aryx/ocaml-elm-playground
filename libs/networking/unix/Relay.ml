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

type t = {
  server : Server.t;
  seats : int;
  (* the connections that have a seat: their id, their player number *)
  mutable seated : (int * int) list;
  mutable forwarded : int;
}

let listen (caps : < Cap.network ; .. >) ~(bind : string) ~(port : int) ~(players : int) : t * int =
  let server, port = Server.listen caps ~bind ~port () in
  ({ server; seats = players; seated = []; forwarded = 0 }, port)

let players (t : t) : int list = List.map snd t.seated
let forwarded (t : t) : int = t.forwarded

(* the lowest number no connected player has *)
let free_seat (t : t) : int option =
  List.find_opt (fun n -> not (List.mem n (players t))) (List.init t.seats Fun.id)

(* the relay's whole rule: a seat for who comes (and its number, 02
 * then the number), each packet copied to the others *)
let step (t : t) : unit =
  List.iter
    (fun (e : Server.event) ->
      match e with
      | Joined id -> (
          match free_seat t with
          | Some n ->
              t.seated <- t.seated @ [ (id, n) ];
              Server.send t.server id (String.init 2 (fun i -> Char.chr (if i = 0 then 2 else n)))
          | None -> Server.close t.server id)
      | Message (id, packet) ->
          if List.mem_assoc id t.seated then
            List.iter
              (fun (other, _) ->
                if other <> id then begin
                  Server.send t.server other packet;
                  t.forwarded <- t.forwarded + 1
                end)
              t.seated
      | Left id -> t.seated <- List.remove_assoc id t.seated)
    (Server.step t.server);
  Server.flush t.server

let wait (t : t) (timeout : float) : unit = Server.wait t.server timeout
