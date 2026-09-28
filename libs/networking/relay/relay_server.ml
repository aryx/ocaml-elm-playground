(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The relay server: players connect to it (WebSocket, from a browser
 * or a native program), and it copies each one's packets to the others
 * (networking/unix/Relay.mli). Its parameters as name=value: port (8765),
 * bind (127.0.0.1: this computer only; 0.0.0.0 for every network it is
 * on), players (2). Then, e.g.:
 *
 *   dune exec games/arcade/TinySpacewar.exe -- net=relay
 *   http://localhost:8001/games/arcade/web/TinySpacewar.html?net=relay
 *
 * a native player and a browser in the same duel. *)

let flag (name : string) (default : string) : string =
  Array.to_list Sys.argv
  |> List.find_map (fun a ->
         match String.index_opt a '=' with
         | Some i when String.sub a 0 i = name -> Some (String.sub a (i + 1) (String.length a - i - 1))
         | _ -> None)
  |> Option.value ~default

let () =
  Cap.main (fun caps ->
      let bind = flag "bind" "127.0.0.1" and players = int_of_string (flag "players" "2") in
      let relay, port = Relay.listen caps ~bind ~port:(int_of_string (flag "port" "8765")) ~players in
      Printf.printf "relay on %s:%d, for %d players\n%!" bind port players;
      let seen = ref [] in
      while true do
        Relay.wait relay 1.0;
        Relay.step relay;
        let now = Relay.players relay in
        if now <> !seen then begin
          Printf.printf "players connected: %s\n%!" (String.concat ", " (List.map string_of_int now));
          seen := now
        end
      done)
