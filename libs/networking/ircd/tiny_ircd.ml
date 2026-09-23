(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The IRC server of TinyIRC (networking/unix/Irc_server.mli): users,
 * nicks, channels, over WebSocket. Its parameters as name=value: port
 * (6667), bind (127.0.0.1: this computer only; 0.0.0.0 for every network
 * it is on). Then:
 *
 *   dune exec apps/internet/TinyIRC.exe -- nick=alice
 *   http://localhost:8001/apps/internet/web/TinyIRC.html?nick=bob *)

let flag (name : string) (default : string) : string =
  Array.to_list Sys.argv
  |> List.find_map (fun a ->
         match String.index_opt a '=' with
         | Some i when String.sub a 0 i = name -> Some (String.sub a (i + 1) (String.length a - i - 1))
         | _ -> None)
  |> Option.value ~default

let () =
  Cap.main (fun caps ->
      let bind = flag "bind" "127.0.0.1" in
      let irc, port = Irc_server.create caps ~bind ~port:(int_of_string (flag "port" "6667")) () in
      Printf.printf "TinyIRC's server on %s:%d\n%!" bind port;
      while true do
        Irc_server.wait irc 1.0;
        Irc_server.step irc
      done)
