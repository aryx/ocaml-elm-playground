(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The universe of UniverseBall.ml's worlds (HtDP's first universe, see
 * networking/unix/Universe_server.mli): the worlds in a queue, the
 * first one holding the ball. A world joining an empty universe gets it
 * at once; when the one holding it says "done", it goes to the back of
 * the queue and the next one gets "go"; if the one holding it leaves,
 * the next one gets it. A program of its own, native only:
 *
 *   dune exec examples/UniverseBallServer.exe -- port=4567 bind=0.0.0.0 *)

(* the state: the worlds, the first one holding the ball *)
type universe = Universe_server.iworld list

let add_world (u : universe) (w : Universe_server.iworld) : universe Universe_server.bundle =
  if u = [] then ([ w ], [ (w, "go") ], []) else (u @ [ w ], [], [])

let pass_the_ball (u : universe) (w : Universe_server.iworld) (message : string) : universe Universe_server.bundle =
  match u with
  | first :: rest when first = w && message = "done" -> (
      let u = rest @ [ first ] in
      match u with next :: _ -> (u, [ (next, "go") ], []) | [] -> (u, [], []))
  | _ -> (u, [], [])

let remove_world (u : universe) (w : Universe_server.iworld) : universe Universe_server.bundle =
  let had_the_ball = match u with first :: _ -> first = w | [] -> false in
  let u = List.filter (( <> ) w) u in
  match u with next :: _ when had_the_ball -> (u, [ (next, "go") ], []) | _ -> (u, [], [])

let flag (name : string) (default : string) : string =
  Array.to_list Sys.argv
  |> List.find_map (fun a ->
         match String.index_opt a '=' with
         | Some i when String.sub a 0 i = name -> Some (String.sub a (i + 1) (String.length a - i - 1))
         | _ -> None)
  |> Option.value ~default

let () =
  Cap.main (fun caps ->
      Universe_server.universe caps ~bind:(flag "bind" "127.0.0.1") ~port:(int_of_string (flag "port" "4567")) []
        ~on_new:add_world ~on_msg:pass_the_ball ~on_disconnect:remove_world ())
