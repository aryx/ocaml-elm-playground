(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Program.mli *)

let collecting = ref false

(* in reverse link order *)
let entries : (string * (unit -> unit)) list ref = ref []

let collect () : unit = collecting := true

let main (name : string) (entry : unit -> unit) : unit =
  if not !collecting then entry ()
  else if List.mem_assoc name !entries then
    failwith (Printf.sprintf "Program.main: two programs named %s" name)
  else entries := (name, entry) :: !entries

let collected () : (string * (unit -> unit)) list = List.rev !entries

(* None: the process's own, Sys.argv *)
let given_argv : string array option ref = ref None

let argv () : string array =
  match !given_argv with
  | Some a -> a
  | None -> Sys.argv

let run (name : string) ~(argv : string array) : unit =
  let entry = List.assoc name !entries in
  given_argv := Some argv;
  entry ()
