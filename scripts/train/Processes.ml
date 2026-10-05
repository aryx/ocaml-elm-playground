(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Processes.mli *)

(* a job's result, or why it has none: sent back either way, so that
 * a process that fails says so. The first version sent the result
 * alone, and a job that raised sent nothing: the trainer of Go died
 * at its ninety-seventh iteration of a hundred on "End_of_file", with
 * no word of which game or why (notes_ai_dark_arts.md) *)
type 'a sent = Done of 'a | Failed of string

(* each job in a process of its own; what each sent back, in order *)
let attempts (jobs : (unit -> 'a) list) : 'a sent list =
  let started =
    List.map
      (fun job ->
        let (from_child, to_parent) = Unix.pipe () in
        match Unix.fork () with
        | 0 ->
            Unix.close from_child;
            let oc = Unix.out_channel_of_descr to_parent in
            let sent = try Done (job ()) with e -> Failed (Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ()) in
            Marshal.to_channel oc sent [];
            close_out oc;
            exit 0
        | pid ->
            Unix.close to_parent;
            (pid, Unix.in_channel_of_descr from_child))
      jobs
  in
  List.map
    (fun (pid, ic) ->
      (* nothing at all sent: the process was killed from outside *)
      let sent = try Marshal.from_channel ic with End_of_file -> Failed "it ended without a word (killed?)" in
      close_in ic;
      ignore (Unix.waitpid [] pid);
      sent)
    started

let together (jobs : (unit -> 'a) list) : 'a list =
  List.mapi
    (fun number sent ->
      match sent with
      | Done result -> result
      | Failed why -> failwith (Printf.sprintf "process %d of %d: %s" number (List.length jobs) why))
    (attempts jobs)

(* the same, for jobs a run can do without one of: those that failed
 * are said and left out *)
let those_that_finish (what : string) (jobs : (unit -> 'a) list) : 'a list =
  List.concat
    (List.mapi
       (fun number sent ->
         match sent with
         | Done result -> [ result ]
         | Failed why ->
             Printf.printf "  one of the %s was lost (process %d): %s\n%!" what number why;
             [])
       (attempts jobs))
