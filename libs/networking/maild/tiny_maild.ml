(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The mail server of TinyEudora (networking/unix/Mail_server.mli):
 * SMTP in, POP3 out, a maildrop per user, over WebSocket. Its
 * parameters as name=value: smtp (8025) and pop (8110), the ports; bind
 * (127.0.0.1: this computer only); domain (tiny: mail for bob@tiny is
 * bob's); users (alice:x,bob:y -- passwords checked; any, without it);
 * spool (a directory: each maildrop an mbox file there, user.mbox,
 * read at the start and written as it changes, so the mail survives
 * the server). Then:
 *
 *   dune exec apps/internet/TinyEudora.exe -- user=alice@tiny
 *   dune exec apps/internet/TinyEudora.exe -- user=bob@tiny *)

let flag (name : string) (default : string) : string =
  Array.to_list Sys.argv
  |> List.find_map (fun a ->
         match String.index_opt a '=' with
         | Some i when String.sub a 0 i = name -> Some (String.sub a (i + 1) (String.length a - i - 1))
         | _ -> None)
  |> Option.value ~default

(* "alice:x,bob:y" *)
let users (s : string) : (string * string) list option =
  if s = "" then None
  else Some (List.filter_map (fun p -> match String.split_on_char ':' p with [ u; pw ] -> Some (u, pw) | _ -> None) (String.split_on_char ',' s))

(* the spool's mbox files, user.mbox *)
let read_spool (caps : < Cap.open_in ; Cap.readdir ; .. >) (dir : string) : (string * Mbox.entry list) list =
  let (_ : Cap.FS_.readdir) = caps#readdir dir in
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.filter_map (fun f ->
         if Filename.check_suffix f ".mbox" then
           let ic = CapStdlib.open_in caps (Filename.concat dir f) in
           let text = Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic)) in
           Some (Filename.chop_suffix f ".mbox", Mbox.parse text)
         else None)

let write_spool (caps : < Cap.open_out ; .. >) (dir : string) (user : string) (entries : Mbox.entry list) : unit =
  let path = Filename.concat dir (user ^ ".mbox") in
  let (_ : Cap.FS_.open_out) = caps#open_out path in
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc (Mbox.to_string entries))

let () =
  Cap.main (fun caps ->
      let bind = flag "bind" "127.0.0.1" and domain = flag "domain" "tiny" and spool = flag "spool" "" in
      let maildrops = if spool = "" then [] else read_spool caps spool in
      let changed = if spool = "" then fun _ _ -> () else write_spool caps spool in
      let server, smtp, pop =
        Mail_server.create caps ~bind ~smtp_port:(int_of_string (flag "smtp" "8025")) ~pop_port:(int_of_string (flag "pop" "8110")) ~domain
          ?passwords:(users (flag "users" "")) ~maildrops ~changed ()
      in
      Printf.printf "TinyEudora's mail server for @%s: SMTP on %s:%d, POP3 on %s:%d%s\n%!" domain bind smtp bind pop
        (if spool = "" then " (in memory)" else ", spool " ^ spool);
      while true do
        Mail_server.wait server 0.1;
        Mail_server.step server ~now:(Unix.gettimeofday ())
      done)
