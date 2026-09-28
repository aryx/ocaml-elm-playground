(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tls_tunnel.mli *)

let program = "openssl"

let connect (caps : < Cap.exec ; .. >) ~(host : string) ~(port : int) : (Transport.t, string) result =
  let (_ : Cap.Exec.t) = caps#exec program in
  try
    let to_r, to_w = Unix.pipe ~cloexec:true () and from_r, from_w = Unix.pipe ~cloexec:true () in
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY; Unix.O_CLOEXEC ] 0 in
    let address = Printf.sprintf "%s:%d" host port in
    let pid =
      Unix.create_process program
        [| program; "s_client"; "-quiet"; "-verify_return_error"; "-connect"; address; "-servername"; host |]
        to_r from_w devnull
    in
    List.iter Unix.close [ to_r; from_w; devnull ];
    Unix.set_nonblock from_r;
    (* writing to a tunnel that ended would kill us with SIGPIPE: an
       error instead (EPIPE), which ends the connection *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    let pending = Buffer.create 4096 and closed = ref None in
    let finish why =
      if !closed = None then (
        closed := Some why;
        (try Unix.close to_w with Unix.Unix_error _ -> ());
        (try Unix.close from_r with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [ Unix.WNOHANG ] pid))
    in
    let send line =
      if !closed = None then
        let s = line ^ "\r\n" in
        try ignore (Unix.write_substring to_w s 0 (String.length s)) with Unix.Unix_error (e, _, _) -> finish (Unix.error_message e)
    in
    let receive () =
      let chunk = Bytes.create 65536 in
      let rec read () =
        if !closed = None then
          match Unix.read from_r chunk 0 (Bytes.length chunk) with
          | 0 -> finish "closed by the server"
          | n ->
              Buffer.add_subbytes pending chunk 0 n;
              read ()
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
          | exception Unix.Unix_error (e, _, _) -> finish (Unix.error_message e)
      in
      read ();
      (* the complete lines; a partial one waits for its end *)
      let all = Buffer.contents pending in
      match String.rindex_opt all '\n' with
      | None -> []
      | Some i ->
          Buffer.clear pending;
          Buffer.add_string pending (String.sub all (i + 1) (String.length all - i - 1));
          String.split_on_char '\n' (String.sub all 0 i)
          |> List.map (fun l -> if l <> "" && l.[String.length l - 1] = '\r' then String.sub l 0 (String.length l - 1) else l)
    in
    let status () = match !closed with None -> "over TLS to " ^ address ^ " (openssl)" | Some why -> address ^ ": " ^ why in
    Ok { Transport.send; receive; status; player = (fun () -> None) }
  with Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "can't run %s: %s" program (Unix.error_message e))
