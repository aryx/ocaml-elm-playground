(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tls_tunnel.mli *)

let tests (caps : < Cap.network ; Cap.exec ; .. >) =
  Testo.categorize "Tls_tunnel"
    [
      Testo.create "a self-signed certificate: refused" (fun () ->
          let (_ : Cap.Exec.t) = caps#exec "openssl" in
          let dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "tls_tunnel.%d" (Unix.getpid ())) in
          if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
          let key = Filename.concat dir "key.pem" and cert = Filename.concat dir "cert.pem" in
          let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
          let quiet argv = Unix.create_process argv.(0) argv null null null in
          ignore (Unix.waitpid [] (quiet [| "openssl"; "req"; "-x509"; "-newkey"; "rsa:2048"; "-nodes"; "-keyout"; key; "-out"; cert; "-days"; "1"; "-subj"; "/CN=localhost" |]));
          let port = 20000 + (Unix.getpid () mod 10000) in
          let server = quiet [| "openssl"; "s_server"; "-quiet"; "-accept"; string_of_int port; "-cert"; cert; "-key"; key |] in
          Fun.protect
            ~finally:(fun () -> Unix.kill server Sys.sigterm; ignore (Unix.waitpid [] server); Unix.close null)
            (fun () ->
              Unix.sleepf 0.5;
              match Tls_tunnel.connect caps ~host:"localhost" ~port with
              | Error why -> Alcotest.fail why
              | Ok t ->
                  t.send "hello";
                  let got = ref [] in
                  for _ = 1 to 30 do
                    got := !got @ t.receive ();
                    Unix.sleepf 0.1
                  done;
                  Alcotest.(check (list string)) "nothing through" [] !got;
                  Alcotest.(check bool) ("closed: " ^ t.status ()) true (not (String.starts_with ~prefix:"over TLS" (t.status ())))));
    ]
