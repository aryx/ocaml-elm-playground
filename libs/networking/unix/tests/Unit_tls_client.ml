(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tls_client.mli *)

let dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "tls_client.%d" (Unix.getpid ()))
let null () = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0

let quiet (argv : string array) : int =
  let n = null () in
  let pid = Unix.create_process argv.(0) argv n n n in
  Unix.close n;
  pid

(* a self-signed certificate for localhost, its key of [kind] ("ec" or
 * "rsa") *)
let certificate (kind : string) : string * string =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
  let key = Filename.concat dir (kind ^ ".key") and cert = Filename.concat dir (kind ^ ".pem") in
  let newkey = if kind = "ec" then [| "-newkey"; "ec"; "-pkeyopt"; "ec_paramgen_curve:P-256" |] else [| "-newkey"; "rsa:2048" |] in
  ignore
    (Unix.waitpid []
       (quiet
          (Array.concat
             [ [| "openssl"; "req"; "-x509" |]; newkey; [| "-nodes"; "-keyout"; key; "-out"; cert; "-days"; "2"; "-subj"; "/CN=localhost"; "-addext"; "subjectAltName=DNS:localhost" |] ])));
  (key, cert)

let port = ref (21000 + (Unix.getpid () mod 9000))

(* [with_server kind suite f]: f the port and the server's certificate *)
let with_server ?(extra = [||]) (kind : string) (suite : string) (f : int -> X509.t -> unit) : unit =
  let key, cert = certificate kind in
  incr port;
  let p = !port in
  let server =
    quiet (Array.append [| "openssl"; "s_server"; "-quiet"; "-www"; "-tls1_3"; "-ciphersuites"; suite; "-accept"; string_of_int p; "-cert"; cert; "-key"; key |] extra)
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.kill server Sys.sigterm;
      ignore (Unix.waitpid [] server))
    (fun () ->
      Unix.sleepf 0.5;
      let pem = In_channel.with_open_bin cert In_channel.input_all in
      f p (Result.get_ok (X509.parse (List.hd (Pem.certificates pem)))))

let get = "GET / HTTP/1.0\r\n\r\n"

let page caps ~trust ~host p =
  match Tls_client.exchange ~trust ~timeout:10. caps ~host ~port:p get with
  | Ok answer -> Ok (String.sub answer 0 (min 15 (String.length answer)))
  | Error e -> Error e

let tests (caps : < Cap.network ; Cap.open_in ; Cap.exec ; .. >) =
  let (_ : Cap.Exec.t) = caps#exec "openssl" in
  Testo.categorize "Tls_client"
    [
      Testo.create "a page over ChaCha20-Poly1305, ECDSA" (fun () ->
          with_server "ec" "TLS_CHACHA20_POLY1305_SHA256" (fun p cert ->
              Alcotest.(check (result string string)) "200" (Ok "HTTP/1.0 200 ok") (page caps ~trust:[ cert ] ~host:"localhost" p)));
      Testo.create "a page over AES-128-GCM, RSA (PSS)" (fun () ->
          with_server "rsa" "TLS_AES_128_GCM_SHA256" (fun p cert ->
              Alcotest.(check (result string string)) "200" (Ok "HTTP/1.0 200 ok") (page caps ~trust:[ cert ] ~host:"localhost" p)));
      Testo.create "a server asking for our certificate: an empty one (Gmail's SMTP)" (fun () ->
          with_server ~extra:[| "-verify"; "1" |] "ec" "TLS_CHACHA20_POLY1305_SHA256" (fun p cert ->
              Alcotest.(check (result string string)) "200" (Ok "HTTP/1.0 200 ok") (page caps ~trust:[ cert ] ~host:"localhost" p)));
      Testo.create "refused: a root not trusted, another name" (fun () ->
          with_server "ec" "TLS_CHACHA20_POLY1305_SHA256" (fun p cert ->
              Alcotest.(check bool) "no roots" true (Result.is_error (page caps ~trust:[] ~host:"localhost" p));
              Alcotest.(check bool) "127.0.0.1 is not localhost" true (Result.is_error (page caps ~trust:[ cert ] ~host:"127.0.0.1" p))));
    ]
