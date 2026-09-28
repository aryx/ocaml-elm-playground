(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* tls_get: a page fetched over our own TLS 1.3 (Tls_client.mli, Tls13.mli),
 * and what happened on the way: the cipher the server chose, the chain
 * it sent (each certificate's name and key), which root we trusted, and
 * the answer's first lines -- `curl -I`, with the handshake shown.
 *
 *   dune exec networking/tlsget/tls_get.exe -- https://en.wikipedia.org/
 *   dune exec networking/tlsget/tls_get.exe -- https://github.com/ 5
 *
 * (the number: how many lines of the answer to show, 12 by default) *)

let key_name (k : X509.public_key) : string =
  match k with
  | Rsa (n, _) -> Printf.sprintf "RSA %d" (Bignum.bits n)
  | Ec (c, _) -> if Ecdsa.size c = 32 then "ECDSA P-256" else "ECDSA P-384"
  | Other oid -> "? " ^ oid

let () =
  Cap.main (fun caps ->
      let url = if Array.length Sys.argv > 1 then Sys.argv.(1) else "https://example.com/" in
      let lines = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 12 in
      match Url.parse url with
      | Ok { scheme = Some "https"; authority = Some a; path; query; _ } -> (
          let port = Option.value a.port ~default:443 in
          let target = (if path = "" then "/" else path) ^ match query with Some q -> "?" ^ q | None -> "" in
          let t0 = Unix.gettimeofday () in
          match Tls_client.connect caps ~host:a.host ~port () with
          | Error e -> prerr_endline e
          | Ok t ->
              let rec handshake () =
                Tls_client.step t;
                match Tls_client.state t with Handshaking -> ignore (Unix.select [] [] [] 0.01); handshake () | s -> s
              in
              (match handshake () with
              | Open ->
                  let m = Tls_client.machine t in
                  Printf.printf "TLS 1.3 with %s:%d in %.0f ms, %s\n" a.host port (1000. *. (Unix.gettimeofday () -. t0))
                    (match Tls13.cipher m with Some Chacha20_poly1305 -> "ChaCha20-Poly1305" | _ -> "AES-128-GCM");
                  List.iter (fun (c : X509.t) -> Printf.printf "  %-40s %s\n" c.common_name (key_name c.key)) (Tls13.certificates m)
              | Failed why -> Printf.printf "refused: %s\n" why
              | _ -> ());
              Tls_client.close t;
              if Tls_client.state t <> Tls13.Handshaking then
                match
                  Tls_client.exchange caps ~host:a.host ~port
                    (Printf.sprintf "GET %s HTTP/1.1\r\nHost: %s\r\nUser-Agent: tls_get (ocaml-elm-playground)\r\nConnection: close\r\n\r\n" target a.host)
                with
                | Ok answer ->
                    Printf.printf "%d bytes:\n" (String.length answer);
                    List.iteri (fun i l -> if i < lines then print_endline ("  " ^ l)) (String.split_on_char '\n' answer)
                | Error e -> Printf.printf "error: %s\n" e)
      | _ -> prerr_endline "usage: tls_get https://host/path")
