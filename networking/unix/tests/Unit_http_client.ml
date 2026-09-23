(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_http_client.mli *)

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let tests =
  Testo.categorize "Http_client"
    [
      Testo.create "a redirection followed, to a chunked body" (fun () ->
          Testutil_server.(with_server (respond site)) (fun port ->
              match Http_client.get (Testutil_server.url port "/old") with
              | Ok (r : Http.response) ->
                  Alcotest.(check int) "status" 200 r.status;
                  Alcotest.(check string) "body" "Wikipedia" r.body
              | Error e -> Alcotest.fail e));
      Testo.create "a 404 is an answer, given back" (fun () ->
          Testutil_server.(with_server (respond site)) (fun port ->
              match Http_client.get (Testutil_server.url port "/nothing") with
              | Ok (r : Http.response) ->
                  Alcotest.(check int) "status" 404 r.status;
                  Alcotest.(check string) "body" "not here\n" r.body
              | Error e -> Alcotest.fail e));
      Testo.create "refused: too many redirections, https" (fun () ->
          Testutil_server.(with_server (respond site)) (fun port ->
              Alcotest.(check bool) "a loop" true (Result.is_error (Http_client.get (Testutil_server.url port "/loop")));
              (match Http_client.get (Testutil_server.url port "/secure") with
              | Error e ->
                  Alcotest.(check bool) "the new URL in the message" true (String.starts_with ~prefix:"https://example.com/" e)
              | Ok _ -> Alcotest.fail "followed to https");
              Alcotest.(check bool) "https" true (Result.is_error (Http_client.get "https://example.com/"))));
      Testo.create "a closed port" (fun () ->
          (* a port the kernel just gave us, and closed again: nobody listens *)
          let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
          let port = match Unix.getsockname sock with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
          Unix.close sock;
          Alcotest.(check bool) "connection refused" true (Result.is_error (Http_client.get (Testutil_server.url port "/"))));
    ]
