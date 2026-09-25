(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_http_server.mli *)

(* a canned site: / is a page, /old moves to /, the rest is not here;
 * what was asked is kept, to be checked *)
let asked = ref []

let handler : Http_server.handler =
 fun ~peer (r : Http.request) _body ->
  asked := (peer, r.meth, r.target) :: !asked;
  match r.target with
  | "/" -> Http.response 200 ~content_type:"text/html" "<title>Home</title>"
  | "/old" -> { (Http.response 301 ~content_type:"text/html" "moved") with headers = [ ("Location", "/") ] }
  | _ -> Http.response 404 ~content_type:"text/html" "<h1>Not here</h1>"

(* the server and requests stepped in turn, one process, as two
 * programs' frame loops would be, until every request is done *)
let run (server : Http_server.t) (requests : Http_request.t list) : unit =
  let rec go n =
    Http_server.step server handler;
    List.iter Http_request.step requests;
    if List.exists (fun r -> Http_request.result r = None) requests && n < 2000 then (
      Http_server.wait server 0.005;
      go (n + 1))
  in
  go 0

let result (r : Http_request.t) : int * string =
  match Http_request.result r with
  | Some (Ok response) -> (response.status, response.body)
  | Some (Error _) -> Alcotest.fail "failed"
  | None -> Alcotest.fail "not done"

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Http_server"
    [
      Testo.create "a page, a redirection, a 404, at once" (fun () ->
          asked := [];
          let server, port = Http_server.listen caps ~bind:"127.0.0.1" ~port:0 in
          let url path = Printf.sprintf "http://127.0.0.1:%d%s" port path in
          let home = Http_request.start caps (url "/") in
          let old = Http_request.start caps (url "/old") in
          let missing = Http_request.start caps (url "/nowhere") in
          run server [ home; old; missing ];
          Alcotest.(check (pair int string)) "/" (200, "<title>Home</title>") (result home);
          Alcotest.(check (pair int string)) "/old, followed" (200, "<title>Home</title>") (result old);
          Alcotest.(check string) "where it ended" (url "/") (Http_request.url old);
          Alcotest.(check (pair int string)) "/nowhere" (404, "<h1>Not here</h1>") (result missing);
          Alcotest.(check int) "four requests (the redirection's second)" 4 (List.length !asked);
          Alcotest.(check bool) "from this computer" true (List.for_all (fun (peer, _, _) -> peer = "127.0.0.1") !asked);
          Alcotest.(check int) "every connection closed" 0 (Http_server.connections server));
      Testo.create "garbage: 400, and closed" (fun () ->
          let server, port = Http_server.listen caps ~bind:"127.0.0.1" ~port:0 in
          let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
          ignore (Unix.write_substring fd "HELLO\r\n\r\n" 0 9);
          for _ = 1 to 20 do
            Http_server.step server handler;
            Http_server.wait server 0.005
          done;
          let buf = Bytes.create 1000 in
          let n = Unix.read fd buf 0 1000 in
          Unix.close fd;
          let r = match Http.parse_response (Bytes.sub_string buf 0 n) with Ok r -> r | Error e -> Alcotest.fail e in
          Alcotest.(check int) "400" 400 r.status);
    ]
