(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_http.mli *)

let ok = function Ok x -> x | Error e -> Alcotest.fail e

(* the response's body, parsed from the status line, the headers and
 * what follows the empty line *)
let body_of (head : string list) (rest : string) : string =
  (ok (Http.parse_response (String.concat "\r\n" head ^ "\r\n\r\n" ^ rest))).body

let wikipedia = "4\r\nWiki\r\n5\r\npedia\r\nE\r\n in\r\n\r\nchunks.\r\n0\r\n\r\n"

let tests =
  Testo.categorize "Http"
    [
      Testo.create "the request of the diagram" (fun () ->
          Alcotest.(check string) "bytes"
            "GET /images/turtle.gif HTTP/1.1\r\nHost: elm-lang.org\r\nUser-Agent: elm_playground\r\nConnection: close\r\n\r\n"
            (Http.request_to_string (Http.get ~host:"elm-lang.org" "/images/turtle.gif")));
      Testo.create "the status line" (fun () ->
          Alcotest.(check (triple string int string)) "200" ("HTTP/1.1", 200, "OK") (ok (Http.parse_status_line "HTTP/1.1 200 OK"));
          Alcotest.(check (triple string int string))
            "a reason with spaces" ("HTTP/1.0", 404, "Not Found")
            (ok (Http.parse_status_line "HTTP/1.0 404 Not Found"));
          Alcotest.(check (triple string int string)) "no reason" ("HTTP/1.1", 204, "") (ok (Http.parse_status_line "HTTP/1.1 204"));
          Alcotest.(check bool) "not HTTP" true (Result.is_error (Http.parse_status_line "SSH-2.0-OpenSSH"));
          Alcotest.(check bool) "a 4-digit status" true (Result.is_error (Http.parse_status_line "HTTP/1.1 2000 OK")));
      Testo.create "the worked example: Wikipedia's chunked body" (fun () ->
          Alcotest.(check string) "joined" "Wikipedia in\r\n\r\nchunks." (ok (Http.dechunk wikipedia)));
      Testo.create "chunk extensions and trailers ignored" (fun () ->
          Alcotest.(check string) "joined" "Wikipedia"
            (ok (Http.dechunk "4;name=value\r\nWiki\r\n5\r\npedia\r\n0\r\nExpires: never\r\n\r\n")));
      Testo.create "a chunked body cut short" (fun () ->
          Alcotest.(check bool) "inside a chunk" true (Result.is_error (Http.dechunk "a\r\nWiki"));
          Alcotest.(check bool) "before the last chunk" true (Result.is_error (Http.dechunk "4\r\nWiki\r\n")));
      Testo.create "the four ways a body ends" (fun () ->
          Alcotest.(check string) "1. a 304 has none" "" (body_of [ "HTTP/1.1 304 Not Modified"; "Content-Length: 5" ] "hello");
          Alcotest.(check string) "2. chunked, before Content-Length" "Wikipedia in\r\n\r\nchunks."
            (body_of [ "HTTP/1.1 200 OK"; "Content-Length: 3"; "transfer-encoding: Chunked" ] wikipedia);
          Alcotest.(check string) "3. Content-Length" "hello" (body_of [ "HTTP/1.1 200 OK"; "content-length: 5" ] "hello, and more");
          Alcotest.(check string) "4. until closed" "hello, and more" (body_of [ "HTTP/1.1 200 OK" ] "hello, and more"));
      Testo.create "a body shorter than its Content-Length" (fun () ->
          Alcotest.(check bool) "truncated" true
            (Result.is_error (Http.parse_response "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello")));
      Testo.create "headers: case-insensitive, trimmed, lone LFs" (fun () ->
          let r = ok (Http.parse_response "HTTP/1.1 301 Moved\nLOCATION:   /new  \n\n") in
          Alcotest.(check (option string)) "Location" (Some "/new") (Http.header "Location" r.headers);
          Alcotest.(check bool) "a redirect" true (Http.is_redirect r.status));
      Testo.create "refused: folded headers, compression" (fun () ->
          Alcotest.(check bool) "folded" true
            (Result.is_error (Http.parse_response "HTTP/1.1 200 OK\r\nX-A: 1\r\n  2\r\n\r\n"));
          Alcotest.(check bool) "space before the colon" true
            (Result.is_error (Http.parse_response "HTTP/1.1 200 OK\r\nX-A : 1\r\n\r\n"));
          Alcotest.(check bool) "gzip" true
            (Result.is_error (Http.parse_response "HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\n\r\n...")));
      Testo.create "the server's side: a request, whole or not yet" (fun () ->
          let show (p : Http.parsed_request) =
            match p with
            | Incomplete -> "incomplete"
            | Bad _ -> "bad"
            | Request (r, body, used) -> Printf.sprintf "%s %s body=%S used=%d" r.meth r.target body used
          in
          let check what s expected = Alcotest.(check string) what expected (show (Http.parse_request s)) in
          check "no empty line yet" "GET / HTTP/1.1\r\nHost: a\r\n" "incomplete";
          check "whole" "GET / HTTP/1.1\r\nHost: a\r\n\r\n" "GET / body=\"\" used=27";
          check "a body to come" "POST /f HTTP/1.1\r\nContent-Length: 3\r\n\r\nab" "incomplete";
          check "the body come, and more" "POST /f HTTP/1.1\r\nContent-Length: 3\r\n\r\nabcGET" "POST /f body=\"abc\" used=42";
          check "not a request line" "HELLO\r\n\r\n" "bad";
          check "a bad length" "POST / HTTP/1.1\r\nContent-Length: x\r\n\r\n" "bad");
      Testo.create "the server's side: a response, read back" (fun () ->
          let bytes = Http.response_to_string (Http.response 404 ~content_type:"text/html" "<p>no") in
          Alcotest.(check string)
            "the bytes"
            "HTTP/1.1 404 Not Found\r\nContent-Type: text/html\r\nContent-Length: 5\r\nConnection: close\r\n\r\n<p>no"
            bytes;
          let r = ok (Http.parse_response bytes) in
          Alcotest.(check (pair int string)) "parsed back" (404, "<p>no") (r.status, r.body));
    ]
