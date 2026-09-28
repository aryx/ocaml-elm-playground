(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_http_request.mli *)

let frame = 1. /. 60.

(* the request stepped like a frame loop would: a step, then the rest
 * of the frame's time asleep; the result, the number of frames, and
 * the longest step *)
let run_frames (r : Http_request.t) : (Http.response, Http_request.error) result * int * float =
  let rec go frames longest =
    let t0 = Unix.gettimeofday () in
    Http_request.step r;
    let longest = Float.max longest (Unix.gettimeofday () -. t0) in
    match Http_request.result r with
    | Some result -> (result, frames, longest)
    | None ->
        Unix.sleepf frame;
        go (frames + 1) longest
  in
  go 1 0.

let response : Http.response Alcotest.testable =
  Alcotest.testable
    (fun fmt (r : Http.response) -> Format.fprintf fmt "%d %s, %S" r.status r.reason r.body)
    (fun (a : Http.response) b -> a.status = b.status && a.headers = b.headers && a.body = b.body)

let ok = function Ok x -> x | Error e -> Alcotest.fail e

let tests (caps : < Cap.network ; .. >) =
  Testo.categorize "Http_request"
    [
      Testo.create "the same responses as the blocking client" (fun () ->
          Testutil_server.(with_server (respond site)) (fun port ->
              List.iter
                (fun path ->
                  let url = Testutil_server.url port path in
                  match run_frames (Http_request.start caps url) with
                  | Ok r, _, _ -> Alcotest.check response path (ok (Http_client.get caps url)) r
                  | Error _, _, _ -> Alcotest.fail path)
                [ "/old"; "/nothing" ]));
      Testo.create "the name resolved on a thread: the same responses" (fun () ->
          let resolver = Worker.create 1 in
          Testutil_server.(with_server (respond site)) (fun port ->
              List.iter
                (fun path ->
                  let url = Testutil_server.url port path in
                  let r = Http_request.start ~resolver caps url in
                  (* claude: nothing resolved yet: start returned at once *)
                  Alcotest.(check bool) "not done at once" true (Http_request.result r = None);
                  match run_frames r with
                  | Ok r, _, _ -> Alcotest.check response path (ok (Http_client.get caps url)) r
                  | Error _, _, _ -> Alcotest.fail path)
                [ "/old"; "/nothing" ]));
      Testo.create "refused like the blocking client" (fun () ->
          Testutil_server.(with_server (respond site)) (fun port ->
              let error path = match run_frames (Http_request.start caps (Testutil_server.url port path)) with Error e, _, _ -> Some e | Ok _, _, _ -> None in
              Alcotest.(check bool) "a loop" true (match error "/loop" with Some (Failed _) -> true | _ -> false);
              Alcotest.(check bool) "to https" true (match error "/secure" with Some (Bad_url _) -> true | _ -> false);
              Alcotest.(check bool) "not a URL we get" true
                (match Http_request.result (Http_request.start caps "ftp://a/") with Some (Error (Bad_url _)) -> true | _ -> false)));
      Testo.create "a byte at a time: the same response" (fun () ->
          let answer = Testutil_server.site 0 "GET /new?v=2 HTTP/1.1" in
          let dribble _port _request fd =
            String.iter (fun c -> Tcp.send_all fd (String.make 1 c); Unix.sleepf 0.002) answer
          in
          Testutil_server.with_server dribble (fun port ->
              match run_frames (Http_request.start caps (Testutil_server.url port "/new?v=2")) with
              | Ok r, _, _ -> Alcotest.check response "joined" (ok (Http.parse_response answer)) r
              | Error _, _, _ -> Alcotest.fail "no response"));
      Testo.create "a slow server doesn't stop the frames" (fun () ->
          let slow port request fd = Unix.sleepf 0.3; Testutil_server.respond Testutil_server.site port request fd in
          Testutil_server.with_server slow (fun port ->
              match run_frames (Http_request.start caps (Testutil_server.url port "/nothing")) with
              | Ok r, frames, longest ->
                  Alcotest.(check int) "answered" 404 r.status;
                  Alcotest.(check bool) (Printf.sprintf "%d frames went on meanwhile" frames) true (frames >= 10);
                  Alcotest.(check bool) (Printf.sprintf "the longest step, %.1f ms, less than a frame" (longest *. 1000.)) true (longest < frame)
              | Error _, _, _ -> Alcotest.fail "no response"));
      Testo.create "a silent server times out" (fun () ->
          let silent _port _request _fd = Unix.sleepf 5. in
          Testutil_server.with_server silent (fun port ->
              match run_frames (Http_request.start ~timeout:0.2 caps (Testutil_server.url port "/")) with
              | Error Timeout, _, _ -> ()
              | _ -> Alcotest.fail "no timeout"));
    ]
