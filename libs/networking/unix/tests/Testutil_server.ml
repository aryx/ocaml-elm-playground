(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_server.mli *)

(* the request line a server received: "GET /x HTTP/1.1" *)
let read_request_line (fd : Unix.file_descr) : string =
  (* the request is small, and ends with an empty line: read until it *)
  let b = Buffer.create 256 in
  let byte = Bytes.create 1 in
  let rec go () =
    let s = Buffer.contents b in
    if String.length s >= 4 && String.sub s (String.length s - 4) 4 = "\r\n\r\n" then ()
    else if Unix.read fd byte 0 1 = 1 then (
      Buffer.add_bytes b byte;
      go ())
  in
  go ();
  let s = Buffer.contents b in
  match String.index_opt s '\r' with Some i -> String.sub s 0 i | None -> s

let with_server (handle : int -> string -> Unix.file_descr -> unit) (f : int -> unit) : unit =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen sock 8;
  let port = match Unix.getsockname sock with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
  match Unix.fork () with
  | 0 ->
      (* the child: serve forever, until killed; never back into the
       * test runner (hence _exit) *)
      (try
         let rec serve () =
           let fd, _ = Unix.accept sock in
           (try handle port (read_request_line fd) fd with Unix.Unix_error _ -> ());
           Unix.close fd;
           serve ()
         in
         serve ()
       with _ -> ());
      Unix._exit 0
  | child ->
      Unix.close sock;
      Fun.protect
        ~finally:(fun () ->
          Unix.kill child Sys.sigkill;
          ignore (Unix.waitpid [] child))
        (fun () -> f port)

let site (port : int) (request_line : string) : string =
  match request_line with
  | "GET /old HTTP/1.1" -> "HTTP/1.1 301 Moved Permanently\r\nLocation: new?v=2\r\nContent-Length: 0\r\n\r\n"
  | "GET /new?v=2 HTTP/1.1" ->
      "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n"
  | "GET /loop HTTP/1.1" -> Printf.sprintf "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:%d/loop\r\n\r\n" port
  (* claude: to https:// where nobody listens, so that following it
   * needs no Internet *)
  | "GET /secure HTTP/1.1" -> "HTTP/1.1 301 Moved Permanently\r\nLocation: https://127.0.0.1:1/\r\n\r\n"
  | _ -> "HTTP/1.1 404 Not Found\r\nContent-Length: 9\r\n\r\nnot here\n"

let respond (answer : int -> string -> string) (port : int) (request_line : string) (fd : Unix.file_descr) : unit =
  Tcp.send_all fd (answer port request_line)

let url (port : int) (path : string) : string = Printf.sprintf "http://127.0.0.1:%d%s" port path
