(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A tiny web server, after CERN httpd (1990) and NCSA httpd (1993): a
 * directory's files, each under its path, for TinyMosaic (and any
 * browser) to browse with no Internet. Its parameters as name=value:
 * root (apps/internet/site), port (8080), bind (127.0.0.1: this computer
 * only; 0.0.0.0 for every network it is on). Then:
 *
 *   dune exec networking/httpd/tiny_httpd.exe
 *   dune exec apps/internet/TinyMosaic.exe -- url=http://localhost:8080/
 *
 * What a web server does, in the order it does it (the handler below):
 *
 *   - the path of the request, its %-escapes decoded ("/a%20b.html" is
 *     "a b.html"), its query dropped;
 *   - refused if it climbs out of the root ("/../../etc/passwd", the
 *     first attack every web server met): 403;
 *   - a directory: its index.html if it has one, else a page listing
 *     its files, each a link (NCSA's "Index of /");
 *   - a file: its bytes, with a Content-Type guessed from its
 *     extension (.html is text/html, .gif image/gif): the browser
 *     believes it, so a wrong guess shows a picture as text;
 *   - nothing there: 404, with a page saying so;
 *   - and a line in the log, the Common Log Format of NCSA httpd,
 *     which every server still writes and every log reader reads:
 *     127.0.0.1 - - [25/Sep/2026:10:02:03 +0000] "GET / HTTP/1.1" 200 1411
 *
 * Over Http_server (networking/unix), the event loop: a connection
 * per request, many at once, no threads. Not done: HEAD, POST and CGI
 * (plan_browser_teaching.md, phase 9), If-Modified-Since and 304,
 * keep-alive, and every header a browser might care for but the type
 * and the length. *)

let flag (name : string) (default : string) : string =
  Array.to_list Sys.argv
  |> List.find_map (fun a ->
         match String.index_opt a '=' with
         | Some i when String.sub a 0 i = name -> Some (String.sub a (i + 1) (String.length a - i - 1))
         | _ -> None)
  |> Option.value ~default

(*****************************************************************************)
(* Paths *)
(*****************************************************************************)

(* "%20" is a space: a URL's path, as the file system names it *)
let percent_decode (s : string) : string =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      if s.[i] = '%' && i + 2 < String.length s then (
        match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with
        | Some c ->
            Buffer.add_char b (Char.chr c);
            go (i + 3)
        | None ->
            Buffer.add_char b '%';
            go (i + 1))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(* the path's segments, or None if one of them climbs out of the root *)
let segments (target : string) : string list option =
  let path = match String.index_opt target '?' with Some i -> String.sub target 0 i | None -> target in
  let parts = String.split_on_char '/' (percent_decode path) |> List.filter (fun p -> p <> "" && p <> ".") in
  if List.exists (fun p -> p = ".." || String.contains p '\000') parts then None else Some parts

let content_type (file : string) : string =
  match String.lowercase_ascii (Filename.extension file) with
  | ".html" | ".htm" -> "text/html"
  | ".txt" | ".ml" | ".mli" -> "text/plain; charset=utf-8"
  | ".css" -> "text/css"
  | ".gif" -> "image/gif"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".xbm" -> "image/x-xbitmap"
  | _ -> "application/octet-stream"

(*****************************************************************************)
(* Answers *)
(*****************************************************************************)

let page (status : int) (title : string) (body : string) : Http.response =
  Http.response status ~content_type:"text/html"
    (Printf.sprintf "<HTML><HEAD><TITLE>%s</TITLE></HEAD><BODY><H1>%s</H1>\n%s</BODY></HTML>\n" title title body)

let read_file (caps : < Cap.open_in ; .. >) (path : string) : string =
  let ic = CapStdlib.open_in caps path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> really_input_string ic (in_channel_length ic))

(* NCSA's "Index of": a directory's entries, each a link *)
let listing (caps : < Cap.readdir ; .. >) (dir : string) (url_path : string) : Http.response =
  let (_ : Cap.FS_.readdir) = caps#readdir dir in
  let entries = Sys.readdir dir |> Array.to_list |> List.sort compare in
  let base = if url_path = "/" then "/" else url_path ^ "/" in
  page 200 ("Index of " ^ url_path)
    ("<UL>\n"
    ^ String.concat ""
        (List.map
           (fun e -> Printf.sprintf "<LI><A HREF=\"%s%s%s\">%s</A>\n" base e (if Sys.is_directory (Filename.concat dir e) then "/" else "") e)
           entries)
    ^ "</UL>\n")

let handler (caps : < Cap.open_in ; Cap.readdir ; .. >) (root : string) : Http_server.handler =
 fun ~peer:_ (r : Http.request) _body ->
  if r.meth <> "GET" then page 501 "Not Implemented" ("<P>" ^ r.meth ^ " is not done here: GET only.\n")
  else
    match segments r.target with
    | None -> page 403 "Forbidden" "<P>That path leaves the server's directory.\n"
    | Some parts -> (
        let path = List.fold_left Filename.concat root parts in
        let url_path = "/" ^ String.concat "/" parts in
        match (Sys.file_exists path, Sys.file_exists path && Sys.is_directory path) with
        | false, _ -> page 404 "Not Found" ("<P>Nothing at " ^ url_path ^ " here.\n")
        | true, true ->
            let index = Filename.concat path "index.html" in
            if Sys.file_exists index then Http.response 200 ~content_type:"text/html" (read_file caps index)
            else listing caps path url_path
        | true, false -> Http.response 200 ~content_type:(content_type path) (read_file caps path))

(* NCSA's Common Log Format *)
let log_line (peer : string) (r : Http.request) (response : Http.response) : string =
  let t = Unix.gmtime (Unix.time ()) in
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
  Printf.sprintf "%s - - [%02d/%s/%d:%02d:%02d:%02d +0000] \"%s %s HTTP/1.1\" %d %d" peer t.tm_mday months.(t.tm_mon)
    (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec r.meth r.target response.status (String.length response.body)

let () =
  Cap.main (fun caps ->
      let root = flag "root" "apps/internet/site" and bind = flag "bind" "127.0.0.1" in
      let server, port = Http_server.listen caps ~bind ~port:(int_of_string (flag "port" "8080")) in
      Printf.printf "tiny_httpd: %s on http://%s:%d/\n%!" root bind port;
      let answer = handler caps root in
      let logged ~peer r body =
        let response = answer ~peer r body in
        print_endline (log_line peer r response);
        response
      in
      while true do
        Http_server.step server logged;
        Http_server.wait server 1.
      done)
