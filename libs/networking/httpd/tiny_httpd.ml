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
 * per request, many at once, no threads. And CGI (below): a path
 * under /cgi-bin/ runs the program of that name in root/cgi-bin/, a
 * form's GET or POST its input (site/cgi-bin/echo, a shell script,
 * shows what it was sent). Not done: HEAD, If-Modified-Since and 304,
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

(*****************************************************************************)
(* CGI *)
(*****************************************************************************)

(* The Common Gateway Interface (NCSA httpd, 1993, from the lab that
 * made Mosaic): a URL under /cgi-bin/ names a program, which the server
 * runs for the request, and whose output is the answer -- the first
 * dynamic pages, and for ten years most of them (Perl scripts, mostly).
 * The request goes in by the program's environment and its standard
 * input:
 *
 *   REQUEST_METHOD=POST   QUERY_STRING=a=1 (the URL's, after the ?)
 *   CONTENT_TYPE=application/x-www-form-urlencoded   CONTENT_LENGTH=17
 *   stdin: the body, 17 bytes (a POSTed form's fields)
 *
 * and the answer comes out on its standard output, headers first, as
 * HTTP's, then an empty line, then the page:
 *
 *   Content-Type: text/html
 *   (empty line)
 *   <TITLE>...
 *
 * a "Status: 404 Not Found" header saying the status (200 if none). A
 * program per request, in any language, knowing nothing of sockets:
 * the server's work, and its cost (a process started for every page,
 * which FastCGI (1996) and then servers running the code themselves
 * did away with). This one waits for the program, holding up the other
 * connections meanwhile; NCSA's server had a process per connection,
 * so did not. *)

let read_all (fd : Unix.file_descr) : string =
  let b = Buffer.create 4096 and chunk = Bytes.create 4096 in
  let rec go () =
    match Unix.read fd chunk 0 4096 with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes b chunk 0 n;
        go ()
  in
  go ();
  Buffer.contents b

(* where [sub] is in [s], if it is *)
let find (s : string) (sub : string) : int option =
  let n = String.length sub in
  let rec go i = if i + n > String.length s then None else if String.sub s i n = sub then Some i else go (i + 1) in
  go 0

(* the program's output: its headers (and the Status: one) then the
 * page *)
let cgi_response (output : string) : Http.response =
  let header_end, body_start =
    match find output "\r\n\r\n" with
    | Some i -> (i, i + 4)
    | None -> ( match find output "\n\n" with Some i -> (i, i + 2) | None -> (0, 0))
  in
  let headers =
    String.sub output 0 header_end |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           match String.index_opt line ':' with
           | Some i -> Some (String.trim (String.sub line 0 i), String.trim (String.sub line (i + 1) (String.length line - i - 1)))
           | None -> None)
  in
  let status =
    match Http.header "Status" headers with
    | Some s -> ( match int_of_string_opt (List.hd (String.split_on_char ' ' s)) with Some n -> n | None -> 200)
    | None -> 200
  in
  {
    (Http.response status ~content_type:(Option.value (Http.header "Content-Type" headers) ~default:"text/plain") "")
    with
    body = String.sub output body_start (String.length output - body_start);
  }

let cgi (caps : < Cap.exec ; .. >) (program : string) ~(peer : string) (r : Http.request) (body : string) : Http.response =
  let (_ : Cap.Exec.t) = caps#exec program in
  let query = match String.index_opt r.target '?' with Some i -> String.sub r.target (i + 1) (String.length r.target - i - 1) | None -> "" in
  let env =
    [|
      "GATEWAY_INTERFACE=CGI/1.1"; "SERVER_SOFTWARE=tiny_httpd"; "SERVER_PROTOCOL=HTTP/1.1"; "PATH=/usr/bin:/bin";
      "REQUEST_METHOD=" ^ r.meth; "QUERY_STRING=" ^ query; "SCRIPT_NAME=" ^ program; "REMOTE_ADDR=" ^ peer;
      "CONTENT_TYPE=" ^ Option.value (Http.header "Content-Type" r.headers) ~default:"";
      "CONTENT_LENGTH=" ^ string_of_int (String.length body);
    |]
  in
  let in_r, in_w = Unix.pipe ~cloexec:true () and out_r, out_w = Unix.pipe ~cloexec:true () in
  let pid = Unix.create_process_env program [| program |] env in_r out_w Unix.stderr in
  Unix.close in_r;
  Unix.close out_w;
  ignore (Unix.write_substring in_w body 0 (String.length body));
  Unix.close in_w;
  let output = read_all out_r in
  Unix.close out_r;
  ignore (Unix.waitpid [] pid);
  cgi_response output

(*****************************************************************************)
(* The handler *)
(*****************************************************************************)

let handler (caps : < Cap.open_in ; Cap.readdir ; Cap.exec ; .. >) (root : string) : Http_server.handler =
 fun ~peer (r : Http.request) body ->
  match segments r.target with
  | None -> page 403 "Forbidden" "<P>That path leaves the server's directory.\n"
  | Some ("cgi-bin" :: [ name ]) ->
      let program = Filename.concat (Filename.concat root "cgi-bin") name in
      if Sys.file_exists program then cgi caps program ~peer r body
      else page 404 "Not Found" ("<P>No program " ^ name ^ " here.\n")
  | Some _ when r.meth <> "GET" -> page 501 "Not Implemented" ("<P>" ^ r.meth ^ " is for the programs of /cgi-bin/ only.\n")
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
