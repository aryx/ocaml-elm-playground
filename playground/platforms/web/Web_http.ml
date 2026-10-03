(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Web_http.mli *)

open Basics

(* claude: Audio.loop_from's files, fetched in the background (an
 * XMLHttpRequest, its response as bytes): a plain name from the page's
 * own server, a URL elsewhere if that server allows it (CORS) *)
let fetch_web (source : string) (k : string option -> unit) : unit =
  let xhr = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "XMLHttpRequest") [||] in
  ignore (Ojs.call xhr "open" [| Ojs.string_to_js "GET"; Ojs.string_to_js source |]);
  Ojs.set_prop_ascii xhr "responseType" (Ojs.string_to_js "arraybuffer");
  Ojs.set_prop_ascii xhr "onload"
    (Ojs.fun_to_js 1 (fun _ ->
         let status = Ojs.int_of_js (Ojs.get_prop_ascii xhr "status") in
         if status >= 200 && status < 300 then (
           let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| Ojs.get_prop_ascii xhr "response" |] in
           let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
           k (Some (String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i))))))
         else k None));
  Ojs.set_prop_ascii xhr "onerror" (Ojs.fun_to_js 1 (fun _ -> k None));
  ignore (Ojs.call xhr "send" [||])

(* claude: getAllResponseHeaders' text, "name: value" lines ending in
 * CR LF, as pairs *)
let parse_headers (s : string) : (string * string) list =
  String.split_on_char '\n' s
  |> List.filter_map (fun line ->
         match String.index_opt line ':' with
         | None -> None
         | Some i -> Some (String.trim (String.sub line 0 i), String.trim (String.sub line (i +.. 1) (String.length line -.. i -.. 1))))

(* claude: Cmd.Http_get, performed by the browser: an XMLHttpRequest
 * for bytes (an arraybuffer: an image is not text, and a page's
 * encoding is the program's to decode), its answer whatever the status
 * (Playground.Http reads it), no answer at all -- status 0: the
 * network, or a server refusing another site's page, CORS --
 * Network_error. A Cmd.Http_post the same, a POST with its body (a
 * form's fields: ASCII, which the browser sends as they are) *)
let fetch_response ?post (url : string) (k : (Cmd.http_response, Cmd.http_error) result -> unit) : unit =
  let xhr = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "XMLHttpRequest") [||] in
  let meth = if post = None then "GET" else "POST" in
  match Ojs.call xhr "open" [| Ojs.string_to_js meth; Ojs.string_to_js url |] with
  | exception _ -> k (Error (Cmd.Bad_url url))
  | _ ->
      Ojs.set_prop_ascii xhr "timeout" (Ojs.int_to_js 30000);
      Ojs.set_prop_ascii xhr "responseType" (Ojs.string_to_js "arraybuffer");
      Ojs.set_prop_ascii xhr "onload"
        (Ojs.fun_to_js 1 (fun _ ->
             let status = Ojs.int_of_js (Ojs.get_prop_ascii xhr "status") in
             let bytes = Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Uint8Array") [| Ojs.get_prop_ascii xhr "response" |] in
             let n = Ojs.int_of_js (Ojs.get_prop_ascii bytes "length") in
             let body = String.init n (fun i -> Char.chr (Ojs.int_of_js (Ojs.array_get bytes i))) in
             let headers = parse_headers (Ojs.string_of_js (Ojs.call xhr "getAllResponseHeaders" [||])) in
             let url = Ojs.string_of_js (Ojs.get_prop_ascii xhr "responseURL") in
             k (Ok { url; status; headers; body })));
      Ojs.set_prop_ascii xhr "onerror"
        (Ojs.fun_to_js 1 (fun _ -> k (Error (Cmd.Network_error (url ^ ": no answer (network, or CORS)")))));
      Ojs.set_prop_ascii xhr "ontimeout" (Ojs.fun_to_js 1 (fun _ -> k (Error Cmd.Timeout)));
      match post with
      | Some (content_type, body) ->
          ignore (Ojs.call xhr "setRequestHeader" [| Ojs.string_to_js "Content-Type"; Ojs.string_to_js content_type |]);
          ignore (Ojs.call xhr "send" [| Ojs.string_to_js body |])
      | None -> ignore (Ojs.call xhr "send" [||])
