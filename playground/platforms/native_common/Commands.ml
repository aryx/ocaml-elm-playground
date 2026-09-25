(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Commands.mli *)

type 'msg in_flight =
  | Now of 'msg
  | Request of Cap.network * Http_request.t * ((Cmd.http_response, Cmd.http_error) result -> 'msg)
type 'msg t = { mutable in_flight : 'msg in_flight list }

let create () : 'msg t = { in_flight = [] }

(* claude: https://, until TLS is ours (plan_teaching_other.md 4b):
 * curl, as graphics/images' Download uses it -- blocking, so the frame
 * waits while it fetches, the one request that does. The answer in the
 * same shape as Http_request's: the URL after the redirections, the
 * status, the headers (the last response's: a redirection's are
 * dropped when the next status line comes), the body's bytes. *)
let is_https (url : string) : bool = String.length url >= 8 && String.lowercase_ascii (String.sub url 0 8) = "https://"

let curl_get ?post (caps : Cap.network) (url : string) : (Cmd.http_response, Cmd.http_error) result =
  match Url.parse url with
  | Ok { authority = Some a; _ } -> (
      (* the connection only once the host is granted *)
      let (_ : Cap.Network.t) = caps#network a.host in
      let body = Buffer.create 16384 and headers = ref [] in
      let conn = Curl.init () in
      Fun.protect
        ~finally:(fun () -> Curl.cleanup conn)
        (fun () ->
          Curl.set_url conn url;
          (match post with
          | Some (content_type, body) ->
              Curl.set_postfields conn body;
              Curl.set_postfieldsize conn (String.length body);
              Curl.set_httpheader conn [ "Content-Type: " ^ content_type ]
          | None -> ());
          Curl.set_followlocation conn true;
          Curl.set_timeout conn 30;
          Curl.set_writefunction conn (fun s ->
              Buffer.add_string body s;
              String.length s);
          Curl.set_headerfunction conn (fun line ->
              (if String.length line >= 5 && String.sub line 0 5 = "HTTP/" then headers := []
               else
                 match String.index_opt line ':' with
                 | Some i ->
                     headers :=
                       (String.trim (String.sub line 0 i), String.trim (String.sub line (i + 1) (String.length line - i - 1)))
                       :: !headers
                 | None -> ());
              String.length line);
          match Curl.perform conn with
          | () ->
              Ok
                {
                  Cmd.url = Curl.get_effectiveurl conn;
                  status = Curl.get_responsecode conn;
                  headers = List.rev !headers;
                  body = Buffer.contents body;
                }
          | exception Curl.CurlException (_, _, why) -> Error (Cmd.Network_error why)))
  | _ -> Error (Cmd.Bad_url url)

let perform (t : 'msg t) (cmd : 'msg Cmd.t) : unit =
  Cmd.to_list cmd
  |> List.iter (fun (c : 'msg Cmd.t) ->
         match c with
         | Msg msg -> t.in_flight <- t.in_flight @ [ Now msg ]
         | Http_get (caps, url, k) when is_https url -> t.in_flight <- t.in_flight @ [ Now (k (curl_get caps url)) ]
         | Http_get (caps, url, k) -> t.in_flight <- t.in_flight @ [ Request (caps, Http_request.start caps url, k) ]
         | Http_post (caps, url, post, k) when is_https url ->
             t.in_flight <- t.in_flight @ [ Now (k (curl_get ~post caps url)) ]
         | Http_post (caps, url, post, k) ->
             t.in_flight <- t.in_flight @ [ Request (caps, Http_request.start ~post caps url, k) ]
         | None | Batch _ -> (* to_list flattened them *) ())

(* Http_request's answer as Cmd's, with the URL of the last
 * redirection; one to https:// (what most http:// sites answer today)
 * refused by our client, and so taken on by curl *)
let answer (caps : Cap.network) (request : Http_request.t) (r : (Http.response, Http_request.error) result) :
    (Cmd.http_response, Cmd.http_error) result =
  match r with
  | Ok response ->
      Ok { url = Http_request.url request; status = response.status; headers = response.headers; body = response.body }
  | Error (Bad_url _) when is_https (Http_request.url request) -> curl_get caps (Http_request.url request)
  | Error (Bad_url why) -> Error (Bad_url why)
  | Error Timeout -> Error Timeout
  | Error (Failed why) -> Error (Network_error why)

let step (t : 'msg t) : 'msg list =
  let finished, pending =
    t.in_flight
    |> List.partition_map (fun f ->
           match f with
           | Now msg -> Left msg
           | Request (caps, r, k) -> (
               Http_request.step r;
               match Http_request.result r with Some result -> Left (k (answer caps r result)) | None -> Right f))
  in
  t.in_flight <- pending;
  finished
