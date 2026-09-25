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

type answer = (Cmd.http_response, Cmd.http_error) result

type 'msg in_flight =
  | Now of 'msg
  | Request of Cap.network * Http_request.t * (answer -> 'msg)
  (* curl, on a thread of the pool *)
  | Curl of answer Worker.job * (answer -> 'msg)

type 'msg t = {
  mutable in_flight : 'msg in_flight list;
  (* with threads (threads=on): where what blocks is done *)
  pool : Worker.t option;
}

(* claude: four threads, Netscape's four connections: at most four
 * names resolved or https:// fetches waiting at once, the others
 * queued *)
let create ?(threads = false) () : 'msg t =
  { in_flight = []; pool = (if threads then Some (Worker.create 4) else None) }

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

(* curl's fetch: at once, the frame waiting, or on a thread *)
let curl ?post (t : 'msg t) (caps : Cap.network) (url : string) (k : answer -> 'msg) : 'msg in_flight =
  match t.pool with
  | None -> Now (k (curl_get ?post caps url))
  | Some pool -> Curl (Worker.submit pool (fun () -> curl_get ?post caps url), k)

let perform (t : 'msg t) (cmd : 'msg Cmd.t) : unit =
  Cmd.to_list cmd
  |> List.iter (fun (c : 'msg Cmd.t) ->
         match c with
         | Msg msg -> t.in_flight <- t.in_flight @ [ Now msg ]
         | Http_get (caps, url, k) when is_https url -> t.in_flight <- t.in_flight @ [ curl t caps url k ]
         | Http_get (caps, url, k) ->
             t.in_flight <- t.in_flight @ [ Request (caps, Http_request.start ?resolver:t.pool caps url, k) ]
         | Http_post (caps, url, post, k) when is_https url -> t.in_flight <- t.in_flight @ [ curl ~post t caps url k ]
         | Http_post (caps, url, post, k) ->
             t.in_flight <- t.in_flight @ [ Request (caps, Http_request.start ~post ?resolver:t.pool caps url, k) ]
         | None | Batch _ -> (* to_list flattened them *) ())

(* Http_request's answer as Cmd's, with the URL of the last
 * redirection *)
let answer (request : Http_request.t) (r : (Http.response, Http_request.error) result) : answer =
  match r with
  | Ok response ->
      Ok { url = Http_request.url request; status = response.status; headers = response.headers; body = response.body }
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
               match Http_request.result r with
               | None -> Right f
               (* a redirection to https:// (what most http:// sites
                * answer today), refused by our client: taken on by curl *)
               | Some (Error (Bad_url _)) when is_https (Http_request.url r) -> (
                   match curl t caps (Http_request.url r) k with Now msg -> Left msg | f -> Right f)
               | Some result -> Left (k (answer r result)))
           | Curl (job, k) -> (
               match Worker.poll job with
               | None -> Right f
               | Some (Ok result) -> Left (k result)
               | Some (Error e) -> Left (k (Error (Network_error (Printexc.to_string e))))))
  in
  t.in_flight <- pending;
  finished
