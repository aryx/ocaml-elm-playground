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
  (* https://, on a thread of the pool *)
  | Blocking of answer Worker.job * (answer -> 'msg)

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

(* claude: https://, by Http_client over our own TLS 1.3 (Tls_client,
 * Tls13; plan_tls.md) -- blocking, so the frame waits while it
 * fetches, or on a thread of the pool (threads=on). The answer in the
 * same shape as Http_request's: the URL after the redirections, the
 * status, the headers, the body's bytes. *)
let is_https (url : string) : bool = String.length url >= 8 && String.lowercase_ascii (String.sub url 0 8) = "https://"

let https_get ?post (caps : Cap.network) (url : string) : (Cmd.http_response, Cmd.http_error) result =
  match Http_client.fetch ?post caps url with
  | Ok (url, response) -> Ok { Cmd.url; status = response.status; headers = response.headers; body = response.body }
  | Error why -> Error (Cmd.Network_error why)

(* the blocking fetch: at once, the frame waiting, or on a thread *)
let blocking ?post (t : 'msg t) (caps : Cap.network) (url : string) (k : answer -> 'msg) : 'msg in_flight =
  match t.pool with
  | None -> Now (k (https_get ?post caps url))
  | Some pool -> Blocking (Worker.submit pool (fun () -> https_get ?post caps url), k)

let perform (t : 'msg t) (cmd : 'msg Cmd.t) : unit =
  Cmd.to_list cmd
  |> List.iter (fun (c : 'msg Cmd.t) ->
         match c with
         | Msg msg -> t.in_flight <- t.in_flight @ [ Now msg ]
         | Http_get (caps, url, k) when is_https url -> t.in_flight <- t.in_flight @ [ blocking t caps url k ]
         | Http_get (caps, url, k) ->
             t.in_flight <- t.in_flight @ [ Request (caps, Http_request.start ?resolver:t.pool caps url, k) ]
         | Http_post (caps, url, post, k) when is_https url -> t.in_flight <- t.in_flight @ [ blocking ~post t caps url k ]
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
                * answer today), which Http_request leaves to the
                * blocking client *)
               | Some (Error (Bad_url _)) when is_https (Http_request.url r) -> (
                   match blocking t caps (Http_request.url r) k with Now msg -> Left msg | f -> Right f)
               | Some result -> Left (k (answer r result)))
           | Blocking (job, k) -> (
               match Worker.poll job with
               | None -> Right f
               | Some (Ok result) -> Left (k result)
               | Some (Error e) -> Left (k (Error (Network_error (Printexc.to_string e))))))
  in
  t.in_flight <- pending;
  finished
