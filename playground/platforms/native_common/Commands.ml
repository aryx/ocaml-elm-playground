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

type 'msg in_flight = Now of 'msg | Request of Http_request.t * ((Cmd.http_response, Cmd.http_error) result -> 'msg)
type 'msg t = { mutable in_flight : 'msg in_flight list }

let create () : 'msg t = { in_flight = [] }

let perform (t : 'msg t) (cmd : 'msg Cmd.t) : unit =
  Cmd.to_list cmd
  |> List.iter (fun (c : 'msg Cmd.t) ->
         match c with
         | Msg msg -> t.in_flight <- t.in_flight @ [ Now msg ]
         | Http_get (caps, url, k) -> t.in_flight <- t.in_flight @ [ Request (Http_request.start caps url, k) ]
         | None | Batch _ -> (* to_list flattened them *) ())

(* Http_request's answer as Cmd's, with the URL of the last
 * redirection *)
let answer (request : Http_request.t) (r : (Http.response, Http_request.error) result) :
    (Cmd.http_response, Cmd.http_error) result =
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
           | Request (r, k) -> (
               Http_request.step r;
               match Http_request.result r with Some result -> Left (k (answer r result)) | None -> Right f))
  in
  t.in_flight <- pending;
  finished
