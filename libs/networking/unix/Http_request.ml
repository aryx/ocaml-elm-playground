(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Http_request.mli *)

type error = Bad_url of string | Timeout | Failed of string

type state =
  (* the name being resolved on a thread of the pool (Worker) *)
  | Resolving of string * Unix.addr_info list Worker.job
  (* the connection begun; the addresses to try if it fails *)
  | Connecting of Unix.file_descr * Unix.addr_info list
  (* the request's bytes, and how many are written *)
  | Sending of Unix.file_descr * int
  | Receiving of Unix.file_descr * Buffer.t
  | Done of (Http.response, error) result

type t = {
  (* the authority to reach the network, for the redirections too *)
  caps : Cap.network;
  mutable state : state;
  mutable url : Url.t;
  mutable request : string;
  mutable post : (string * string) option; (* a POST's content type and body; a redirection makes it a GET *)
  mutable redirects_left : int;
  deadline : float;
  (* where getaddrinfo is called: a pool's thread, or this one *)
  resolver : Worker.t option;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ready_to_write (fd : Unix.file_descr) : bool =
  let _, writable, _ = Unix.select [] [ fd ] [] 0. in
  writable <> []

let ready_to_read (fd : Unix.file_descr) : bool =
  let readable, _, _ = Unix.select [ fd ] [] [] 0. in
  readable <> []

let failed (t : t) (why : string) : state = Done (Error (Failed (Printf.sprintf "%s: %s" (Url.to_string t.url) why)))

(* a non-blocking connect to the first of [addresses] that doesn't fail
 * at once: EINPROGRESS is the normal answer, "later" *)
let rec connect (t : t) (addresses : Unix.addr_info list) : state =
  match addresses with
  | [] -> failed t "can't connect"
  | a :: others -> (
      let fd = Unix.socket a.ai_family a.ai_socktype a.ai_protocol in
      Unix.set_nonblock fd;
      match Unix.connect fd a.ai_addr with
      | () -> Sending (fd, 0)
      | exception Unix.Unix_error (Unix.EINPROGRESS, _, _) -> Connecting (fd, others)
      | exception Unix.Unix_error (e, _, _) ->
          Unix.close fd;
          if others = [] then failed t (Unix.error_message e) else connect t others)

let resolved (t : t) (host : string) (addresses : Unix.addr_info list) : state =
  match addresses with [] -> failed t (Printf.sprintf "can't resolve %S" host) | addresses -> connect t addresses

(* the name resolved (the one blocking call: here, or on the resolver's
 * thread), the connection begun *)
let begin_request (t : t) : state =
  match Http_client.prepare ?post:t.post t.url with
  | Error why -> Done (Error (Bad_url why))
  | Ok (host, port, request) -> (
      t.request <- request;
      let (_ : Cap.Network.t) = t.caps#network host in
      let resolve () = Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] in
      match t.resolver with
      | Some pool -> Resolving (host, Worker.submit pool resolve)
      | None -> resolved t host (resolve ()))

(* the server closed: the response parsed, or the next request of a
 * redirection begun *)
let answered (t : t) (bytes : string) : state =
  match Http.parse_response bytes with
  | Error why -> failed t why
  | Ok response -> (
      match (Http.is_redirect response.status, Http.header "Location" response.headers) with
      | true, Some location -> (
          if t.redirects_left = 0 then failed t "too many redirections"
          else
            match Url.parse location with
            | Error why -> Done (Error (Bad_url why))
            | Ok next ->
                t.url <- Url.resolve t.url next;
                t.redirects_left <- t.redirects_left - 1;
                (* the answer to a POST, elsewhere, is fetched with a
                 * GET: 303 says so, and browsers do it for 301 and 302
                 * too (RFC 9110 15.4.2-4) -- "POST, then redirect,
                 * then GET", so that reloading the answer does not
                 * post again *)
                t.post <- None;
                begin_request t)
      | _ -> Done (Ok response))

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let start ?(max_redirects = 5) ?(timeout = 30.) ?post ?resolver (caps : < Cap.network ; .. >) (s : string) : t =
  let t =
    {
      caps = (caps :> Cap.network);
      state = Done (Error (Bad_url s));
      url = Url.{ scheme = None; authority = None; path = s; query = None; fragment = None };
      request = "";
      post;
      redirects_left = max_redirects;
      deadline = Unix.gettimeofday () +. timeout;
      resolver;
    }
  in
  (match Url.parse s with
  | Error why -> t.state <- Done (Error (Bad_url why))
  | Ok url ->
      t.url <- url;
      t.state <- begin_request t);
  t

(* one transition, if the socket allows it; the loop in [step] repeats
 * until none is possible *)
let transition (t : t) : state option =
  match t.state with
  | Done _ -> None
  | Resolving (host, job) -> (
      match Worker.poll job with
      | None -> None
      | Some (Ok addresses) -> Some (resolved t host addresses)
      | Some (Error e) -> Some (failed t (Printexc.to_string e)))
  | Connecting (fd, others) ->
      if not (ready_to_write fd) then None
      else (
        (* writable: connected, or failed -- SO_ERROR says which *)
        match Unix.getsockopt_error fd with
        | None -> Some (Sending (fd, 0))
        | Some e ->
            Unix.close fd;
            Some (if others = [] then failed t (Unix.error_message e) else connect t others))
  | Sending (fd, pos) -> (
      if not (ready_to_write fd) then None
      else
        match Unix.write_substring fd t.request pos (String.length t.request - pos) with
        | n when pos + n = String.length t.request -> Some (Receiving (fd, Buffer.create 65536))
        | n -> Some (Sending (fd, pos + n))
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> None
        | exception Unix.Unix_error (e, _, _) ->
            Unix.close fd;
            Some (failed t (Unix.error_message e)))
  | Receiving (fd, b) -> (
      if not (ready_to_read fd) then None
      else
        let chunk = Bytes.create 65536 in
        match Unix.read fd chunk 0 (Bytes.length chunk) with
        | 0 ->
            Unix.close fd;
            Some (answered t (Buffer.contents b))
        | n ->
            Buffer.add_subbytes b chunk 0 n;
            Some (Receiving (fd, b))
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> None
        | exception Unix.Unix_error (e, _, _) ->
            Unix.close fd;
            Some (failed t (Unix.error_message e)))

let fd_of (state : state) : Unix.file_descr option =
  match state with
  | Connecting (fd, _) | Sending (fd, _) | Receiving (fd, _) -> Some fd
  | Resolving _ | Done _ -> None

let rec step (t : t) : unit =
  match t.state with
  | Done _ -> ()
  | state when Unix.gettimeofday () > t.deadline ->
      Option.iter Unix.close (fd_of state);
      t.state <- Done (Error Timeout)
  | _ -> (
      match transition t with
      | None -> ()
      | Some state ->
          t.state <- state;
          step t)

let result (t : t) : (Http.response, error) result option =
  match t.state with Done r -> Some r | _ -> None

let url (t : t) : string = Url.to_string t.url

let wait (ts : t list) (timeout : float) : unit =
  let reads, writes =
    List.fold_left
      (fun (reads, writes) t ->
        match t.state with
        | Connecting (fd, _) | Sending (fd, _) -> (reads, fd :: writes)
        | Receiving (fd, _) -> (fd :: reads, writes)
        | Resolving _ | Done _ -> (reads, writes))
      ([], []) ts
  in
  if reads <> [] || writes <> [] then ignore (Unix.select reads writes [] timeout)
  else if List.exists (fun t -> match t.state with Resolving _ -> true | _ -> false) ts then
    (* no socket yet, a thread resolving: a moment, not the whole
     * timeout (the thread can't wake a select) *)
    Unix.sleepf (Float.min timeout 0.001)
