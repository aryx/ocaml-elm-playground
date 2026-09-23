(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Relay_client.mli *)

let connect (caps : < Cap.network ; .. >) ~(host : string) ~(port : int) : Transport.t =
  let fd = Tcp.connect caps ~host ~port () in
  Unix.set_nonblock fd;
  let seed = ref (Lehmer.scramble (int_of_float (Unix.gettimeofday () *. 1000.))) in
  let bytes n =
    String.init n (fun _ ->
        seed := Lehmer.next !seed;
        Char.chr (int_of_float (256. *. Lehmer.to_unit !seed)))
  in
  let key = Base64.encode (bytes 16) in
  let inbox = ref "" and outbox = ref (Websocket.request ~host:(Printf.sprintf "%s:%d" host port) ~path:"/" ~key) in
  let upgraded = ref false and player = ref None and closed = ref false in
  let flush () =
    if !outbox <> "" && not !closed then
      match Unix.write_substring fd !outbox 0 (String.length !outbox) with
      | n -> outbox := String.sub !outbox n (String.length !outbox - n)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
      | exception Unix.Unix_error _ -> closed := true
  in
  let read () =
    let buf = Bytes.create 65536 in
    let rec go () =
      match Unix.read fd buf 0 (Bytes.length buf) with
      | 0 -> closed := true
      | n ->
          inbox := !inbox ^ Bytes.sub_string buf 0 n;
          go ()
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
      | exception Unix.Unix_error _ -> closed := true
    in
    if not !closed then go ()
  in
  (* the answer to the handshake: 101, and the accept of our key *)
  let upgrade () =
    match Websocket.handshake !inbox with
    | None -> ()
    | Some (headers, stop) ->
        inbox := String.sub !inbox stop (String.length !inbox - stop);
        if List.assoc_opt "sec-websocket-accept" headers = Some (Websocket.accept key) then upgraded := true
        else closed := true
  in
  let rec frames acc =
    match Websocket.decode !inbox with
    | Incomplete -> List.rev acc
    | Bad _ ->
        closed := true;
        List.rev acc
    | Frame (f, n) -> (
        inbox := String.sub !inbox n (String.length !inbox - n);
        match f.opcode with
        (* the relay's welcome: which player we are *)
        | Binary when !player = None && String.length f.payload = 2 && f.payload.[0] = '\002' ->
            player := Some (Char.code f.payload.[1]);
            frames acc
        | Binary -> frames (f.payload :: acc)
        | Close ->
            closed := true;
            List.rev acc
        | _ -> frames acc)
  in
  {
    send =
      (fun packet ->
        if !upgraded then outbox := !outbox ^ Websocket.encode ~mask:(bytes 4) { fin = true; opcode = Binary; payload = packet };
        flush ());
    receive =
      (fun () ->
        flush ();
        read ();
        if not !upgraded then upgrade ();
        if !upgraded then frames [] else []);
    status =
      (fun () ->
        match (!closed, !player) with
        | true, _ -> Printf.sprintf "the relay at %s:%d closed the connection (full?)" host port
        | false, None -> Printf.sprintf "connecting to the relay at %s:%d" host port
        | false, Some _ -> Printf.sprintf "through the relay at %s:%d" host port);
    player = (fun () -> !player);
  }
