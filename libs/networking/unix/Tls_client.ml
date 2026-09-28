(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tls_client.mli *)

let bundles = [ "/etc/ssl/certs/ca-certificates.crt"; "/etc/pki/tls/certs/ca-bundle.crt"; "/etc/ssl/cert.pem"; "/etc/ssl/ca-bundle.pem" ]
let roots : X509.t list option ref = ref None

(* the roots and the randomness, read under the network's authority
   (Tls_client.mli says why) *)
let read_file (path : string) : string = In_channel.with_open_bin path In_channel.input_all

let system_roots (_ : < Cap.network ; .. >) : X509.t list =
  match !roots with
  | Some r -> r
  | None ->
      let r =
        match List.find_opt Sys.file_exists bundles with
        | Some path -> List.filter_map (fun der -> Result.to_option (X509.parse der)) (Pem.certificates (read_file path))
        | None -> []
      in
      roots := Some r;
      r

(* randomness from the kernel *)
let random (n : int) : string = In_channel.with_open_bin "/dev/urandom" (fun ic -> really_input_string ic n)

(* The chains already checked in this program -- the host and the
   certificates' bytes -- until the first of them expires: a page's
   twenty pictures from one host are one chain checked, not twenty
   (three signatures each, 30 ms apiece). The handshake's own signature,
   CertificateVerify, is checked every time: it proves the server has
   the key now. A mutex, since Commands fetches on several threads. *)
let verified : (string, float) Hashtbl.t = Hashtbl.create 16
let verified_lock = Mutex.create ()

let locked (f : unit -> 'a) : 'a =
  Mutex.lock verified_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock verified_lock) f

let verify_cached ~(trust : X509.t list) ~(host : string) (chain : X509.t list) : (unit, string) result =
  let now = Unix.gettimeofday () in
  let key = host ^ "\000" ^ String.concat "" (List.map (fun (c : X509.t) -> c.der) chain) in
  let known = locked (fun () -> Hashtbl.find_opt verified key) in
  match known with
  | Some until when now <= until -> Ok ()
  | _ ->
      let r = X509.verify ~trust ~now ~host chain in
      if r = Ok () then (
        let until = List.fold_left (fun t (c : X509.t) -> min t c.not_after) infinity chain in
        locked (fun () -> Hashtbl.replace verified key until));
      r

type t = {
  fd : Unix.file_descr;
  host : string;
  mutable machine : Tls13.t;
  mutable outbox : string; (* application data waiting for the handshake *)
  mutable eof : bool;
  mutable closed : bool;
}

let write (t : t) (bytes : string) : unit = if bytes <> "" && not t.closed then try Tcp.send_all t.fd bytes with Unix.Unix_error _ -> t.eof <- true

let connect (caps : < Cap.network ; .. >) ?trust ~(host : string) ~(port : int) () : (t, string) result =
  let trust = match trust with Some t -> t | None -> system_roots caps in
  match Tcp.connect caps ~host ~port () with
  | exception Unix.Unix_error (e, _, _) -> Error (Printf.sprintf "can't reach %s:%d: %s" host port (Unix.error_message e))
  | exception Failure why -> Error why
  | fd ->
      let r = random 96 in
      let verify chain = verify_cached ~trust ~host chain in
      let machine, hello = Tls13.client ~host ~random:(String.sub r 0 32) ~secret:(String.sub r 32 32) ~session_id:(String.sub r 64 32) ~verify in
      let t = { fd; host; machine; outbox = ""; eof = false; closed = false } in
      write t hello;
      Unix.set_nonblock fd;
      Ok t

let step (t : t) : unit =
  if not (t.eof || t.closed) then begin
    let buf = Bytes.create 65536 in
    let rec read acc =
      match Unix.read t.fd buf 0 (Bytes.length buf) with
      | 0 ->
          t.eof <- true;
          acc
      | n -> read (acc ^ Bytes.sub_string buf 0 n)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> acc
      | exception Unix.Unix_error _ ->
          t.eof <- true;
          acc
    in
    let bytes = read "" in
    if bytes <> "" then begin
      let m, answer = Tls13.received t.machine bytes in
      t.machine <- m;
      write t answer
    end;
    (* the handshake done: what was queued *)
    if Tls13.state t.machine = Open && t.outbox <> "" then begin
      let m, records = Tls13.write t.machine t.outbox in
      t.machine <- m;
      t.outbox <- "";
      write t records
    end
  end

let state (t : t) : Tls13.state =
  match Tls13.state t.machine with Handshaking when t.eof -> Failed "the connection closed during the handshake" | s -> s

let machine (t : t) : Tls13.t = t.machine

let send (t : t) (data : string) : unit =
  t.outbox <- t.outbox ^ data;
  step t

let receive (t : t) : string =
  step t;
  let m, data = Tls13.read t.machine in
  t.machine <- m;
  data

let close (t : t) : unit =
  if not t.closed then begin
    let m, alert = Tls13.close t.machine in
    t.machine <- m;
    write t alert;
    t.closed <- true;
    try Unix.close t.fd with Unix.Unix_error _ -> ()
  end

let lines (t : t) : Transport.t =
  let partial = ref "" in
  let receive () =
    let all = !partial ^ receive t in
    match String.rindex_opt all '\n' with
    | None ->
        partial := all;
        []
    | Some i ->
        partial := String.sub all (i + 1) (String.length all - i - 1);
        String.split_on_char '\n' (String.sub all 0 i) |> List.map (fun l -> if l <> "" && l.[String.length l - 1] = '\r' then String.sub l 0 (String.length l - 1) else l)
  in
  let status () =
    match state t with
    | Handshaking -> "TLS 1.3 handshake with " ^ t.host
    | Open -> Printf.sprintf "TLS 1.3 to %s (%s)" t.host (match Tls13.cipher t.machine with Some Chacha20_poly1305 -> "ChaCha20-Poly1305" | _ -> "AES-128-GCM")
    | Closed -> t.host ^ ": closed"
    | Failed why -> t.host ^ ": " ^ why
  in
  { Transport.send = (fun line -> send t (line ^ "\r\n")); receive; status; player = (fun () -> None) }

let connect_lines (caps : < Cap.network ; .. >) ~(host : string) ~(port : int) : (Transport.t, string) result =
  Result.map lines (connect caps ~host ~port ())

let exchange ?trust ?(timeout = 30.) (caps : < Cap.network ; .. >) ~(host : string) ~(port : int) (request : string) : (string, string) result =
  match connect caps ?trust ~host ~port () with
  | Error e -> Error e
  | Ok t ->
      send t request;
      let answer = Buffer.create 65536 in
      let rec wait silent =
        match state t with
        | Failed why -> Error why
        | Closed -> Ok ()
        | _ when t.eof -> Ok ()
        | _ when silent > timeout -> Error (Printf.sprintf "%s: no answer in %.0f s" host timeout)
        | _ ->
            let got = receive t in
            Buffer.add_string answer got;
            if got = "" then (
              ignore (Unix.select [ t.fd ] [] [] 0.05);
              wait (silent +. 0.05))
            else wait 0.
      in
      let r = wait 0. in
      Buffer.add_string answer (receive t);
      close t;
      Result.map (fun () -> Buffer.contents answer) r
