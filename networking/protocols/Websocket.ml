(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Websocket.mli *)

(*****************************************************************************)
(* The handshake *)
(*****************************************************************************)

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let accept (key : string) : string = Base64.encode (Sha1.digest (key ^ guid))

let request ~(host : string) ~(path : string) ~(key : string) : string =
  Printf.sprintf
    "GET %s HTTP/1.1\r\nHost: %s\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n\r\n"
    path host key

let response ~(key : string) : string =
  Printf.sprintf "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n"
    (accept key)

let handshake (bytes : string) : ((string * string) list * int) option =
  let rec find i = if i + 4 > String.length bytes then None else if String.sub bytes i 4 = "\r\n\r\n" then Some i else find (i + 1) in
  match find 0 with
  | None -> None
  | Some stop ->
      let lines = String.split_on_char '\n' (String.sub bytes 0 stop) |> List.map String.trim in
      let headers =
        List.filter_map
          (fun line ->
            match String.index_opt line ':' with
            | Some i -> Some (String.lowercase_ascii (String.sub line 0 i), String.trim (String.sub line (i + 1) (String.length line - i - 1)))
            | None -> None)
          (List.tl lines)
      in
      Some (("", List.hd lines) :: headers, stop + 4)

(*****************************************************************************)
(* Frames *)
(*****************************************************************************)

type opcode = Continuation | Text | Binary | Close | Ping | Pong
type frame = { fin : bool; opcode : opcode; payload : string }
type decoded = Frame of frame * int | Incomplete | Bad of string

let code (op : opcode) : int = match op with Continuation -> 0 | Text -> 1 | Binary -> 2 | Close -> 8 | Ping -> 9 | Pong -> 10

let opcode_of (n : int) : opcode option =
  match n with 0 -> Some Continuation | 1 -> Some Text | 2 -> Some Binary | 8 -> Some Close | 9 -> Some Ping | 10 -> Some Pong | _ -> None

(* byte i xored with mask[i mod 4]: masking and unmasking alike *)
let xor_mask (mask : string) (s : string) : string = String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3])) s

let encode ?(mask : string option) (f : frame) : string =
  let b = Buffer.create (String.length f.payload + 14) in
  Buffer.add_char b (Char.chr ((if f.fin then 0x80 else 0) lor code f.opcode));
  let masked = if mask = None then 0 else 0x80 in
  let n = String.length f.payload in
  if n < 126 then Buffer.add_char b (Char.chr (masked lor n))
  else if n < 65536 then begin
    Buffer.add_char b (Char.chr (masked lor 126));
    Buffer.add_char b (Char.chr (n lsr 8));
    Buffer.add_char b (Char.chr (n land 255))
  end
  else begin
    Buffer.add_char b (Char.chr (masked lor 127));
    for i = 7 downto 0 do
      Buffer.add_char b (Char.chr (if i >= 4 then 0 else (n lsr (8 * i)) land 255))
    done
  end;
  (match mask with
  | Some m ->
      Buffer.add_string b m;
      Buffer.add_string b (xor_mask m f.payload)
  | None -> Buffer.add_string b f.payload);
  Buffer.contents b

(* frames bigger than this are refused: our messages are small, and a
 * length from the network is not to be trusted *)
let max_payload = 1 lsl 20

let decode (s : string) : decoded =
  let n = String.length s in
  let byte i = Char.code s.[i] in
  if n < 2 then Incomplete
  else
    match opcode_of (byte 0 land 0x0f) with
    | None -> Bad (Printf.sprintf "unknown opcode %d" (byte 0 land 0x0f))
    | Some opcode -> (
        let fin = byte 0 land 0x80 <> 0 and masked = byte 1 land 0x80 <> 0 in
        let len7 = byte 1 land 0x7f in
        (* the payload's length, and where the mask (or the payload) starts *)
        let length =
          if len7 < 126 then Some (len7, 2)
          else if len7 = 126 then if n < 4 then None else Some ((byte 2 lsl 8) lor byte 3, 4)
          else if n < 10 then None
          else if byte 2 lor byte 3 lor byte 4 lor byte 5 <> 0 then Some (max_int, 10)
          else Some ((byte 6 lsl 24) lor (byte 7 lsl 16) lor (byte 8 lsl 8) lor byte 9, 10)
        in
        match length with
        | None -> Incomplete
        | Some (len, _) when len > max_payload -> Bad (Printf.sprintf "a frame of %d bytes" len)
        | Some (len, at) ->
            let at, mask = if masked then (at + 4, if n >= at + 4 then Some (String.sub s at 4) else None) else (at, None) in
            if n < at + len || (masked && mask = None) then Incomplete
            else
              let payload = String.sub s at len in
              let payload = match mask with Some m -> xor_mask m payload | None -> payload in
              Frame ({ fin; opcode; payload }, at + len))
