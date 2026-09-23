(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_websocket.mli *)

let hex (s : string) : string = String.concat " " (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq s)))

let frame (s : string) : Websocket.frame =
  match Websocket.decode s with Frame (f, n) when n = String.length s -> f | _ -> Alcotest.fail "not one whole frame"

let tests =
  Testo.categorize "Websocket"
    [
      Testo.create "the worked example: the RFC's handshake" (fun () ->
          Alcotest.(check string) "accept" "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" (Websocket.accept "dGhlIHNhbXBsZSBub25jZQ==");
          let req = Websocket.request ~host:"127.0.0.1:8765" ~path:"/" ~key:"dGhlIHNhbXBsZSBub25jZQ==" in
          match Websocket.handshake (req ^ "\x82\x00") with
          | Some (headers, stop) ->
              Alcotest.(check (option string)) "the key" (Some "dGhlIHNhbXBsZSBub25jZQ==") (List.assoc_opt "sec-websocket-key" headers);
              Alcotest.(check int) "what follows" (String.length req) stop
          | None -> Alcotest.fail "no handshake");
      Testo.create "the worked example: Hello, unmasked and masked" (fun () ->
          let hello = { Websocket.fin = true; opcode = Text; payload = "Hello" } in
          Alcotest.(check string) "unmasked" "81 05 48 65 6c 6c 6f" (hex (Websocket.encode hello));
          Alcotest.(check string) "masked" "81 85 37 fa 21 3d 7f 9f 4d 51 58" (hex (Websocket.encode ~mask:"\x37\xfa\x21\x3d" hello));
          Alcotest.(check string) "unmasked back" "Hello" (frame "\x81\x85\x37\xfa\x21\x3d\x7f\x9f\x4d\x51\x58").payload);
      Testo.create "the worked example: 256 and 65,536 bytes" (fun () ->
          let big n = Websocket.encode { fin = true; opcode = Binary; payload = String.make n 'x' } in
          Alcotest.(check string) "256" "82 7e 01 00" (hex (String.sub (big 256) 0 4));
          Alcotest.(check string) "65536" "82 7f 00 00 00 00 00 01 00 00" (hex (String.sub (big 65536) 0 10));
          Alcotest.(check int) "back" 65536 (String.length (frame (big 65536)).payload));
      Testo.create "a stream cut anywhere" (fun () ->
          let a = Websocket.encode ~mask:"abcd" { fin = true; opcode = Binary; payload = "first" } in
          let b = Websocket.encode { fin = true; opcode = Binary; payload = String.make 300 'y' } in
          let stream = a ^ b in
          for cut = 0 to String.length a - 1 do
            match Websocket.decode (String.sub stream 0 cut) with
            | Incomplete -> ()
            | _ -> Alcotest.fail (Printf.sprintf "whole at %d bytes" cut)
          done;
          match Websocket.decode stream with
          | Frame (f, n) ->
              Alcotest.(check string) "the first" "first" f.payload;
              Alcotest.(check int) "the second after it" 300 (String.length (frame (String.sub stream n (String.length stream - n))).payload)
          | _ -> Alcotest.fail "no first frame");
      Testo.create "garbage refused" (fun () ->
          let bad s = match Websocket.decode s with Bad _ -> true | _ -> false in
          Alcotest.(check bool) "opcode 3" true (bad "\x83\x00");
          Alcotest.(check bool) "a gigabyte announced" true (bad "\x82\x7f\x00\x00\x00\x00\x40\x00\x00\x00"));
    ]
