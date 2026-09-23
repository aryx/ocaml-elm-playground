(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_wire.mli *)

let hex (s : string) : string = String.concat " " (List.map (fun c -> Printf.sprintf "%02X" (Char.code c)) (List.of_seq (String.to_seq s)))

let unhex (h : string) : string =
  String.split_on_char ' ' h |> List.filter (( <> ) "") |> List.map (fun b -> Char.chr (int_of_string ("0x" ^ b))) |> List.to_seq |> String.of_seq

(* lockstep's input message, the .mli's worked example *)
type input_message = { tick : int; inputs : string }

let write_input (m : input_message) (w : Wire.writer) : unit =
  Wire.put_u8 w 1;
  Wire.put_varint w m.tick;
  Wire.put_string w m.inputs

let read_input (r : Wire.reader) : input_message =
  match Wire.get_u8 r with
  | 1 ->
      let tick = Wire.get_varint r in
      let inputs = Wire.get_string r in
      { tick; inputs }
  | kind -> Wire.fail r (Printf.sprintf "unknown message type %d" kind)

let refused (read : Wire.reader -> 'a) (bytes : string) : bool = Result.is_error (Wire.parse read bytes)

let tests =
  Testo.categorize "Wire"
    [
      Testo.create "the worked example: lockstep's input message, 7 bytes" (fun () ->
          let bytes = Wire.to_bytes (write_input { tick = 300; inputs = "\x05\x05\x04" }) in
          Alcotest.(check string) "bytes" "01 82 2C 03 05 05 04" (hex bytes);
          Alcotest.(check int) "700 bytes a second" 700 ((String.length bytes + 28) * 20);
          match Wire.parse read_input bytes with
          | Ok m -> Alcotest.(check (pair int string)) "back" (300, "\x05\x05\x04") (m.tick, m.inputs)
          | Error e -> Alcotest.fail e);
      Testo.create "MIDI's varint table" (fun () ->
          List.iter
            (fun (n, h) ->
              Alcotest.(check string) (string_of_int n) h (hex (Wire.to_bytes (fun w -> Wire.put_varint w n)));
              Alcotest.(check (result int string)) h (Ok n) (Wire.parse Wire.get_varint (unhex h)))
            [ (0, "00"); (127, "7F"); (128, "81 00"); (200, "81 48"); (300, "82 2C"); (480, "83 60"); (16383, "FF 7F");
              (16384, "81 80 00"); ((1 lsl 28) - 1, "FF FF FF 7F") ]);
      Testo.create "zigzag: small negatives stay small" (fun () ->
          List.iter
            (fun (n, h) ->
              Alcotest.(check string) (string_of_int n) h (hex (Wire.to_bytes (fun w -> Wire.put_signed w n)));
              Alcotest.(check (result int string)) h (Ok n) (Wire.parse Wire.get_signed (unhex h)))
            [ (0, "00"); (-1, "01"); (1, "02"); (-2, "03"); (-64, "7F"); (64, "81 00"); (-(1 lsl 27), "FF FF FF 7F") ]);
      Testo.create "u16, big-endian" (fun () ->
          Alcotest.(check string) "0x1234" "12 34" (hex (Wire.to_bytes (fun w -> Wire.put_u16 w 0x1234))));
      Testo.create "out of range, refused when written" (fun () ->
          List.iter
            (fun write -> Alcotest.(check bool) "raises" true (try ignore (Wire.to_bytes write); false with Invalid_argument _ -> true))
            [ (fun w -> Wire.put_u8 w 256); (fun w -> Wire.put_u16 w (-1)); (fun w -> Wire.put_varint w (1 lsl 28));
              (fun w -> Wire.put_signed w (1 lsl 27)) ]);
      Testo.create "garbage, refused when read" (fun () ->
          List.iter
            (fun (why, h) -> Alcotest.(check bool) why true (refused read_input (unhex h)))
            [ ("empty", ""); ("an unknown type", "02 82 2C 00"); ("the tick cut", "01 82");
              ("a varint that doesn't end", "01 FF FF FF FF 7F 00"); ("a useless leading byte", "01 80 82 2C 00");
              ("a string past the end", "01 01 09 05"); ("bytes left over", "01 01 00 FF") ]);
      Testo.create "10,000 random strings: never an exception, one value one encoding" (fun () ->
          let seed = ref (Lehmer.scramble 2026) in
          let byte () = seed := Lehmer.next !seed; int_of_float (256. *. Lehmer.to_unit !seed) in
          let parsed = ref 0 in
          for _ = 1 to 10_000 do
            let bytes = String.init (1 + (byte () mod 8)) (fun i -> if i = 0 then Char.chr (byte () mod 2) else Char.chr (byte ())) in
            match Wire.parse read_input bytes with
            | Ok m ->
                incr parsed;
                Alcotest.(check string) "re-encoded" (hex bytes) (hex (Wire.to_bytes (write_input m)))
            | Error _ -> ()
          done;
          Alcotest.(check bool) (Printf.sprintf "%d parsed, some" !parsed) true (!parsed > 0));
    ]
