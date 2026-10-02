(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_vorbis.mli *)

let t = Testo.create
let read_file (file : string) : string = In_channel.with_open_bin file In_channel.input_all

(* a 16-bit PCM WAV's channels, as the integers they hold *)
let wav_channels (s : string) : int array array =
  let u16 at = Char.code s.[at] lor (Char.code s.[at + 1] lsl 8) in
  let u32 at = u16 at lor (u16 (at + 2) lsl 16) in
  let rec chunks at channels =
    let id = String.sub s at 4 and size = u32 (at + 4) in
    if id = "fmt " then chunks (at + 8 + size) (u16 (at + 10))
    else if id = "data" then
      Array.init channels (fun c ->
          Array.init (size / 2 / channels) (fun i ->
              let v = u16 (at + 8 + (2 * ((i * channels) + c))) in
              if v >= 32768 then v - 65536 else v))
    else chunks (at + 8 + size + (size land 1)) channels
  in
  chunks 12 1

(* a file's sound is libvorbis's, a rounding apart: as many samples,
 * none further than 2 of 32,768 *)
let check_file (name : string) ~(rate : int) ~(channels : int) () =
  let t, ours = Vorbis.of_ogg (read_file (Filename.concat "vorbis" (name ^ ".ogg"))) in
  let expected = wav_channels (read_file (Filename.concat "vorbis" (name ^ ".expected.wav"))) in
  Alcotest.(check int) "rate" rate (Vorbis.rate t);
  Alcotest.(check int) "channels" channels (Vorbis.channels t);
  Alcotest.(check int) "channels decoded" (Array.length expected) (Array.length ours);
  Array.iteri
    (fun c theirs ->
      Alcotest.(check int) "samples" (Array.length theirs) (Array.length ours.(c));
      let worst = ref 0 and loud = ref 0 in
      Array.iteri
        (fun i v ->
          let x = max (-32768) (min 32767 (int_of_float (Float.round (ours.(c).(i) *. 32768.)))) in
          worst := max !worst (abs (x - v));
          loud := max !loud (abs v))
        theirs;
      if !worst > 2 then Alcotest.failf "%s, channel %d: %d away from libvorbis's" name c !worst;
      (* and it is a sound, not silence twice *)
      if !loud < 10000 then Alcotest.failf "%s, channel %d: too quiet (%d)" name c !loud)
    expected

(* the specification's example (3.2.1) *)
let test_codewords () =
  Alcotest.(check (array int)) "codes"
    [| 0b00; 0b0100; 0b0101; 0b0110; 0b0111; 0b10; 0b110; 0b111 |]
    (Vorbis.codewords [| 2; 4; 4; 4; 4; 2; 3; 3 |]);
  Alcotest.(check (array int)) "an entry not used" [| 0b0; -1; 0b10; 0b11 |] (Vorbis.codewords [| 1; 0; 2; 2 |]);
  match Vorbis.codewords [| 1; 1; 1 |] with
  | exception Failure _ -> ()
  | _ -> Alcotest.fail "three codes of one bit"

(* the fast transform is the definition's *)
let test_dct4 () =
  List.iter
    (fun m ->
      let x = Array.init m (fun i -> sin (float_of_int i *. 0.37) +. (0.5 *. cos (float_of_int (i * i) *. 0.01))) in
      let simple = Vorbis.dct4_simple x and fast = Vorbis.dct4 x in
      Array.iteri (fun i v -> if Float.abs (v -. fast.(i)) > 1e-9 *. float_of_int m then Alcotest.failf "m = %d, at %d: %g, not %g" m i fast.(i) v) simple)
    [ 4; 8; 32; 128; 1024 ]

(* a page: "OggS", 22 bytes of header, the segments' lengths, the data *)
let page (granule : int) (segments : string list) : string =
  let b = Buffer.create 64 in
  Buffer.add_string b "OggS\000\000";
  for i = 0 to 7 do Buffer.add_char b (Char.chr ((granule lsr (8 * i)) land 255)) done;
  Buffer.add_string b (String.make 12 '\000');
  Buffer.add_char b (Char.chr (List.length segments));
  List.iter (fun s -> Buffer.add_char b (Char.chr (String.length s))) segments;
  List.iter (Buffer.add_string b) segments;
  Buffer.contents b

let test_ogg () =
  let long = String.make 255 'x' in
  let file = page 0 [ "one"; "two" ] ^ page 0 [ long ] ^ page 7 [ "y"; long; "" ] in
  (* a packet goes on in the next page; one of 255 bytes ends with a segment of none *)
  Alcotest.(check (list string)) "packets" [ "one"; "two"; long ^ "y"; long ] (Ogg.packets file);
  Alcotest.(check (option int)) "length" (Some 7) (Ogg.length file);
  Alcotest.(check (list string)) "not Ogg" [] (Ogg.packets "RIFF....");
  Alcotest.(check (option int)) "no length" None (Ogg.length "")

let test_headers () =
  (match Vorbis.create ~identification:"\001theora" ~setup:"" with exception Failure _ -> () | _ -> Alcotest.fail "not Vorbis");
  match Vorbis.of_packets [ "one" ] with exception Failure _ -> () | _ -> Alcotest.fail "one packet"

(* a packet cut short is played as far as it goes; the first gives nothing *)
let test_packets () =
  match Ogg.packets (read_file (Filename.concat "vorbis" "bell.ogg")) with
  | identification :: _ :: setup :: first :: second :: third :: _ ->
      let d = Vorbis.create ~identification ~setup in
      Alcotest.(check int) "the first packet: no sound yet" 0 (Array.length (Vorbis.decode d first).(0));
      let whole = Array.length (Vorbis.decode d second).(0) in
      Alcotest.(check bool) "the second: some" true (whole > 0);
      let cut = Vorbis.decode d (String.sub third 0 (String.length third / 2)) in
      Alcotest.(check bool) "a cut one: as many samples as its block says" true (Array.length cut.(0) > 0);
      Alcotest.(check int) "a header again: none" 0 (Array.length (Vorbis.decode d setup).(0))
  | _ -> Alcotest.fail "bell.ogg's packets"

let tests =
  Testo.categorize "Vorbis"
    [
      t "the codes from their lengths" test_codewords;
      t "the transform, fast and by its definition" test_dct4;
      t "Ogg's pages and packets" test_ogg;
      t "headers refused" test_headers;
      t "packets one by one" test_packets;
      t "bell.ogg (mono, libvorbis)" (check_file "bell" ~rate:44100 ~channels:1);
      t "stereo.ogg (coupled, short blocks)" (check_file "stereo" ~rate:44100 ~channels:2);
      t "low.ogg (22,050 Hz)" (check_file "low" ~rate:22050 ~channels:2);
      t "native.ogg (ffmpeg's encoder)" (check_file "native" ~rate:44100 ~channels:2);
    ]
