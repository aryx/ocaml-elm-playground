(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Resample: a sine read a fifth faster, the three methods'
 * errors measured; the lengths; a file's rate *)

let t = Testo.create
let n = 4096

(* the sine read faster lands on bin 64 of 4096 (689.1 Hz): whatever
 * else is in the spectrum is the method's error, in dB below it *)
let error ?(bin = 64) (kind : Resample.kind) (ratio : float) : float =
  let out_hz = Spectrum.bin_frequency ~n bin in
  let sine = Signal.of_function 0.5 (fun t -> sin (2. *. Float.pi *. out_hz /. ratio *. t)) in
  let y = Resample.faster kind ratio sine in
  let m = Spectrum.magnitudes (Spectrum.fft (Array.sub y 0 n)) in
  let worst = ref 0. in
  Array.iteri (fun k v -> if k <> bin && v > !worst then worst := v) m;
  20. *. log10 (!worst /. m.(bin))

(* a fifth up (2^(7/12)), to 689 Hz and to 2756 Hz: each method's
 * loudest error; the higher the note, the fewer samples a period to
 * guess between, the worse *)
let test_errors () =
  let fifth = 2. ** (7. /. 12.) in
  List.iter
    (fun (bin, kind, expected) ->
      Alcotest.(check (float 0.1))
        (Printf.sprintf "a fifth up to %.0f Hz, %s: the loudest error (dB)" (Spectrum.bin_frequency ~n bin) (Resample.name kind))
        expected (error ~bin kind fifth))
    [ (64, Nearest, -39.6); (64, Linear, -79.2); (64, Cubic, -112.5); (256, Nearest, -27.3); (256, Linear, -54.6); (256, Cubic, -75.3) ]

(* pitch and length go together; and a file's rate *)
let test_lengths () =
  let s = Array.init 44100 (fun i -> float_of_int i) in
  Alcotest.(check int) "an octave up: half as long" 22050 (Array.length (Resample.faster Linear 2. s));
  Alcotest.(check int) "an octave down: twice" 88200 (Array.length (Resample.faster Linear 0.5 s));
  (* a ramp read at half speed, linearly: the halves in between *)
  Alcotest.(check (float 1e-9)) "between samples 10 and 11" 10.5 (Resample.faster Linear 0.5 s).(21);
  let at_22050 = Array.init 22050 (fun i -> float_of_int i) in
  Alcotest.(check int) "22,050 a second, played at 44,100: twice the samples" 44100
    (Array.length (Resample.to_rate Linear 22050 at_22050))

(* a WAV file as they come: a LIST chunk before the data, two channels,
 * 22,050 a second -- read as one channel at 44,100: the channels
 * averaged, twice the samples, the in-between ones interpolated *)
let test_wav_file () =
  let b = Buffer.create 100 in
  let u32 v = Buffer.add_int32_le b (Int32.of_int v) and u16 v = Buffer.add_uint16_le b v in
  let frames = [ (1000, 3000); (2000, 4000); (3000, 5000) ] in
  Buffer.add_string b "RIFF";
  u32 (4 + (8 + 4) + (8 + 16) + 8 + (4 * List.length frames));
  Buffer.add_string b "WAVE";
  Buffer.add_string b "LIST";
  u32 4;
  Buffer.add_string b "INFO";
  Buffer.add_string b "fmt ";
  u32 16;
  u16 1;
  u16 2;
  u32 22050;
  u32 (22050 * 4);
  u16 4;
  u16 16;
  Buffer.add_string b "data";
  u32 (4 * List.length frames);
  List.iter (fun (l, r) -> Buffer.add_int16_le b l; Buffer.add_int16_le b r) frames;
  match Wav.of_string (Buffer.contents b) with
  | Error e -> Alcotest.fail e
  | Ok s ->
      Alcotest.(check int) "twice the samples" 6 (Array.length s);
      Alcotest.(check (list (float 1e-6))) "the channels averaged, the halves between"
        (List.map (fun x -> x /. 32767.) [ 2000.; 2500.; 3000.; 3500.; 4000.; 2000. ])
        (Array.to_list s)

let tests =
  Testo.categorize "Resample"
    [ t "a fifth up: each method's error" test_errors; t "lengths, and a file's rate" test_lengths;
      t "a WAV file as they come: other chunks, stereo, 22,050" test_wav_file ]
