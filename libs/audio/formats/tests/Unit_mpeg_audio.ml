(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* MPEG audio: our bell encoded by ffmpeg, twolame and LAME
 * (mpeg/make_mpeg.sh), decoded by us and compared with what ffmpeg
 * decodes from the same files *)

let t = Testo.create

let read_file (file : string) : string = In_channel.with_open_bin file In_channel.input_all

(* a 16-bit PCM WAV's channels, as the integers they hold (Wav.of_string
 * mixes stereo down): the fmt chunk's channel count, the data chunk *)
let wav_channels (s : string) : int array array =
  let u16 at = Char.code s.[at] lor (Char.code s.[at + 1] lsl 8) in
  let u32 at = u16 at lor (u16 (at + 2) lsl 16) in
  let rec chunks at channels =
    let id = String.sub s at 4 and size = u32 (at + 4) in
    if id = "fmt " then chunks (at + 8 + size) (u16 (at + 10))
    else if id = "data" then
      let n = size / 2 / channels in
      Array.init channels (fun c ->
          Array.init n (fun i ->
              let v = u16 (at + 8 + (2 * ((i * channels) + c))) in
              if v >= 32768 then v - 65536 else v))
    else chunks (at + 8 + size + (size land 1)) channels
  in
  chunks 12 1

(* the root mean square and the largest of the differences, in steps of
 * 16 bits (1/32,768) *)
let differences (ours : float array) (theirs : int array) : float * int =
  let n = min (Array.length ours) (Array.length theirs) in
  let sum = ref 0. and worst = ref 0 in
  for i = 0 to n - 1 do
    let d = Signal.to_int16 ours.(i) - theirs.(i) in
    sum := !sum +. float_of_int (d * d);
    worst := max !worst (abs d)
  done;
  (sqrt (!sum /. float_of_int (max 1 n)), !worst)

(* ours as close to ffmpeg's as 16 bits allow: 1 step apart here and
 * there (two decoders' floats, each made 16 bits: ffmpeg rounds a
 * mono file's but truncates a stereo MP3's, half its samples 1 step
 * below ours -- checked by hand against its float output), where a
 * mistake in a table or a step is off by hundreds *)
let check_file ?(rate = 44100) (name : string) (layer : int) (channels : int) () =
  let ours = Mpeg_audio.decode (read_file (Filename.concat "mpeg" name)) in
  (* bell.mp2's is bell_mp2.expected.wav *)
  let expected = String.map (fun c -> if c = '.' then '_' else c) name ^ ".expected.wav" in
  let expected = wav_channels (read_file (Filename.concat "mpeg" expected)) in
  match ours with
  | Error e -> Alcotest.fail e
  | Ok (h, sound) ->
      Alcotest.(check int) "layer" layer h.layer;
      Alcotest.(check int) "channels" channels h.channels;
      Alcotest.(check int) "sample rate" rate h.sample_rate;
      Alcotest.(check int) "samples" (Array.length expected.(0)) (Array.length sound.left);
      Array.iteri
        (fun c theirs ->
          let rms, worst = differences (if c = 0 then sound.left else sound.right) theirs in
          Printf.printf "%s channel %d: rms %.3f, worst %d\n" name c rms worst;
          if rms > 1. || worst > 2 then Alcotest.failf "%s, channel %d: rms %.3f, worst %d" name c rms worst)
        expected

(* Layer3_tables.mli: each table prefix-free (Vlc.of_list raises if
 * not) and complete, its Kraft sum 1: no bit string left unused *)
let test_huffman_tables () =
  let check name codes =
    ignore (Vlc.of_list (List.mapi (fun i c -> (c, i)) (Array.to_list codes)));
    Alcotest.(check (float 1e-12)) name 1. (Vlc.kraft (List.map (fun c -> (c, ())) (Array.to_list codes)))
  in
  Array.iteri
    (fun i t -> Option.iter (fun (codes, size, _) -> Alcotest.(check int) "size" (size * size) (Array.length codes); check (Printf.sprintf "table %d" i) codes) t)
    Layer3_tables.pairs;
  check "count1 A" Layer3_tables.quad_a;
  check "count1 B" Layer3_tables.quad_b;
  (* the standard's first table, and table 13's longest code, 19 bits *)
  Alcotest.(check (array string)) "table 1" [| "1"; "001"; "01"; "000" |] Layer3_tables.t1;
  Alcotest.(check int) "19 bits" 19
    (Array.fold_left (fun m c -> max m (String.length (String.concat "" (String.split_on_char ' ' c)))) 0 Layer3_tables.t13)

(* Imdct.mli's claim: blocks overlapping by half, windowed before the
 * MDCT and after its inverse, add back to the signal times n/4 -- the
 * aliases cancel *)
let test_tdac () =
  List.iter
    (fun n ->
      let w = Imdct.window (if n = 36 then 0 else 2) in
      let half = n / 2 in
      let signal = Array.init (6 * half) (fun i -> sin (float_of_int i *. 0.37) +. (0.5 *. cos (float_of_int (i * i) *. 0.01))) in
      let out = Array.make (Array.length signal) 0. in
      for block = 0 to 4 do
        let x = Array.init n (fun i -> signal.((block * half) + i) *. w.(i)) in
        let y = Imdct.imdct (Imdct.mdct x) in
        Array.iteri (fun i v -> out.((block * half) + i) <- out.((block * half) + i) +. (v *. w.(i))) y
      done;
      (* the first and last half have one block only *)
      for i = half to (5 * half) - 1 do
        Alcotest.(check (float 1e-9)) (Printf.sprintf "n = %d, sample %d" n i) (signal.(i) *. float_of_int n /. 4.) out.(i)
      done)
    [ 36; 12 ];
  (* and the start and stop windows keep the sum of squares across the
   * change, to a short block's *)
  let start = Imdct.window 1 and stop = Imdct.window 3 and short = Imdct.window 2 in
  for i = 0 to 5 do
    Alcotest.(check (float 1e-12)) "start then short" 1. ((start.(24 + i) ** 2.) +. (short.(i) ** 2.));
    Alcotest.(check (float 1e-12)) "short then stop" 1. ((short.(6 + i) ** 2.) +. (stop.(6 + i) ** 2.))
  done

(* Polyphase.mli: the window, 1.144989 at its middle, symmetric but for
 * the signs of every other block of 64 *)
let test_window () =
  let d = Polyphase.window in
  Alcotest.(check (float 1e-9)) "D[256]" 1.144989014 d.(256);
  Alcotest.(check (float 1e-9)) "D[64]" 0.003250122 d.(64);
  for i = 1 to 255 do
    let sign = if (i / 64) mod 2 = ((512 - i) / 64) mod 2 then 1. else -1. in
    Alcotest.(check (float 1e-12)) (Printf.sprintf "D[%d]" i) d.(i) (sign *. d.(512 - i))
  done

(* what the stereo MP3 exercises, from its side information: mid/side
 * frames, short blocks and the windows around them -- and no intensity
 * stereo, which we don't read *)
let test_stereo_mp3_features () =
  let s = read_file (Filename.concat "mpeg" "stereo.mp3") in
  let ms = ref 0 and intensity = ref 0 and block_types = Array.make 4 0 in
  List.iter
    (fun (at, (h : Mpeg_audio_header.t)) ->
      if h.mode = Joint_stereo && h.mode_extension land 2 <> 0 then incr ms;
      if h.mode = Joint_stereo && h.mode_extension land 1 <> 0 then incr intensity;
      let b = Bits.of_string s in
      Bits.seek b (8 * (at + 4 + if h.crc then 2 else 0));
      let si = Layer3.side_info h b in
      Array.iter (Array.iter (fun (g : Layer3.granule) -> block_types.(g.block_type) <- block_types.(g.block_type) + 1)) si.granules)
    (Mpeg_audio_header.frames s);
  Printf.printf "mid/side frames %d, block types %d %d %d %d\n" !ms block_types.(0) block_types.(1) block_types.(2) block_types.(3);
  Alcotest.(check int) "no intensity stereo" 0 !intensity;
  if !ms = 0 then Alcotest.fail "no mid/side frame";
  Array.iteri (fun t n -> if n = 0 then Alcotest.failf "no granule of block type %d" t) block_types

let tests =
  Testo.categorize "Mpeg_audio"
    [
      t "Layer III's Huffman tables: prefix-free, complete" test_huffman_tables;
      t "the MDCT: overlapped blocks add back, aliases cancelled" test_tdac;
      t "the synthesis window" test_window;
      t "the stereo MP3: mid/side, short blocks, their windows" test_stereo_mp3_features;
      t "MP2, mono: ffmpeg's samples" (check_file "bell.mp2" 2 1);
      t "MP2, joint stereo: ffmpeg's samples" (check_file "stereo.mp2" 2 2);
      t "MP2, MPEG-2 at 24,000 Hz: ffmpeg's samples" (check_file ~rate:24000 "lsf.mp2" 2 2);
      t "MP3, mono: ffmpeg's samples" (check_file "bell.mp3" 3 1);
      t "MP3, joint stereo: ffmpeg's samples" (check_file "stereo.mp3" 3 2);
      t "MP3, MPEG-2 at 22,050 Hz: ffmpeg's samples" (check_file ~rate:22050 "lsf_mono.mp3" 3 1);
      t "MP3, MPEG-2 at 24,000 Hz, joint stereo: ffmpeg's samples" (check_file ~rate:24000 "lsf_stereo.mp3" 3 2);
    ]
