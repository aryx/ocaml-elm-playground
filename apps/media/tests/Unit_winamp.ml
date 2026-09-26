(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* TinyWinamp's machinery: Id3's tags read and written, Graphic_eq's
 * bands heard *)

let t = Testo.create
let bytes name = Lazy.force (List.assoc name Our_media.playlist)

(*****************************************************************************)
(* Id3 *)
(*****************************************************************************)

let tag : Id3.t = { title = "Bell and Chirps"; artist = "Our Synth"; album = "Tiny Hits"; year = "2026"; track = Some 1 }

let test_v1 () =
  let mp3 = bytes "lame_encoded.mp3" in
  let tagged = mp3 ^ Id3.v1_to_string tag in
  Alcotest.(check int) "128 bytes" 128 (String.length (Id3.v1_to_string tag));
  Alcotest.(check bool) "read back" true (Id3.v1 tagged = Some tag);
  Alcotest.(check bool) "none before" true (Id3.v1 mp3 = None);
  (* thirty bytes a field: the rest cut *)
  let long = "The Rise and Fall of Ziggy Stardust and the Spiders from Mars" in
  (match Id3.v1 (Id3.v1_to_string { tag with album = long }) with
  | Some t -> Alcotest.(check string) "cut" "The Rise and Fall of Ziggy St" (String.sub t.album 0 29)
  | None -> Alcotest.fail "no tag");
  (* the decoder skips the tag at the end as junk: the same sound *)
  match (Mpeg_audio.decode mp3, Mpeg_audio.decode tagged) with
  | Ok (_, a), Ok (_, b) -> Alcotest.(check bool) "the same samples" true (a.left = b.left && a.right = b.right)
  | _ -> Alcotest.fail "not decoded"

(* a v2 frame: [size] how the version writes it *)
let frame ~(syncsafe : bool) (id : string) (data : string) : string =
  let n = String.length data in
  let size =
    if syncsafe then String.init 4 (fun i -> Char.chr ((n lsr (7 * (3 - i))) land 0x7F))
    else String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 0xFF))
  in
  id ^ size ^ "\000\000" ^ data

let tag_v2 ~(major : int) (frames : string list) : string =
  let body = String.concat "" frames ^ String.make 10 '\000' (* padding *) in
  let n = String.length body in
  "ID3" ^ String.make 1 (Char.chr major) ^ "\000\000" ^ String.init 4 (fun i -> Char.chr ((n lsr (7 * (3 - i))) land 0x7F)) ^ body

let test_v2 () =
  (* ffmpeg's tag in our MP3: only its encoder *)
  (match Id3.v2_frames (bytes "lame_encoded.mp3") with
  | [ ("TSSE", encoder) ] -> Alcotest.(check string) "ffmpeg's" "Lavf" (String.sub encoder 0 4)
  | _ -> Alcotest.fail "not ffmpeg's one frame");
  (* v2.3: Latin-1 and UTF-16 after a byte order mark, both to UTF-8 *)
  let v23 =
    tag_v2 ~major:3
      [ frame ~syncsafe:false "TIT2" "\000Caf\233"; frame ~syncsafe:false "TPE1" "\001\255\254O\000u\000i\000"; frame ~syncsafe:false "TRCK" "\0003/12" ]
  in
  (match Id3.read v23 with
  | Some t ->
      Alcotest.(check string) "Latin-1" "Caf\xC3\xA9" t.title;
      Alcotest.(check string) "UTF-16" "Oui" t.artist;
      Alcotest.(check (option int)) "3 of 12" (Some 3) t.track
  | None -> Alcotest.fail "no tag");
  (* v2.4: a frame's size syncsafe -- 200 bytes are 01 48 there, 00 C8
   * in v2.3 *)
  let title = String.make 199 'a' in
  match Id3.v2_frames (tag_v2 ~major:4 [ frame ~syncsafe:true "TIT2" ("\003" ^ title); frame ~syncsafe:true "TPE1" "\003b" ]) with
  | [ ("TIT2", t); ("TPE1", "b") ] -> Alcotest.(check int) "the whole title" 199 (String.length t)
  | _ -> Alcotest.fail "v2.4's frames"

let test_read () =
  (* v2 has no title, v1 has: v1's *)
  let both = bytes "lame_encoded.mp3" ^ Id3.v1_to_string tag in
  Alcotest.(check string) "v1's title, under v2" "Our Synth - Bell and Chirps" (Id3.display ~name:"x.mp3" (Id3.read both));
  Alcotest.(check string) "the title alone" "Oui" (Id3.display ~name:"x.mp3" (Some { Id3.empty with title = "Oui" }));
  Alcotest.(check string) "no tag: the name" "bell" (Id3.display ~name:"dir/bell.wav" None)

(*****************************************************************************)
(* Graphic_eq *)
(*****************************************************************************)

(* a sine at [f] Hz, 1 s; its level in dB once through, the last half
 * (the filters settled) *)
let level ~(preamp : float) ~(gains : float array) (f : float) : float =
  let n = Signal.rate in
  let x = Array.init n (fun i -> sin (2. *. Float.pi *. f *. float_of_int i /. float_of_int Signal.rate)) in
  let s : Signal.stereo = { left = Array.copy x; right = Array.copy x } in
  Graphic_eq.process (Graphic_eq.create ()) ~preamp ~gains s;
  let peak = ref 0. in
  for i = n / 2 to n - 1 do
    peak := Float.max !peak (Float.abs s.left.(i))
  done;
  20. *. log10 !peak

let flat = Array.make 10 0.

let test_eq () =
  Alcotest.(check (float 1e-3)) "flat: nothing" 0. (level ~preamp:0. ~gains:flat 440.);
  Alcotest.(check (float 1e-3)) "the preamp alone" (-6.) (level ~preamp:(-6.) ~gains:flat 440.);
  (* a band up 12 dB, heard at its frequency, and (almost) not two
   * octaves away *)
  let k = 4 (* 1 kHz *) in
  let gains = Array.mapi (fun j _ -> if j = k then 12. else 0.) flat in
  Alcotest.(check (float 0.05)) "1 kHz up 12 dB" 12. (level ~preamp:0. ~gains 1000.);
  if level ~preamp:0. ~gains 250. > 1. then Alcotest.fail "250 Hz raised";
  (* the curve drawn is what is heard *)
  List.iter
    (fun f -> Alcotest.(check (float 0.1)) (Printf.sprintf "the curve at %g Hz" f) (level ~preamp:(-3.) ~gains f) (Graphic_eq.response ~preamp:(-3.) ~gains f))
    [ 100.; 700.; 1000.; 2000. ];
  Alcotest.(check string) "Flat first" "Flat" (fst (List.hd Graphic_eq.presets));
  List.iter (fun (name, g) -> Alcotest.(check int) name 10 (Array.length g)) Graphic_eq.presets

let tests =
  Testo.categorize "Winamp"
    [
      t "ID3v1: written, read, skipped by the decoder" test_v1;
      t "ID3v2: frames, encodings, v2.3 and v2.4's sizes" test_v2;
      t "the two tags together, and what the playlist shows" test_read;
      t "the graphic EQ: flat, a band, the curve" test_eq;
    ]
