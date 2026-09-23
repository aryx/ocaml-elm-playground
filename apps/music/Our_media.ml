(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Our_media.mli *)

(*****************************************************************************)
(* The tracker's song *)
(*****************************************************************************)

let instrument name (a : float array) ~loop : Mod.instrument =
  let data = Mod.data_of_floats a in
  { name; finetune = 0; volume = 64; loop_start = 0; loop_length = (if loop then String.length data else 0); data }

let cell ?(i = 1) ?(e = 0) ?(x = 0) (n : string) : Mod.cell =
  { instrument = i; period = Option.value (Mod.period_of_name n) ~default:0; effect = e; param = x }

let soundtracker_song : Mod.song =
  let pi2 = 2. *. Float.pi in
  let lead = instrument "lead" (Array.init 32 (fun i -> if i < 8 then 0.6 else -0.2)) ~loop:true in
  let bass =
    instrument "bass" (Array.init 64 (fun i -> let u = float_of_int i /. 64. in 0.8 *. if u < 0.5 then (4. *. u) -. 1. else 3. -. (4. *. u))) ~loop:true
  in
  let kick =
    instrument "kick"
      (Array.init 2400 (fun i ->
           let t = float_of_int i /. 8287. in
           0.9 *. exp (-.t *. 18.) *. sin (pi2 *. ((60. *. t) +. (140. *. (1. -. exp (-.t *. 30.)) /. 30.)))))
      ~loop:false
  in
  let seed = ref 7 in
  let snare =
    instrument "snare"
      (Array.init 2000 (fun i ->
           seed := ((!seed * 1103515245) + 12345) land 0x7FFFFFFF;
           0.7 *. exp (-.float_of_int i /. 350.) *. ((float_of_int ((!seed lsr 8) land 0xFF) /. 128.) -. 1.)))
      ~loop:false
  in
  let drums = List.concat_map (fun r -> [ (r, 2, cell ~i:3 "C-2"); (r + 4, 3, cell ~i:4 ~e:0xC ~x:0x30 "C-2") ]) [ 0; 8; 16; 24; 32; 40; 48; 56 ] in
  let bass_line = List.mapi (fun k n -> (k * 8, 1, cell ~i:2 n)) [ "A-1"; "A-1"; "F-1"; "F-1"; "C-2"; "C-2"; "G-1"; "G-1" ] in
  let melody notes = List.mapi (fun k n -> (k * 4, 0, cell ~i:1 n)) notes in
  let first = melody [ "A-2"; "C-3"; "E-3"; "A-3"; "G-3"; "E-3"; "C-3"; "D-3"; "F-2"; "A-2"; "C-3"; "F-3"; "E-3"; "C-3"; "B-2"; "G-2" ] in
  (* the second time, chords: the arpeggio's minor and major thirds *)
  let second =
    List.mapi (fun k (n, x) -> (k * 8, 0, cell ~i:1 ~e:0 ~x n)) [ ("A-2", 0x37); ("A-2", 0x37); ("F-2", 0x47); ("F-2", 0x47); ("C-3", 0x47); ("C-3", 0x47); ("G-2", 0x47); ("G-2", 0x47) ]
  in
  let pattern cells =
    Array.init 64 (fun r -> Array.init 4 (fun c -> match List.find_opt (fun (r', c', _) -> r = r' && c = c') cells with Some (_, _, x) -> x | None -> Mod.empty_cell))
  in
  let blank = instrument "" [||] ~loop:false in
  {
    title = "tiny soundtracker";
    instruments = Array.init 31 (fun k -> match k with 0 -> lead | 1 -> bass | 2 -> kick | 3 -> snare | _ -> blank);
    restart = 127;
    positions = [| 0; 1 |];
    patterns = [| pattern (first @ bass_line @ drums); pattern (second @ bass_line @ drums) |];
    tag = "M.K.";
  }

(*****************************************************************************)
(* The playlist *)
(*****************************************************************************)

let frere_jacques =
  {|X:1
T:Frere Jacques (a round, traditional)
L:1/8
Q:1/4=120
K:C
V:1
C2 D2 E2 C2 | C2 D2 E2 C2 | E2 F2 G4 | E2 F2 G4 |
GA GF E2 C2 | GA GF E2 C2 | C2 G,2 C4 | C2 G,2 C4 |
V:2
z8 | z8 | C2 D2 E2 C2 | C2 D2 E2 C2 | E2 F2 G4 | E2 F2 G4 |
GA GF E2 C2 | GA GF E2 C2 |
|}

let au_clair_de_la_lune = "do do do re mi:2 re:2 | do mi re re do:4 | do mi re re do:4"

(* An animated GIF of our own: a ball bouncing across 16 x 16 frames.
 * There is no GIF writer in graphics/images, and a real one would
 * compress (LZW builds its dictionary as it goes: Lzw.mli). This one
 * writes the LZW codes without compressing: each pixel its own code,
 * and a clear code every two pixels, before the dictionary would grow
 * past 3-bit codes -- a valid GIF89a any reader decodes, at 9 bits for
 * 2 pixels, the price of simplicity. *)
let bouncing_ball_gif : string =
  let b = Buffer.create 2048 in
  let u8 v = Buffer.add_char b (Char.chr (v land 0xFF)) and u16 v = Buffer.add_uint16_le b v in
  let w = 16 and h = 16 in
  Buffer.add_string b "GIF89a";
  u16 w;
  u16 h;
  (* a global table of 4 colors (2^(1+1)), 2 bits of color resolution *)
  u8 0x91;
  u8 0;
  u8 0;
  List.iter (fun (r, g, bl) -> u8 r; u8 g; u8 bl) [ (30, 30, 60); (120, 170, 240); (250, 210, 60); (240, 240, 240) ];
  (* loop forever: Netscape's extension *)
  Buffer.add_string b "\x21\xFF\x0BNETSCAPE2.0\x03\x01\x00\x00\x00";
  let frame k =
    (* the ball's center, bouncing: x across, y a parabola *)
    let cx = 3 + (k * 2) and cy = 12 - ((k * (5 - k)) / 1) in
    let pixel x y = if ((x - cx) * (x - cx)) + ((y - cy) * (y - cy)) <= 5 then 2 else if y >= 14 then 3 else 1 in
    (* the graphic control: 15 hundredths of a second *)
    Buffer.add_string b "\x21\xF9\x04\x04";
    u16 15;
    u8 0;
    u8 0;
    (* the image, the whole screen *)
    u8 0x2C;
    u16 0;
    u16 0;
    u16 w;
    u16 h;
    u8 0;
    (* the codes, 3 bits each, least significant first: clear (4), two
     * pixels, clear, two pixels, ..., end (5) *)
    u8 2;
    let bits = ref 0 and nbits = ref 0 and data = Buffer.create 512 in
    let code c =
      bits := !bits lor (c lsl !nbits);
      nbits := !nbits + 3;
      while !nbits >= 8 do
        Buffer.add_char data (Char.chr (!bits land 0xFF));
        bits := !bits lsr 8;
        nbits := !nbits - 8
      done
    in
    for i = 0 to (w * h) - 1 do
      if i mod 2 = 0 then code 4;
      code (pixel (i mod w) (i / w))
    done;
    code 5;
    if !nbits > 0 then Buffer.add_char data (Char.chr (!bits land 0xFF));
    (* in sub-blocks of at most 255 bytes, then an empty one *)
    let d = Buffer.contents data in
    let rec blocks at =
      if at < String.length d then (
        let n = min 255 (String.length d - at) in
        u8 n;
        Buffer.add_string b (String.sub d at n);
        blocks (at + n))
    in
    blocks 0;
    u8 0
  in
  List.iter frame [ 0; 1; 2; 3; 4; 5 ];
  u8 0x3B;
  Buffer.contents b

let playlist : (string * string) list =
  let midi = match Abc.parse frere_jacques with Ok tune -> Midi.of_tune tune | Error e -> failwith e in
  let bell = Synth.render (Synth.voice (Fm { ratio = 1.4; index = 5. }) 440. |> Synth.lasting 2. |> Synth.fading) in
  [
    ("frere_jacques.abc", frere_jacques);
    ("frere_jacques.mid", midi);
    ("la_lune.doremi", au_clair_de_la_lune);
    ("tiny_soundtracker.mod", Mod.to_string soundtracker_song);
    ("bell.wav", Wav.to_string bell);
    ("demo_picture.png", Our_pictures.demo_picture_png);
    ("demo_picture.gif", Our_pictures.demo_picture_gif);
    ("bouncing_ball.gif", bouncing_ball_gif);
    ("demo_picture.jpg", Our_pictures.demo_picture_jpg);
    ("mario_stand.xpm", Our_pictures.mario_stand_xpm);
  ]
