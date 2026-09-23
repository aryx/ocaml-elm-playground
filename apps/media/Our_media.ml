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
    ("tiny_soundtracker.mod", Mod.to_string Our_songs.soundtracker_song);
    ("bell.wav", Wav.to_string bell);
    ("demo_picture.png", Our_pictures.demo_picture_png);
    ("demo_picture.gif", Our_pictures.demo_picture_gif);
    ("bouncing_ball.gif", bouncing_ball_gif);
    ("demo_picture.jpg", Our_pictures.demo_picture_jpg);
    ("mario_stand.xpm", Our_pictures.mario_stand_xpm);
  ]
