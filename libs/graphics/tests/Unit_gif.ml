(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Gif: GIFs of our own (gifs/, see make_gifs.py there), Lzw.mli's
 * worked example among them *)

let t = Testo.create

let read_file (file : string) : string =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let gif (name : string) : string = read_file (Filename.concat "gifs" (name ^ ".gif"))

let pixel (img : Rgba_image.t) (x : int) (y : int) : int * int * int * int =
  let o = ((y * img.width) + x) * 4 in
  (img.rgba.{o}, img.rgba.{o + 1}, img.rgba.{o + 2}, img.rgba.{o + 3})

let rgba = Alcotest.(pair (pair int int) (pair int int))
let check_pixel msg expected img x y =
  let r, g, b, a = pixel img x y and er, eg, eb, ea = expected in
  Alcotest.check rgba msg ((er, eg), (eb, ea)) ((r, g), (b, a))

let test_lzw () =
  (* Lzw.mli's worked example (Unit_lzw.ml) as a file: 6 x 1, color 1
   * is red *)
  let img = Gif.decode (gif "lzw") in
  for x = 0 to 5 do
    check_pixel "red" (255, 0, 0, 255) img x 0
  done

(* Each frame's size, delay and CRC-32 of its pixels. The same as PIL
 * reads them (fully transparent pixels made 0, 0, 0, 0 as ours are),
 * except anim's last frame: see test_anim *)
let expected =
  [
    ("lzw", [ (6, 1, 0.1, 0x6C909A0B) ]);
    (* 256 random colors: the dictionary fills, and the encoder clears *)
    ("noise", [ (64, 64, 0.1, 0xD1135A31) ]);
    ("interlaced", [ (16, 21, 0.1, 0x12FD172A) ]);
    ( "anim",
      [ (8, 8, 0.1, 0xC751A842); (8, 8, 0.1, 0x17C2CD67); (8, 8, 0.1, 0x0373C228); (8, 8, 0.25, 0x426F3C8B) ] );
  ]

let crc (img : Rgba_image.t) : int =
  (* unsigned, as the table above writes it (natively, 63-bit ints) *)
  Int32.to_int (Crc32.string (String.init (Bigarray.Array1.dim img.rgba) (fun i -> Char.chr img.rgba.{i}))) land 0xFFFF_FFFF

let test_frames () =
  expected
  |> List.iter (fun (name, frames) ->
         let got =
           Gif.animation (gif name) |> List.map (fun ((img : Rgba_image.t), delay) -> (img.width, img.height, delay, crc img))
         in
         Alcotest.(check (list (pair (pair int int) (pair (float 1e-9) int))))
           name
           (List.map (fun (w, h, d, c) -> ((w, h), (d, c))) frames)
           (List.map (fun (w, h, d, c) -> ((w, h), (d, c))) got))

(* anim.gif, 8 x 8, colors (x + y) mod 4 of black, red, green, blue
 * (make_gifs.py), then three patches *)
let test_anim () =
  match List.map fst (Gif.animation (gif "anim")) with
  | [ f1; f2; f3; f4 ] ->
      check_pixel "frame 1, (1, 0): red" (255, 0, 0, 255) f1 1 0;
      (* a 3 x 3 patch at (2, 2), its own palette, index 0 transparent,
       * then disposal 3 *)
      check_pixel "frame 2, (2, 2): transparent, frame 1 shows through" (0, 0, 0, 255) f2 2 2;
      check_pixel "frame 2, (3, 2): the local palette's 1" (200, 100, 50, 255) f2 3 2;
      check_pixel "frame 2, (3, 3): the local palette's 2" (50, 100, 200, 255) f2 3 3;
      (* a 4 x 2 patch at (4, 5), then disposal 2 *)
      check_pixel "frame 3, (3, 2): put back as before frame 2" (255, 0, 0, 255) f3 3 2;
      check_pixel "frame 3, (4, 5): the patch's blue" (0, 0, 255, 255) f3 4 5;
      (* a 2 x 2 patch at (0, 0), index 2 transparent *)
      check_pixel "frame 4, (0, 1): the patch's blue" (0, 0, 255, 255) f4 0 1;
      check_pixel "frame 4, (1, 0): transparent, frame 1's red shows" (255, 0, 0, 255) f4 1 0;
      (* where PIL differs: it clears to the background color (index
       * 0, opaque black), as the spec says; browsers, and we, clear to
       * transparent *)
      check_pixel "frame 4, (4, 5): cleared by frame 3's disposal" (0, 0, 0, 0) f4 4 5
  | frames -> Alcotest.failf "%d frames, not 4" (List.length frames)

let test_corrupt () =
  List.iter
    (fun (msg, s) ->
      match Gif.decode s with
      | _ -> Alcotest.failf "%s: decoded" msg
      | exception Failure _ -> ())
    [ ("not a GIF", "PNG89a" ^ String.make 20 '\000');
      ("cut in the palette", String.sub (gif "lzw") 0 16);
      ("cut in the frame's header", String.sub (gif "lzw") 0 30);
      ("cut in the pixels", String.sub (gif "lzw") 0 38) ]

let tests =
  Testo.categorize "Gif"
    [
      t "LZW's worked example, as a file" test_lzw;
      t "our GIFs, the pixels PIL reads" test_frames;
      t "an animation: patches, transparency, disposals" test_anim;
      t "corrupt files, refused" test_corrupt;
    ]
