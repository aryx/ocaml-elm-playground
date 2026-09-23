(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The image codecs under node (see dune): each check prints a line,
 * and a failure makes the exit code 1. The expected values are the
 * native tests' (Unit_deflate, Unit_png, Unit_gif, Unit_jpeg). *)

let failures = ref 0

let check (what : string) (ok : bool) : unit =
  Printf.printf "%s %s\n" (if ok then "ok  " else "FAIL") what;
  if not ok then incr failures

let read (file : string) : string =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let pixels_crc (img : Rgba_image.t) : int32 =
  Crc32.string (String.init (Bigarray.Array1.dim img.rgba) (fun i -> Char.chr img.rgba.{i}))

let () =
  Printf.printf "Sys.int_size = %d\n" Sys.int_size;
  (* the checksums' worked examples *)
  check "CRC-32 (123456789)" (Crc32.string "123456789" = 0xCBF43926l);
  check "Adler-32 (Wikipedia)" (Adler32.string "Wikipedia" = 0x11E60398l);
  (* DEFLATE: the tutorial's 6 bytes, and a round trip *)
  check "Deflate: abcabcabcabc" (Deflate.deflate "abcabcabcabc" = "\x4B\x4C\x4A\x86\x23\x00");
  let s = String.init 100_000 (fun i -> Char.chr (((i * 7) + (i / 1000)) land 255)) in
  check "Zlib: compressed and back" (Zlib.decompress (Zlib.compress s) = s);
  (* PNG: PngSuite, the CRC-32 of the pixels pypng reads *)
  [ ("basn2c08", 0x2FB54036l); ("basi0g16", 0x8B47D810l); ("basn6a16", 0x285BE560l);
    ("tbrn2c08", 0x0370EF89l); ("s35i3p04", 0xB8C8407Dl); ("z09n2c08", 0x67290C15l) ]
  |> List.iter (fun (name, crc) ->
         let img = Png.decode (read (Printf.sprintf "../pngsuite/%s.png" name)) in
         check ("PNG " ^ name) (pixels_crc img = crc);
         check ("PNG " ^ name ^ ", written and read back") ((Png.decode (Png.encode img)).rgba = img.rgba));
  (* GIF: the animation's four pictures *)
  let crcs = List.map (fun (img, _) -> pixels_crc img) (Gif.animation (read "../gifs/anim.gif")) in
  check "GIF anim.gif" (crcs = [ 0xC751A842l; 0x17C2CD67l; 0x0373C228l; 0x426F3C8Bl ]);
  (* JPEG: within 2 levels of libjpeg *)
  [ "q75_420"; "restart"; "gray" ]
  |> List.iter (fun name ->
         let ours = Jpeg.decode (read (Printf.sprintf "../jpegs/%s.jpg" name)) in
         let theirs = Png.decode (read (Printf.sprintf "../jpegs/%s.expected.png" name)) in
         let worst = ref 0 in
         for i = 0 to Bigarray.Array1.dim ours.rgba - 1 do
           worst := max !worst (abs (ours.rgba.{i} - theirs.rgba.{i}))
         done;
         check (Printf.sprintf "JPEG %s (%d levels from libjpeg)" name !worst) (!worst <= 2));
  if !failures > 0 then begin
    Printf.printf "%d failures\n" !failures;
    exit 1
  end
