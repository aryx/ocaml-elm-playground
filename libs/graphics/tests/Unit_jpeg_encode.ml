(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Jpeg_encode: the standard's tables and the quality scaling, the
 * encoder against libjpeg on the same picture (jpegs/make_jpegs.py),
 * and the quality's price in bytes and in dB *)

let t = Testo.create
let read_file (file : string) : string = In_channel.with_open_bin file In_channel.input_all
let source () : Rgba_image.t = Png.decode (read_file (Filename.concat "jpegs" "source.png"))

let test_tables () =
  let first quality = (Jpeg_encode.scaled ~quality Jpeg_encode.luminance_table).(0) in
  Alcotest.(check (list int)) "16 at quality 50, 75, 25" [ 16; 8; 32 ] [ first 50; first 75; first 25 ];
  Alcotest.(check bool) "quality 100: every entry 1" true (Array.for_all (( = ) 1) (Jpeg_encode.scaled ~quality:100 Jpeg_encode.chrominance_table));
  Alcotest.(check bool) "quality 1: every entry within a byte" true (Array.for_all (fun v -> v >= 1 && v <= 255) (Jpeg_encode.scaled ~quality:1 Jpeg_encode.luminance_table))

let test_libjpeg () =
  (* the same picture, quality and subsampling as libjpeg's fixtures:
   * the same loss (the same tables, the same scaling), within a few
   * bytes (libjpeg's header is a little longer) *)
  let src = source () in
  List.iter
    (fun (sub, name) ->
      let ours = Jpeg_encode.encode ~quality:75 ~subsampling:sub src and theirs = read_file (Filename.concat "jpegs" (name ^ ".jpg")) in
      let db s = Psnr.psnr src (Jpeg.decode s) in
      if abs (String.length ours - String.length theirs) * 20 > String.length theirs then
        Alcotest.failf "%s: %d bytes, libjpeg's %d" name (String.length ours) (String.length theirs);
      if Float.abs (db ours -. db theirs) > 0.2 then Alcotest.failf "%s: %.2f dB, libjpeg's %.2f" name (db ours) (db theirs))
    [ (`S420, "q75_420"); (`S444, "q75_444") ]

let test_quality () =
  (* more quality, more bytes, less loss; and a flat picture, whose
   * blocks are all DC, comes back exactly *)
  let src = source () in
  let at q = let s = Jpeg_encode.encode ~quality:q ~subsampling:`S444 src in (String.length s, Psnr.psnr src (Jpeg.decode s)) in
  let (s25, d25), (s75, d75), (s95, d95) = (at 25, at 75, at 95) in
  if not (s25 < s75 && s75 < s95) then Alcotest.failf "sizes %d %d %d" s25 s75 s95;
  if not (d25 < d75 && d75 < d95) then Alcotest.failf "dB %.1f %.1f %.1f" d25 d75 d95;
  let flat = Rgba_image.create ~width:19 ~height:13 in
  for i = 0 to (19 * 13) - 1 do flat.rgba.{4 * i} <- 100; flat.rgba.{(4 * i) + 1} <- 100; flat.rgba.{(4 * i) + 2} <- 100; flat.rgba.{(4 * i) + 3} <- 255 done;
  Alcotest.(check (float 0.)) "a flat gray, exactly" infinity (Psnr.psnr flat (Jpeg.decode (Jpeg_encode.encode flat)));
  (* noise at quality 100: the largest values, the rarest codes, every
   * table's corners -- and still not exact (the DCT's rounding) *)
  let noise = Rgba_image.create ~width:32 ~height:32 in
  let seed = ref 1 in
  for i = 0 to (4 * 32 * 32) - 1 do
    seed := ((!seed * 1103515245) + 12345) land 0x7FFFFFFF;
    noise.rgba.{i} <- (if i mod 4 = 3 then 255 else (!seed lsr 16) land 0xFF)
  done;
  let db = Psnr.psnr noise (Jpeg.decode (Jpeg_encode.encode ~quality:100 ~subsampling:`S444 noise)) in
  if db < 40. || db = infinity then Alcotest.failf "noise at quality 100: %.1f dB" db

let tests =
  Testo.categorize "Jpeg_encode"
    [ t "the tables, and quality's scaling" test_tables; t "as libjpeg does, on the same picture" test_libjpeg; t "quality: bytes against dB" test_quality ]
