(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images/png: Png.mli's worked example, PngSuite (Willem van
 * Schaik's test pictures, a subset in pngsuite/, see its LICENSE), and
 * the repository's own PNGs *)

let t = Testo.create

let read_file (file : string) : string =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let suite (name : string) : string = Filename.concat "pngsuite" (name ^ ".png")

(* the CRC-32 of [s] as an unsigned int, as the tables below write it
 * (the tests run natively, where an int has 63 bits) *)
let crc32 (s : string) : int = Int32.to_int (Crc32.string s) land 0xFFFF_FFFF

let test_paeth () =
  Alcotest.(check int) "a 100, b 120, c 90: b" 120 (Png.paeth 100 120 90);
  Alcotest.(check int) "a flat area: a" 7 (Png.paeth 7 7 7);
  Alcotest.(check int) "a vertical edge: a" 200 (Png.paeth 200 10 10)

let test_chunks () =
  let types = List.map fst (Png.chunks (read_file (suite "basn0g01"))) in
  Alcotest.(check (list string)) "basn0g01's chunks" [ "IHDR"; "gAMA"; "IDAT"; "IEND" ] types;
  (* oi9: the pixels cut into 1-byte IDATs *)
  let idats = Png.chunks (read_file (suite "oi9n2c16")) |> List.filter (fun (typ, _) -> typ = "IDAT") in
  if List.length idats < 100 then Alcotest.failf "oi9n2c16: only %d IDATs" (List.length idats)

(* Every PngSuite file but the corrupt ones: its size, and the CRC-32 of
 * its RGBA pixels as pypng (a PNG decoder in Python) reads them,
 * converted by Png.mli's rules (16 bits: the high byte; 1, 2, 4 bits:
 * scaled to 0..255; the tRNS color: alpha 0). Not stb_image's pixels:
 * the one bundled with the OCaml binding gets 9 of these files wrong --
 * the interlaced 16-bit ones (basi0g16's pixel (2, 0), 0x1200, is 0
 * there, not 18), and a tRNS color on a gray or RGB picture, left
 * opaque (tbbn0g04, tbrn2c08, ...). An interlaced file and its
 * non-interlaced twin have the same pixels, so the same CRC, and so do
 * the IDAT splits (the oi files) and the compression levels (z00 to
 * z09). *)
let pngsuite =
  [
    ("basi0g01", 32, 32, 0x0DA28714);
    ("basi0g02", 32, 32, 0x2E3FE285);
    ("basi0g04", 32, 32, 0x8D0F641B);
    ("basi0g08", 32, 32, 0xC395683C);
    ("basi0g16", 32, 32, 0x8B47D810);
    ("basi2c08", 32, 32, 0x2FB54036);
    ("basi2c16", 32, 32, 0xF3BB75E6);
    ("basi3p01", 32, 32, 0x4D8431A4);
    ("basi3p02", 32, 32, 0xE4DBB6BC);
    ("basi3p04", 32, 32, 0x671F880F);
    ("basi3p08", 32, 32, 0x39528682);
    ("basi4a08", 32, 32, 0x905D5B60);
    ("basi4a16", 32, 32, 0x9C7C3556);
    ("basi6a08", 32, 32, 0xA74DF32C);
    ("basi6a16", 32, 32, 0x285BE560);
    ("basn0g01", 32, 32, 0x0DA28714);
    ("basn0g02", 32, 32, 0x2E3FE285);
    ("basn0g04", 32, 32, 0x8D0F641B);
    ("basn0g08", 32, 32, 0xC395683C);
    ("basn0g16", 32, 32, 0x8B47D810);
    ("basn2c08", 32, 32, 0x2FB54036);
    ("basn2c16", 32, 32, 0xF3BB75E6);
    ("basn3p01", 32, 32, 0x4D8431A4);
    ("basn3p02", 32, 32, 0xE4DBB6BC);
    ("basn3p04", 32, 32, 0x671F880F);
    ("basn3p08", 32, 32, 0x39528682);
    ("basn4a08", 32, 32, 0x905D5B60);
    ("basn4a16", 32, 32, 0x9C7C3556);
    ("basn6a08", 32, 32, 0xA74DF32C);
    ("basn6a16", 32, 32, 0x285BE560);
    ("f00n0g08", 32, 32, 0x0B907DEC);
    ("f00n2c08", 32, 32, 0x9AD4B08B);
    ("f01n0g08", 32, 32, 0x2119C97F);
    ("f01n2c08", 32, 32, 0xE31D06F2);
    ("f02n0g08", 32, 32, 0xC03634D7);
    ("f02n2c08", 32, 32, 0xBA0D4B27);
    ("f03n0g08", 32, 32, 0x3A9C7B91);
    ("f03n2c08", 32, 32, 0x6D296175);
    ("f04n0g08", 32, 32, 0x28FCA0B1);
    ("f04n2c08", 32, 32, 0xC5C4BAAD);
    ("oi1n0g16", 32, 32, 0x8B47D810);
    ("oi1n2c16", 32, 32, 0xF3BB75E6);
    ("oi2n0g16", 32, 32, 0x8B47D810);
    ("oi2n2c16", 32, 32, 0xF3BB75E6);
    ("oi4n0g16", 32, 32, 0x8B47D810);
    ("oi4n2c16", 32, 32, 0xF3BB75E6);
    ("oi9n0g16", 32, 32, 0x8B47D810);
    ("oi9n2c16", 32, 32, 0xF3BB75E6);
    ("s01i3p01", 1, 1, 0x9F62CDE3);
    ("s01n3p01", 1, 1, 0x9F62CDE3);
    ("s02i3p01", 2, 2, 0xFC958EBF);
    ("s02n3p01", 2, 2, 0xFC958EBF);
    ("s03i3p01", 3, 3, 0xF53615D1);
    ("s03n3p01", 3, 3, 0xF53615D1);
    ("s04i3p01", 4, 4, 0xCE2B2AA8);
    ("s04n3p01", 4, 4, 0xCE2B2AA8);
    ("s05i3p02", 5, 5, 0x71F99A5F);
    ("s05n3p02", 5, 5, 0x71F99A5F);
    ("s06i3p02", 6, 6, 0x1707AE6E);
    ("s06n3p02", 6, 6, 0x1707AE6E);
    ("s07i3p02", 7, 7, 0xF3A27B20);
    ("s07n3p02", 7, 7, 0xF3A27B20);
    ("s08i3p02", 8, 8, 0x2EB65A34);
    ("s08n3p02", 8, 8, 0x2EB65A34);
    ("s09i3p02", 9, 9, 0x44D29BB4);
    ("s09n3p02", 9, 9, 0x44D29BB4);
    ("s32i3p04", 32, 32, 0x9410D2A5);
    ("s32n3p04", 32, 32, 0x9410D2A5);
    ("s33i3p04", 33, 33, 0xD001D86B);
    ("s33n3p04", 33, 33, 0xD001D86B);
    ("s34i3p04", 34, 34, 0x17CFE1AD);
    ("s34n3p04", 34, 34, 0x17CFE1AD);
    ("s35i3p04", 35, 35, 0xB8C8407D);
    ("s35n3p04", 35, 35, 0xB8C8407D);
    ("s36i3p04", 36, 36, 0xD5AEC69B);
    ("s36n3p04", 36, 36, 0xD5AEC69B);
    ("s37i3p04", 37, 37, 0xA1563224);
    ("s37n3p04", 37, 37, 0xA1563224);
    ("s38i3p04", 38, 38, 0xBDAF2E8A);
    ("s38n3p04", 38, 38, 0xBDAF2E8A);
    ("s39i3p04", 39, 39, 0x5CB9F129);
    ("s39n3p04", 39, 39, 0x5CB9F129);
    ("s40i3p04", 40, 40, 0xBF29AFA5);
    ("s40n3p04", 40, 40, 0xBF29AFA5);
    ("tbbn0g04", 32, 32, 0x5C8EAF83);
    ("tbbn2c16", 32, 32, 0x0370EF89);
    ("tbbn3p08", 32, 32, 0x9D56CD67);
    ("tbgn2c16", 32, 32, 0x0370EF89);
    ("tbgn3p08", 32, 32, 0x9D56CD67);
    ("tbrn2c08", 32, 32, 0x0370EF89);
    ("tbwn0g16", 32, 32, 0xB24D0A34);
    ("tbwn3p08", 32, 32, 0x9D56CD67);
    ("tbyn3p08", 32, 32, 0x9D56CD67);
    ("tp0n0g08", 32, 32, 0x57965874);
    ("tp0n2c08", 32, 32, 0x679D24B4);
    ("tp0n3p08", 32, 32, 0x130AA165);
    ("tp1n3p08", 32, 32, 0x9D56CD67);
    ("z00n2c08", 32, 32, 0x67290C15);
    ("z03n2c08", 32, 32, 0x67290C15);
    ("z06n2c08", 32, 32, 0x67290C15);
    ("z09n2c08", 32, 32, 0x67290C15);
  ]

let test_pngsuite () =
  let wrong =
    pngsuite
    |> List.filter_map (fun (name, w, h, crc) ->
           let img = Png.decode (read_file (suite name)) in
           let pixels = String.init (Bigarray.Array1.dim img.rgba) (fun i -> Char.chr img.rgba.{i}) in
           let got = (img.width, img.height, crc32 pixels) in
           if got = (w, h, crc) then None
           else
             let gw, gh, gcrc = got in
             Some (Printf.sprintf "%s: %dx%d, CRC 0x%08X; expected %dx%d, 0x%08X" name gw gh gcrc w h crc))
  in
  if wrong <> [] then Alcotest.failf "%d files wrong:\n%s" (List.length wrong) (String.concat "\n" wrong)

(* PngSuite's corrupt files, all starting with an x: a bad signature
 * (xs1 to xs7: a byte changed; xcr, xlf: line endings converted), a
 * bad CRC (xcs, xhd), an unknown color type (xc1, xc9) or bit depth
 * (xd0, xd3, xd9), no IDAT (xdt) *)
let test_corrupt () =
  [ "xc1n0g08"; "xc9n2c08"; "xcrn0g04"; "xcsn0g01"; "xd0n2c08"; "xd3n2c08"; "xd9n2c08";
    "xdtn0g01"; "xhdn0g08"; "xlfn0g04"; "xs1n0g01"; "xs2n0g01"; "xs4n0g01"; "xs7n0g01" ]
  |> List.iter (fun name ->
         match Png.decode (read_file (suite name)) with
         | _ -> Alcotest.failf "%s: decoded, but it's corrupt" name
         | exception Failure msg -> print_endline (name ^ ": " ^ msg))

(* What the games draw: the repository's PNGs (see graphics/tests/dune's
 * deps), their size and the CRC-32 of their pixels -- the pixels
 * stb_image, which decoded them before Png, gave them too *)
let test_ours () =
  [ ("../../examples/checker.png", 64, 64, 0x268573CE);
    ("../../games/adventure/tomb.png", 128, 128, 0x60AE66E3);
    ("../../games/fps/minecraft.png", 256, 256, 0xF95C0E51) ]
  |> List.iter (fun (file, w, h, crc) ->
         let img = Png.decode (read_file file) in
         let pixels = String.init (Bigarray.Array1.dim img.rgba) (fun i -> Char.chr img.rgba.{i}) in
         Alcotest.(check (pair int int)) (file ^ ": size") (w, h) (img.width, img.height);
         Alcotest.(check int) (file ^ ": CRC-32 of the pixels") crc (crc32 pixels))

(* every PngSuite picture written and read back: the same pixels, and
 * without alpha the same colors, opaque *)
let test_encode () =
  pngsuite
  |> List.iter (fun (name, _, _, _) ->
         let img = Png.decode (read_file (suite name)) in
         let back = Png.decode (Png.encode img) in
         if back.rgba <> img.rgba then Alcotest.failf "%s: written and read back, not the same" name;
         let rgb = Png.decode (Png.encode ~alpha:false img) in
         for i = 0 to (img.width * img.height) - 1 do
           for k = 0 to 2 do
             if rgb.rgba.{(i * 4) + k} <> img.rgba.{(i * 4) + k} then Alcotest.failf "%s: RGB, pixel %d" name i
           done;
           if rgb.rgba.{(i * 4) + 3} <> 255 then Alcotest.failf "%s: RGB, pixel %d not opaque" name i
         done)

let tests =
  Testo.categorize "Png"
    [
      t "Paeth's predictor" test_paeth;
      t "chunks" test_chunks;
      t "PngSuite, the pixels pypng reads" test_pngsuite;
      t "PngSuite's corrupt files, refused" test_corrupt;
      t "our textures, the pixels the games draw" test_ours;
      t "written and read back" test_encode;
    ]
