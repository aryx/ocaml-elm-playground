(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images/jpeg: the worked examples of Dct.mli and Jpeg.mli,
 * and JPEGs of our own (jpegs/, see make_jpegs.py there) compared with
 * the pixels libjpeg decodes them to *)

let t = Testo.create

let read_file (file : string) : string =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let jpeg (name : string) : string = read_file (Filename.concat "jpegs" (name ^ ".jpg"))

let test_zigzag () =
  (* Jpeg.mli's table: where each position of a block comes in the
   * file *)
  let order =
    [| 0; 1; 5; 6; 14; 15; 27; 28; 2; 4; 7; 13; 16; 26; 29; 42; 3; 8; 12; 17; 25; 30; 41; 43; 9; 11; 18; 24; 31;
       40; 44; 53; 10; 19; 23; 32; 39; 45; 52; 54; 20; 22; 33; 38; 46; 51; 55; 60; 21; 34; 37; 47; 50; 56; 59;
       61; 35; 36; 48; 49; 57; 58; 62; 63 |]
  in
  Array.iteri (fun k pos -> Alcotest.(check int) (Printf.sprintf "coefficient %d" k) k order.(pos)) Jpeg.zigzag;
  Alcotest.(check int) "the 3rd coefficient starts the 2nd row" 8 Jpeg.zigzag.(2)

let test_extend () =
  Alcotest.(check (list int)) "size 2: 00 01 10 11" [ -3; -2; 2; 3 ] (List.map (fun v -> Jpeg.extend v 2) [ 0; 1; 2; 3 ]);
  Alcotest.(check (list int)) "size 1: 0 1" [ -1; 1 ] (List.map (fun v -> Jpeg.extend v 1) [ 0; 1 ]);
  Alcotest.(check int) "size 0" 0 (Jpeg.extend 0 0)

let test_dct () =
  (* the worked example: F(0, 0) = 80 alone, a flat block of 10 *)
  let dc = Array.make 64 0. in
  dc.(0) <- 80.;
  List.iter
    (fun (name, idct) ->
      Array.iteri
        (fun i x -> if abs_float (x -. 10.) > 1e-6 then Alcotest.failf "%s: sample %d is %g, not 10" name i x)
        (idct dc))
    [ ("the formula", Dct.idct); ("AAN", Dct.idct_aan) ];
  (* the two inverses agree, and the forward one undoes them *)
  Random.init 1;
  for _ = 1 to 100 do
    let f = Array.init 64 (fun _ -> Random.float 400. -. 200.) in
    let a = Dct.idct f and b = Dct.idct_aan f and back = Dct.fdct (Dct.idct f) in
    for i = 0 to 63 do
      if abs_float (a.(i) -. b.(i)) > 1e-5 then Alcotest.failf "AAN differs from the formula: %g, %g" a.(i) b.(i);
      if abs_float (back.(i) -. f.(i)) > 1e-9 then Alcotest.fail "fdct doesn't undo idct"
    done
  done

(* the largest difference, per channel, between [name]'s pixels and
 * libjpeg's *)
let max_difference ?idct ?upsampling (name : string) : int =
  let ours = Jpeg.decode ?idct ?upsampling (jpeg name) in
  let theirs = Png.decode (read_file (Filename.concat "jpegs" (name ^ ".expected.png"))) in
  Alcotest.(check (pair int int)) (name ^ ": size") (theirs.width, theirs.height) (ours.width, ours.height);
  let worst = ref 0 in
  for i = 0 to Bigarray.Array1.dim ours.rgba - 1 do
    worst := max !worst (abs (ours.rgba.{i} - theirs.rgba.{i}))
  done;
  !worst

(* Decoders may differ by a level or two: the inverse transform's
 * rounding (libjpeg's is in integers), the upsampling's *)
let test_libjpeg () =
  [ "q75_444"; "q75_422"; "q75_420"; "gray"; "restart" ]
  |> List.iter (fun name ->
         let d = max_difference name in
         if d > 2 then Alcotest.failf "%s: %d levels from libjpeg" name d;
         let d = max_difference ~idct:Dct.idct name in
         if d > 2 then Alcotest.failf "%s, the formula's IDCT: %d levels from libjpeg" name d)

(* Box upsampling repeats each color sample: where color is at half
 * size, it shows on the edges (tens of levels); libjpeg, and `Triangle,
 * weigh the two nearest samples. Where color is at full size (4:4:4),
 * nothing to upsample, the same pixels. *)
let test_upsampling () =
  let box = max_difference ~upsampling:`Box "q75_444" in
  if box > 2 then Alcotest.failf "4:4:4, box: %d levels from libjpeg" box;
  let box = max_difference ~upsampling:`Box "q75_420" in
  if box < 20 then Alcotest.failf "4:2:0, box: only %d levels from libjpeg" box

let test_refused () =
  List.iter
    (fun (msg, s) ->
      match Jpeg.decode s with
      | _ -> Alcotest.failf "%s: decoded" msg
      | exception Failure m -> print_endline (msg ^ ": " ^ m))
    [ ("progressive", jpeg "progressive");
      ("CMYK", jpeg "cmyk");
      ("not a JPEG", "\x89PNG\r\n\x1a\n");
      ("cut in the header", String.sub (jpeg "q75_420") 0 100) ]

let tests =
  Testo.categorize "Jpeg"
    [
      t "the zigzag" test_zigzag;
      t "extend: the value of s bits" test_extend;
      t "the DCT: the worked example, the formula and AAN" test_dct;
      t "our JPEGs, the pixels libjpeg decodes" test_libjpeg;
      t "box and triangle upsampling" test_upsampling;
      t "progressive, CMYK, corrupt: refused" test_refused;
    ]
