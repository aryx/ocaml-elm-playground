(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jpeg_encode.mli *)

(*****************************************************************************)
(* The standard's tables (T.81 Annex K) *)
(*****************************************************************************)

let luminance_table =
  [| 16; 11; 10; 16; 24; 40; 51; 61;
     12; 12; 14; 19; 26; 58; 60; 55;
     14; 13; 16; 24; 40; 57; 69; 56;
     14; 17; 22; 29; 51; 87; 80; 62;
     18; 22; 37; 56; 68; 109; 103; 77;
     24; 35; 55; 64; 81; 104; 113; 92;
     49; 64; 78; 87; 103; 121; 120; 101;
     72; 92; 95; 98; 112; 100; 103; 99 |]

let chrominance_table =
  [| 17; 18; 24; 47; 99; 99; 99; 99;
     18; 21; 26; 66; 99; 99; 99; 99;
     24; 26; 56; 99; 99; 99; 99; 99;
     47; 66; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99;
     99; 99; 99; 99; 99; 99; 99; 99 |]

let scaled ~(quality : int) (table : int array) : int array =
  let q = max 1 (min 100 quality) in
  let scale = if q < 50 then 5000 / q else 200 - (2 * q) in
  Array.map (fun t -> max 1 (min 255 (((t * scale) + 50) / 100))) table

(* the Huffman tables: 16 counts (codes of 1 to 16 bits), the symbols *)
let dc_luminance = ([| 0; 1; 5; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0 |], Array.init 12 Fun.id)
let dc_chrominance = ([| 0; 3; 1; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0 |], Array.init 12 Fun.id)

let ac_luminance =
  ( [| 0; 2; 1; 3; 3; 2; 4; 3; 5; 5; 4; 4; 0; 0; 1; 0x7d |],
    [| 0x01; 0x02; 0x03; 0x00; 0x04; 0x11; 0x05; 0x12; 0x21; 0x31; 0x41; 0x06; 0x13; 0x51; 0x61; 0x07;
       0x22; 0x71; 0x14; 0x32; 0x81; 0x91; 0xa1; 0x08; 0x23; 0x42; 0xb1; 0xc1; 0x15; 0x52; 0xd1; 0xf0;
       0x24; 0x33; 0x62; 0x72; 0x82; 0x09; 0x0a; 0x16; 0x17; 0x18; 0x19; 0x1a; 0x25; 0x26; 0x27; 0x28;
       0x29; 0x2a; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39; 0x3a; 0x43; 0x44; 0x45; 0x46; 0x47; 0x48; 0x49;
       0x4a; 0x53; 0x54; 0x55; 0x56; 0x57; 0x58; 0x59; 0x5a; 0x63; 0x64; 0x65; 0x66; 0x67; 0x68; 0x69;
       0x6a; 0x73; 0x74; 0x75; 0x76; 0x77; 0x78; 0x79; 0x7a; 0x83; 0x84; 0x85; 0x86; 0x87; 0x88; 0x89;
       0x8a; 0x92; 0x93; 0x94; 0x95; 0x96; 0x97; 0x98; 0x99; 0x9a; 0xa2; 0xa3; 0xa4; 0xa5; 0xa6; 0xa7;
       0xa8; 0xa9; 0xaa; 0xb2; 0xb3; 0xb4; 0xb5; 0xb6; 0xb7; 0xb8; 0xb9; 0xba; 0xc2; 0xc3; 0xc4; 0xc5;
       0xc6; 0xc7; 0xc8; 0xc9; 0xca; 0xd2; 0xd3; 0xd4; 0xd5; 0xd6; 0xd7; 0xd8; 0xd9; 0xda; 0xe1; 0xe2;
       0xe3; 0xe4; 0xe5; 0xe6; 0xe7; 0xe8; 0xe9; 0xea; 0xf1; 0xf2; 0xf3; 0xf4; 0xf5; 0xf6; 0xf7; 0xf8;
       0xf9; 0xfa |] )

let ac_chrominance =
  ( [| 0; 2; 1; 2; 4; 4; 3; 4; 7; 5; 4; 4; 0; 1; 2; 0x77 |],
    [| 0x00; 0x01; 0x02; 0x03; 0x11; 0x04; 0x05; 0x21; 0x31; 0x06; 0x12; 0x41; 0x51; 0x07; 0x61; 0x71;
       0x13; 0x22; 0x32; 0x81; 0x08; 0x14; 0x42; 0x91; 0xa1; 0xb1; 0xc1; 0x09; 0x23; 0x33; 0x52; 0xf0;
       0x15; 0x62; 0x72; 0xd1; 0x0a; 0x16; 0x24; 0x34; 0xe1; 0x25; 0xf1; 0x17; 0x18; 0x19; 0x1a; 0x26;
       0x27; 0x28; 0x29; 0x2a; 0x35; 0x36; 0x37; 0x38; 0x39; 0x3a; 0x43; 0x44; 0x45; 0x46; 0x47; 0x48;
       0x49; 0x4a; 0x53; 0x54; 0x55; 0x56; 0x57; 0x58; 0x59; 0x5a; 0x63; 0x64; 0x65; 0x66; 0x67; 0x68;
       0x69; 0x6a; 0x73; 0x74; 0x75; 0x76; 0x77; 0x78; 0x79; 0x7a; 0x82; 0x83; 0x84; 0x85; 0x86; 0x87;
       0x88; 0x89; 0x8a; 0x92; 0x93; 0x94; 0x95; 0x96; 0x97; 0x98; 0x99; 0x9a; 0xa2; 0xa3; 0xa4; 0xa5;
       0xa6; 0xa7; 0xa8; 0xa9; 0xaa; 0xb2; 0xb3; 0xb4; 0xb5; 0xb6; 0xb7; 0xb8; 0xb9; 0xba; 0xc2; 0xc3;
       0xc4; 0xc5; 0xc6; 0xc7; 0xc8; 0xc9; 0xca; 0xd2; 0xd3; 0xd4; 0xd5; 0xd6; 0xd7; 0xd8; 0xd9; 0xda;
       0xe2; 0xe3; 0xe4; 0xe5; 0xe6; 0xe7; 0xe8; 0xe9; 0xea; 0xf2; 0xf3; 0xf4; 0xf5; 0xf6; 0xf7; 0xf8;
       0xf9; 0xfa |] )

(* each symbol's (code, length): JPEG's canonical codes, given in the
 * order the symbols are listed, the shortest first, each length's
 * first code the last one's next, doubled *)
let codes ((counts, symbols) : int array * int array) : (int * int) array =
  let table = Array.make 256 (0, 0) and code = ref 0 and k = ref 0 in
  Array.iteri
    (fun i n ->
      for _ = 1 to n do
        table.(symbols.(!k)) <- (!code, i + 1);
        incr code;
        incr k
      done;
      code := !code lsl 1)
    counts;
  table

(*****************************************************************************)
(* The bits *)
(*****************************************************************************)

(* bits into bytes, the most significant first; a byte FF followed by 00
 * (byte stuffing, Jpeg.mli) *)
type writer = { out : Buffer.t; mutable acc : int; mutable n : int }

let put (w : writer) (bits : int) (len : int) : unit =
  for i = len - 1 downto 0 do
    w.acc <- (w.acc lsl 1) lor ((bits lsr i) land 1);
    w.n <- w.n + 1;
    if w.n = 8 then (
      Buffer.add_uint8 w.out w.acc;
      if w.acc = 0xFF then Buffer.add_uint8 w.out 0;
      w.acc <- 0;
      w.n <- 0)
  done

(* the last byte filled with 1s, as T.81 asks *)
let flush (w : writer) : unit = if w.n > 0 then put w ((1 lsl (8 - w.n)) - 1) (8 - w.n)

(* a value's size, its bits: Jpeg.extend backwards -- a negative v is
 * sent as v - 1 in [size] bits *)
let size_of (v : int) : int =
  let rec go a s = if a = 0 then s else go (a lsr 1) (s + 1) in
  go (abs v) 0

let value_bits (v : int) (size : int) : int = if v >= 0 then v else v + (1 lsl size) - 1

(*****************************************************************************)
(* A block *)
(*****************************************************************************)

(* one 8 x 8 block of samples (0-255): shifted, transformed, quantized,
 * coded; gives its DC for the next block's prediction *)
let encode_block (w : writer) ~(q : int array) ~(dc : (int * int) array) ~(ac : (int * int) array) (samples : float array) (previous_dc : int) : int =
  let coefs = Dct.fdct (Array.map (fun v -> v -. 128.) samples) in
  let quantized = Array.init 64 (fun k -> let i = Jpeg.zigzag.(k) in int_of_float (Float.round (coefs.(i) /. float_of_int q.(i)))) in
  let symbol (table : (int * int) array) (s : int) =
    let code, len = table.(s) in
    if len = 0 then failwith (Printf.sprintf "JPEG: no code for symbol %02X" s);
    put w code len
  in
  (* the DC: its difference from the previous block's *)
  let diff = quantized.(0) - previous_dc in
  let s = size_of diff in
  symbol dc s;
  put w (value_bits diff s) s;
  (* the AC: (zeros before, size) and the bits, sixteen zeros as F0,
   * the trailing zeros as one end-of-block *)
  let last = ref 63 in
  while !last > 0 && quantized.(!last) = 0 do decr last done;
  let run = ref 0 in
  for k = 1 to !last do
    let v = quantized.(k) in
    if v = 0 then incr run
    else (
      while !run >= 16 do
        symbol ac 0xF0;
        run := !run - 16
      done;
      let s = size_of v in
      symbol ac ((!run lsl 4) lor s);
      put w (value_bits v s) s;
      run := 0)
  done;
  if !last < 63 then symbol ac 0x00;
  quantized.(0)

(*****************************************************************************)
(* The file *)
(*****************************************************************************)

let segment (b : Buffer.t) (marker : int) (body : Buffer.t) : unit =
  Buffer.add_uint8 b 0xFF;
  Buffer.add_uint8 b marker;
  Buffer.add_uint16_be b (2 + Buffer.length body);
  Buffer.add_buffer b body

let encode ?(quality = 75) ?(subsampling = `S420) (img : Rgba_image.t) : string =
  let w = img.width and h = img.height in
  let ql = scaled ~quality luminance_table and qc = scaled ~quality chrominance_table in
  (* the planes, full size; JFIF's YCbCr, full range *)
  let plane f =
    Array.init (w * h) (fun i ->
        let r = float_of_int img.rgba.{4 * i} and g = float_of_int img.rgba.{(4 * i) + 1} and b = float_of_int img.rgba.{(4 * i) + 2} in
        f r g b)
  in
  let y = plane (fun r g b -> (0.299 *. r) +. (0.587 *. g) +. (0.114 *. b)) in
  let cb = plane (fun r g b -> 128. -. (0.168736 *. r) -. (0.331264 *. g) +. (0.5 *. b)) in
  let cr = plane (fun r g b -> 128. +. (0.5 *. r) -. (0.418688 *. g) -. (0.081312 *. b)) in
  (* a sample of a plane, the edges repeated past the picture *)
  let at (p : float array) x y = p.((min (h - 1) y * w) + min (w - 1) x) in
  let side = match subsampling with `S420 -> 2 | `S444 -> 1 in
  let mcu = 8 * side in
  (* an 8 x 8 block of [p] at (x0, y0), each sample the average of
   * [scale] x [scale] pixels (the subsampling) *)
  let block (p : float array) ~(scale : int) (x0 : int) (y0 : int) : float array =
    Array.init 64 (fun i ->
        let bx = x0 + ((i mod 8) * scale) and by = y0 + ((i / 8) * scale) in
        let sum = ref 0. in
        for dy = 0 to scale - 1 do
          for dx = 0 to scale - 1 do sum := !sum +. at p (bx + dx) (by + dy) done
        done;
        !sum /. float_of_int (scale * scale))
  in
  let dcl = codes dc_luminance and acl = codes ac_luminance and dcc = codes dc_chrominance and acc = codes ac_chrominance in
  let wr = { out = Buffer.create (w * h / 4); acc = 0; n = 0 } in
  let py = ref 0 and pcb = ref 0 and pcr = ref 0 in
  for my = 0 to ((h + mcu - 1) / mcu) - 1 do
    for mx = 0 to ((w + mcu - 1) / mcu) - 1 do
      let x0 = mx * mcu and y0 = my * mcu in
      for by = 0 to side - 1 do
        for bx = 0 to side - 1 do
          py := encode_block wr ~q:ql ~dc:dcl ~ac:acl (block y ~scale:1 (x0 + (8 * bx)) (y0 + (8 * by))) !py
        done
      done;
      pcb := encode_block wr ~q:qc ~dc:dcc ~ac:acc (block cb ~scale:side x0 y0) !pcb;
      pcr := encode_block wr ~q:qc ~dc:dcc ~ac:acc (block cr ~scale:side x0 y0) !pcr
    done
  done;
  flush wr;
  (* the markers around the scan *)
  let b = Buffer.create (Buffer.length wr.out + 700) in
  Buffer.add_string b "\xFF\xD8";
  let body = Buffer.create 64 in
  Buffer.add_string body "JFIF\000\001\001\000";
  Buffer.add_uint16_be body 1;
  Buffer.add_uint16_be body 1;
  Buffer.add_string body "\000\000";
  segment b 0xE0 body;
  let body = Buffer.create 130 in
  List.iter
    (fun (id, table) ->
      Buffer.add_uint8 body id;
      for k = 0 to 63 do Buffer.add_uint8 body table.(Jpeg.zigzag.(k)) done)
    [ (0, ql); (1, qc) ];
  segment b 0xDB body;
  let body = Buffer.create 17 in
  Buffer.add_uint8 body 8;
  Buffer.add_uint16_be body h;
  Buffer.add_uint16_be body w;
  Buffer.add_uint8 body 3;
  List.iter (fun (id, hv, tq) -> Buffer.add_uint8 body id; Buffer.add_uint8 body hv; Buffer.add_uint8 body tq) [ (1, (side lsl 4) lor side, 0); (2, 0x11, 1); (3, 0x11, 1) ];
  segment b 0xC0 body;
  let body = Buffer.create 420 in
  List.iter
    (fun (tc, ((counts, symbols) : int array * int array)) ->
      Buffer.add_uint8 body tc;
      Array.iter (Buffer.add_uint8 body) counts;
      Array.iter (Buffer.add_uint8 body) symbols)
    [ (0x00, dc_luminance); (0x10, ac_luminance); (0x01, dc_chrominance); (0x11, ac_chrominance) ];
  segment b 0xC4 body;
  let body = Buffer.create 12 in
  Buffer.add_uint8 body 3;
  List.iter (fun (id, tables) -> Buffer.add_uint8 body id; Buffer.add_uint8 body tables) [ (1, 0x00); (2, 0x11); (3, 0x11) ];
  Buffer.add_string body "\000\063\000";
  segment b 0xDA body;
  Buffer.add_buffer b wr.out;
  Buffer.add_string b "\xFF\xD9";
  Buffer.contents b
