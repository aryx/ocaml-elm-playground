(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mpeg1_encode.mli *)

type stats = { macroblocks : int; intra : int; skipped : int; candidates : int }

(*****************************************************************************)
(* Bits out *)
(*****************************************************************************)

type writer = { out : Buffer.t; mutable acc : int; mutable n : int }

let put (w : writer) (bits : int) (len : int) : unit =
  for i = len - 1 downto 0 do
    w.acc <- (w.acc lsl 1) lor ((bits lsr i) land 1);
    w.n <- w.n + 1;
    if w.n = 8 then (
      Buffer.add_uint8 w.out w.acc;
      w.acc <- 0;
      w.n <- 0)
  done

(* a code as the standard prints it, "0000 0101 11" *)
let code (w : writer) (c : string) : unit = String.iter (function '0' -> put w 0 1 | '1' -> put w 1 1 | _ -> ()) c

(* a start code, on a byte boundary (zero bits before it are allowed) *)
let start_code (w : writer) (c : int) : unit =
  if w.n > 0 then put w 0 (8 - w.n);
  put w 1 24;
  put w c 8

(* each value's code, from Vlc's tables *)
let lookup (table : (string * 'a) list) : 'a -> string =
  let h = Hashtbl.create 64 in
  List.iter (fun (c, v) -> if not (Hashtbl.mem h v) then Hashtbl.add h v c) table;
  fun v -> match Hashtbl.find_opt h v with Some c -> c | None -> invalid_arg "Mpeg1_encode: a value with no code"

let address_increment = lookup Vlc.address_increment
let coded_block_pattern = lookup Vlc.coded_block_pattern
let motion_code = lookup Vlc.motion_code
let dc_size_luminance = lookup Vlc.dc_size_luminance
let dc_size_chrominance = lookup Vlc.dc_size_chrominance

let coefficient : int * int -> string option =
  let h = Hashtbl.create 128 in
  List.iter (function c, Vlc.Coeff (run, level) -> Hashtbl.replace h (run, level) c | _ -> ()) Vlc.dct_next;
  Hashtbl.find_opt h

(* a value's size and bits, as JPEG's (Jpeg.extend backwards) *)
let size_of (v : int) : int =
  let rec go a s = if a = 0 then s else go (a lsr 1) (s + 1) in
  go (abs v) 0

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

(* a block's levels in zigzag order, from [start] (0 a non-intra block's
 * first, 1 after an intra DC), as (run, level) codes, escapes, and an
 * end of block *)
let coefficients (w : writer) (levels : int array) ~(start : int) : unit =
  let run = ref 0 and first = ref (start = 0) in
  for k = start to 63 do
    let l = levels.(k) in
    if l = 0 then incr run
    else (
      (match (!run, abs l) with
      | 0, 1 -> code w (if !first then "1" else "11")
      | r, a -> (
          match coefficient (r, a) with
          | Some c -> code w c
          | None ->
              (* the escape: the run and the level written plainly *)
              code w "0000 01";
              put w r 6;
              if l >= 128 then (put w 0 8; put w l 8)
              else if l <= -128 then (put w 0x80 8; put w (l + 256) 8)
              else put w (l land 0xFF) 8));
      (* the sign, but after an escape *)
      (match (!run, abs l) with 0, 1 -> put w (if l < 0 then 1 else 0) 1 | r, a -> if coefficient (r, a) <> None then put w (if l < 0 then 1 else 0) 1);
      run := 0;
      first := false)
  done;
  code w "10"

let clamp_level (l : int) : int = max (-255) (min 255 l)

(*****************************************************************************)
(* Frames: whole macroblocks *)
(*****************************************************************************)

type frame = { y : Bytes.t; cb : Bytes.t; cr : Bytes.t }

(* a picture's planes, its edges repeated to whole macroblocks *)
let padded ~(mbw : int) ~(mbh : int) (img : Rgba_image.t) : frame =
  let p = Yuv.of_image Studio C420 img in
  let cw, ch = Yuv.chroma_size C420 ~width:p.width ~height:p.height in
  let pad (plane : Bytes.t) ~w ~h ~stride ~rows = Bytes.init (stride * rows) (fun i -> Bytes.get plane ((min (h - 1) (i / stride) * w) + min (w - 1) (i mod stride))) in
  { y = pad p.y ~w:p.width ~h:p.height ~stride:(mbw * 16) ~rows:(mbh * 16);
    cb = pad p.cb ~w:cw ~h:ch ~stride:(mbw * 8) ~rows:(mbh * 8);
    cr = pad p.cr ~w:cw ~h:ch ~stride:(mbw * 8) ~rows:(mbh * 8) }

(* block [i] of macroblock (mx, my): its plane, row length, corner *)
let block_place (f : frame) ~(mbw : int) ~(mx : int) ~(my : int) (i : int) : Bytes.t * int * int * int =
  if i < 4 then (f.y, mbw * 16, (mx * 16) + (i land 1 * 8), (my * 16) + (i lsr 1 * 8))
  else ((if i = 4 then f.cb else f.cr), mbw * 8, mx * 8, my * 8)

let read_block (plane : Bytes.t) ~(stride : int) ~(x : int) ~(y : int) : int array =
  Array.init 64 (fun k -> Char.code (Bytes.get plane (((y + (k / 8)) * stride) + x + (k mod 8))))

let write_block (plane : Bytes.t) ~(stride : int) ~(x : int) ~(y : int) (values : int array) : unit =
  Array.iteri (fun k v -> Bytes.set plane (((y + (k / 8)) * stride) + x + (k mod 8)) (Char.chr (max 0 (min 255 v)))) values

(* the IDCT of dequantized coefficients, rounded: the decoder's *)
let reconstruct (coefs : float array) : int array = Array.map (fun v -> int_of_float (Float.round v)) (Dct.idct_aan coefs)

(*****************************************************************************)
(* The stream *)
(*****************************************************************************)

let encode ?(quantizer = 5) ?(gop = 12) ?(search = Motion.Full) ?(range = 10) ~(rate : int * int) (frames : Rgba_image.t list) : string * stats =
  let first = match frames with [] -> invalid_arg "Mpeg1_encode.encode: no frames" | f :: _ -> f in
  let width = first.width and height = first.height in
  let rate_code =
    let rec find i = if i >= Array.length Mpeg1.picture_rates then invalid_arg "Mpeg1_encode.encode: not one of MPEG-1's rates" else if Mpeg1.picture_rates.(i) = rate && i > 0 then i else find (i + 1) in
    find 0
  in
  let q = max 1 (min 31 quantizer) in
  let mbw = (width + 15) / 16 and mbh = (height + 15) / 16 in
  (* the vectors' range: f_code's, enough for [range] whole pixels and
   * the half pixel around them *)
  let f_code = let rec go f = if (16 lsl (f - 1)) - 1 >= (2 * range) + 1 then f else go (f + 1) in go 1 in
  let fscale = 1 lsl (f_code - 1) in
  let w = { out = Buffer.create 65536; acc = 0; n = 0 } in
  let stats = ref { macroblocks = 0; intra = 0; skipped = 0; candidates = 0 } in
  (* the sequence header *)
  start_code w 0xB3;
  put w width 12;
  put w height 12;
  put w 1 4 (* square pixels *);
  put w rate_code 4;
  put w 0x3FFFF 18 (* a variable bit rate *);
  put w 1 1;
  put w 20 10 (* the decoder's buffer, in 16 KB *);
  put w 0 1;
  put w 0 1 (* the default intra matrix *);
  put w 0 1 (* and non-intra *);
  let num, den = rate in
  let reference = ref None in
  List.iteri
    (fun index (img : Rgba_image.t) ->
      if img.width <> width || img.height <> height then invalid_arg "Mpeg1_encode.encode: frames of different sizes";
      let source = padded ~mbw ~mbh img in
      let in_gop = index mod gop in
      let intra_picture = in_gop = 0 || !reference = None in
      if in_gop = 0 then (
        (* a group of pictures, its time code, closed: no picture refers
         * to one before it *)
        let seconds = index * den / num in
        start_code w 0xB8;
        put w 0 1;
        put w (seconds / 3600) 5;
        put w (seconds / 60 mod 60) 6;
        put w 1 1;
        put w (seconds mod 60) 6;
        put w (index - (seconds * num / den)) 6;
        put w 1 1;
        put w 0 1);
      start_code w 0x00;
      put w in_gop 10;
      put w (if intra_picture then 1 else 2) 3;
      put w 0xFFFF 16;
      if not intra_picture then (put w 0 1; put w f_code 3);
      put w 0 1;
      let recon = { y = Bytes.make (mbw * mbh * 256) '\000'; cb = Bytes.make (mbw * mbh * 64) '\128'; cr = Bytes.make (mbw * mbh * 64) '\128' } in
      for my = 0 to mbh - 1 do
        (* a slice a row: the predictions start over at each *)
        start_code w (my + 1);
        put w q 5;
        put w 0 1;
        let dc = [| 128; 128; 128 |] and pf = [| 0; 0 |] in
        let last_coded = ref (-1) in
        for mx = 0 to mbw - 1 do
          stats := { !stats with macroblocks = !stats.macroblocks + 1 };
          let increment () =
            let inc = ref (mx - !last_coded) in
            while !inc > 33 do code w "0000 0001 000"; inc := !inc - 33 done;
            code w (address_increment !inc);
            last_coded := mx
          in
          (* an intra macroblock: each block alone, its DC from the one
           * before *)
          let intra ~(first_code : string) =
            increment ();
            code w first_code;
            Array.fill pf 0 2 0;
            for i = 0 to 5 do
              let plane, stride, x, y = block_place source ~mbw ~mx ~my i in
              let f = Dct.fdct (Array.map float_of_int (read_block plane ~stride ~x ~y)) in
              let component = if i < 4 then 0 else i - 3 in
              let dcv = max 0 (min 255 (int_of_float (Float.round (f.(0) /. 8.)))) in
              let levels = Array.init 64 (fun k -> if k = 0 then 0 else let n = Jpeg.zigzag.(k) in clamp_level (int_of_float (Float.round (f.(n) *. 8. /. float_of_int (q * Mpeg1.default_intra.(n)))))) in
              let diff = dcv - dc.(component) in
              let s = size_of diff in
              code w ((if component = 0 then dc_size_luminance else dc_size_chrominance) s);
              if s > 0 then put w (if diff < 0 then diff + (1 lsl s) - 1 else diff) s;
              dc.(component) <- dcv;
              coefficients w levels ~start:1;
              (* what the decoder will have *)
              let coefs = Array.make 64 0. in
              coefs.(0) <- float_of_int (dcv * 8);
              for k = 1 to 63 do let n = Jpeg.zigzag.(k) in coefs.(n) <- float_of_int (Mpeg1.dequantize ~intra:true ~q ~m:Mpeg1.default_intra.(n) levels.(k)) done;
              let rplane, _, _, _ = block_place recon ~mbw ~mx ~my i in
              write_block rplane ~stride ~x ~y (reconstruct coefs)
            done
          in
          match !reference with
          | Some ref_frame when not intra_picture ->
              let plane (b : Bytes.t) = { Motion.bytes = b; stride = mbw * 16; rows = mbh * 16 } in
              let v, sad, tried = Motion.estimate search ~range (plane source.y) (plane ref_frame.y) ~x:(mx * 16) ~y:(my * 16) in
              stats := { !stats with candidates = !stats.candidates + tried };
              (* no vector unless it's clearly better: a zero vector is
               * cheaper to say, and can be skipped *)
              let zero_sad = Motion.sad (plane source.y) (plane ref_frame.y) ~x:(mx * 16) ~y:(my * 16) (0, 0) in
              let v = if zero_sad <= sad + 64 then (0, 0) else v in
              let sad = if v = (0, 0) then zero_sad else sad in
              (* the macroblock's own variation, what an intra one would
               * have to code *)
              let luma = Array.init 256 (fun k -> Char.code (Bytes.get source.y ((((my * 16) + (k / 16)) * mbw * 16) + (mx * 16) + (k mod 16)))) in
              let mean = Array.fold_left ( + ) 0 luma / 256 in
              let activity = Array.fold_left (fun a p -> a + abs (p - mean)) 0 luma in
              if sad > activity + 512 then (
                stats := { !stats with intra = !stats.intra + 1 };
                intra ~first_code:"0001 1")
              else (
                (* the prediction, and the residual's levels *)
                let cv = (fst v / 2, snd v / 2) in
                let predictions =
                  Array.init 6 (fun i ->
                      let plane, stride, x, y = block_place ref_frame ~mbw ~mx ~my i in
                      Mpeg1.prediction plane ~stride ~rows:(Bytes.length plane / stride) ~x ~y ~size:8 (if i < 4 then v else cv))
                in
                let levels =
                  Array.init 6 (fun i ->
                      let plane, stride, x, y = block_place source ~mbw ~mx ~my i in
                      let src = read_block plane ~stride ~x ~y in
                      let f = Dct.fdct (Array.mapi (fun k s -> float_of_int (s - predictions.(i).(k))) src) in
                      Array.init 64 (fun k -> let c = f.(Jpeg.zigzag.(k)) in clamp_level (compare c 0. * int_of_float (Float.abs c /. float_of_int (2 * q)))))
                in
                let cbp = ref 0 in
                Array.iteri (fun i l -> if Array.exists (( <> ) 0) l then cbp := !cbp lor (32 lsr i)) levels;
                let ends_slice = mx = 0 || mx = mbw - 1 in
                dc.(0) <- 128;
                dc.(1) <- 128;
                dc.(2) <- 128;
                if v = (0, 0) && !cbp = 0 && not ends_slice then (
                  (* skipped: not a bit; the decoder copies the same place *)
                  stats := { !stats with skipped = !stats.skipped + 1 };
                  Array.fill pf 0 2 0)
                else (
                  increment ();
                  let vector () =
                    List.iteri
                      (fun c comp ->
                        let delta = comp - pf.(c) in
                        let delta = if delta < -16 * fscale then delta + (32 * fscale) else if delta > (16 * fscale) - 1 then delta - (32 * fscale) else delta in
                        pf.(c) <- comp;
                        if delta = 0 then code w (motion_code 0)
                        else (
                          let a = abs delta - 1 in
                          code w (motion_code (compare delta 0 * ((a / fscale) + 1)));
                          if fscale > 1 then put w (a mod fscale) (f_code - 1)))
                      [ fst v; snd v ]
                  in
                  if v = (0, 0) && !cbp <> 0 then (
                    code w "01" (* no vector, a correction: "the same place" *);
                    Array.fill pf 0 2 0)
                  else if !cbp = 0 then (code w "001"; vector ()) (* moved, nothing to correct *)
                  else (code w "1"; vector ());
                  if !cbp <> 0 then code w (coded_block_pattern !cbp);
                  Array.iteri (fun i l -> if !cbp land (32 lsr i) <> 0 then coefficients w l ~start:0) levels);
                (* what the decoder will have: the prediction, and the
                 * coded blocks' residuals *)
                for i = 0 to 5 do
                  let values =
                    if !cbp land (32 lsr i) = 0 then predictions.(i)
                    else (
                      let coefs = Array.make 64 0. in
                      Array.iteri (fun k l -> let n = Jpeg.zigzag.(k) in coefs.(n) <- float_of_int (Mpeg1.dequantize ~intra:false ~q ~m:16 l)) levels.(i);
                      let r = reconstruct coefs in
                      Array.mapi (fun k p -> p + r.(k)) predictions.(i))
                  in
                  let rplane, stride, x, y = block_place recon ~mbw ~mx ~my i in
                  write_block rplane ~stride ~x ~y values
                done)
          | _ -> intra ~first_code:"1"
        done
      done;
      reference := Some recon)
    frames;
  start_code w 0xB7;
  (Buffer.contents w.out, !stats)
