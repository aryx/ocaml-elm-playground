(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Layer3.mli *)

type granule = {
  part2_3_length : int;
  big_values : int;
  global_gain : int;
  scalefac_compress : int;
  block_type : int;
  mixed : bool;
  table_select : int array;
  subblock_gain : int array;
  region0_count : int;
  region1_count : int;
  preflag : bool;
  scalefac_scale : bool;
  count1_table : int;
}

type side_info = { main_data_begin : int; scfsi : bool array array; granules : granule array array }

(*****************************************************************************)
(* Side information *)
(*****************************************************************************)

(* the reads in sequence: OCaml evaluates a record's fields in no set
 * order. MPEG-2's lower rates: scalefac_compress of 9 bits, and no
 * preflag bit (the scalefactors' coding says it, [preflag_lsf]) *)
let granule ~(mpeg1 : bool) ~(preflag_lsf : int -> bool) (b : Bits.t) : granule =
  let read = Bits.read b in
  let part2_3_length = read 12 in
  let big_values = read 9 in
  let global_gain = read 8 in
  let scalefac_compress = read (if mpeg1 then 4 else 9) in
  let switching = read 1 = 1 in
  let block_type, mixed, table_select, subblock_gain, region0_count, region1_count =
    if switching then (
      let block_type = read 2 in
      let mixed = read 1 = 1 in
      let t0 = read 5 in
      let t1 = read 5 in
      let g0 = read 3 in
      let g1 = read 3 in
      let g2 = read 3 in
      (* the regions implicit: region 1 to the end *)
      let region0_count = if block_type = 2 && not mixed then 8 else 7 in
      (block_type, mixed, [| t0; t1; 0 |], [| g0; g1; g2 |], region0_count, 20 - region0_count))
    else
      let t0 = read 5 in
      let t1 = read 5 in
      let t2 = read 5 in
      let region0_count = read 4 in
      let region1_count = read 3 in
      (0, false, [| t0; t1; t2 |], [| 0; 0; 0 |], region0_count, region1_count)
  in
  let preflag = if mpeg1 then read 1 = 1 else preflag_lsf scalefac_compress in
  let scalefac_scale = read 1 = 1 in
  let count1_table = read 1 in
  { part2_3_length; big_values; global_gain; scalefac_compress; block_type; mixed; table_select; subblock_gain;
    region0_count; region1_count; preflag; scalefac_scale; count1_table }

(* intensity stereo's right channel codes its scalefactors (directions,
 * not loudnesses) its own way, in MPEG-2 *)
let intensity_right (h : Mpeg_audio_header.t) (ch : int) : bool =
  ch = 1 && h.mode = Mpeg_audio_header.Joint_stereo && h.mode_extension land 1 <> 0

let side_info (h : Mpeg_audio_header.t) (b : Bits.t) : side_info =
  let nch = h.channels and mpeg1 = h.version = Mpeg_audio_header.Mpeg1 in
  (* MPEG-2: a smaller reservoir, one granule, no scfsi *)
  let main_data_begin = Bits.read b (if mpeg1 then 9 else 8) in
  Bits.skip b (match (mpeg1, nch) with true, 1 -> 5 | true, _ -> 3 | false, 1 -> 1 | false, _ -> 2) (* private bits *);
  let scfsi = Array.init nch (fun _ -> Array.init 4 (fun _ -> mpeg1 && Bits.read b 1 = 1)) in
  let granules =
    Array.init (if mpeg1 then 2 else 1) (fun _ ->
        Array.init nch (fun ch -> granule ~mpeg1 ~preflag_lsf:(fun sfc -> sfc >= 500 && not (intensity_right h ch)) b))
  in
  { main_data_begin; scfsi; granules }

(*****************************************************************************)
(* Part 2: the scalefactors *)
(*****************************************************************************)

(* into [long] (22 bands, kept from granule to granule: scfsi) and
 * [short] (13 bands of 3 windows) *)
let scalefactors (b : Bits.t) (g : granule) (scfsi : bool array) (gr : int) (long : int array) (short : int array array) :
    unit =
  let slen1, slen2 = Layer3_tables.slen.(g.scalefac_compress) in
  if g.block_type = 2 then (
    (* mixed: the long bands of the 2 lowest subbands, then the short
     * bands from 3 up *)
    if g.mixed then
      for sfb = 0 to 7 do
        long.(sfb) <- Bits.read b slen1
      done;
    for sfb = (if g.mixed then 3 else 0) to 11 do
      for w = 0 to 2 do
        short.(sfb).(w) <- Bits.read b (if sfb < 6 then slen1 else slen2)
      done
    done;
    short.(12) <- [| 0; 0; 0 |])
  else (
    (* 4 groups of bands, each read, or reused from granule 0 *)
    [| (0, 5); (6, 10); (11, 15); (16, 20) |]
    |> Array.iteri (fun i (first, last) ->
           if gr = 0 || not scfsi.(i) then
             for sfb = first to last do
               long.(sfb) <- Bits.read b (if i < 2 then slen1 else slen2)
             done);
    long.(21) <- 0)

(* MPEG-2's (ISO/IEC 13818-3, 2.4.3.2): scalefac_compress, 9 bits,
 * picks one of 3 codings -- 6 for intensity stereo's right channel --
 * each giving the bits (slen) of 4 partitions of bands, and how many
 * bands each has; read in the bands' order (a short band's 3 windows
 * in turn, the long bands first in a mixed block) *)
let scalefactors_lsf (b : Bits.t) (g : granule) ~(intensity_right : bool) (long : int array) (short : int array array) :
    unit =
  let c = g.scalefac_compress in
  let coding, slen =
    if not intensity_right then
      (* parenthesized: lsr binds tighter than land and mod *)
      if c < 400 then (0, [| (c lsr 4) / 5; (c lsr 4) mod 5; (c land 15) lsr 2; c land 3 |])
      else if c < 500 then
        let c = c - 400 in
        (1, [| (c lsr 2) / 5; (c lsr 2) mod 5; c land 3; 0 |])
      else
        let c = c - 500 in
        (2, [| c / 3; c mod 3; 0; 0 |])
    else
      let c = c lsr 1 in
      if c < 180 then (3, [| c / 36; c mod 36 / 6; c mod 36 mod 6; 0 |])
      else if c < 244 then
        let c = c - 180 in
        (4, [| (c mod 64) lsr 4; (c mod 16) lsr 2; c land 3; 0 |])
      else
        let c = c - 244 in
        (5, [| c / 3; c mod 3; 0; 0 |])
  in
  let block = if g.block_type <> 2 then 0 else if g.mixed then 2 else 1 in
  let values = ref [] in
  Array.iteri
    (fun part count ->
      for _ = 1 to count do
        values := Bits.read b slen.(part) :: !values
      done)
    Layer3_tables.nr_of_sfb.(coding).(block);
  (* the i-th value, in the bands' order, to its band *)
  Array.fill long 0 22 0;
  Array.iter (fun w -> Array.fill w 0 3 0) short;
  List.iteri
    (fun i v ->
      match block with
      | 0 -> long.(i) <- v
      | 1 -> short.(i / 3).(i mod 3) <- v
      | _ -> if i < 6 then long.(i) <- v else short.(3 + ((i - 6) / 3)).((i - 6) mod 3) <- v)
    (List.rev !values)

(*****************************************************************************)
(* Part 3: the Huffman codes *)
(*****************************************************************************)

let tree (codes : string array) (size : int) : (int * int) Vlc.t =
  Vlc.of_list (List.mapi (fun i code -> (code, (i / size, i mod size))) (Array.to_list codes))

let pair_trees : ((int * int) Vlc.t * int) option Lazy.t array =
  Array.map (fun t -> lazy (Option.map (fun (codes, size, linbits) -> (tree codes size, linbits)) t)) Layer3_tables.pairs

let quad_trees =
  let quad codes = lazy (Vlc.of_list (List.mapi (fun i code -> (code, i)) (Array.to_list codes))) in
  [| quad Layer3_tables.quad_a; quad Layer3_tables.quad_b |]

(* the 576 integers into [is], until the bit [last]; the lines after
 * the count1 region are zeros *)
let huffman (b : Bits.t) (g : granule) (rate : int) (last : int) (is : int array) : unit =
  Array.fill is 0 576 0;
  let bands = Layer3_tables.long_bands rate in
  let big = min 576 (2 * g.big_values) in
  (* the 3 regions of the big values, each its table *)
  let region1, region2 =
    (* short blocks: region 0 the first 3 short bands (36 lines at
     * MPEG-1's rates), region 1 the rest *)
    if g.block_type = 2 then ((if g.mixed then bands.(8) else 3 * (Layer3_tables.short_bands rate).(3)), 576)
    else (bands.(min 22 (g.region0_count + 1)), bands.(min 22 (g.region0_count + g.region1_count + 2)))
  in
  let i = ref 0 in
  while !i < big do
    let table = g.table_select.(if !i < region1 then 0 else if !i < region2 then 1 else 2) in
    (match Lazy.force pair_trees.(table) with
    | None -> ()
    | Some (tree, linbits) ->
        let x, y = Vlc.read b tree in
        (* 15 and linbits more; then the sign *)
        let value v =
          let v = if linbits > 0 && v = 15 then v + Bits.read b linbits else v in
          if v <> 0 && Bits.read b 1 = 1 then -v else v
        in
        let x = value x in
        is.(!i) <- x;
        is.(!i + 1) <- value y);
    i := !i + 2
  done;
  (* count1: quadruples of 0s and 1s, until the part's bits are used *)
  let quads = Lazy.force quad_trees.(g.count1_table) in
  while Bits.position b < last && !i + 4 <= 576 do
    let q = Vlc.read b quads in
    for k = 0 to 3 do
      let v = (q lsr (3 - k)) land 1 in
      is.(!i + k) <- (if v <> 0 && Bits.read b 1 = 1 then -1 else v)
    done;
    i := !i + 4
  done;
  (* past the end: the last quadruple was the stuffing's bits *)
  if Bits.position b > last && !i >= 4 then Array.fill is (!i - 4) 4 0

(*****************************************************************************)
(* Requantization *)
(*****************************************************************************)

(* |is|^(4/3), for the integers a code (and its linbits) can give *)
let pow43 = lazy (Array.init 8207 (fun i -> float_of_int i ** (4. /. 3.)))

let requantize (g : granule) (rate : int) (long : int array) (short : int array array) (is : int array) : float array =
  let pow43 = Lazy.force pow43 in
  let xr = Array.make 576 0. in
  let gain = 2. ** (0.25 *. float_of_int (g.global_gain - 210)) in
  let multiplier = if g.scalefac_scale then 1. else 0.5 in
  let line i scale =
    let v = is.(i) in
    let r = pow43.(abs v) *. gain *. scale in
    xr.(i) <- (if v < 0 then -.r else r)
  in
  let long_band sfb first last =
    let pre = if g.preflag then Layer3_tables.pretab.(sfb) else 0 in
    let scale = 2. ** (-.multiplier *. float_of_int (long.(sfb) + pre)) in
    for i = first to last - 1 do
      line i scale
    done
  in
  let lb = Layer3_tables.long_bands rate in
  if g.block_type = 2 then (
    let sb = Layer3_tables.short_bands rate in
    (* mixed: the long bands below line 36 (the 2 lowest subbands) *)
    if g.mixed then
      for sfb = 0 to 21 do
        if lb.(sfb + 1) <= 36 then long_band sfb lb.(sfb) lb.(sfb + 1)
      done;
    (* short bands: each window's lines one after the other *)
    for sfb = (if g.mixed then 3 else 0) to 12 do
      let width = sb.(sfb + 1) - sb.(sfb) in
      for w = 0 to 2 do
        let scale = 2. ** (-2. *. float_of_int g.subblock_gain.(w)) *. 2. ** (-.multiplier *. float_of_int short.(sfb).(w)) in
        for f = 0 to width - 1 do
          line ((3 * sb.(sfb)) + (w * width) + f) scale
        done
      done
    done)
  else
    for sfb = 0 to 21 do
      long_band sfb lb.(sfb) lb.(sfb + 1)
    done;
  xr

(*****************************************************************************)
(* Stereo, reordering, alias reduction *)
(*****************************************************************************)

(* intensity stereo's directions, line by line, from the right channel's
 * scalefactors, above the last band where its spectrum isn't zero (per
 * window in short blocks); 7 where a line isn't intensity-coded. The
 * last band, which has no scalefactor, takes the one below it's
 * (2.4.3.4.9.3; MPEG-1's, MPEG-2's variant not done) *)
let intensity_positions (g : granule) (rate : int) (right : float array) (long : int array) (short : int array array) :
    int array =
  let positions = Array.make 576 7 in
  let lb = Layer3_tables.long_bands rate and sb = Layer3_tables.short_bands rate in
  let set first last p = Array.fill positions first (last - first) p in
  if g.block_type = 2 then (
    let first_short = if g.mixed then 3 else 0 in
    let all_windows_empty = ref true in
    for w = 0 to 2 do
      (* the highest short band with a line of this window not zero *)
      let top = ref (-1) in
      for sfb = first_short to 12 do
        let width = sb.(sfb + 1) - sb.(sfb) in
        for f = 0 to width - 1 do
          if right.((3 * sb.(sfb)) + (w * width) + f) <> 0. then top := sfb
        done
      done;
      if !top >= 0 then all_windows_empty := false;
      for sfb = max (!top + 1) first_short to 12 do
        let width = sb.(sfb + 1) - sb.(sfb) in
        let start = (3 * sb.(sfb)) + (w * width) in
        let p = if sfb < 12 then short.(sfb).(w) else if !top < 11 then short.(11).(w) else 7 in
        set start (start + width) p
      done
    done;
    (* a mixed block's long bands, when the short ones are all intensity *)
    if g.mixed && !all_windows_empty then
      for sfb = 0 to 21 do
        if lb.(sfb + 1) <= 36 then set lb.(sfb) lb.(sfb + 1) long.(sfb)
      done)
  else (
    (* the band after the one holding the highest line not zero *)
    let last = ref (-1) in
    Array.iteri (fun i v -> if v <> 0. then last := i) right;
    let first = ref 0 in
    while !first < 22 && lb.(!first) <= !last do
      incr first
    done;
    for sfb = !first to 21 do
      set lb.(sfb) lb.(sfb + 1) (if sfb < 21 then long.(sfb) else if !first <= 20 then long.(20) else 7)
    done);
  positions

(* joint stereo back to left and right, line by line: an intensity
 * line's single spectrum (sent in the left channel) split between the
 * two by its direction, ratio tan(is_pos pi / 12) -- 0 all right, 6
 * all left; the other lines, mid/side if on: M = (L + R) / sqrt 2, S =
 * (L - R) / sqrt 2 *)
let joint_stereo ~(mid_side : bool) (positions : int array) (left : float array) (right : float array) : unit =
  for i = 0 to 575 do
    let p = positions.(i) in
    if p <> 7 then (
      let ratio = tan (float_of_int p *. Float.pi /. 12.) in
      let v = left.(i) in
      left.(i) <- v *. ratio /. (1. +. ratio);
      right.(i) <- v /. (1. +. ratio))
    else if mid_side then (
      let m = left.(i) and s = right.(i) in
      left.(i) <- (m +. s) /. sqrt 2.;
      right.(i) <- (m -. s) /. sqrt 2.)
  done

(* short blocks: from each band's window after window, to each line's 3
 * windows side by side -- a subband's 18 values then 6 lines of 3
 * windows, what the IMDCT of each window picks from *)
let reorder (g : granule) (rate : int) (xr : float array) : unit =
  if g.block_type = 2 then (
    let sb = Layer3_tables.short_bands rate in
    let src = Array.copy xr in
    for sfb = (if g.mixed then 3 else 0) to 12 do
      let start = sb.(sfb) and width = sb.(sfb + 1) - sb.(sfb) in
      for w = 0 to 2 do
        for f = 0 to width - 1 do
          xr.((3 * (start + f)) + w) <- src.((3 * start) + (w * width) + f)
        done
      done
    done)

(* the encoder's filterbank lets neighbouring subbands overlap; 8
 * butterflies across each border undo what it mixed (Table B.9) *)
let ci = [| -0.6; -0.535; -0.33; -0.185; -0.095; -0.041; -0.0142; -0.0037 |]
let cs = Array.map (fun c -> 1. /. sqrt (1. +. (c *. c))) ci
let ca = Array.map (fun c -> c /. sqrt (1. +. (c *. c))) ci

let antialias (g : granule) (xr : float array) : unit =
  let borders = if g.block_type = 2 then if g.mixed then 1 else 0 else 31 in
  for sb = 1 to borders do
    for k = 0 to 7 do
      let lo = (18 * sb) - 1 - k and hi = (18 * sb) + k in
      let bu = xr.(lo) and bd = xr.(hi) in
      xr.(lo) <- (bu *. cs.(k)) -. (bd *. ca.(k));
      xr.(hi) <- (bd *. cs.(k)) +. (bu *. ca.(k))
    done
  done

(*****************************************************************************)
(* IMDCT, overlap-add *)
(*****************************************************************************)

(* each subband's 18 lines into 18 time slots of it, in [out] from slot
 * [slot]: the IMDCT's first half added to the last block's second *)
let hybrid (g : granule) (xr : float array) (overlap : float array array) (out : float array) (slot : int) : unit =
  for sb = 0 to 31 do
    let block_type = if g.block_type = 2 && g.mixed && sb < 2 then 0 else g.block_type in
    let z =
      if block_type = 2 then (
        (* 3 short IMDCTs, 6 samples apart, from sample 6 *)
        let z = Array.make 36 0. and win = Imdct.window 2 in
        for w = 0 to 2 do
          let y = Imdct.imdct (Array.init 6 (fun k -> xr.((18 * sb) + (3 * k) + w))) in
          for i = 0 to 11 do
            z.(6 + (6 * w) + i) <- z.(6 + (6 * w) + i) +. (y.(i) *. win.(i))
          done
        done;
        z)
      else
        let win = Imdct.window block_type in
        Array.mapi (fun i v -> v *. win.(i)) (Imdct.imdct (Array.sub xr (18 * sb) 18))
    in
    for i = 0 to 17 do
      let v = z.(i) +. overlap.(sb).(i) in
      (* the odd subbands' odd samples negated: the encoder's
       * filterbank left them frequency-inverted *)
      out.(((slot + i) * 32) + sb) <- (if sb land 1 = 1 && i land 1 = 1 then -.v else v);
      overlap.(sb).(i) <- z.(i + 18)
    done
  done

(*****************************************************************************)
(* A frame *)
(*****************************************************************************)

type state = {
  mutable reservoir : string; (* the last frames' main data, 511 bytes at most *)
  overlap : float array array array; (* per channel, per subband, 18 *)
  long : int array array; (* per channel, the long bands' scalefactors *)
}

let create () : state =
  { reservoir = ""; overlap = Array.init 2 (fun _ -> Array.make_matrix 32 18 0.); long = Array.make_matrix 2 22 0 }

let decode (st : state) (h : Mpeg_audio_header.t) (file : string) (at : int) : float array array =
  let nch = h.channels and mpeg1 = h.version = Mpeg_audio_header.Mpeg1 in
  let granules = if mpeg1 then 2 else 1 in
  let header = 4 + if h.crc then 2 else 0 in
  let bits = Bits.of_string file in
  Bits.seek bits (8 * (at + header));
  let si = side_info h bits in
  let side_bytes = match (mpeg1, nch) with true, 1 -> 17 | true, _ -> 32 | false, 1 -> 9 | false, _ -> 17 in
  let data_start = at + header + side_bytes in
  let data = String.sub file data_start (max 0 (at + h.length - data_start)) in
  let start = String.length st.reservoir - si.main_data_begin in
  let main = st.reservoir ^ data in
  st.reservoir <- String.sub main (max 0 (String.length main - 511)) (min 511 (String.length main));
  let out = Array.init nch (fun _ -> Array.make (granules * 18 * 32) 0.) in
  if start >= 0 then (
    let b = Bits.of_string main in
    Bits.seek b (8 * start);
    for gr = 0 to granules - 1 do
      let shorts = Array.init nch (fun _ -> Array.make_matrix 13 3 0) in
      let xr =
        Array.init nch (fun ch ->
            let g = si.granules.(gr).(ch) in
            let part = Bits.position b in
            let short = shorts.(ch) in
            if mpeg1 then scalefactors b g si.scfsi.(ch) gr st.long.(ch) short
            else scalefactors_lsf b g ~intensity_right:(intensity_right h ch) st.long.(ch) short;
            let is = Array.make 576 0 in
            huffman b g h.sample_rate (part + g.part2_3_length) is;
            Bits.seek b (part + g.part2_3_length);
            requantize g h.sample_rate st.long.(ch) short is)
      in
      if h.mode = Mpeg_audio_header.Joint_stereo && h.mode_extension <> 0 then (
        let g = si.granules.(gr).(1) in
        let positions =
          (* MPEG-2's intensity stereo codes its directions another way:
           * not done, its lines left as they are *)
          if h.mode_extension land 1 <> 0 && mpeg1 then intensity_positions g h.sample_rate xr.(1) st.long.(1) shorts.(1)
          else Array.make 576 7
        in
        joint_stereo ~mid_side:(h.mode_extension land 2 <> 0) positions xr.(0) xr.(1));
      for ch = 0 to nch - 1 do
        let g = si.granules.(gr).(ch) in
        reorder g h.sample_rate xr.(ch);
        antialias g xr.(ch);
        hybrid g xr.(ch) st.overlap.(ch) out.(ch) (18 * gr)
      done
    done);
  out
