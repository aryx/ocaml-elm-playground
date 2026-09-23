(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mpeg1.mli *)

type kind = I | P | B
type how = Intra | Forward | Backward | Both | Zero | Skipped
type info = { kind : kind; macroblocks : (how * (int * int) * (int * int)) array; mb_width : int }
type header = { width : int; height : int; rate : int * int; kinds : kind array }

(*****************************************************************************)
(* The sequence *)
(*****************************************************************************)

let picture_rates = [| (0, 1); (24000, 1001); (24, 1); (25, 1); (30000, 1001); (30, 1); (50, 1); (60000, 1001); (60, 1) |]

(* the default intra matrix, natural order: coarser for the high
 * frequencies, as JPEG's tables (Jpeg_encode.mli) *)
let default_intra =
  [| 8; 16; 19; 22; 26; 27; 29; 34;
     16; 16; 22; 24; 27; 29; 34; 37;
     19; 22; 26; 27; 29; 34; 34; 38;
     22; 22; 26; 27; 29; 34; 37; 40;
     22; 26; 27; 29; 32; 35; 40; 48;
     26; 27; 29; 32; 35; 40; 48; 58;
     26; 27; 29; 34; 38; 46; 56; 69;
     27; 29; 35; 38; 46; 56; 69; 83 |]

type sequence = { width : int; height : int; mbw : int; mbh : int; rate : int * int; intra_m : int array; non_intra_m : int array }

(* after the start code B3 *)
let sequence_header (b : Bits.t) : sequence =
  let width = Bits.read b 12 and height = Bits.read b 12 in
  Bits.skip b 4 (* the pixels' aspect *);
  let rate = picture_rates.(min 8 (Bits.read b 4)) in
  Bits.skip b (18 + 1 + 10 + 1) (* bit rate, a marker, the buffer's size, "constrained" *);
  let matrix default = if Bits.read b 1 = 1 then (let m = Array.make 64 0 in for k = 0 to 63 do m.(Jpeg.zigzag.(k)) <- Bits.read b 8 done; m) else default in
  let intra_m = matrix default_intra in
  let non_intra_m = matrix (Array.make 64 16) in
  if width = 0 || height = 0 then failwith "MPEG-1: a picture of no size";
  { width; height; mbw = (width + 15) / 16; mbh = (height + 15) / 16; rate; intra_m; non_intra_m }

(*****************************************************************************)
(* Pictures: three planes, whole macroblocks *)
(*****************************************************************************)

type frame = { y : Bytes.t; cb : Bytes.t; cr : Bytes.t }

let new_frame (sq : sequence) : frame =
  let n = sq.mbw * sq.mbh * 256 in
  { y = Bytes.make n '\000'; cb = Bytes.make (n / 4) '\128'; cr = Bytes.make (n / 4) '\128' }

let to_image (sq : sequence) (f : frame) : Rgba_image.t =
  let crop (plane : Bytes.t) ~stride ~w ~h = Bytes.init (w * h) (fun i -> Bytes.get plane ((i / w * stride) + (i mod w))) in
  let cw, ch = Yuv.chroma_size C420 ~width:sq.width ~height:sq.height in
  Yuv.to_image Studio
    { width = sq.width; height = sq.height; chroma = C420;
      y = crop f.y ~stride:(sq.mbw * 16) ~w:sq.width ~h:sq.height;
      cb = crop f.cb ~stride:(sq.mbw * 8) ~w:cw ~h:ch;
      cr = crop f.cr ~stride:(sq.mbw * 8) ~w:cw ~h:ch }

(*****************************************************************************)
(* Motion compensation *)
(*****************************************************************************)

let predict (row : int array) (x : int) (v : int) : int =
  let at i = row.(max 0 (min (Array.length row - 1) i)) in
  let full = x + (v asr 1) in
  if v land 1 = 0 then at full else (at full + at (full + 1) + 1) / 2

(* a [size] x [size] square of [plane] (its rows [stride] long, [rows]
 * of them) at (x, y), moved by (vx, vy) half pixels: the pixels, or the
 * averages of two or four of them *)
let prediction (plane : Bytes.t) ~(stride : int) ~(rows : int) ~(x : int) ~(y : int) ~(size : int) ((vx, vy) : int * int) : int array =
  let get px py = Char.code (Bytes.get plane ((max 0 (min (rows - 1) py) * stride) + max 0 (min (stride - 1) px))) in
  let fx = vx asr 1 and hx = vx land 1 and fy = vy asr 1 and hy = vy land 1 in
  Array.init (size * size) (fun k ->
      let px = x + (k mod size) + fx and py = y + (k / size) + fy in
      match (hx, hy) with
      | 0, 0 -> get px py
      | 1, 0 -> (get px py + get (px + 1) py + 1) / 2
      | 0, _ -> (get px py + get px (py + 1) + 1) / 2
      | _ -> (get px py + get (px + 1) py + get px (py + 1) + get (px + 1) (py + 1) + 2) / 4)

(* a macroblock's prediction, its three planes: from one reference, or
 * the average of two (a B's "both") *)
let predict_macroblock (sq : sequence) ~(mx : int) ~(my : int) (refs : (frame * (int * int)) list) : int array * int array * int array =
  let one (f, (vx, vy)) =
    (* the color's vector: half the brightness's, towards 0 *)
    let cv = (vx / 2, vy / 2) in
    ( prediction f.y ~stride:(sq.mbw * 16) ~rows:(sq.mbh * 16) ~x:(mx * 16) ~y:(my * 16) ~size:16 (vx, vy),
      prediction f.cb ~stride:(sq.mbw * 8) ~rows:(sq.mbh * 8) ~x:(mx * 8) ~y:(my * 8) ~size:8 cv,
      prediction f.cr ~stride:(sq.mbw * 8) ~rows:(sq.mbh * 8) ~x:(mx * 8) ~y:(my * 8) ~size:8 cv )
  in
  match List.map one refs with
  | [ p ] -> p
  | [ (y1, b1, r1); (y2, b2, r2) ] ->
      let avg a b = Array.mapi (fun i v -> (v + b.(i) + 1) / 2) a in
      (avg y1 y2, avg b1 b2, avg r1 r2)
  | _ -> invalid_arg "predict_macroblock"

(* a macroblock's planes written: [plane i] block i's 64 values or none *)
let write_macroblock (sq : sequence) (cur : frame) ~(mx : int) ~(my : int) ((py, pb, pr) : int array * int array * int array) : unit =
  let put plane ~stride ~x0 ~y0 ~size values =
    Array.iteri (fun k v -> Bytes.set plane (((y0 + (k / size)) * stride) + x0 + (k mod size)) (Char.chr (max 0 (min 255 v)))) values
  in
  put cur.y ~stride:(sq.mbw * 16) ~x0:(mx * 16) ~y0:(my * 16) ~size:16 py;
  put cur.cb ~stride:(sq.mbw * 8) ~x0:(mx * 8) ~y0:(my * 8) ~size:8 pb;
  put cur.cr ~stride:(sq.mbw * 8) ~x0:(mx * 8) ~y0:(my * 8) ~size:8 pr

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

let address_increment = Vlc.of_list Vlc.address_increment
let mb_type_i = Vlc.of_list Vlc.mb_type_i
let mb_type_p = Vlc.of_list Vlc.mb_type_p
let mb_type_b = Vlc.of_list Vlc.mb_type_b
let coded_block_pattern = Vlc.of_list Vlc.coded_block_pattern
let motion_code = Vlc.of_list Vlc.motion_code
let dc_size_luminance = Vlc.of_list Vlc.dc_size_luminance
let dc_size_chrominance = Vlc.of_list Vlc.dc_size_chrominance
let dct_first = Vlc.of_list Vlc.dct_first
let dct_next = Vlc.of_list Vlc.dct_next

let sign (v : int) : int = compare v 0

let dequantize ~(intra : bool) ~(q : int) ~(m : int) (level : int) : int =
  let v = if intra then 2 * level * q * m / 16 else ((2 * level) + sign level) * q * m / 16 in
  (* made odd, towards 0: the mismatch control *)
  let v = if v land 1 = 0 && v <> 0 then v - sign v else v in
  max (-2048) (min 2047 v)

(* a block's 64 values after the IDCT (not yet rounded or added):
 * intra, its DC predicted from [dc.(component)]; else a residual *)
let block (b : Bits.t) ~(intra : bool) ~(q : int) ~(matrix : int array) ~(dc : int array) ~(component : int) : float array =
  let coefs = Array.make 64 0. in
  let i = ref (-1) in
  if intra then (
    let size = Vlc.read b (if component = 0 then dc_size_luminance else dc_size_chrominance) in
    let diff = if size = 0 then 0 else (let v = Bits.read b size in if v < 1 lsl (size - 1) then v - (1 lsl size) + 1 else v) in
    dc.(component) <- dc.(component) + diff;
    coefs.(0) <- float_of_int (dc.(component) * 8);
    i := 0);
  let place run level =
    i := !i + run + 1;
    if !i > 63 then failwith "MPEG-1: a block of more than 64 coefficients";
    let n = Jpeg.zigzag.(!i) in
    coefs.(n) <- float_of_int (dequantize ~intra ~q ~m:matrix.(n) level)
  in
  let rec coefficients first =
    match Vlc.read b (if first && not intra then dct_first else dct_next) with
    | Vlc.Eob -> ()
    | Vlc.Escape ->
        let run = Bits.read b 6 in
        let level = Bits.read b 8 in
        let level = if level = 0 then Bits.read b 8 else if level = 128 then Bits.read b 8 - 256 else if level > 128 then level - 256 else level in
        place run level;
        coefficients false
    | Vlc.Coeff (run, level) ->
        place run (if Bits.read b 1 = 1 then -level else level);
        coefficients false
  in
  coefficients true;
  Dct.idct_aan coefs

(*****************************************************************************)
(* Slices and macroblocks *)
(*****************************************************************************)

type picture_header = { kind : kind; fwd_full : bool; fwd_f : int; bwd_full : bool; bwd_f : int }

(* a vector's component: its difference from the predictor [pred.(c)],
 * in f_code's range, wrapped around it; the predictor updated. Gives
 * half pixels. *)
let motion (b : Bits.t) ~(f_code : int) ~(full : bool) (pred : int array) (c : int) : int =
  let code = Vlc.read b motion_code in
  let r_size = f_code - 1 in
  let d = if code = 0 || r_size = 0 then code else (let r = Bits.read b r_size in let d = ((abs code - 1) lsl r_size) + r + 1 in if code < 0 then -d else d) in
  let fscale = 1 lsl r_size in
  let v = pred.(c) + d in
  let v = if v > (fscale lsl 4) - 1 then v - (fscale lsl 5) else if v < -(fscale lsl 4) then v + (fscale lsl 5) else v in
  pred.(c) <- v;
  if full then 2 * v else v

let slice (b : Bits.t) (sq : sequence) (ph : picture_header) ~(past : frame option) ~(future : frame option) (cur : frame) (mbs : (how * (int * int) * (int * int)) array) (row : int) : unit =
  let q = ref (Bits.read b 5) in
  while Bits.read b 1 = 1 do Bits.skip b 8 done;
  let addr = ref ((row * sq.mbw) - 1) in
  let dc = [| 128; 128; 128 |] and pf = [| 0; 0 |] and pb = [| 0; 0 |] in
  let reset_dc () = Array.fill dc 0 3 128 in
  let refs fwd bwd fv bv =
    (match (fwd, past) with true, Some f -> [ (f, fv) ] | _ -> []) @ match (bwd, future) with true, Some f -> [ (f, bv) ] | _ -> []
  in
  (* the last macroblock's prediction, which a B's skipped ones repeat *)
  let last = ref (false, false, (0, 0), (0, 0)) in
  let continue = ref true in
  while !continue do
    let incr = ref 0 in
    let rec increment () = match Vlc.read b address_increment with -1 -> increment () | -2 -> incr := !incr + 33; increment () | n -> incr := !incr + n in
    increment ();
    (* the skipped macroblocks: in a P, the reference's at the same
     * place; in a B, the last macroblock's prediction again *)
    for k = 1 to !incr - 1 do
      let a = !addr + k in
      let mx = a mod sq.mbw and my = a / sq.mbw in
      reset_dc ();
      (match ph.kind with
      | P ->
          Array.fill pf 0 2 0;
          Option.iter (fun f -> write_macroblock sq cur ~mx ~my (predict_macroblock sq ~mx ~my [ (f, (0, 0)) ])) past
      | B ->
          let fwd, bwd, fv, bv = !last in
          let r = refs fwd bwd fv bv in
          if r <> [] then write_macroblock sq cur ~mx ~my (predict_macroblock sq ~mx ~my r)
      | I -> failwith "MPEG-1: a macroblock skipped in an I picture");
      if a >= 0 && a < Array.length mbs then mbs.(a) <- (let _, _, fv, bv = !last in (Skipped, (if ph.kind = B then fv else (0, 0)), if ph.kind = B then bv else (0, 0)))
    done;
    addr := !addr + !incr;
    let a = !addr in
    if a < 0 || a >= sq.mbw * sq.mbh then failwith "MPEG-1: a macroblock outside the picture";
    let mx = a mod sq.mbw and my = a / sq.mbw in
    let t = Vlc.read b (match ph.kind with I -> mb_type_i | P -> mb_type_p | B -> mb_type_b) in
    if t.quant then q := Bits.read b 5;
    let fv = if t.forward then (let h = motion b ~f_code:ph.fwd_f ~full:ph.fwd_full pf 0 in (h, motion b ~f_code:ph.fwd_f ~full:ph.fwd_full pf 1)) else (0, 0) in
    let bv = if t.backward then (let h = motion b ~f_code:ph.bwd_f ~full:ph.bwd_full pb 0 in (h, motion b ~f_code:ph.bwd_f ~full:ph.bwd_full pb 1)) else (0, 0) in
    let cbp = if t.intra then 63 else if t.pattern then Vlc.read b coded_block_pattern else 0 in
    if t.intra then (
      Array.fill pf 0 2 0;
      Array.fill pb 0 2 0;
      (* each block alone, JPEG's way, its DC from the block before *)
      let values = Array.init 6 (fun i -> block b ~intra:true ~q:!q ~matrix:sq.intra_m ~dc ~component:(if i < 4 then 0 else i - 3)) in
      let r v = int_of_float (Float.round v) in
      let py = Array.init 256 (fun k -> let x = k mod 16 and y = k / 16 in r values.((y / 8 * 2) + (x / 8)).((y mod 8 * 8) + (x mod 8))) in
      write_macroblock sq cur ~mx ~my (py, Array.map r values.(4), Array.map r values.(5));
      mbs.(a) <- (Intra, (0, 0), (0, 0)))
    else (
      reset_dc ();
      (* a P's macroblock without a vector: the same place, and the
       * vector's predictor back to 0 *)
      if ph.kind = P && not t.forward then Array.fill pf 0 2 0;
      let fwd = t.forward || ph.kind = P in
      let r = refs fwd t.backward fv bv in
      let py, pbl, prl = if r = [] then (Array.make 256 128, Array.make 64 128, Array.make 64 128) else predict_macroblock sq ~mx ~my r in
      (* the residual of the coded blocks, added *)
      for i = 0 to 5 do
        if cbp land (32 lsr i) <> 0 then (
          let res = block b ~intra:false ~q:!q ~matrix:sq.non_intra_m ~dc ~component:0 in
          let add (target : int array) ~size ~x0 ~y0 = Array.iteri (fun k v -> let t = ((y0 + (k / 8)) * size) + x0 + (k mod 8) in target.(t) <- target.(t) + int_of_float (Float.round v)) res in
          if i < 4 then add py ~size:16 ~x0:(i land 1 * 8) ~y0:(i lsr 1 * 8) else add (if i = 4 then pbl else prl) ~size:8 ~x0:0 ~y0:0)
      done;
      write_macroblock sq cur ~mx ~my (py, pbl, prl);
      last := (fwd, t.backward, fv, bv);
      mbs.(a) <- ((match (t.forward, t.backward) with true, true -> Both | true, false -> Forward | false, true -> Backward | false, false -> Zero), fv, bv));
    (* the slice ends where a start code begins: 23 zero bits *)
    if Bits.peek b 23 = 0 then continue := false
  done

(* a picture, after its start code; the reader left after the start
 * code that follows it (returned) *)
let picture (b : Bits.t) (sq : sequence) ~(past : frame option) ~(future : frame option) : frame * info * int option =
  Bits.skip b 10 (* its place in display order: the reordering below doesn't need it *);
  let kind = match Bits.read b 3 with 1 -> I | 2 -> P | 3 -> B | 4 -> failwith "MPEG-1: a D picture, not read here" | _ -> failwith "MPEG-1: a picture of no kind" in
  Bits.skip b 16;
  let fwd_full, fwd_f = if kind <> I then (let full = Bits.read b 1 = 1 in (full, Bits.read b 3)) else (false, 1) in
  let bwd_full, bwd_f = if kind = B then (let full = Bits.read b 1 = 1 in (full, Bits.read b 3)) else (false, 1) in
  while Bits.read b 1 = 1 do Bits.skip b 8 done;
  let ph = { kind; fwd_full; fwd_f; bwd_full; bwd_f } in
  let cur = new_frame sq in
  let mbs = Array.make (sq.mbw * sq.mbh) (Skipped, (0, 0), (0, 0)) in
  let code = ref (Bits.next_start_code b) in
  while !code = Some 0xB5 || !code = Some 0xB2 do code := Bits.next_start_code b done;
  while (match !code with Some c -> c >= 0x01 && c <= 0xAF | None -> false) do
    let row = Option.get !code - 1 in
    (* a corrupt slice: what was decoded of it kept, on to the next *)
    (try slice b sq ph ~past:(if kind = B then past else future) ~future cur mbs row with Failure _ | Invalid_argument _ -> ());
    code := Bits.next_start_code b
  done;
  (cur, { kind; macroblocks = mbs; mb_width = sq.mbw }, !code)

(*****************************************************************************)
(* The stream, in display order *)
(*****************************************************************************)

(* the decoder between two frames shown: where it is in the stream, the
 * sequence's parameters, the two references (the older, and the latest
 * with whether it was shown) *)
type state = { pos : int; sq : sequence; older : frame option; latest : (frame * info) option; shown : bool }

let of_string (s : string) : header * Movie.t * (int -> info) =
  let b = Bits.of_string s in
  let sq = match Bits.next_start_code b with Some 0xB3 -> sequence_header b | _ -> failwith "MPEG-1: not a video stream (no sequence header)" in
  let start_pos = Bits.position b in
  (* the pictures' kinds, from their headers, reordered for display *)
  let decode_order = ref [] in
  let scan = Bits.of_string s in
  let rec walk () =
    match Bits.next_start_code scan with
    | Some 0x00 ->
        Bits.skip scan 10;
        decode_order := (match Bits.read scan 3 with 1 -> I | 2 -> P | _ -> B) :: !decode_order;
        walk ()
    | Some _ -> walk ()
    | None -> ()
  in
  walk ();
  let display = ref [] and held = ref None in
  List.iter (fun k -> if k = B then display := k :: !display else (Option.iter (fun h -> display := h :: !display) !held; held := Some k)) (List.rev !decode_order);
  Option.iter (fun h -> display := h :: !display) !held;
  let kinds = Array.of_list (List.rev !display) in
  if kinds = [||] then failwith "MPEG-1: no pictures";
  let infos = Hashtbl.create 64 and shown_count = ref 0 in
  (* on to the next frame to show: decode until one is ready *)
  let rec next (st : state) : state * Rgba_image.t =
    let show (f, info) st =
      Hashtbl.replace infos !shown_count info;
      incr shown_count;
      (st, to_image st.sq f)
    in
    Bits.seek b st.pos;
    let rec code () =
      match Bits.next_start_code b with
      | Some 0xB3 -> Some (`Sequence (sequence_header b))
      | Some 0x00 -> Some `Picture
      | Some 0xB7 | None -> None
      | Some _ -> code ()
    in
    match code () with
    | Some (`Sequence sq) -> next { st with pos = Bits.position b; sq }
    | None -> (
        (* the end: the last reference, if not shown yet *)
        match st.latest with
        | Some l when not st.shown -> show l { st with pos = 8 * String.length s; shown = true }
        | _ -> failwith "MPEG-1: no more frames")
    | Some `Picture -> (
        let latest = Option.map fst st.latest in
        let f, info, after = picture b st.sq ~past:st.older ~future:latest in
        (* the next start code's own bytes, to read again *)
        let pos = match after with Some _ -> Bits.position b - 32 | None -> 8 * String.length s in
        match info.kind with
        | B -> show (f, info) { st with pos }
        | I | P -> (
            let st' = { st with pos; older = latest; latest = Some (f, info); shown = false } in
            match st.latest with Some l when not st.shown -> show l st' | _ -> next st'))
  in
  let start () =
    shown_count := 0;
    { pos = start_pos; sq; older = None; latest = None; shown = true }
  in
  let num, den = sq.rate in
  let period = if num = 0 then 0.04 else float_of_int den /. float_of_int num in
  let count = Array.length kinds in
  let movie = Movie.sequential ~width:sq.width ~height:sq.height ~times:(Array.init count (fun i -> float_of_int i *. period)) ~duration:(float_of_int count *. period) ~start ~next in
  let info i =
    ignore (movie.frame i);
    match Hashtbl.find_opt infos i with Some x -> x | None -> { kind = kinds.(i); macroblocks = [||]; mb_width = sq.mbw }
  in
  ({ width = sq.width; height = sq.height; rate = sq.rate; kinds }, movie, info)
