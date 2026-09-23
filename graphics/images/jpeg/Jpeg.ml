(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jpeg.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type component = {
  id : int;
  (* sampling factors *)
  h : int;
  v : int;
  (* its quantization table *)
  tq : int;
  (* its Huffman tables, set by each scan *)
  mutable td : int;
  mutable ta : int;
  (* the previous block's DC coefficient *)
  mutable pred : int;
  (* the samples, whole blocks: pw x ph, pw = the MCUs across * h * 8 *)
  plane : Bytes.t;
  pw : int;
  ph : int;
  (* the samples that are in the picture, the rest is padding *)
  cw : int;
  ch : int;
}

type frame = {
  width : int;
  height : int;
  comps : component array;
  hmax : int;
  vmax : int;
  mcux : int;
  mcuy : int;
}

(*****************************************************************************)
(* The zigzag, and the values of bits *)
(*****************************************************************************)

(* along the anti-diagonals, alternately up and down *)
let zigzag : int array =
  let z = Array.make 64 0 and k = ref 0 in
  for s = 0 to 14 do
    let cells = List.filter (fun (_, c) -> c >= 0 && c < 8) (List.init 8 (fun r -> (r, s - r))) in
    List.iter
      (fun (r, c) ->
        z.(!k) <- (r * 8) + c;
        incr k)
      (if s mod 2 = 0 then List.rev cells else cells)
  done;
  z

let extend (v : int) (s : int) : int = if s = 0 then 0 else if v < 1 lsl (s - 1) then v - (1 lsl s) + 1 else v

(*****************************************************************************)
(* Reading a scan's bits *)
(*****************************************************************************)

type reader = {
  s : string;
  mutable pos : int;
  mutable buf : int;
  mutable cnt : int;
  (* a marker reached: no more data bits, only zeros *)
  mutable marker : bool;
}

(* the next byte of data: FF 00 is FF; FF and anything else is a
 * marker *)
let next_byte (r : reader) : int =
  if r.marker || r.pos >= String.length r.s then (r.marker <- true; 0)
  else
    let b = Char.code r.s.[r.pos] in
    if b <> 0xFF then (r.pos <- r.pos + 1; b)
    else if r.pos + 1 < String.length r.s && r.s.[r.pos + 1] = '\000' then (r.pos <- r.pos + 2; 0xFF)
    else (r.marker <- true; 0)

(* bits come most significant first *)
let bit (r : reader) : int =
  if r.cnt = 0 then begin
    r.buf <- next_byte r;
    r.cnt <- 8
  end;
  r.cnt <- r.cnt - 1;
  (r.buf lsr r.cnt) land 1

let receive (r : reader) (n : int) : int =
  let v = ref 0 in
  for _ = 1 to n do
    v := (!v lsl 1) lor bit r
  done;
  !v

(* at a restart marker: drop the bits left, skip the marker *)
let restart (r : reader) : unit =
  r.buf <- 0;
  r.cnt <- 0;
  r.marker <- false;
  let s = r.s in
  let rec find p =
    if p + 1 >= String.length s then failwith "JPEG: a restart marker is missing"
    else if s.[p] = '\xFF' && Char.code s.[p + 1] >= 0xD0 && Char.code s.[p + 1] <= 0xD7 then p + 2
    else find (p + 1)
  in
  r.pos <- find r.pos

(*****************************************************************************)
(* A block *)
(*****************************************************************************)

let decode_block (r : reader) ~(dc : Huffman.t) ~(ac : Huffman.t) ~(q : int array) (comp : component) :
    float array =
  let coefs = Array.make 64 0. in
  let t = Huffman.decode (fun () -> bit r) dc in
  if t > 11 then failwith "JPEG: a DC difference of more than 11 bits";
  comp.pred <- comp.pred + extend (receive r t) t;
  coefs.(0) <- float (comp.pred * q.(0));
  let rec ac_loop k =
    if k < 64 then begin
      let rs = Huffman.decode (fun () -> bit r) ac in
      let run = rs lsr 4 and size = rs land 15 in
      if size = 0 then (if run = 15 then ac_loop (k + 16) (* else: the end of the block *))
      else begin
        let k = k + run in
        if k > 63 then failwith "JPEG: a coefficient past the end of the block";
        coefs.(zigzag.(k)) <- float (extend (receive r size) size * q.(k));
        ac_loop (k + 1)
      end
    end
  in
  ac_loop 1;
  coefs

(* the block's samples, level-shifted back to 0..255, at block (bx, by)
 * of the component's plane *)
let put_block (comp : component) ~(bx : int) ~(by : int) (samples : float array) : unit =
  for y = 0 to 7 do
    for x = 0 to 7 do
      let v = int_of_float (Float.round (samples.((y * 8) + x) +. 128.)) in
      Bytes.set comp.plane ((((by * 8) + y) * comp.pw) + (bx * 8) + x) (Char.chr (max 0 (min 255 v)))
    done
  done

(*****************************************************************************)
(* Upsampling and color *)
(*****************************************************************************)

(* the component's samples at full size, width x height, as floats:
 * each axis scaled by hmax / h (or vmax / v) *)
let upsample (upsampling : [ `Box | `Triangle ]) (f : frame) (comp : component) : float array =
  if f.hmax mod comp.h <> 0 || f.vmax mod comp.v <> 0 then failwith "JPEG: sampling factors that don't divide";
  let fx = f.hmax / comp.h and fy = f.vmax / comp.v in
  let sample x y = float (Char.code (Bytes.get comp.plane ((y * comp.pw) + x))) in
  (* where output [o] takes its samples along an axis scaled by
   * [factor], [n] samples in the picture: a list of (index, weight) *)
  let taps factor n o =
    if factor = 2 && upsampling = `Triangle then
      let j = o / 2 in
      let other = if o mod 2 = 0 then max 0 (j - 1) else min (n - 1) (j + 1) in
      [ (min (n - 1) j, 0.75); (other, 0.25) ]
    else [ (min (n - 1) (o / factor), 1.) ]
  in
  (* vertically, into a (pw x height) array, then horizontally *)
  let tall = Array.make (comp.pw * f.height) 0. in
  for y = 0 to f.height - 1 do
    let ts = taps fy comp.ch y in
    for x = 0 to comp.pw - 1 do
      tall.((y * comp.pw) + x) <- List.fold_left (fun acc (j, w) -> acc +. (w *. sample x j)) 0. ts
    done
  done;
  let full = Array.make (f.width * f.height) 0. in
  for x = 0 to f.width - 1 do
    let ts = taps fx comp.cw x in
    for y = 0 to f.height - 1 do
      full.((y * f.width) + x) <- List.fold_left (fun acc (i, w) -> acc +. (w *. tall.((y * comp.pw) + i))) 0. ts
    done
  done;
  full

let to_rgba (upsampling : [ `Box | `Triangle ]) (f : frame) : Rgba_image.t =
  let img = Rgba_image.create ~width:f.width ~height:f.height in
  let planes = Array.map (upsample upsampling f) f.comps in
  let clamp v = max 0 (min 255 (int_of_float (Float.round v))) in
  for i = 0 to (f.width * f.height) - 1 do
    let r, g, b =
      if Array.length planes = 1 then
        let y = planes.(0).(i) in
        (y, y, y)
      else
        let y = planes.(0).(i) and cb = planes.(1).(i) -. 128. and cr = planes.(2).(i) -. 128. in
        (y +. (1.402 *. cr), y -. (0.344136 *. cb) -. (0.714136 *. cr), y +. (1.772 *. cb))
    in
    img.rgba.{i * 4} <- clamp r;
    img.rgba.{(i * 4) + 1} <- clamp g;
    img.rgba.{(i * 4) + 2} <- clamp b;
    img.rgba.{(i * 4) + 3} <- 255
  done;
  img

(*****************************************************************************)
(* Segments *)
(*****************************************************************************)

let u16 (s : string) (i : int) : int = (Char.code s.[i] lsl 8) lor Char.code s.[i + 1]

let parse_frame (seg : string) : frame =
  let byte i = Char.code seg.[i] in
  if byte 0 <> 8 then failwith (Printf.sprintf "JPEG: %d-bit samples, not supported" (byte 0));
  let height = u16 seg 1 and width = u16 seg 3 and n = byte 5 in
  if width = 0 || height = 0 then failwith "JPEG: empty picture (or a height given later, not supported)";
  if n = 4 then failwith "JPEG: CMYK (4 components), not supported";
  if n <> 1 && n <> 3 then failwith (Printf.sprintf "JPEG: %d components" n);
  let specs = Array.init n (fun k -> (byte (6 + (3 * k)), byte (7 + (3 * k)) lsr 4, byte (7 + (3 * k)) land 15, byte (8 + (3 * k)))) in
  let hmax = Array.fold_left (fun m (_, h, _, _) -> max m h) 1 specs in
  let vmax = Array.fold_left (fun m (_, _, v, _) -> max m v) 1 specs in
  let mcux = (width + (8 * hmax) - 1) / (8 * hmax) and mcuy = (height + (8 * vmax) - 1) / (8 * vmax) in
  let comps =
    Array.map
      (fun (id, h, v, tq) ->
        if h < 1 || h > 4 || v < 1 || v > 4 || tq > 3 then failwith "JPEG: bad sampling factors or table";
        let pw = mcux * h * 8 and ph = mcuy * v * 8 in
        { id; h; v; tq; td = 0; ta = 0; pred = 0; plane = Bytes.make (pw * ph) '\000'; pw; ph;
          cw = ((width * h) + hmax - 1) / hmax; ch = ((height * v) + vmax - 1) / vmax })
      specs
  in
  { width; height; comps; hmax; vmax; mcux; mcuy }

(* the scan starting at [pos], just after its header: returns where its
 * data ends, the next marker *)
let decode_scan (s : string) (pos : int) (f : frame) (scomps : component list) ~dc ~ac ~qt ~restart_interval ~idct : int =
  let r = { s; pos; buf = 0; cnt = 0; marker = false } in
  let table (tables : Huffman.t option array) i =
    match tables.(i) with Some t -> t | None -> failwith "JPEG: a Huffman table used but not defined"
  in
  let block comp ~bx ~by =
    let q = match qt.(comp.tq) with Some q -> q | None -> failwith "JPEG: a quantization table used but not defined" in
    put_block comp ~bx ~by (idct (decode_block r ~dc:(table dc comp.td) ~ac:(table ac comp.ta) ~q comp))
  in
  List.iter (fun c -> c.pred <- 0) scomps;
  (* the MCUs of the scan: with one component, a block each, over its
   * own size; else the frame's MCUs *)
  let units_across, units_down, unit =
    match scomps with
    | [ c ] -> ((c.cw + 7) / 8, (c.ch + 7) / 8, fun ux uy -> block c ~bx:ux ~by:uy)
    | _ ->
        ( f.mcux, f.mcuy,
          fun ux uy ->
            List.iter
              (fun c ->
                for by = 0 to c.v - 1 do
                  for bx = 0 to c.h - 1 do
                    block c ~bx:((ux * c.h) + bx) ~by:((uy * c.v) + by)
                  done
                done)
              scomps )
  in
  let total = units_across * units_down in
  for n = 0 to total - 1 do
    if restart_interval > 0 && n > 0 && n mod restart_interval = 0 then begin
      restart r;
      List.iter (fun c -> c.pred <- 0) scomps
    end;
    unit (n mod units_across) (n / units_across)
  done;
  (* the next marker: FF and neither 00 nor a restart *)
  let rec next p =
    if p + 1 >= String.length s then String.length s
    else if s.[p] = '\xFF' && s.[p + 1] <> '\000' && s.[p + 1] <> '\xFF'
            && not (Char.code s.[p + 1] >= 0xD0 && Char.code s.[p + 1] <= 0xD7)
    then p
    else next (p + 1)
  in
  next r.pos

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let decode ?(idct = Dct.idct_aan) ?(upsampling = `Triangle) (s : string) : Rgba_image.t =
  let len = String.length s in
  if len < 4 || s.[0] <> '\xFF' || s.[1] <> '\xD8' then failwith "JPEG: not a JPEG (no SOI)";
  let qt = Array.make 4 None and dc = Array.make 4 None and ac = Array.make 4 None in
  let frame = ref None and restart_interval = ref 0 and scans = ref 0 in
  let rec loop pos =
    if pos + 1 >= len then () (* no EOI: accepted, as viewers do *)
    else if s.[pos] <> '\xFF' then failwith (Printf.sprintf "JPEG: a marker expected at %d" pos)
    else
      let m = Char.code s.[pos + 1] in
      if m = 0xFF then loop (pos + 1) (* fill bytes *)
      else if m = 0xD9 then () (* EOI *)
      else if m = 0xD8 || m = 0x01 || (m >= 0xD0 && m <= 0xD7) then loop (pos + 2)
      else begin
        if pos + 4 > len then failwith "JPEG: the file ends early";
        let seglen = u16 s (pos + 2) in
        if pos + 2 + seglen > len then failwith "JPEG: the file ends inside a segment";
        let seg = String.sub s (pos + 4) (seglen - 2) in
        let byte i = Char.code seg.[i] in
        let after = pos + 2 + seglen in
        match m with
        | 0xDB ->
            let rec tables i =
              if i < String.length seg then begin
                let precision = byte i lsr 4 and id = byte i land 3 in
                let size = if precision = 0 then 1 else 2 in
                qt.(id) <-
                  Some (Array.init 64 (fun k -> if size = 1 then byte (i + 1 + k) else u16 seg (i + 1 + (2 * k))));
                tables (i + 1 + (64 * size))
              end
            in
            tables 0;
            loop after
        | 0xC4 ->
            let rec tables i =
              if i < String.length seg then begin
                let cls = byte i lsr 4 and id = byte i land 3 in
                let counts = Array.init 16 (fun k -> byte (i + 1 + k)) in
                let n = Array.fold_left ( + ) 0 counts in
                let symbols = Array.init n (fun k -> byte (i + 17 + k)) in
                (if cls = 0 then dc else ac).(id) <- Some (Huffman.of_counts counts symbols);
                tables (i + 17 + n)
              end
            in
            tables 0;
            loop after
        | 0xC0 | 0xC1 ->
            frame := Some (parse_frame seg);
            loop after
        | 0xC2 | 0xC6 | 0xCA | 0xCE -> failwith "JPEG: progressive, not supported"
        | 0xC3 | 0xC7 | 0xCB | 0xCF -> failwith "JPEG: lossless, not supported"
        | 0xC5 | 0xC9 | 0xCD -> failwith "JPEG: hierarchical or arithmetic-coded, not supported"
        | 0xDD ->
            restart_interval := u16 seg 0;
            loop after
        | 0xDA ->
            let f = match !frame with Some f -> f | None -> failwith "JPEG: a scan before the frame (SOF)" in
            let n = byte 0 in
            let scomps =
              List.init n (fun k ->
                  let id = byte (1 + (2 * k)) and tables = byte (2 + (2 * k)) in
                  match List.find_opt (fun c -> c.id = id) (Array.to_list f.comps) with
                  | Some c ->
                      c.td <- tables lsr 4;
                      c.ta <- tables land 15;
                      c
                  | None -> failwith "JPEG: a scan of an unknown component")
            in
            incr scans;
            loop (decode_scan s after f scomps ~dc ~ac ~qt ~restart_interval:!restart_interval ~idct)
        | _ -> loop after (* APPn, COM, ...: skipped *)
      end
  in
  loop 2;
  match !frame with
  | None -> failwith "JPEG: no frame (SOF)"
  | Some _ when !scans = 0 -> failwith "JPEG: no scan, no pixels"
  | Some f -> to_rgba upsampling f
