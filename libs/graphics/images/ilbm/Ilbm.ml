(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ilbm.mli *)

type range = { low : int; high : int; rate : int; active : bool; reverse : bool }
type t = { width : int; height : int; planes : int; pixels : Bytes.t; palette : (int * int * int) array; ranges : range list }

let steps_per_second (r : range) : float = float_of_int r.rate *. 60. /. 16384.

(* a row of a plane: 16 bits a word, padded *)
let row_bytes (width : int) : int = (width + 15) / 16 * 2

let plane_rows (t : t) (y : int) : Bytes.t list =
  List.init t.planes (fun p ->
      let row = Bytes.make (row_bytes t.width) '\000' in
      for x = 0 to t.width - 1 do
        let colour = Char.code (Bytes.get t.pixels ((y * t.width) + x)) in
        if (colour lsr p) land 1 = 1 then
          let i = x / 8 in
          Bytes.set row i (Char.chr (Char.code (Bytes.get row i) lor (0x80 lsr (x mod 8))))
      done;
      row)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let u16 b v = Buffer.add_char b (Char.chr ((v lsr 8) land 0xff)); Buffer.add_char b (Char.chr (v land 0xff))
let u32 b v = u16 b ((v lsr 16) land 0xffff); u16 b (v land 0xffff)

(* a chunk: its name, its length, its data, and a pad byte to an even length *)
let chunk (b : Buffer.t) (id : string) (data : string) : unit =
  Buffer.add_string b id;
  u32 b (String.length data);
  Buffer.add_string b data;
  if String.length data mod 2 = 1 then Buffer.add_char b '\000'

let encode (t : t) : string =
  let bmhd = Buffer.create 20 in
  u16 bmhd t.width;
  u16 bmhd t.height;
  u16 bmhd 0;
  u16 bmhd 0;
  Buffer.add_char bmhd (Char.chr t.planes);
  Buffer.add_char bmhd '\000' (* no mask *);
  Buffer.add_char bmhd '\001' (* ByteRun1 *);
  Buffer.add_char bmhd '\000';
  u16 bmhd 0 (* the transparent colour *);
  Buffer.add_char bmhd '\010';
  Buffer.add_char bmhd '\011' (* the pixels' aspect, 10:11, low resolution's *);
  u16 bmhd t.width;
  u16 bmhd t.height;
  let cmap = Buffer.create 96 in
  Array.iter (fun (r, g, b) -> List.iter (fun v -> Buffer.add_char cmap (Char.chr v)) [ r; g; b ]) t.palette;
  let body = Buffer.create (t.width * t.height / 2) in
  for y = 0 to t.height - 1 do
    List.iter (fun row -> Buffer.add_bytes body (Packbits.encode row)) (plane_rows t y)
  done;
  let form = Buffer.create 4096 in
  Buffer.add_string form "ILBM";
  chunk form "BMHD" (Buffer.contents bmhd);
  chunk form "CMAP" (Buffer.contents cmap);
  List.iter
    (fun r ->
      let c = Buffer.create 8 in
      u16 c 0;
      u16 c r.rate;
      u16 c ((if r.active then 1 else 0) lor if r.reverse then 2 else 0);
      Buffer.add_char c (Char.chr r.low);
      Buffer.add_char c (Char.chr r.high);
      chunk form "CRNG" (Buffer.contents c))
    t.ranges;
  chunk form "BODY" (Buffer.contents body);
  let file = Buffer.create (Buffer.length form + 8) in
  chunk file "FORM" (Buffer.contents form);
  Buffer.contents file

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let decode (s : string) : t =
  let n = String.length s in
  let byte i = if i < n then Char.code s.[i] else failwith "ILBM: cut short" in
  let get16 i = (byte i lsl 8) lor byte (i + 1) in
  let get32 i = (get16 i lsl 16) lor get16 (i + 2) in
  if n < 12 || String.sub s 0 4 <> "FORM" || String.sub s 8 4 <> "ILBM" then failwith "ILBM: not an IFF ILBM file";
  let width = ref 0 and height = ref 0 and planes = ref 0 and masking = ref 0 and compression = ref 0 in
  let palette = ref [||] and ranges = ref [] and body = ref None in
  (* the chunks, one after the other; the ones we don't know skipped *)
  let rec chunks i =
    if i + 8 <= n then begin
      let id = String.sub s i 4 and len = get32 (i + 4) in
      let data = i + 8 in
      (match id with
      | "BMHD" ->
          width := get16 data;
          height := get16 (data + 2);
          planes := byte (data + 8);
          masking := byte (data + 9);
          compression := byte (data + 10)
      | "CMAP" -> palette := Array.init (len / 3) (fun k -> (byte (data + (3 * k)), byte (data + (3 * k) + 1), byte (data + (3 * k) + 2)))
      | "CRNG" ->
          let flags = get16 (data + 4) in
          ranges := { rate = get16 (data + 2); active = flags land 1 = 1; reverse = flags land 2 = 2; low = byte (data + 6); high = byte (data + 7) } :: !ranges
      | "BODY" -> body := Some (data, len)
      | _ -> ());
      chunks (data + len + (len mod 2))
    end
  in
  chunks 12;
  let w = !width and h = !height and np = !planes in
  if w = 0 || np = 0 then failwith "ILBM: no BMHD";
  let pixels = Bytes.make (w * h) '\000' in
  (match !body with
  | None -> failwith "ILBM: no BODY"
  | Some (start, _) ->
      let rb = row_bytes w in
      let src = Bytes.unsafe_of_string s in
      let pos = ref start in
      let read_row () =
        if !compression = 1 then begin
          let row, next = Packbits.decode src ~pos:!pos ~len:rb in
          pos := next;
          row
        end
        else begin
          let row = Bytes.sub src !pos rb in
          pos := !pos + rb;
          row
        end
      in
      for y = 0 to h - 1 do
        for p = 0 to np - 1 do
          let row = read_row () in
          for x = 0 to w - 1 do
            if Char.code (Bytes.get row (x / 8)) land (0x80 lsr (x mod 8)) <> 0 then
              let i = (y * w) + x in
              Bytes.set pixels i (Char.chr (Char.code (Bytes.get pixels i) lor (1 lsl p)))
          done
        done;
        (* a mask plane, when there is one: read, not kept *)
        if !masking = 1 then ignore (read_row ())
      done);
  { width = w; height = h; planes = np; pixels; palette = !palette; ranges = List.rev !ranges }

let to_rgba (t : t) : Rgba_image.t =
  let img = Rgba_image.create ~width:t.width ~height:t.height in
  for i = 0 to (t.width * t.height) - 1 do
    let c = Char.code (Bytes.get t.pixels i) in
    let r, g, b = if c < Array.length t.palette then t.palette.(c) else (0, 0, 0) in
    Bigarray.Array1.set img.rgba (4 * i) r;
    Bigarray.Array1.set img.rgba ((4 * i) + 1) g;
    Bigarray.Array1.set img.rgba ((4 * i) + 2) b;
    Bigarray.Array1.set img.rgba ((4 * i) + 3) 255
  done;
  img
