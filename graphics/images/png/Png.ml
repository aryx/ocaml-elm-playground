(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Png.mli *)

let signature = "\x89PNG\r\n\x1a\n"

(*****************************************************************************)
(* Chunks *)
(*****************************************************************************)

(* a length, a width or a height: at most 2^31 - 1, says the spec,
 * which fits an int in JavaScript too *)
let u32 (s : string) (i : int) : int =
  let byte k = Char.code s.[i + k] in
  if byte 0 >= 0x80 then failwith "PNG: a number of 2^31 or more";
  (byte 0 lsl 24) lor (byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3

(* a CRC: all 32 bits, an Int32 (see Crc32.mli) *)
let i32 (s : string) (i : int) : int32 =
  let byte k = Char.code s.[i + k] in
  Int32.logor (Int32.shift_left (Int32.of_int (byte 0)) 24) (Int32.of_int ((byte 1 lsl 16) lor (byte 2 lsl 8) lor byte 3))

let chunks (s : string) : (string * string) list =
  if String.length s < 8 || String.sub s 0 8 <> signature then failwith "PNG: not a PNG (bad signature)";
  let rec loop pos acc =
    if pos + 12 > String.length s then failwith "PNG: the file ends before IEND";
    let len = u32 s pos in
    if pos + 12 + len > String.length s then failwith "PNG: the file ends inside a chunk";
    let typ = String.sub s (pos + 4) 4 in
    if Crc32.update 0l s ~pos:(pos + 4) ~len:(4 + len) <> i32 s (pos + 8 + len) then
      failwith (Printf.sprintf "PNG: bad CRC in chunk %s, the file is corrupt" typ);
    let acc = (typ, String.sub s (pos + 8) len) :: acc in
    if typ = "IEND" then List.rev acc else loop (pos + 12 + len) acc
  in
  loop 8 []

(*****************************************************************************)
(* Filters *)
(*****************************************************************************)

let paeth (a : int) (b : int) (c : int) : int =
  let p = a + b - c in
  let pa = abs (p - a) and pb = abs (p - b) and pc = abs (p - c) in
  if pa <= pb && pa <= pc then a else if pb <= pc then b else c

(* what [filter] predicts from left, up and up-left *)
let predict (filter : int) (a : int) (b : int) (c : int) : int =
  match filter with
  | 0 -> 0
  | 1 -> a
  | 2 -> b
  | 3 -> (a + b) / 2
  | 4 -> paeth a b c
  | f -> failwith (Printf.sprintf "PNG: unknown filter %d" f)

(* undo row [cur]'s filter in place; [prev] is the row above, already
 * unfiltered (zeros above the first row); [bpp] the bytes a pixel, at
 * least 1, which is how far back "left" is *)
let unfilter (filter : int) ~(bpp : int) ~(prev : Bytes.t) (cur : Bytes.t) : unit =
  let get r i = Char.code (Bytes.get r i) in
  for i = 0 to Bytes.length cur - 1 do
    let x = get cur i in
    let a = if i >= bpp then get cur (i - bpp) else 0 in
    let b = get prev i in
    let c = if i >= bpp then get prev (i - bpp) else 0 in
    Bytes.set cur i (Char.chr ((x + predict filter a b c) land 0xFF))
  done

(* the other way: row [cur] filtered by [filter], into [out] *)
let filter_row (filter : int) ~(bpp : int) ~(prev : Bytes.t) (cur : Bytes.t) (out : Bytes.t) : unit =
  let get r i = Char.code (Bytes.unsafe_get r i) in
  for i = 0 to Bytes.length cur - 1 do
    let a = if i >= bpp then get cur (i - bpp) else 0 in
    let c = if i >= bpp then get prev (i - bpp) else 0 in
    Bytes.unsafe_set out i (Char.unsafe_chr ((get cur i - predict filter a (get prev i) c) land 0xFF))
  done

(*****************************************************************************)
(* Pixels *)
(*****************************************************************************)

type header = {
  width : int;
  height : int;
  depth : int;
  color_type : int;
  interlace : bool;
}

let channels (h : header) : int =
  match h.color_type with 0 -> 1 | 2 -> 3 | 3 -> 1 | 4 -> 2 | _ -> 4

let parse_header (data : string) : header =
  if String.length data <> 13 then failwith "PNG: bad IHDR";
  let byte i = Char.code data.[i] in
  let h =
    { width = u32 data 0; height = u32 data 4; depth = byte 8; color_type = byte 9;
      interlace = byte 12 = 1 }
  in
  let depths =
    match h.color_type with
    | 0 -> [ 1; 2; 4; 8; 16 ]
    | 3 -> [ 1; 2; 4; 8 ]
    | 2 | 4 | 6 -> [ 8; 16 ]
    | t -> failwith (Printf.sprintf "PNG: unknown color type %d" t)
  in
  if not (List.mem h.depth depths) then
    failwith (Printf.sprintf "PNG: bit depth %d with color type %d" h.depth h.color_type);
  if byte 10 <> 0 || byte 11 <> 0 || byte 12 > 1 then failwith "PNG: unknown compression, filter or interlace method";
  if h.width = 0 || h.height = 0 then failwith "PNG: empty picture";
  h

(* Adam7's passes: (x start, y start, x step, y step) *)
let adam7 = [ (0, 0, 8, 8); (4, 0, 8, 8); (0, 4, 4, 8); (2, 0, 4, 4); (0, 2, 2, 4); (1, 0, 2, 2); (0, 1, 1, 2) ]

let decode (s : string) : Rgba_image.t =
  let chunks = chunks s in
  let h =
    match chunks with
    | ("IHDR", data) :: _ -> parse_header data
    | _ -> failwith "PNG: IHDR is not the first chunk"
  in
  let palette = ref "" and trns = ref None and idat = Buffer.create (String.length s) in
  List.iter
    (fun (typ, data) ->
      match typ with
      | "IHDR" | "IEND" -> ()
      | "PLTE" -> palette := data
      | "tRNS" -> trns := Some data
      | "IDAT" -> Buffer.add_string idat data
      | _ when typ.[0] >= 'A' && typ.[0] <= 'Z' -> failwith ("PNG: unknown critical chunk " ^ typ)
      | _ -> ())
    chunks;
  if h.color_type = 3 && !palette = "" then failwith "PNG: a palette picture without PLTE";
  if Buffer.length idat = 0 then failwith "PNG: no IDAT, no pixels";
  let raw = Zlib.decompress (Buffer.contents idat) in
  let n = channels h in
  let bits = n * h.depth in
  let bpp = max 1 (bits / 8) in
  let img = Rgba_image.create ~width:h.width ~height:h.height in
  (* the [k]th sample of a row (channel k mod n of pixel k / n), at its
   * own depth, 16 bits included *)
  let sample (row : Bytes.t) (k : int) : int =
    let byte i = Char.code (Bytes.get row i) in
    match h.depth with
    | 8 -> byte k
    | 16 -> (byte (2 * k) lsl 8) lor byte ((2 * k) + 1)
    | d ->
        let bit = k * d in
        (byte (bit / 8) lsr (8 - d - (bit mod 8))) land ((1 lsl d) - 1)
  in
  (* to 0..255 *)
  let to8 v = match h.depth with 16 -> v lsr 8 | 8 -> v | d -> v * 255 / ((1 lsl d) - 1) in
  (* the transparent gray or RGB value, at the file's depth *)
  let key =
    match !trns with
    | Some t when (h.color_type = 0 && String.length t >= 2) || (h.color_type = 2 && String.length t >= 6) ->
        let u16 i = (Char.code t.[i] lsl 8) lor Char.code t.[i + 1] in
        Some (if h.color_type = 0 then [ u16 0 ] else [ u16 0; u16 2; u16 4 ])
    | _ -> None
  in
  let set x y r g b a =
    let o = ((y * h.width) + x) * 4 in
    img.rgba.{o} <- r;
    img.rgba.{o + 1} <- g;
    img.rgba.{o + 2} <- b;
    img.rgba.{o + 3} <- a
  in
  let pixel row i x y =
    let v k = sample row ((i * n) + k) in
    match h.color_type with
    | 0 ->
        let g = v 0 in
        set x y (to8 g) (to8 g) (to8 g) (if key = Some [ g ] then 0 else 255)
    | 2 ->
        let r = v 0 and g = v 1 and b = v 2 in
        set x y (to8 r) (to8 g) (to8 b) (if key = Some [ r; g; b ] then 0 else 255)
    | 3 ->
        let idx = v 0 in
        if (3 * idx) + 2 >= String.length !palette then failwith "PNG: a palette index out of the palette";
        let c k = Char.code !palette.[(3 * idx) + k] in
        let a = match !trns with Some t when idx < String.length t -> Char.code t.[idx] | _ -> 255 in
        set x y (c 0) (c 1) (c 2) a
    | 4 ->
        let g = to8 (v 0) in
        set x y g g g (to8 (v 1))
    | _ -> set x y (to8 (v 0)) (to8 (v 1)) (to8 (v 2)) (to8 (v 3))
  in
  let passes = if h.interlace then adam7 else [ (0, 0, 1, 1) ] in
  let pos = ref 0 in
  List.iter
    (fun (x0, y0, dx, dy) ->
      let pw = (h.width - x0 + dx - 1) / dx and ph = (h.height - y0 + dy - 1) / dy in
      if pw > 0 && ph > 0 then begin
        let rowbytes = ((pw * bits) + 7) / 8 in
        let prev = ref (Bytes.make rowbytes '\000') in
        for j = 0 to ph - 1 do
          if !pos + 1 + rowbytes > String.length raw then failwith "PNG: not enough pixel data";
          let filter = Char.code raw.[!pos] in
          let cur = Bytes.of_string (String.sub raw (!pos + 1) rowbytes) in
          pos := !pos + 1 + rowbytes;
          unfilter filter ~bpp ~prev:!prev cur;
          for i = 0 to pw - 1 do
            pixel cur i (x0 + (i * dx)) (y0 + (j * dy))
          done;
          prev := cur
        done
      end)
    passes;
  img

let scanlines (s : string) : (int * Bytes.t) list =
  let chunks = chunks s in
  let h = match chunks with ("IHDR", data) :: _ -> parse_header data | _ -> failwith "PNG: IHDR is not the first chunk" in
  if h.interlace then failwith "PNG: scanlines of an interlaced picture";
  let raw = Zlib.decompress (String.concat "" (List.filter_map (fun (t, d) -> if t = "IDAT" then Some d else None) chunks)) in
  let rowbytes = ((h.width * channels h * h.depth) + 7) / 8 in
  if String.length raw < (rowbytes + 1) * h.height then failwith "PNG: not enough pixel data";
  List.init h.height (fun j ->
      let p = j * (rowbytes + 1) in
      (Char.code raw.[p], Bytes.of_string (String.sub raw (p + 1) rowbytes)))

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

(* u32 and i32 the other way *)
let string_of_u32 (n : int) : string = String.init 4 (fun k -> Char.chr ((n lsr (8 * (3 - k))) land 0xFF))
let string_of_i32 (v : int32) : string =
  String.init 4 (fun k -> Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical v (8 * (3 - k))) 0xFFl)))

let chunk (b : Buffer.t) (typ : string) (data : string) : unit =
  let body = typ ^ data in
  Buffer.add_string b (string_of_u32 (String.length data));
  Buffer.add_string b body;
  Buffer.add_string b (string_of_i32 (Crc32.string body))

(* the sum of the filtered bytes read as signed, a guess at how well
 * DEFLATE will do: small differences compress *)
let cost (row : Bytes.t) : int =
  let sum = ref 0 in
  Bytes.iter (fun ch -> let v = Char.code ch in sum := !sum + if v < 128 then v else 256 - v) row;
  !sum

let encode ?(alpha = true) ?filter (img : Rgba_image.t) : string =
  let bpp = if alpha then 4 else 3 in
  let rowbytes = img.width * bpp in
  let raw = Buffer.create ((rowbytes + 1) * img.height) in
  let prev = ref (Bytes.make rowbytes '\000') in
  let filtered = Array.init 5 (fun _ -> Bytes.create rowbytes) in
  for y = 0 to img.height - 1 do
    let cur = Bytes.init rowbytes (fun i -> Char.unsafe_chr img.rgba.{(((y * img.width) + (i / bpp)) * 4) + (i mod bpp)}) in
    (* each filter, and the one that costs least *)
    Array.iteri (fun f out -> filter_row f ~bpp ~prev:!prev cur out) filtered;
    let best = ref (match filter with Some f -> f | None -> 0) in
    if filter = None then
      for f = 1 to 4 do
        if cost filtered.(f) < cost filtered.(!best) then best := f
      done;
    Buffer.add_char raw (Char.chr !best);
    Buffer.add_bytes raw filtered.(!best);
    prev := cur
  done;
  let b = Buffer.create (Buffer.length raw / 8) in
  Buffer.add_string b signature;
  chunk b "IHDR"
    (string_of_u32 img.width ^ string_of_u32 img.height ^ String.make 1 '\008'
    ^ String.make 1 (if alpha then '\006' else '\002')
    ^ "\000\000\000");
  chunk b "IDAT" (Zlib.compress (Buffer.contents raw));
  chunk b "IEND" "";
  Buffer.contents b
