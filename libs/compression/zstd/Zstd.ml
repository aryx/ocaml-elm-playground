(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Zstd.mli *)

let fail (what : string) = failwith ("Zstd: " ^ what)

let byte (s : string) (i : int) : int = if i < String.length s then Char.code s.[i] else fail "the data ends early"

(* [n] bytes, little-endian *)
let le (s : string) (pos : int) (n : int) : int =
  let v = ref 0 in
  for i = n - 1 downto 0 do
    v := (!v lsl 8) lor byte s (pos + i)
  done;
  !v

let sub (s : string) (pos : int) (len : int) : string =
  if pos + len > String.length s then fail "the data ends early";
  String.sub s pos len

(* what a frame's blocks share: a block may use the tables of the one
 * before, and the three last distances *)
type frame = {
  mutable huffman : Huffman.t option;
  mutable lengths : Fse.t option;
  mutable offsets : Fse.t option;
  mutable matches : Fse.t option;
  mutable rep1 : int;
  mutable rep2 : int;
  mutable rep3 : int;
}

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

(* the weights of the literals' Huffman code, 1 to 255 of them, the
 * last one left out *)
let weights (s : string) (pos : int) : int list * int =
  let header = byte s pos in
  if header >= 128 then begin
    (* as they are, 4 bits each *)
    let n = header - 127 in
    let weight i =
      let b = byte s (pos + 1 + (i / 2)) in
      if i land 1 = 0 then b lsr 4 else b land 15
    in
    (List.init n weight, pos + 1 + ((n + 1) / 2))
  end
  else begin
    (* FSE-coded, [header] bytes: two states taking turns, until the
     * stream has no bit left for the one just used; the other still
     * holds a weight *)
    let table, p = Fse.read_distribution s ~pos:(pos + 1) ~max_accuracy:6 in
    let r = Fse.backward s ~pos:p ~len:(pos + 1 + header - p) in
    let a = Fse.start table r in
    let b = Fse.start table r in
    let rec turns a b acc n =
      if n > 255 then fail "more than 255 Huffman weights";
      let acc = Fse.symbol table a :: acc in
      let a = Fse.next table r a in
      if Fse.left r < 0 then List.rev (Fse.symbol table b :: acc) else turns b a acc (n + 1)
    in
    (turns a b [] 0, pos + 1 + header)
  end

let huffman_code (s : string) (pos : int) : Huffman.t * int =
  let weights, pos = weights s pos in
  (* a weight w is a code of [max_bits + 1 - w] bits, which takes
   * 2^(w - 1) of the 2^max_bits there are; the last weight is the one
   * that takes what the others left *)
  let sum = List.fold_left (fun sum w -> if w > 0 then sum + (1 lsl (w - 1)) else sum) 0 weights in
  if sum = 0 then fail "Huffman weights all zero";
  let max_bits = Fse.log2 sum + 1 in
  if max_bits > 11 then fail "a Huffman code longer than 11 bits";
  let rest = (1 lsl max_bits) - sum in
  if rest land (rest - 1) <> 0 then fail "Huffman weights that no last weight completes";
  let weights = weights @ [ Fse.log2 rest + 1 ] in
  if List.length weights > 256 then fail "more than 256 Huffman weights";
  (* zstd's code is DEFLATE's in a mirror (Zstd.mli): the alphabet
   * backwards here, the bits flipped in [huffman_stream] *)
  let lengths = Array.make 256 0 in
  List.iteri (fun sym w -> if w > 0 then lengths.(255 - sym) <- max_bits + 1 - w) weights;
  (Huffman.of_lengths lengths, pos)

(* [n] literals from the [len] bytes at [pos], read backwards *)
let huffman_stream (code : Huffman.t) (s : string) ~(pos : int) ~(len : int) (out : Buffer.t) (n : int) : unit =
  let r = Fse.backward s ~pos ~len in
  for _ = 1 to n do
    Buffer.add_char out (Char.chr (255 - Huffman.decode (fun () -> 1 - Fse.bits r 1) code))
  done;
  if Fse.left r <> 0 then fail "a Huffman stream not read to its first bit"

(* the literals of the block at [pos], and where its sequences start *)
let literals (f : frame) (s : string) (pos : int) : string * int =
  let b0 = byte s pos in
  let kind = b0 land 3 and format = (b0 lsr 2) land 3 in
  (* the header's sizes are fields of bits, their widths set by
   * [format] *)
  let field ~from n = Fse.field s ~bit:((pos * 8) + from) n in
  if kind < 2 then begin
    (* 0 raw, the bytes as they are; 1 RLE, one byte repeated *)
    let size, header =
      match format with
      | 0 | 2 -> (field ~from:3 5, 1)
      | 1 -> (field ~from:4 12, 2)
      | _ -> (field ~from:4 20, 3)
    in
    let pos = pos + header in
    if kind = 0 then (sub s pos size, pos + size) else (String.make size (Char.chr (byte s pos)), pos + 1)
  end
  else begin
    (* 2 Huffman codes, their description first; 3 the codes of the
     * block before *)
    let width, header =
      match format with
      | 0 | 1 -> (10, 3)
      | 2 -> (14, 4)
      | _ -> (18, 5)
    in
    let size = field ~from:4 width and compressed = field ~from:(4 + width) width in
    let pos = pos + header in
    let stop = pos + compressed in
    let code, pos =
      if kind = 2 then begin
        let code, pos = huffman_code s pos in
        f.huffman <- Some code;
        (code, pos)
      end
      else
        match f.huffman with
        | Some code -> (code, pos)
        | None -> fail "the Huffman code of the block before, in a first block"
    in
    let out = Buffer.create size in
    if format = 0 then huffman_stream code s ~pos ~len:(stop - pos) out size
    else begin
      (* four streams, each a quarter of the literals, so that a
       * processor decodes the four at once: the sizes of the first
       * three, then the streams *)
      let quarter = (size + 3) / 4 in
      let sizes = List.init 3 (fun i -> le s (pos + (2 * i)) 2) in
      let pos = pos + 6 in
      let last = stop - pos - List.fold_left ( + ) 0 sizes in
      if size < 3 * quarter then fail "four streams for less than four literals";
      let _stop =
        List.fold_left2
          (fun pos len n ->
            huffman_stream code s ~pos ~len out n;
            pos + len)
          pos (sizes @ [ last ])
          [ quarter; quarter; quarter; size - (3 * quarter) ]
      in
      ()
    end;
    (Buffer.contents out, stop)
  end

(*****************************************************************************)
(* Sequences *)
(*****************************************************************************)

(* the extra bits of each code of a literals' length and of a match's
 * length; a code's base is where the code before ends *)
let length_extra = Array.append (Array.make 16 0) [| 1; 1; 1; 1; 2; 2; 3; 3; 4; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16 |]
let match_extra = Array.append (Array.make 32 0) [| 1; 1; 1; 1; 2; 2; 3; 3; 4; 4; 5; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16 |]

let bases ~(first : int) (extra : int array) : int array =
  let base = Array.make (Array.length extra) first in
  for code = 1 to Array.length extra - 1 do
    base.(code) <- base.(code - 1) + (1 lsl extra.(code - 1))
  done;
  base

let length_base = bases ~first:0 length_extra
let match_base = bases ~first:3 match_extra

(* the tables of a block that sends none: what Yann Collet measured on
 * his files *)
let default_lengths : Fse.t Lazy.t =
  lazy
    (Fse.of_distribution ~accuracy:6
       [| 4; 3; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 2; 1; 1; 1; 1; 1; -1; -1; -1; -1 |])

let default_offsets : Fse.t Lazy.t =
  lazy
    (Fse.of_distribution ~accuracy:5
       [| 1; 1; 1; 1; 1; 1; 2; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; -1; -1; -1; -1; -1 |])

let default_matches : Fse.t Lazy.t =
  lazy
    (Fse.of_distribution ~accuracy:6
       (Array.concat [ [| 1; 4; 3; 2; 2; 2; 2; 2; 2 |]; Array.make 37 1; Array.make 7 (-1) ]))

(* one of the three tables, said by 2 bits *)
let table (mode : int) (s : string) (pos : int) ~(default : Fse.t Lazy.t) ~(max_accuracy : int) ~(before : Fse.t option) :
    Fse.t * int =
  match mode with
  | 0 -> (Lazy.force default, pos)
  | 1 -> (Fse.single (byte s pos), pos + 1)
  | 2 -> Fse.read_distribution s ~pos ~max_accuracy
  | _ -> (
      match before with
      | Some t -> (t, pos)
      | None -> fail "the table of the block before, in a first block")

(* the distance an offset's value says. Above 3, a new distance, the
 * value less 3. 1, 2 and 3 are the last distance used, the one before
 * and the one before that -- and when no literal precedes the match,
 * the one before, the one before that and the last less one: the last
 * distance again would have been the match before going on. The one
 * used becomes the last. *)
let distance (f : frame) (value : int) ~(literals : int) : int =
  (* which of the three (0: none, a new one; 4: the last less one) *)
  let which = if value > 3 then 0 else if literals = 0 then value + 1 else value in
  let d =
    match which with
    | 0 -> value - 3
    | 1 -> f.rep1
    | 2 -> f.rep2
    | 3 -> f.rep3
    | _ -> f.rep1 - 1
  in
  if d = 0 then fail "a distance of zero";
  if which <> 1 then begin
    if which <> 2 then f.rep3 <- f.rep2;
    f.rep2 <- f.rep1;
    f.rep1 <- d
  end;
  d

(* the sequences from [pos] to [stop], run over the block's literals *)
let sequences (f : frame) (s : string) ~(pos : int) ~(stop : int) (literals : string) (out : Buffer.t) ~(start : int) : unit =
  let b0 = byte s pos in
  let n, pos =
    if b0 < 128 then (b0, pos + 1)
    else if b0 < 255 then (((b0 - 128) lsl 8) + byte s (pos + 1), pos + 2)
    else (le s (pos + 1) 2 + 0x7F00, pos + 3)
  in
  (* how many literals the sequences have used *)
  let used = ref 0 in
  if n > 0 then begin
    let modes = byte s pos in
    let lengths, pos = table (modes lsr 6) s (pos + 1) ~default:default_lengths ~max_accuracy:9 ~before:f.lengths in
    let offsets, pos = table ((modes lsr 4) land 3) s pos ~default:default_offsets ~max_accuracy:8 ~before:f.offsets in
    let matches, pos = table ((modes lsr 2) land 3) s pos ~default:default_matches ~max_accuracy:9 ~before:f.matches in
    f.lengths <- Some lengths;
    f.offsets <- Some offsets;
    f.matches <- Some matches;
    (* one stream for the three codes and their extra bits *)
    let r = Fse.backward s ~pos ~len:(stop - pos) in
    let ls = ref (Fse.start lengths r) in
    let os = ref (Fse.start offsets r) in
    let ms = ref (Fse.start matches r) in
    for i = 1 to n do
      let lc = Fse.symbol lengths !ls and oc = Fse.symbol offsets !os and mc = Fse.symbol matches !ms in
      if lc >= Array.length length_base || mc >= Array.length match_base || oc > 30 then fail "a code out of range";
      (* an offset's code is its number of extra bits *)
      let value = (1 lsl oc) + Fse.bits r oc in
      let len = match_base.(mc) + Fse.bits r match_extra.(mc) in
      let lits = length_base.(lc) + Fse.bits r length_extra.(lc) in
      if i < n then begin
        ls := Fse.next lengths r !ls;
        ms := Fse.next matches r !ms;
        os := Fse.next offsets r !os
      end;
      let d = distance f value ~literals:lits in
      (* "[lits] literals, then [len] bytes from [d] back" *)
      if !used + lits > String.length literals then fail "more literals used than the block has";
      Buffer.add_substring out literals !used lits;
      used := !used + lits;
      let from = Buffer.length out - d in
      if from < start then fail "a distance further back than the data";
      (* one byte at a time: the copy may read what it just wrote
       * (Inflate.mli) *)
      for j = 0 to len - 1 do
        Buffer.add_char out (Buffer.nth out (from + j))
      done
    done;
    if Fse.left r <> 0 then fail "the sequences not read to their first bit"
  end
  else if pos <> stop then fail "bytes after no sequence";
  (* the literals after the last match *)
  Buffer.add_substring out literals !used (String.length literals - !used)

(*****************************************************************************)
(* Frames *)
(*****************************************************************************)

let magic = "\x28\xB5\x2F\xFD"

(* the frame at [pos] decoded into [out]; where it ends *)
let frame (s : string) (pos : int) (out : Buffer.t) : int =
  let descriptor = byte s (pos + 4) in
  if descriptor land 8 <> 0 then fail "a reserved bit set";
  let single_segment = descriptor land 32 <> 0 in
  (* past the window's size, which only a decoder short of memory
   * needs: we keep the whole output *)
  let pos = pos + if single_segment then 5 else 6 in
  let dictionary = [| 0; 1; 2; 4 |].(descriptor land 3) in
  if le s pos dictionary <> 0 then fail "a frame that needs a dictionary";
  let pos = pos + dictionary in
  let size =
    match descriptor lsr 6 with
    | 0 -> if single_segment then Some (byte s pos) else None
    | 1 -> Some (le s pos 2 + 256)
    | 2 -> Some (le s pos 4)
    | _ -> Some (le s pos 8)
  in
  let pos = pos + [| (if single_segment then 1 else 0); 2; 4; 8 |].(descriptor lsr 6) in
  let start = Buffer.length out in
  let f = { huffman = None; lengths = None; offsets = None; matches = None; rep1 = 1; rep2 = 4; rep3 = 8 } in
  let rec blocks pos =
    let header = le s pos 3 in
    let size = header lsr 3 and pos = pos + 3 in
    let pos =
      match (header lsr 1) land 3 with
      | 0 ->
          Buffer.add_string out (sub s pos size);
          pos + size
      | 1 ->
          Buffer.add_string out (String.make size (Char.chr (byte s pos)));
          pos + 1
      | 2 ->
          let stop = pos + size in
          if stop > String.length s then fail "the data ends early";
          let lits, pos = literals f s pos in
          if pos > stop then fail "literals longer than their block";
          sequences f s ~pos ~stop lits out ~start;
          stop
      | _ -> fail "block type 3 doesn't exist"
    in
    if header land 1 = 0 then blocks pos else pos
  in
  let pos = blocks pos in
  let content = Buffer.sub out start (Buffer.length out - start) in
  (match size with
  | Some size when size <> String.length content -> fail "not the length the frame says"
  | _ -> ());
  if descriptor land 4 = 0 then pos
  else begin
    (* the low half of XXH64, little-endian *)
    if String.get_int32_le (sub s pos 4) 0 <> Int64.to_int32 (Xxhash.xxh64 content) then fail "wrong checksum, the data is corrupt";
    pos + 4
  end

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let decompress (s : string) : string =
  let n = String.length s in
  if n = 0 then fail "no frame";
  let out = Buffer.create (4 * n) in
  let rec frames pos =
    if pos < n then
      if pos + 4 <= n && String.sub s pos 4 = magic then frames (frame s pos out)
      else if byte s pos land 0xF0 = 0x50 && le s (pos + 1) 3 = 0x184D2A then
        (* a skippable frame, someone's own data: its length, then it *)
        frames (pos + 8 + le s (pos + 4) 4)
      else fail "not a zstd frame (no 28 B5 2F FD)"
    else if pos > n then fail "the data ends early"
  in
  frames 0;
  Buffer.contents out
