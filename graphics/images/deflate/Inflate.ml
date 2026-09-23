(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Inflate.mli *)

(*****************************************************************************)
(* Reading bits *)
(*****************************************************************************)

type input = {
  s : string;
  mutable pos : int;
  (* bits read from s but not used yet, the next one in bit 0 *)
  mutable bitbuf : int;
  mutable bitcnt : int;
}

(* the next [n] bits, the first one read as the least significant *)
let bits (inp : input) (n : int) : int =
  while inp.bitcnt < n do
    if inp.pos >= String.length inp.s then failwith "Inflate: the data ends early";
    inp.bitbuf <- inp.bitbuf lor (Char.code inp.s.[inp.pos] lsl inp.bitcnt);
    inp.pos <- inp.pos + 1;
    inp.bitcnt <- inp.bitcnt + 8
  done;
  let v = inp.bitbuf land ((1 lsl n) - 1) in
  inp.bitbuf <- inp.bitbuf lsr n;
  inp.bitcnt <- inp.bitcnt - n;
  v

let decode (inp : input) (h : Huffman.t) : int = Huffman.decode (fun () -> bits inp 1) h

(*****************************************************************************)
(* Stored blocks *)
(*****************************************************************************)

let stored (inp : input) (out : Buffer.t) : unit =
  (* to the next byte boundary *)
  inp.bitbuf <- 0;
  inp.bitcnt <- 0;
  let s = inp.s and p = inp.pos in
  if p + 4 > String.length s then failwith "Inflate: the data ends early";
  let byte i = Char.code s.[p + i] in
  let len = byte 0 lor (byte 1 lsl 8) and nlen = byte 2 lor (byte 3 lsl 8) in
  if len <> nlen lxor 0xFFFF then failwith "Inflate: stored block, LEN is not the complement of NLEN";
  if p + 4 + len > String.length s then failwith "Inflate: the data ends early";
  Buffer.add_substring out s (p + 4) len;
  inp.pos <- p + 4 + len

(*****************************************************************************)
(* Huffman blocks *)
(*****************************************************************************)

(* lengths for the codes 257..285: a base, and how many extra bits to
 * add to it *)
let length_base =
  [| 3; 4; 5; 6; 7; 8; 9; 10; 11; 13; 15; 17; 19; 23; 27; 31; 35; 43; 51; 59;
     67; 83; 99; 115; 131; 163; 195; 227; 258 |]
let length_extra =
  [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4;
     5; 5; 5; 5; 0 |]

(* distances for the codes 0..29 *)
let dist_base =
  [| 1; 2; 3; 4; 5; 7; 9; 13; 17; 25; 33; 49; 65; 97; 129; 193; 257; 385;
     513; 769; 1025; 1537; 2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577 |]
let dist_extra =
  [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10;
     11; 11; 12; 12; 13; 13 |]

(* decode literals and (length, distance) pairs until the end of the
 * block *)
let codes (inp : input) (out : Buffer.t) ~(lit : Huffman.t) ~(dist : Huffman.t) : unit =
  let rec loop () =
    let sym = decode inp lit in
    if sym < 256 then begin
      Buffer.add_char out (Char.chr sym);
      loop ()
    end
    else if sym > 256 then begin
      let sym = sym - 257 in
      if sym >= 29 then failwith "Inflate: a length code out of range";
      let len = length_base.(sym) + bits inp length_extra.(sym) in
      let dsym = decode inp dist in
      if dsym >= 30 then failwith "Inflate: a distance code out of range";
      let d = dist_base.(dsym) + bits inp dist_extra.(dsym) in
      let start = Buffer.length out - d in
      if start < 0 then failwith "Inflate: a distance further back than the data";
      (* one byte at a time: the copy may read what it just wrote *)
      for i = 0 to len - 1 do
        Buffer.add_char out (Buffer.nth out (start + i))
      done;
      loop ()
    end
    (* 256: the end of the block *)
  in
  loop ()

let fixed_codes : (Huffman.t * Huffman.t) Lazy.t =
  lazy
    (let lengths =
       Array.init 288 (fun sym ->
           if sym < 144 then 8 else if sym < 256 then 9 else if sym < 280 then 7 else 8)
     in
     (Huffman.of_lengths lengths, Huffman.of_lengths (Array.make 30 5)))

(* the order the code length code's lengths are sent in *)
let order = [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

let dynamic_codes (inp : input) : Huffman.t * Huffman.t =
  let nlen = bits inp 5 + 257 in
  let ndist = bits inp 5 + 1 in
  let ncode = bits inp 4 + 4 in
  if nlen > 286 || ndist > 30 then failwith "Inflate: too many length or distance codes";
  let code_lengths = Array.make 19 0 in
  for i = 0 to ncode - 1 do
    code_lengths.(order.(i)) <- bits inp 3
  done;
  let lencode = Huffman.of_lengths code_lengths in
  (* the literal/length and distance lengths, one sequence *)
  let lengths = Array.make (nlen + ndist) 0 in
  let i = ref 0 in
  while !i < nlen + ndist do
    let sym = decode inp lencode in
    if sym < 16 then begin
      lengths.(!i) <- sym;
      incr i
    end
    else begin
      let value, times =
        match sym with
        | 16 ->
            if !i = 0 then failwith "Inflate: a repeat with nothing to repeat";
            (lengths.(!i - 1), 3 + bits inp 2)
        | 17 -> (0, 3 + bits inp 3)
        | _ -> (0, 11 + bits inp 7)
      in
      if !i + times > nlen + ndist then failwith "Inflate: too many code lengths";
      for _ = 1 to times do
        lengths.(!i) <- value;
        incr i
      done
    end
  done;
  if lengths.(256) = 0 then failwith "Inflate: no code for the end of the block";
  (Huffman.of_lengths (Array.sub lengths 0 nlen), Huffman.of_lengths (Array.sub lengths nlen ndist))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let inflate (s : string) ~(pos : int) : string * int =
  let inp = { s; pos; bitbuf = 0; bitcnt = 0 } in
  let out = Buffer.create (4 * (String.length s - pos)) in
  let rec blocks () =
    let last = bits inp 1 in
    (match bits inp 2 with
    | 0 -> stored inp out
    | 1 ->
        let lit, dist = Lazy.force fixed_codes in
        codes inp out ~lit ~dist
    | 2 ->
        let lit, dist = dynamic_codes inp in
        codes inp out ~lit ~dist
    | _ -> failwith "Inflate: block type 3 doesn't exist");
    if last = 0 then blocks ()
  in
  blocks ();
  (Buffer.contents out, inp.pos)
