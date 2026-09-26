(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Poly1305.mli *)

let m26 = 0x3ffffff
let le32 (s : string) (i : int) : int = Char.code s.[i] lor (Char.code s.[i + 1] lsl 8) lor (Char.code s.[i + 2] lsl 16) lor (Char.code s.[i + 3] lsl 24)

let mac ~(key : string) (message : string) : string =
  (* r, clamped, as five limbs of 26 bits *)
  let r0 = le32 key 0 land 0x3ffffff and r1 = (le32 key 3 lsr 2) land 0x3ffff03 and r2 = (le32 key 6 lsr 4) land 0x3ffc0ff in
  let r3 = (le32 key 9 lsr 6) land 0x3f03fff and r4 = (le32 key 12 lsr 8) land 0x00fffff in
  let s1 = r1 * 5 and s2 = r2 * 5 and s3 = r3 * 5 and s4 = r4 * 5 in
  let h0 = ref 0 and h1 = ref 0 and h2 = ref 0 and h3 = ref 0 and h4 = ref 0 in
  let n = String.length message in
  let rec blocks i =
    if i < n then begin
      (* a block, and its 1 above: 2^128 for a full one; a last short
         one gets its 1 right after its bytes, then zeros *)
      let len = min 16 (n - i) in
      let b = if len = 16 then String.sub message i 16 else String.sub message i len ^ "\001" ^ String.make (15 - len) '\000' in
      let hibit = if len = 16 then 1 lsl 24 else 0 in
      h0 := !h0 + (le32 b 0 land m26);
      h1 := !h1 + ((le32 b 3 lsr 2) land m26);
      h2 := !h2 + ((le32 b 6 lsr 4) land m26);
      h3 := !h3 + ((le32 b 9 lsr 6) land m26);
      h4 := !h4 + ((le32 b 12 lsr 8) lor hibit);
      (* times r, 2^130 folded back as 5 *)
      let d0 = (!h0 * r0) + (!h1 * s4) + (!h2 * s3) + (!h3 * s2) + (!h4 * s1) in
      let d1 = (!h0 * r1) + (!h1 * r0) + (!h2 * s4) + (!h3 * s3) + (!h4 * s2) in
      let d2 = (!h0 * r2) + (!h1 * r1) + (!h2 * r0) + (!h3 * s4) + (!h4 * s3) in
      let d3 = (!h0 * r3) + (!h1 * r2) + (!h2 * r1) + (!h3 * r0) + (!h4 * s4) in
      let d4 = (!h0 * r4) + (!h1 * r3) + (!h2 * r2) + (!h3 * r1) + (!h4 * r0) in
      let c = d0 lsr 26 in
      h0 := d0 land m26;
      let d1 = d1 + c in
      let c = d1 lsr 26 in
      h1 := d1 land m26;
      let d2 = d2 + c in
      let c = d2 lsr 26 in
      h2 := d2 land m26;
      let d3 = d3 + c in
      let c = d3 lsr 26 in
      h3 := d3 land m26;
      let d4 = d4 + c in
      let c = d4 lsr 26 in
      h4 := d4 land m26;
      h0 := !h0 + (c * 5);
      let c = !h0 lsr 26 in
      h0 := !h0 land m26;
      h1 := !h1 + c;
      blocks (i + 16)
    end
  in
  blocks 0;
  (* fully carried, then h - p if that is not negative: h mod p *)
  let c = !h1 lsr 26 in
  h1 := !h1 land m26;
  h2 := !h2 + c;
  let c = !h2 lsr 26 in
  h2 := !h2 land m26;
  h3 := !h3 + c;
  let c = !h3 lsr 26 in
  h3 := !h3 land m26;
  h4 := !h4 + c;
  let c = !h4 lsr 26 in
  h4 := !h4 land m26;
  h0 := !h0 + (c * 5);
  let c = !h0 lsr 26 in
  h0 := !h0 land m26;
  h1 := !h1 + c;
  let g0 = !h0 + 5 in
  let c = g0 lsr 26 in
  let g0 = g0 land m26 in
  let g1 = !h1 + c in
  let c = g1 lsr 26 in
  let g1 = g1 land m26 in
  let g2 = !h2 + c in
  let c = g2 lsr 26 in
  let g2 = g2 land m26 in
  let g3 = !h3 + c in
  let c = g3 lsr 26 in
  let g3 = g3 land m26 in
  let g4 = !h4 + c - (1 lsl 26) in
  let h0, h1, h2, h3, h4 = if g4 >= 0 then (g0, g1, g2, g3, g4) else (!h0, !h1, !h2, !h3, !h4) in
  (* to 128 bits, plus s *)
  let w0 = (h0 lor (h1 lsl 26)) land 0xffffffff and w1 = ((h1 lsr 6) lor (h2 lsl 20)) land 0xffffffff in
  let w2 = ((h2 lsr 12) lor (h3 lsl 14)) land 0xffffffff and w3 = ((h3 lsr 18) lor (h4 lsl 8)) land 0xffffffff in
  let f = w0 + le32 key 16 in
  let o0 = f land 0xffffffff in
  let f = w1 + le32 key 20 + (f lsr 32) in
  let o1 = f land 0xffffffff in
  let f = w2 + le32 key 24 + (f lsr 32) in
  let o2 = f land 0xffffffff in
  let f = w3 + le32 key 28 + (f lsr 32) in
  let o3 = f land 0xffffffff in
  let words = [| o0; o1; o2; o3 |] in
  String.init 16 (fun i -> Char.chr ((words.(i / 4) lsr (8 * (i mod 4))) land 0xff))
