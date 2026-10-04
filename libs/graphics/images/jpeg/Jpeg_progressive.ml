(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jpeg_progressive.mli *)

type scan = { ss : int; se : int; ah : int; al : int }

(* the value of s bits (Jpeg.extend: negatives are sent shifted) *)
let extend (v : int) (s : int) : int = if s = 0 then 0 else if v < 1 lsl (s - 1) then v - (1 lsl s) + 1 else v

(* an end-of-band run's length, from its symbol's r: this block and
 * 2^r - 1 + (r more bits) after it; what is left after this one *)
let run_after (receive : int -> int) (r : int) : int = (1 lsl r) - 1 + if r > 0 then receive r else 0

let block (sc : scan) ~(bit : unit -> int) ~(receive : int -> int) ~(dc : unit -> Huffman.t) ~(ac : unit -> Huffman.t) ~(pred : int ref)
    ~(run : int ref) (coefs : int array) (at : int) : unit =
  let one = 1 lsl sc.al in
  (* a coefficient already not zero: one bit, its correction at bit Al *)
  let corrected (k : int) : unit =
    let c = coefs.(at + k) in
    if bit () = 1 && c land one = 0 then coefs.(at + k) <- (if c > 0 then c + one else c - one)
  in
  if sc.ss = 0 then (
    (* the DC: its difference from the block before, or one more bit *)
    if sc.ah = 0 then (
      let t = Huffman.decode bit (dc ()) in
      if t > 11 then failwith "JPEG: a DC difference of more than 11 bits";
      pred := !pred + extend (receive t) t;
      coefs.(at) <- !pred lsl sc.al)
    else if bit () = 1 then coefs.(at) <- coefs.(at) lor one)
  else if sc.ah = 0 then (
    (* the AC's first scan: as baseline, in the band; or a run of blocks with nothing *)
    if !run > 0 then decr run
    else
      let table = ac () in
      let rec go (k : int) =
        if k <= sc.se then (
          let rs = Huffman.decode bit table in
          let r = rs lsr 4 and s = rs land 15 in
          if s = 0 then (if r = 15 then go (k + 16) else run := run_after receive r)
          else
            let k = k + r in
            if k > 63 then failwith "JPEG: a coefficient past the end of the block";
            coefs.(at + k) <- extend (receive s) s * one;
            go (k + 1))
      in
      go sc.ss)
  else if !run > 0 then (
    (* refining, in an end-of-band run: the corrections alone *)
    decr run;
    for k = sc.ss to sc.se do
      if coefs.(at + k) <> 0 then corrected k
    done)
  else
    (* refining: a symbol (r, 1) is r coefficients still zero, then one
     * that becomes +1 or -1 at bit Al; those already not zero met on
     * the way each take a correction bit *)
    let table = ac () in
    let rec go (k : int) =
      if k <= sc.se then (
        let rs = Huffman.decode bit table in
        let r = rs lsr 4 and s = rs land 15 in
        let value, zeros =
          if s = 0 then
            if r = 15 then (0, 15)
            else (
              run := run_after receive r;
              (* to the band's end: no zero becomes anything *)
              (0, 64))
          else if s <> 1 then failwith "JPEG: a refining scan with a value of more than one bit"
          else ((if bit () = 1 then one else -one), r)
        in
        (* past [zeros] zeros, correcting what is not zero; then the value *)
        let rec place (k : int) (zeros : int) : int =
          if k > sc.se then k
          else if coefs.(at + k) <> 0 then (corrected k; place (k + 1) zeros)
          else if zeros = 0 then (coefs.(at + k) <- value; k + 1)
          else place (k + 1) (zeros - 1)
        in
        go (place k zeros))
    in
    go sc.ss
