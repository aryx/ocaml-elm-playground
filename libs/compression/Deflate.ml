(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Deflate.mli *)

(*****************************************************************************)
(* Writing bits *)
(*****************************************************************************)

type writer = {
  out : Buffer.t;
  (* bits not written yet, the first one in bit 0 *)
  mutable bits : int;
  mutable n : int;
}

(* [n] bits of [v], least significant first *)
let put (w : writer) (v : int) (n : int) : unit =
  w.bits <- w.bits lor (v lsl w.n);
  w.n <- w.n + n;
  while w.n >= 8 do
    Buffer.add_char w.out (Char.chr (w.bits land 0xFF));
    w.bits <- w.bits lsr 8;
    w.n <- w.n - 8
  done

(* a Huffman code goes most significant bit first: reversed *)
let put_code (w : writer) ((code, len) : int * int) : unit =
  let rev = ref 0 in
  for k = 0 to len - 1 do
    if code land (1 lsl k) <> 0 then rev := !rev lor (1 lsl (len - 1 - k))
  done;
  put w !rev len

(*****************************************************************************)
(* Symbols *)
(*****************************************************************************)

let fixed_lit : (int * int) array Lazy.t =
  lazy
    (Huffman.codes
       (Array.init 288 (fun sym -> if sym < 144 then 8 else if sym < 256 then 9 else if sym < 280 then 7 else 8)))

let fixed_dist : (int * int) array Lazy.t = lazy (Huffman.codes (Array.make 30 5))

(* the last code whose base is at most [v] *)
let code_of (base : int array) (v : int) : int =
  let c = ref 0 in
  while !c + 1 < Array.length base && base.(!c + 1) <= v do
    incr c
  done;
  !c

let literal (w : writer) (b : int) : unit = put_code w (Lazy.force fixed_lit).(b)

let pair (w : writer) ~(len : int) ~(dist : int) : unit =
  let lc = code_of Inflate.length_base len in
  put_code w (Lazy.force fixed_lit).(257 + lc);
  put w (len - Inflate.length_base.(lc)) Inflate.length_extra.(lc);
  let dc = code_of Inflate.dist_base dist in
  put_code w (Lazy.force fixed_dist).(dc);
  put w (dist - Inflate.dist_base.(dc)) Inflate.dist_extra.(dc)

(*****************************************************************************)
(* LZ77 by hash chains *)
(*****************************************************************************)

let window = 32768
let min_match = 3
let max_match = 258

let deflate ?(chain = 64) (s : string) : string =
  let len = String.length s in
  let w = { out = Buffer.create ((len / 4) + 16); bits = 0; n = 0 } in
  (* one block, the last, fixed codes *)
  put w 1 1;
  put w 1 2;
  let byte i = Char.code (String.unsafe_get s i) in
  let hash i = ((byte i lsl 10) lxor (byte (i + 1) lsl 5) lxor byte (i + 2)) land (window - 1) in
  (* [head.(h)]: the last position with hash h; [prev.(i mod window)]:
   * the one before i with the same hash *)
  let head = Array.make window (-1) and prev = Array.make window (-1) in
  let insert i =
    if i + min_match <= len then begin
      let h = hash i in
      prev.(i land (window - 1)) <- head.(h);
      head.(h) <- i
    end
  in
  (* the longest match for position [i], (length, distance) *)
  let longest i =
    let best_len = ref 0 and best_dist = ref 0 in
    if i + min_match <= len then begin
      let limit = min max_match (len - i) in
      let rec walk p tries =
        if p >= 0 && i - p <= window && tries > 0 && !best_len < limit then begin
          let l = ref 0 in
          while !l < limit && String.unsafe_get s (p + !l) = String.unsafe_get s (i + !l) do
            incr l
          done;
          if !l > !best_len then begin
            best_len := !l;
            best_dist := i - p
          end;
          walk prev.(p land (window - 1)) (tries - 1)
        end
      in
      walk head.(hash i) chain
    end;
    (!best_len, !best_dist)
  in
  let i = ref 0 in
  while !i < len do
    let mlen, dist = longest !i in
    if mlen >= min_match then begin
      pair w ~len:mlen ~dist;
      for k = !i to !i + mlen - 1 do
        insert k
      done;
      i := !i + mlen
    end
    else begin
      literal w (byte !i);
      insert !i;
      incr i
    end
  done;
  (* the end of the block, and the last bits *)
  put_code w (Lazy.force fixed_lit).(256);
  if w.n > 0 then put w 0 (8 - w.n);
  Buffer.contents w.out
