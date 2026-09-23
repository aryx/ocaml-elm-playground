(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Huffman.mli *)

let max_bits = 15

type t = {
  (* how many codes of each length, 0 to max_bits *)
  count : int array;
  (* the symbols by code: sorted by length, then by symbol *)
  symbol : int array;
}

let of_lengths (lengths : int array) : t =
  let count = Array.make (max_bits + 1) 0 in
  Array.iter (fun len -> count.(len) <- count.(len) + 1) lengths;
  (* over-subscribed: at each length, the codes left can't go negative
   * (one code of length 0 to start with; each length doubles them) *)
  let left = ref 1 in
  for len = 1 to max_bits do
    left := (!left * 2) - count.(len);
    if !left < 0 then failwith "Huffman: over-subscribed code lengths"
  done;
  (* where each length's symbols start in [symbol] *)
  let offset = Array.make (max_bits + 1) 0 in
  for len = 1 to max_bits - 1 do
    offset.(len + 1) <- offset.(len) + count.(len)
  done;
  let symbol = Array.make (Array.length lengths) 0 in
  Array.iteri
    (fun sym len ->
      if len <> 0 then begin
        symbol.(offset.(len)) <- sym;
        offset.(len) <- offset.(len) + 1
      end)
    lengths;
  count.(0) <- 0;
  { count; symbol }

(* [code] is the bits read so far, [first] the first code of length
 * [len], [index] where that length's symbols start in [symbol] *)
let decode (next_bit : unit -> int) (h : t) : int =
  let rec loop len code first index =
    if len > max_bits then failwith "Huffman: a code that isn't there"
    else
      let code = code lor next_bit () in
      let count = h.count.(len) in
      if code - first < count then h.symbol.(index + (code - first))
      else loop (len + 1) (code lsl 1) ((first + count) lsl 1) (index + count)
  in
  loop 1 0 0 0

let codes (lengths : int array) : (int * int) array =
  let count = Array.make (max_bits + 1) 0 in
  Array.iter (fun len -> if len <> 0 then count.(len) <- count.(len) + 1) lengths;
  let next = Array.make (max_bits + 1) 0 in
  for len = 1 to max_bits do
    next.(len) <- (next.(len - 1) + count.(len - 1)) lsl 1
  done;
  Array.map
    (fun len ->
      if len = 0 then (0, 0)
      else begin
        let code = next.(len) in
        next.(len) <- code + 1;
        (code, len)
      end)
    lengths
