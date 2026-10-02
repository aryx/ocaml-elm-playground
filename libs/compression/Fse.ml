(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Fse.mli *)

(*****************************************************************************)
(* Bits *)
(*****************************************************************************)

(* the position of the highest bit set: 1 -> 0, 2 and 3 -> 1, 4 -> 2 *)
let rec log2 (n : int) : int = if n <= 1 then 0 else 1 + log2 (n lsr 1)

let field (s : string) ~(bit : int) (n : int) : int =
  let v = ref 0 in
  for i = n - 1 downto 0 do
    let p = bit + i in
    let b = if p < 0 || p lsr 3 >= String.length s then 0 else (Char.code s.[p lsr 3] lsr (p land 7)) land 1 in
    v := (!v lsl 1) lor b
  done;
  !v

type reader = {
  s : string;
  (* the stream's first bit, where the reading ends *)
  first : int;
  (* the lowest bit read so far: the next read is just under it *)
  mutable bit : int;
}

let backward (s : string) ~(pos : int) ~(len : int) : reader =
  if len <= 0 || pos + len > String.length s then failwith "Fse: the data ends early";
  let last = Char.code s.[pos + len - 1] in
  if last = 0 then failwith "Fse: no end mark in the stream's last byte";
  { s; first = pos * 8; bit = ((pos + len - 1) * 8) + log2 last }

let bits (r : reader) (n : int) : int =
  r.bit <- r.bit - n;
  (* under the first bit: zeros *)
  let missing = r.first - r.bit in
  if missing <= 0 then field r.s ~bit:r.bit n else if missing >= n then 0 else field r.s ~bit:r.first (n - missing) lsl missing

let left (r : reader) : int = r.bit - r.first

(*****************************************************************************)
(* The table *)
(*****************************************************************************)

type t = {
  (* the table has 2^accuracy states *)
  accuracy : int;
  (* what each state says: its symbol, *)
  symbol : int array;
  (* and the next state, [base] plus so many bits read *)
  nbits : int array;
  base : int array;
}

let single (symbol : int) : t = { accuracy = 0; symbol = [| symbol |]; nbits = [| 0 |]; base = [| 0 |] }

let of_distribution ~(accuracy : int) (counts : int array) : t =
  let size = 1 lsl accuracy in
  if Array.fold_left (fun sum c -> sum + abs c) 0 counts <> size then failwith "Fse: the counts don't add up to the table's size";
  let symbol = Array.make size 0 in
  (* "less than one" (-1): one state each, from the table's end down *)
  let high = ref size in
  Array.iteri
    (fun sym c ->
      if c = -1 then begin
        decr high;
        symbol.(!high) <- sym
      end)
    counts;
  (* the others: spread by a step that has no factor in common with
   * the size, so that it lands on every state once *)
  let step = (size lsr 1) + (size lsr 3) + 3 in
  let pos = ref 0 in
  Array.iteri
    (fun sym c ->
      for _ = 1 to c do
        symbol.(!pos) <- sym;
        pos := (!pos + step) land (size - 1);
        while !pos >= !high do
          pos := (!pos + step) land (size - 1)
        done
      done)
    counts;
  (* a symbol's [c] states, in the table's order, are the numbers c,
   * c + 1, ... 2c - 1: each reads the bits that bring it back up to
   * [size, 2 size), the states (less [size], to index the table) *)
  let next = Array.map abs counts in
  let nbits = Array.make size 0 and base = Array.make size 0 in
  for state = 0 to size - 1 do
    let sym = symbol.(state) in
    let x = next.(sym) in
    next.(sym) <- x + 1;
    nbits.(state) <- accuracy - log2 x;
    base.(state) <- (x lsl nbits.(state)) - size
  done;
  { accuracy; symbol; nbits; base }

let read_distribution (s : string) ~(pos : int) ~(max_accuracy : int) : t * int =
  let bit = ref (pos * 8) in
  let read n =
    let v = field s ~bit:!bit n in
    bit := !bit + n;
    v
  in
  let accuracy = 5 + read 4 in
  if accuracy > max_accuracy then failwith "Fse: a table bigger than allowed";
  let left = ref (1 lsl accuracy) in
  let counts = ref [] in
  while !left > 0 do
    (* a count is -1 to [left], sent plus one: [left] + 2 values, in
     * [n] bits -- or [n - 1] for the smallest ones, as many as [n]
     * bits have values to [spare] *)
    let n = log2 (!left + 1) + 1 in
    let spare = (1 lsl n) - 1 - (!left + 1) in
    let low = field s ~bit:!bit (n - 1) in
    let v =
      if low < spare then begin
        bit := !bit + n - 1;
        low
      end
      else
        let v = read n in
        if v >= 1 lsl (n - 1) then v - spare else v
    in
    let count = v - 1 in
    left := !left - abs count;
    counts := count :: !counts;
    (* a zero says how many zeros follow it: 2 bits, 3 meaning "and
     * more", 2 bits again *)
    if count = 0 then begin
      let rec zeros () =
        let more = read 2 in
        for _ = 1 to more do
          counts := 0 :: !counts
        done;
        if more = 3 then zeros ()
      in
      zeros ()
    end
  done;
  (* the rest of the last byte is not used *)
  let pos = (!bit + 7) / 8 in
  if pos > String.length s then failwith "Fse: the data ends early";
  (of_distribution ~accuracy (Array.of_list (List.rev !counts)), pos)

(*****************************************************************************)
(* Decoding *)
(*****************************************************************************)

let start (t : t) (r : reader) : int = bits r t.accuracy
let symbol (t : t) (state : int) : int = t.symbol.(state)
let next (t : t) (r : reader) (state : int) : int = t.base.(state) + bits r t.nbits.(state)
