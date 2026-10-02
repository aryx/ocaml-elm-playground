(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Brotli.mli *)

let fail (what : string) = failwith ("Brotli: " ^ what)

(*****************************************************************************)
(* Reading bits *)
(*****************************************************************************)

(* as Inflate's: bits least significant first, at most 24 at a time *)
type input = {
  s : string;
  mutable pos : int;
  (* bits read from s but not used yet, the next one in bit 0: what is
   * left of the byte before [pos] *)
  mutable bitbuf : int;
  mutable bitcnt : int;
}

let bits (inp : input) (n : int) : int =
  while inp.bitcnt < n do
    if inp.pos >= String.length inp.s then fail "the data ends early";
    inp.bitbuf <- inp.bitbuf lor (Char.code inp.s.[inp.pos] lsl inp.bitcnt);
    inp.pos <- inp.pos + 1;
    inp.bitcnt <- inp.bitcnt + 8
  done;
  let v = inp.bitbuf land ((1 lsl n) - 1) in
  inp.bitbuf <- inp.bitbuf lsr n;
  inp.bitcnt <- inp.bitcnt - n;
  v

(* to the next byte boundary, over bits that must be zeros *)
let align (inp : input) : unit =
  if inp.bitbuf <> 0 then fail "bits set before a byte boundary";
  inp.bitcnt <- 0

(* a number from 1 to 256, small ones in few bits: 1 in one bit, 2 in
 * four, then 3 bits saying how many more bits *)
let count (inp : input) : int =
  if bits inp 1 = 0 then 1
  else
    let n = bits inp 3 in
    (1 lsl n) + 1 + bits inp n

(* codes with extra bits: a code's base is where the code before ends *)
let bases ~(first : int) (extra : int array) : int array =
  let base = Array.make (Array.length extra) first in
  for code = 1 to Array.length extra - 1 do
    base.(code) <- base.(code - 1) + (1 lsl extra.(code - 1))
  done;
  base

(*****************************************************************************)
(* Prefix codes *)
(*****************************************************************************)

(* a canonical code as DEFLATE's (Huffman.mli) -- or a single symbol,
 * which then takes no bit at all *)
type code =
  | Single of int
  | Code of Huffman.t

let symbol (inp : input) (code : code) : int =
  match code with
  | Single sym -> sym
  | Code h -> Huffman.decode (fun () -> bits inp 1) h

(* a simple code: 1 to 4 symbols, named *)
let simple_code (inp : input) ~(alphabet : int) : code =
  (* the bits that write the alphabet's last symbol *)
  let rec width n = if n = 0 then 0 else 1 + width (n lsr 1) in
  let width = width (alphabet - 1) in
  let n = bits inp 2 + 1 in
  let symbols = List.init n (fun _ -> bits inp width) in
  if List.exists (fun sym -> sym >= alphabet) symbols then fail "a symbol outside its alphabet";
  if List.length (List.sort_uniq compare symbols) <> n then fail "the same symbol twice in a simple code";
  let code_lengths =
    match n with
    | 1 -> []
    | 2 -> [ 1; 1 ]
    | 3 -> [ 1; 2; 2 ]
    | _ -> if bits inp 1 = 0 then [ 2; 2; 2; 2 ] else [ 1; 2; 3; 3 ]
  in
  match symbols with
  | [ sym ] -> Single sym
  | _ ->
      let lengths = Array.make alphabet 0 in
      List.iter2 (fun sym len -> lengths.(sym) <- len) symbols code_lengths;
      Code (Huffman.of_lengths lengths)

(* the order the code length code's lengths are sent in *)
let order = [| 1; 2; 3; 4; 0; 5; 17; 6; 16; 7; 8; 9; 10; 11; 12; 13; 14; 15 |]

(* one of them, 0 to 5, in a fixed code of its own: 0 is 00, 3 is 10, 4
 * is 01, 2 is 011, 1 is 0111, 5 is 1111 (the first bit read on the
 * right) *)
let short_length (inp : input) : int =
  match bits inp 2 with
  | 0 -> 0
  | 1 -> 4
  | 2 -> 3
  | _ -> if bits inp 1 = 0 then 2 else if bits inp 1 = 0 then 1 else 5

(* a complex code: its lengths, themselves coded, as DEFLATE's dynamic
 * codes (Inflate.mli) *)
let complex_code (inp : input) ~(alphabet : int) ~(skip : int) : code =
  (* the code of the lengths: sent until it is complete, 32 being a
   * code of length 0's worth *)
  let code_lengths = Array.make 18 0 in
  let space = ref 32 and used = ref [] and i = ref skip in
  while !i < 18 && !space > 0 do
    let len = short_length inp in
    code_lengths.(order.(!i)) <- len;
    if len > 0 then begin
      space := !space - (32 lsr len);
      used := order.(!i) :: !used
    end;
    incr i
  done;
  let lengths_code =
    match !used with
    | [ sym ] -> Single sym
    | _ ->
        if !space <> 0 then fail "the code lengths' code is not complete";
        Code (Huffman.of_lengths code_lengths)
  in
  (* the lengths: 0 to 15 a length, 16 "the last length that wasn't 0,
   * 3 to 6 times", 17 "0, 3 to 10 times" -- and a 16 after a 16 (a 17
   * after a 17) makes the repeat before it longer *)
  let lengths = Array.make alphabet 0 in
  let space = ref 32768 and i = ref 0 in
  let last = ref 8 in
  let repeated = ref 0 and times = ref 0 in
  while !i < alphabet && !space > 0 do
    let sym = symbol inp lengths_code in
    if sym < 16 then begin
      times := 0;
      lengths.(!i) <- sym;
      incr i;
      if sym <> 0 then begin
        last := sym;
        space := !space - (32768 lsr sym)
      end
    end
    else begin
      let extra = sym - 14 in
      let len = if sym = 16 then !last else 0 in
      if !repeated <> len then begin
        times := 0;
        repeated := len
      end;
      let before = !times in
      times := (if before > 0 then (before - 2) lsl extra else 0) + 3 + bits inp extra;
      let more = !times - before in
      if !i + more > alphabet then fail "more code lengths than symbols";
      Array.fill lengths !i more len;
      i := !i + more;
      if len <> 0 then space := !space - (more * (32768 lsr len))
    end
  done;
  if !space <> 0 then fail "a code that is not complete";
  Code (Huffman.of_lengths lengths)

let prefix_code (inp : input) ~(alphabet : int) : code =
  match bits inp 2 with
  | 1 -> simple_code inp ~alphabet
  | skip -> complex_code inp ~alphabet ~skip

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

(* one of the three categories (literals, commands, distances): the
 * meta-block's symbols of that category come in blocks, each block of
 * a type, and the type chooses the codes *)
type blocks = {
  types : int;
  type_code : code;
  count_code : code;
  (* the block we are in, the one before, and how many symbols are left
   * in this one *)
  mutable current : int;
  mutable before : int;
  mutable left : int;
}

let count_extra = [| 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 6; 6; 7; 8; 9; 10; 11; 12; 13; 24 |]
let count_base = bases ~first:1 count_extra

let block_count (inp : input) (code : code) : int =
  let c = symbol inp code in
  count_base.(c) + bits inp count_extra.(c)

let blocks (inp : input) : blocks =
  let types = count inp in
  if types = 1 then
    (* one type: one block, as long as a meta-block may be *)
    { types; type_code = Single 0; count_code = Single 0; current = 0; before = 1; left = 1 lsl 24 }
  else begin
    let type_code = prefix_code inp ~alphabet:(types + 2) in
    let count_code = prefix_code inp ~alphabet:26 in
    let left = block_count inp count_code in
    { types; type_code; count_code; current = 0; before = 1; left }
  end

(* the type of the block the next symbol is in; at a block's end, the
 * *block switch*: the next block's type (0 the one before this one, 1
 * this one plus one, else the type plus 2) and its count *)
let block_type (inp : input) (b : blocks) : int =
  if b.left = 0 then begin
    let t =
      match symbol inp b.type_code with
      | 0 -> b.before
      | 1 -> b.current + 1
      | sym -> sym - 2
    in
    b.before <- b.current;
    b.current <- (if t >= b.types then t - b.types else t);
    b.left <- block_count inp b.count_code
  end;
  b.left <- b.left - 1;
  b.current

(*****************************************************************************)
(* Contexts *)
(*****************************************************************************)

(* Lut0: what the last byte is, for the UTF-8 mode, in the high 4 bits
 * of the context's 6 (the ASCII half; the other is below) *)
let lut0 =
  [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 4; 0; 0; 4; 0; 0;
     0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
     8; 12; 16; 12; 12; 20; 12; 16; 24; 28; 12; 12; 32; 12; 36; 12;
     44; 44; 44; 44; 44; 44; 44; 44; 44; 44; 32; 32; 24; 40; 28; 12;
     12; 48; 52; 52; 52; 48; 52; 52; 52; 48; 52; 52; 52; 52; 52; 48;
     52; 52; 52; 52; 52; 48; 52; 52; 52; 52; 52; 24; 12; 28; 12; 12;
     12; 56; 60; 60; 60; 56; 60; 60; 60; 56; 60; 60; 60; 60; 60; 56;
     60; 60; 60; 60; 60; 56; 60; 60; 60; 60; 60; 24; 12; 28; 12; 0 |]

(* Lut1: what the byte before it is, in the low 2 bits *)
let lut1 (c : int) : int =
  match Char.chr c with
  | 'a' .. 'z' -> 3
  | 'A' .. 'Z' | '0' .. '9' -> 2
  | '!' .. '~' -> 1
  | '\224' .. '\255' -> 2
  | _ -> 0

(* Lut2: how big a byte is, in 8 classes *)
let lut2 (c : int) : int = if c = 0 then 0 else if c < 16 then 1 else if c < 64 then 2 else if c < 128 then 3 else if c < 192 then 4 else if c < 240 then 5 else if c < 255 then 6 else 7

let context ~(mode : int) ~(p1 : int) ~(p2 : int) : int =
  match mode with
  | 0 -> p1 land 0x3F
  | 1 -> p1 lsr 2
  | 2 ->
      (* in a character of several bytes, only whether the byte starts
       * it (2, 3) or goes on (0, 1), and its lowest bit *)
      (if p1 < 128 then lut0.(p1) else (if p1 < 192 then 0 else 2) + (p1 land 1)) lor lut1 p2
  | _ -> (lut2 p1 lsl 3) lor lut2 p2

(* a context map: for each block type and each context, which code *)
let context_map (inp : input) ~(size : int) : int array * int =
  let trees = count inp in
  let map = Array.make size 0 in
  if trees > 1 then begin
    (* symbols 1 to [rle] are runs of zeros, 2^symbol and more *)
    let rle = if bits inp 1 = 0 then 0 else bits inp 4 + 1 in
    let code = prefix_code inp ~alphabet:(trees + rle) in
    let i = ref 0 in
    while !i < size do
      let sym = symbol inp code in
      if sym = 0 then incr i
      else if sym <= rle then begin
        let run = (1 lsl sym) + bits inp sym in
        if !i + run > size then fail "a run of zeros past the context map's end";
        i := !i + run
      end
      else begin
        map.(!i) <- sym - rle;
        incr i
      end
    done;
    (* move-to-front, undone: a value is a place in the list of the
     * codes, the one just used first *)
    if bits inp 1 = 1 then begin
      let front = Array.init 256 Fun.id in
      Array.iteri
        (fun i place ->
          let value = front.(place) in
          map.(i) <- value;
          Array.blit front 0 front 1 place;
          front.(0) <- value)
        map
    end
  end;
  (map, trees)

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

(* a command's symbol, 0 to 703, is a cell of 64 then 3 bits of an
 * insert code and 3 of a copy code; the cell says which eight codes
 * of each *)
let insert_cell = [| 0; 0; 0; 0; 8; 8; 0; 16; 8; 16; 16 |]
let copy_cell = [| 0; 8; 0; 8; 0; 8; 16; 0; 16; 8; 16 |]
let insert_extra = [| 0; 0; 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 7; 8; 9; 10; 12; 14; 24 |]
let insert_base = bases ~first:0 insert_extra
let copy_extra = [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 7; 8; 9; 10; 24 |]
let copy_base = bases ~first:2 copy_extra

(* what goes from a meta-block to the next: the four last distances *)
type stream = {
  mutable d1 : int;
  mutable d2 : int;
  mutable d3 : int;
  mutable d4 : int;
}

(* the distance a symbol says, and whether it is a new one, to
 * remember *)
let distance (st : stream) (inp : input) (sym : int) ~(postfix : int) ~(direct : int) : int * bool =
  if sym < 16 then begin
    (* 0 to 3 one of the four last; then the last, or the one before,
     * less or plus 1, 2, 3 *)
    let d =
      match sym with
      | 0 -> st.d1
      | 1 -> st.d2
      | 2 -> st.d3
      | 3 -> st.d4
      | _ ->
          let nudge = ((sym - 4) mod 6 / 2) + 1 in
          (if sym < 10 then st.d1 else st.d2) + if sym land 1 = 0 then -nudge else nudge
    in
    if d <= 0 then fail "a distance of zero or less";
    (d, sym <> 0)
  end
  else if sym < 16 + direct then (sym - 15, true)
  else begin
    (* the others: the code's high bits say how many extra bits and
     * which half of their range, its low [postfix] bits are the
     * distance's own low bits *)
    let code = sym - direct - 16 in
    let nbits = 1 + (code lsr (postfix + 1)) in
    let high = code lsr postfix and low = code land ((1 lsl postfix) - 1) in
    let offset = ((2 + (high land 1)) lsl nbits) - 4 in
    ((((offset + bits inp nbits) lsl postfix) + low + direct + 1), true)
  end

let compressed (st : stream) (inp : input) (out : Buffer.t) ~(len : int) ~(window : int) ~(dictionary : string option) : unit =
  (* the header: the blocks of the three categories, the distances'
   * parameters, the contexts, the codes *)
  let literal_blocks = blocks inp in
  let command_blocks = blocks inp in
  let distance_blocks = blocks inp in
  let postfix = bits inp 2 in
  let direct = bits inp 4 lsl postfix in
  let modes = Array.init literal_blocks.types (fun _ -> bits inp 2) in
  let literal_map, literal_trees = context_map inp ~size:(64 * literal_blocks.types) in
  let distance_map, distance_trees = context_map inp ~size:(4 * distance_blocks.types) in
  let literal_codes = Array.init literal_trees (fun _ -> prefix_code inp ~alphabet:256) in
  let command_codes = Array.init command_blocks.types (fun _ -> prefix_code inp ~alphabet:704) in
  let distance_codes = Array.init distance_trees (fun _ -> prefix_code inp ~alphabet:(16 + direct + (48 lsl postfix))) in
  (* the byte written [back] bytes ago, 0 before the first *)
  let previous back =
    let n = Buffer.length out in
    if n >= back then Char.code (Buffer.nth out (n - back)) else 0
  in
  (* the commands: "insert so many literals, then copy so many bytes
   * from so far back" *)
  let left = ref len in
  while !left > 0 do
    let command = symbol inp command_codes.(block_type inp command_blocks) in
    let cell = command lsr 6 in
    let ic = insert_cell.(cell) + ((command lsr 3) land 7) and cc = copy_cell.(cell) + (command land 7) in
    let insert = insert_base.(ic) + bits inp insert_extra.(ic) in
    let copy = copy_base.(cc) + bits inp copy_extra.(cc) in
    if insert > !left then fail "more literals than the meta-block has bytes";
    for _ = 1 to insert do
      let t = block_type inp literal_blocks in
      let context = context ~mode:modes.(t) ~p1:(previous 1) ~p2:(previous 2) in
      Buffer.add_char out (Char.chr (symbol inp literal_codes.(literal_map.((64 * t) + context))))
    done;
    left := !left - insert;
    (* the last command's copy is not done when its literals end the
     * meta-block *)
    if !left > 0 then begin
      let d, fresh =
        (* under 128, "the same distance again", without a symbol *)
        if command < 128 then (st.d1, false)
        else begin
          let t = block_type inp distance_blocks in
          let code = distance_codes.(distance_map.((4 * t) + min 3 (copy - 2))) in
          distance st inp (symbol inp code) ~postfix ~direct
        end
      in
      let reach = min window (Buffer.length out) in
      if d <= reach then begin
        if fresh then begin
          st.d4 <- st.d3;
          st.d3 <- st.d2;
          st.d2 <- st.d1;
          st.d1 <- d
        end;
        if copy > !left then fail "a copy past the meta-block's end";
        let from = Buffer.length out - d in
        (* one byte at a time: the copy may read what it just wrote
         * (Inflate.mli) *)
        for i = 0 to copy - 1 do
          Buffer.add_char out (Buffer.nth out (from + i))
        done;
        left := !left - copy
      end
      else begin
        (* further back than there is data: a word of the dictionary *)
        let word =
          match dictionary with
          | Some dictionary -> Brotli_dictionary.word dictionary ~length:copy ~id:(d - reach - 1)
          | None -> fail "a word of the static dictionary, and no ~dictionary given (Brotli_words.bytes)"
        in
        if String.length word > !left then fail "a word past the meta-block's end";
        Buffer.add_string out word;
        left := !left - String.length word
      end
    end
  done

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let decompress ?(dictionary : string option) (s : string) : string =
  let inp = { s; pos = 0; bitbuf = 0; bitcnt = 0 } in
  let out = Buffer.create (4 * String.length s) in
  (* the window's size, 2^10 to 2^24: 16 in one bit, the others in 4
   * or 7 *)
  let wbits =
    if bits inp 1 = 0 then 16
    else
      match bits inp 3 with
      | 0 -> (
          match bits inp 3 with
          | 0 -> 17
          | 1 -> fail "a window bigger than 16 MB"
          | n -> 8 + n)
      | n -> 17 + n
  in
  let window = (1 lsl wbits) - 16 in
  let st = { d1 = 4; d2 = 11; d3 = 15; d4 = 16 } in
  let rec meta_blocks () =
    let last = bits inp 1 = 1 in
    if not (last && bits inp 1 = 1) then begin
      (match bits inp 2 with
      | 3 ->
          (* no data: bytes for someone else, skipped *)
          if bits inp 1 = 1 then fail "a reserved bit set";
          let bytes = bits inp 2 in
          let skip = if bytes = 0 then 0 else bits inp (8 * bytes) + 1 in
          if bytes > 1 && (skip - 1) lsr (8 * (bytes - 1)) = 0 then fail "a length in more bytes than it needs";
          align inp;
          inp.pos <- inp.pos + skip;
          if inp.pos > String.length s then fail "the data ends early"
      | n ->
          let nibbles = n + 4 in
          let len = bits inp (4 * nibbles) + 1 in
          if nibbles > 4 && (len - 1) lsr (4 * (nibbles - 1)) = 0 then fail "a length in more nibbles than it needs";
          if (not last) && bits inp 1 = 1 then begin
            (* the bytes as they are *)
            align inp;
            if inp.pos + len > String.length s then fail "the data ends early";
            Buffer.add_substring out s inp.pos len;
            inp.pos <- inp.pos + len
          end
          else compressed st inp out ~len ~window ~dictionary);
      if not last then meta_blocks ()
    end
  in
  meta_blocks ();
  align inp;
  if inp.pos <> String.length s then fail "bytes after the last meta-block";
  Buffer.contents out
