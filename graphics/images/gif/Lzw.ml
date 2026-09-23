(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lzw.mli *)

let max_codes = 4096

let decode ~(min_code_size : int) (data : string) ~(npixels : int) : Bytes.t =
  if min_code_size < 1 || min_code_size > 8 then
    failwith (Printf.sprintf "LZW: a minimum code size of %d" min_code_size);
  let clear = 1 lsl min_code_size in
  let end_ = clear + 1 in
  (* the dictionary: each sequence is a shorter one ([prefix]) plus one
   * index ([suffix]); [first] is its first index, [length] its length *)
  let prefix = Array.make max_codes (-1) in
  let suffix = Array.init max_codes (fun i -> if i < clear then i else 0) in
  let first = Array.copy suffix in
  let length = Array.make max_codes 1 in
  let out = Bytes.make npixels '\000' in
  let outpos = ref 0 in
  (* write [code]'s sequence, from its last index back to its first *)
  let emit code =
    let len = length.(code) in
    let c = ref code in
    for k = len - 1 downto 0 do
      if !outpos + k < npixels then Bytes.set out (!outpos + k) (Char.chr suffix.(!c));
      c := prefix.(!c)
    done;
    outpos := !outpos + len
  in
  (* the bits, least significant first *)
  let pos = ref 0 and bitbuf = ref 0 and bitcnt = ref 0 in
  let read width =
    while !bitcnt < width && !pos < String.length data do
      bitbuf := !bitbuf lor (Char.code data.[!pos] lsl !bitcnt);
      incr pos;
      bitcnt := !bitcnt + 8
    done;
    if !bitcnt < width then None
    else begin
      let v = !bitbuf land ((1 lsl width) - 1) in
      bitbuf := !bitbuf lsr width;
      bitcnt := !bitcnt - width;
      Some v
    end
  in
  let rec loop ~width ~next ~prev =
    if !outpos < npixels then
      match read width with
      | None -> ()
      | Some code when code = clear -> loop ~width:(min_code_size + 1) ~next:(end_ + 1) ~prev:(-1)
      | Some code when code = end_ -> ()
      | Some code when prev < 0 ->
          if code >= clear then failwith "LZW: the first code after a clear is not a color";
          emit code;
          loop ~width ~next ~prev:code
      | Some code ->
          if code > next || (code = next && next >= max_codes) then
            failwith (Printf.sprintf "LZW: code %d, beyond the dictionary (%d)" code next);
          let next =
            if next >= max_codes then next
            else begin
              (* the previous sequence + this one's first index; for
               * KwKwK (code = next), this one is the entry being
               * added, whose first index is the previous one's *)
              prefix.(next) <- prev;
              suffix.(next) <- (if code = next then first.(prev) else first.(code));
              first.(next) <- first.(prev);
              length.(next) <- length.(prev) + 1;
              next + 1
            end
          in
          emit code;
          let width = if next = 1 lsl width && width < 12 then width + 1 else width in
          loop ~width ~next ~prev:code
  in
  loop ~width:(min_code_size + 1) ~next:(end_ + 1) ~prev:(-1);
  out
