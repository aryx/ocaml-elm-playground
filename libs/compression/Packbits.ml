(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* how many times s.[i] repeats from i on, at most 128 *)
let run_length s i =
  let n = Bytes.length s in
  let rec go j = if j < n && j - i < 128 && Bytes.get s j = Bytes.get s i then go (j + 1) else j - i in
  go i

let encode s =
  let n = Bytes.length s in
  let out = Buffer.create (n + (n / 128) + 1) in
  (* literals are gathered until a run of three or more, or 128 of them *)
  let flush lits =
    let k = Buffer.length lits in
    if k > 0 then (
      Buffer.add_char out (Char.chr (k - 1));
      Buffer.add_buffer out lits;
      Buffer.clear lits)
  in
  let lits = Buffer.create 128 in
  let rec go i =
    if i >= n then flush lits
    else
      let r = run_length s i in
      (* a run of two starts a count only where no literals wait: in
         the middle of literals it costs the same either way *)
      if r >= 3 || (r = 2 && Buffer.length lits = 0) then (
        flush lits;
        Buffer.add_char out (Char.chr (257 - r));
        Buffer.add_char out (Bytes.get s i);
        go (i + r))
      else (
        Buffer.add_char lits (Bytes.get s i);
        if Buffer.length lits = 128 then flush lits;
        go (i + 1))
  in
  go 0;
  Buffer.to_bytes out

let decode s ~pos ~len =
  let out = Buffer.create len in
  let rec go pos =
    if Buffer.length out >= len then pos
    else
      let n = Char.code (Bytes.get s pos) in
      if n < 128 then (
        Buffer.add_subbytes out s (pos + 1) (n + 1);
        go (pos + 2 + n))
      else if n = 128 then go (pos + 1)
      else (
        for _ = 1 to 257 - n do
          Buffer.add_char out (Bytes.get s (pos + 1))
        done;
        go (pos + 2))
  in
  let pos = go pos in
  (Buffer.to_bytes out, pos)
