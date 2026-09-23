(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Zlib.mli *)

let decompress (s : string) : string =
  if String.length s < 6 then failwith "zlib: too short";
  let byte i = Char.code s.[i] in
  let cmf = byte 0 and flg = byte 1 in
  if cmf land 0x0F <> 8 then failwith "zlib: not deflate";
  if cmf lsr 4 > 7 then failwith "zlib: window larger than 32 KB";
  if ((cmf * 256) + flg) mod 31 <> 0 then failwith "zlib: bad header check";
  if flg land 0x20 <> 0 then failwith "zlib: a preset dictionary, not supported";
  let data, pos = Inflate.inflate s ~pos:2 in
  if pos + 4 > String.length s then failwith "zlib: no Adler-32 at the end";
  let adler = (byte pos lsl 24) lor (byte (pos + 1) lsl 16) lor (byte (pos + 2) lsl 8) lor byte (pos + 3) in
  if adler <> Adler32.string data then failwith "zlib: wrong Adler-32, the data is corrupt";
  data
