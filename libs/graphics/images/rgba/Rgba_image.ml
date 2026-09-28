(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rgba_image.mli *)

type t = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let create ~(width : int) ~(height : int) : t =
  let rgba = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height * 4) in
  Bigarray.Array1.fill rgba 0;
  { width; height; rgba }
