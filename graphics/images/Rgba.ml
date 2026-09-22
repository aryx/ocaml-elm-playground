(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Rgba.mli *)

let of_stb_image (img : Stb_image.int8 Stb_image.t) : Rgba_image.t =
  let w = img.width and h = img.height and n = img.channels in
  if n = 4 && img.offset = 0 && img.stride = w * 4 then { width = w; height = h; rgba = img.data }
  else begin
    let data = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let src = img.offset + (y * img.stride) + (x * n) in
        let byte k = img.data.{src + k} in
        let r, g, b, a =
          match n with
          | 1 -> (byte 0, byte 0, byte 0, 255)
          | 2 -> (byte 0, byte 0, byte 0, byte 1)
          | 3 -> (byte 0, byte 1, byte 2, 255)
          | _ -> (byte 0, byte 1, byte 2, byte 3)
        in
        let dst = ((y * w) + x) * 4 in
        data.{dst} <- r;
        data.{dst + 1} <- g;
        data.{dst + 2} <- b;
        data.{dst + 3} <- a
      done
    done;
    { width = w; height = h; rgba = data }
  end
