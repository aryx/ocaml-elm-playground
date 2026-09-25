(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_picture.mli *)

type t = Waiting | Arrived of Rgba_image.t | Broken

let decode (bytes : string) : t =
  let starts magic = String.length bytes >= String.length magic && String.sub bytes 0 (String.length magic) = magic in
  try
    if starts "GIF8" then Arrived (Gif.decode bytes)
    else if starts "\x89PNG" then Arrived (Png.decode bytes)
    else if starts "\xFF\xD8" then Arrived (Jpeg.decode bytes)
    else if Svg.sniff bytes then
      (* drawn at its own size (a picture without one: CSS's 300 by 150) *)
      match Svg.parse bytes with
      | Some svg ->
          let w, h = Option.value (Svg.size svg) ~default:(300., 150.) in
          Arrived (Svg.render svg ~width:(int_of_float (Float.round w)) ~height:(int_of_float (Float.round h)))
      | None -> Broken
    else Broken
  with _ -> Broken

let broken_size = 24.

let size (t : t) : (float * float) option =
  match t with
  | Arrived img -> Some (float_of_int img.width, float_of_int img.height)
  | Broken -> Some (broken_size, broken_size)
  | Waiting -> None
