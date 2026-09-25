(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Layers.mli *)

type layer = { name : string; image : Rgba_image.t; opacity : float; mode : Blend.mode; visible : bool }

let make ?(opacity = 1.) ?(mode = Blend.Normal) name image = { name; image; opacity; mode; visible = true }
let transparent name w h = make name (Rgba_image.create ~width:w ~height:h)

(* one layer over the picture [out] holds, in place *)
let composite (out : Rgba_image.t) (l : layer) : unit =
  let n = out.width * out.height in
  let get (img : Rgba_image.t) i = float_of_int (Bigarray.Array1.get img.rgba i) /. 255. in
  for p = 0 to n - 1 do
    let i = 4 * p in
    let as_ = get l.image (i + 3) *. l.opacity in
    if as_ > 0. then begin
      let ab = get out (i + 3) in
      let cb = (get out i, get out (i + 1), get out (i + 2)) and cs = (get l.image i, get l.image (i + 1), get l.image (i + 2)) in
      let br, bg, bb = Blend.blend l.mode cb cs in
      let ao = as_ +. (ab *. (1. -. as_)) in
      let mix c_s c_b blended =
        let c_s' = ((1. -. ab) *. c_s) +. (ab *. blended) in
        ((as_ *. c_s') +. ((1. -. as_) *. ab *. c_b)) /. ao
      in
      let sr, sg, sb = cs and cr, cg, cbl = cb in
      let byte v = Pixels.clamp (int_of_float (Float.round (v *. 255.))) in
      Bigarray.Array1.set out.rgba i (byte (mix sr cr br));
      Bigarray.Array1.set out.rgba (i + 1) (byte (mix sg cg bg));
      Bigarray.Array1.set out.rgba (i + 2) (byte (mix sb cbl bb));
      Bigarray.Array1.set out.rgba (i + 3) (byte ao)
    end
  done

let flatten (layers : layer list) : Rgba_image.t =
  match List.filter (fun l -> l.visible) layers with
  | [ l ] when l.mode = Blend.Normal && l.opacity >= 1. -> l.image
  | [] -> (match layers with l :: _ -> Rgba_image.create ~width:l.image.width ~height:l.image.height | [] -> Rgba_image.create ~width:1 ~height:1)
  | (first :: _) as shown ->
      let out = Rgba_image.create ~width:first.image.width ~height:first.image.height in
      List.iter (composite out) shown;
      out
