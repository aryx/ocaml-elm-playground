(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pixelate.mli *)

let small_size ~(factor : int) (n : int) : int = (n + factor - 1) / factor

(* The simple version: a small pixel read per big pixel written. (Int.min,
 * not min: Stdlib's is polymorphic, a generic comparison per call, and
 * made this 4 times slower.) *)
let nearest_simple ~(factor : int) (small : Framebuffer.t) (big : Framebuffer.t) : unit =
  for y = 0 to big.height - 1 do
    let sy = Int.min (small.height - 1) (y / factor) in
    for x = 0 to big.width - 1 do
      big.pixels.{y, x} <- small.pixels.{sy, Int.min (small.width - 1) (x / factor)}
    done
  done

(* claude: optimization (Opti.enabled): of the [factor] rows of a block,
 * only the first is computed, the others are a copy of the row above
 * (a Bigarray blit, a memcpy); and which small column each big column
 * reads is computed once, not per row. *)
let nearest_rows ~(factor : int) (small : Framebuffer.t) (big : Framebuffer.t) : unit =
  let columns = Array.init big.width (fun x -> Int.min (small.width - 1) (x / factor)) in
  for y = 0 to big.height - 1 do
    if y mod factor = 0 then begin
      let sy = Int.min (small.height - 1) (y / factor) in
      for x = 0 to big.width - 1 do
        big.pixels.{y, x} <- small.pixels.{sy, columns.(x)}
      done
    end
    else Bigarray.Array1.blit (Bigarray.Array2.slice_left big.pixels (y - 1)) (Bigarray.Array2.slice_left big.pixels y)
  done

let nearest ~(factor : int) (small : Framebuffer.t) (big : Framebuffer.t) : unit =
  if !Opti.enabled then nearest_rows ~factor small big else nearest_simple ~factor small big

let factor = ref 1
let next () = factor := (!factor mod 4) + 1

let name ~(width : int) ~(height : int) : string =
  if !factor = 1 then "full"
  else Printf.sprintf "%dx%d, x%d" (small_size ~factor:!factor width) (small_size ~factor:!factor height) !factor

(* the small framebuffer, made again only when its size changes *)
let small : Framebuffer.t option ref = ref None

let draw (big : Framebuffer.t) (render : Framebuffer.t -> scale:float -> unit) : unit =
  let k = !factor in
  if k = 1 then render big ~scale:1.
  else begin
    let width = small_size ~factor:k big.width and height = small_size ~factor:k big.height in
    let fb =
      match !small with
      | Some fb when fb.width = width && fb.height = height -> fb
      | _ ->
          let fb = Framebuffer.create ~width ~height in
          small := Some fb;
          fb
    in
    render fb ~scale:(1. /. float k);
    nearest ~factor:k fb big
  end
