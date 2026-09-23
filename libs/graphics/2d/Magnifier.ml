(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* color shown for the pixels of the area that are outside the window *)
let outside_rgb = 0x808080

let frame_rgb = 0xff0000

(* A 1-pixel-wide rectangle outline, from (x0, y0) to (x1, y1)
 * inclusive, made of spans like everything else: one span for the top
 * row, one for the bottom, and a 1-pixel span per row for each side *)
let outline (fb : Framebuffer.t) ~x0 ~y0 ~x1 ~y1 ~rgb =
  Framebuffer.fill_span fb ~y:y0 ~x0 ~x1:(x1 + 1) ~rgb ~alpha:1.;
  Framebuffer.fill_span fb ~y:y1 ~x0 ~x1:(x1 + 1) ~rgb ~alpha:1.;
  for y = y0 to y1 do
    Framebuffer.fill_span fb ~y ~x0 ~x1:(x0 + 1) ~rgb ~alpha:1.;
    Framebuffer.fill_span fb ~y ~x0:x1 ~x1:(x1 + 1) ~rgb ~alpha:1.
  done

let draw ?(size = 32) ?(zoom = 8) (fb : Framebuffer.t) ~cx ~cy =
  (* the magnified area: size x size pixels, with (cx, cy) in the
   * middle, e.g. for size = 32, from cx - 16 to cx + 15 *)
  let ax = cx - (size / 2) and ay = cy - (size / 2) in
  (* 1. copy the area first: the inset is drawn on the same
   * framebuffer, and may cover part of the area itself (e.g. when the
   * mouse is in the top-right corner) *)
  let area =
    Array.init size (fun j ->
        Array.init size (fun i ->
            let x = ax + i and y = ay + j in
            if x >= 0 && x < fb.width && y >= 0 && y < fb.height then Framebuffer.get_rgb fb ~x ~y
            else outside_rgb))
  in
  (* 2. the inset, top-right: area pixel (i, j) becomes the zoom x zoom
   * block whose top-left corner is (ix + i*zoom, iy + j*zoom) *)
  let inset = size * zoom and margin = 10 in
  let ix = fb.width - inset - margin and iy = margin in
  for j = 0 to size - 1 do
    for i = 0 to size - 1 do
      for dy = 0 to zoom - 1 do
        Framebuffer.fill_span fb ~y:(iy + (j * zoom) + dy) ~x0:(ix + (i * zoom))
          ~x1:(ix + ((i + 1) * zoom)) ~rgb:area.(j).(i) ~alpha:1.
      done
    done
  done;
  (* 3. the grid between blocks: semi-transparent black lines, so that
   * each block's color stays recognizable *)
  for k = 1 to size - 1 do
    Framebuffer.fill_span fb ~y:(iy + (k * zoom)) ~x0:ix ~x1:(ix + inset) ~rgb:0 ~alpha:0.2;
    for y = iy to iy + inset - 1 do
      Framebuffer.fill_span fb ~y ~x0:(ix + (k * zoom)) ~x1:(ix + (k * zoom) + 1) ~rgb:0 ~alpha:0.2
    done
  done;
  (* 4. frames: around the inset, around the area on screen, and
   * around pixel (cx, cy) in the inset *)
  outline fb ~x0:(ix - 1) ~y0:(iy - 1) ~x1:(ix + inset) ~y1:(iy + inset) ~rgb:frame_rgb;
  outline fb ~x0:(ax - 1) ~y0:(ay - 1) ~x1:(ax + size) ~y1:(ay + size) ~rgb:frame_rgb;
  let px = ix + ((cx - ax) * zoom) and py = iy + ((cy - ay) * zoom) in
  outline fb ~x0:px ~y0:py ~x1:(px + zoom) ~y1:(py + zoom) ~rgb:frame_rgb
