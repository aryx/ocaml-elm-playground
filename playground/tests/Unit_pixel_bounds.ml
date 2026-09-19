(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/software/Shape_render_software.pixel_bounds: nothing that
 * render paints may fall outside it (the OpenGL HUD would lose it) *)

open Playground

let t = Testo.create

let check_inside (name : string) (shapes : shape list) () =
  let width = 400 and height = 300 in
  let fb = Framebuffer.create ~width ~height in
  Framebuffer.clear fb ~rgb:0x000000;
  Shape_render_software.render fb shapes;
  match Shape_render_software.pixel_bounds ~width ~height shapes with
  | None -> Alcotest.fail (name ^ ": no bounds")
  | Some (x0, y0, x1, y1) ->
      let outside = ref 0 and painted = ref 0 in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          if Framebuffer.get_rgb fb ~x ~y <> 0 then begin
            incr painted;
            if x < x0 || x >= x1 || y < y0 || y >= y1 then incr outside
          end
        done
      done;
      Alcotest.(check bool) (name ^ ": something painted") true (!painted > 0);
      Alcotest.(check int) (name ^ ": painted outside the bounds") 0 !outside

(* render_region: the same pixels as render, cropped *)
let test_region () =
  let width = 400 and height = 300 in
  let shapes = [ words yellow "PRESS SPACE" |> scale 3. |> move_y (-80.); circle red 30. |> move 50. 60. ] in
  let full = Framebuffer.create ~width ~height in
  Framebuffer.clear full ~rgb:0x000000;
  Shape_render_software.render full shapes;
  let x0, y0, w, h = (37, 150, 250, 90) in
  let part = Framebuffer.create ~width:w ~height:h in
  Framebuffer.clear part ~rgb:0x000000;
  Shape_render_software.render_region ~window:(width, height) ~origin:(x0, y0) part shapes;
  let diff = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      if Framebuffer.get_rgb part ~x ~y <> Framebuffer.get_rgb full ~x:(x0 + x) ~y:(y0 + y) then incr diff
    done
  done;
  Alcotest.(check int) "pixels differing from the full render's" 0 !diff

let tests =
  Testo.categorize "pixel_bounds"
    [ t "render_region, a crop of render" test_region; t "words, big" (check_inside "words" [ words yellow "PRESS SPACE jpgqy" |> scale 3. |> move_y (-80.) ]);
      t "words, rotated" (check_inside "rotated" [ words white "TINY" |> scale 5. |> rotate 30. ]);
      t "a group, moved and scaled"
        (check_inside "group" [ group [ circle red 20.; rectangle blue 40. 10. |> move 30. 0. ] |> scale 1.5 |> move (-100.) 50. ]);
      t "a polygon" (check_inside "polygon" [ polygon green [ (0., 0.); (60., 20.); (10., 70.) ] |> rotate 45. ]) ]
