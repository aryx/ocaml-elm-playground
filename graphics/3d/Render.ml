(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Render.mli *)

type paint = Color of int | Texture of Texture.image
type face = { paint : paint; points : (Vec3.t * (float * float) * Vec3.t) list }
type visibility = Z_buffer | Painters_algorithm

type options = {
  shading : Shading.mode;
  interpolation : Interpolate.mode;
  visibility : visibility;
  wireframe : bool;
  backface_culling : bool;
  bilinear : bool;
}

let default_options =
  {
    shading = Shading.Phong;
    interpolation = Interpolate.Perspective_correct;
    visibility = Z_buffer;
    wireframe = false;
    backface_culling = true;
    bilinear = true;
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let scale_channel (c : int) (brightness : float) : int = int_of_float (float_of_int c *. brightness)

(* the [color] closure for a face, resolving a pixel's final 0xRRGGBB
 * color from its interpolated (u, v) and its brightness: a constant
 * closure for a flat-colored face, a texture sample for a textured one.
 * Computed once per face, not once per pixel, except for the actual
 * texture sampling (genuinely per-pixel, since the color varies across
 * the face) and the [brightness] scaling (genuinely per-pixel too, for
 * Gouraud/Phong -- see Shading, which is what actually computes it;
 * this function doesn't know or care which shading mode produced it). *)
let color_of_paint (options : options) (paint : paint) : u:float -> v:float -> brightness:float -> int =
  let shade (r, g, b) ~brightness : int =
    (scale_channel r brightness lsl 16) lor (scale_channel g brightness lsl 8) lor scale_channel b brightness
  in
  match paint with
  | Color rgb ->
      let (r, g, b) = ((rgb lsr 16) land 0xFF, (rgb lsr 8) land 0xFF, rgb land 0xFF) in
      fun ~u:_ ~v:_ ~brightness -> shade (r, g, b) ~brightness
  | Texture img ->
      let sample = if options.bilinear then Texture.sample_bilinear else Texture.sample_nearest in
      fun ~u ~v ~brightness -> shade (sample img ~u ~v) ~brightness

(*****************************************************************************)
(* The pipeline *)
(*****************************************************************************)

(* fan-triangulate a (convex, e.g. a cube face or a plane) polygon:
 * (p0,p1,p2), (p0,p2,p3), (p0,p3,p4), ... *)
let rec fan_triangles = function
  | p0 :: p1 :: p2 :: rest -> (p0, p1, p2) :: fan_triangles (p0 :: p2 :: rest)
  | _ -> []

let points_of (face : face) : Vec3.t list = List.map (fun (p, _uv, _n) -> p) face.points

let render ?(options = default_options) (fb : Framebuffer.t) (zbuffer : Zbuffer.t) (camera : Camera.t)
    (faces : face list) : unit =
  (* claude: only in Painters_algorithm mode -- Triangle.fill without a
   * z-buffer has no per-pixel depth test at all, so *draw order* is the
   * only thing that determines what ends up on top; sorting faces
   * farthest-from-the-camera-first here, so nearer faces are drawn
   * later and end up covering farther ones, is what makes that mode
   * look right at all (still not correct for intersecting/cyclically-
   * overlapping geometry -- see Painter.mli). The z-buffer mode needs
   * no such sort: its per-pixel depth test makes the result correct
   * regardless of draw order. *)
  let faces, zbuffer =
    match options.visibility with
    | Z_buffer ->
        Zbuffer.clear zbuffer;
        (faces, Some zbuffer)
    | Painters_algorithm -> (Painter.sort_far_to_near ~eye:camera.eye points_of faces, None)
  in
  faces
  |> List.iter (fun (face : face) ->
         if (not options.backface_culling) || Cull.faces_camera ~eye:camera.eye (points_of face) then begin
           let color = color_of_paint options face.paint in
           let projected =
             fan_triangles face.points
             |> List.map (fun (pa, pb, pc) ->
                    ( Project.vertex camera ~width:fb.width ~height:fb.height pa,
                      Project.vertex camera ~width:fb.width ~height:fb.height pb,
                      Project.vertex camera ~width:fb.width ~height:fb.height pc ))
           in
           if options.wireframe then begin
             (* one representative, unlit color for the whole face
              * (sampled at the texture's center for a textured one) --
              * wireframe mode draws bare edges, not shaded pixels, so
              * brightness is always 1. here regardless of the shading
              * mode *)
             let rgb = color ~u:0.5 ~v:0.5 ~brightness:1. in
             projected
             |> List.iter (function Some v0, Some v1, Some v2 -> Triangle.outline fb ~rgb v0 v1 v2 | _ -> ())
           end
           else
             projected
             |> List.iter (function
                  | Some v0, Some v1, Some v2 ->
                      Triangle.fill fb ~zbuffer ~interpolation:options.interpolation ~shading:options.shading ~color v0
                        v1 v2
                  | _ ->
                      (* a vertex is behind the camera (or too near): drop
                       * the whole triangle rather than clip it *)
                      ())
         end)
