(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Gl_textures.mli *)

module Gl = Tgl3.Gl

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* Loading (a local file path or an http(s) URL, with caching and a
 * preload queue) is entirely reused from graphics/images/Texture_decode,
 * shared with the software rasterizer (see plan_opengl.md's Scope).
 * The only new piece is uploading the
 * decoded pixel buffer to the GPU (once per distinct src, cached
 * below by this module) and sampling it in the fragment shader
 * instead of sample_texture's hand-written nearest-neighbor lookup --
 * nearest-neighbor here too (Gl.nearest), for a fair comparison
 * rather than free bilinear filtering that would look different;
 * switching to Gl.linear is a one-line, GPU-only upgrade if ever
 * wanted, unlike native's own "well-known easy upgrade" bilinear note
 * in notes_3d.md section 9. *)

let upload_texture ~(width : int) ~(height : int) ~(format : Gl.enum)
    (data : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t) : int =
  let id = Gl_shaders.int32_bigarray1 1 in
  Gl.gen_textures 1 id;
  let tex = Int32.to_int id.{0} in
  Gl.bind_texture Gl.texture_2d tex;
  (* claude: glTexImage2D otherwise assumes rows are padded to a
   * multiple of 4 bytes -- true for most image dimensions but not
   * guaranteed (e.g. a 1-pixel-wide, 3-channel-per-pixel texture is 3
   * bytes/row); this disables that assumption so any width works. *)
  Gl.pixel_storei Gl.unpack_alignment 1;
  Gl.tex_image2d Gl.texture_2d 0 format width height 0 format Gl.unsigned_byte (`Data data);
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.nearest;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.clamp_to_edge;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.clamp_to_edge;
  tex

(* same magenta convention as native's missing_texture_color *)
let missing_texture_gl_id : int Lazy.t =
  lazy
    (let pixel = Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout [| 255; 0; 255 |] in
     upload_texture ~width:1 ~height:1 ~format:Gl.rgb pixel)

let gl_texture_cache : (string, int) Hashtbl.t = Hashtbl.create 8

let get_or_create_gl_texture (src : string) : int =
  match Hashtbl.find_opt gl_texture_cache src with
  | Some id -> id
  | None ->
      let id =
        match
          match Playground3d.embedded src with
          | Some base64 -> Texture_decode.load_base64 ~key:src ~base64
          | None -> Texture_decode.load src
        with
        | None -> Lazy.force missing_texture_gl_id
        | Some (img : Rgba_image.t) ->
            (* claude: Texture_decode's textures are always RGBA *)
            upload_texture ~width:img.width ~height:img.height ~format:Gl.rgba img.rgba
      in
      Hashtbl.add gl_texture_cache src id;
      id
