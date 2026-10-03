(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Webgl_textures.mli *)

open Js_of_ocaml

(*****************************************************************************)
(* Textures *)
(*****************************************************************************)
(* The other backends decode their textures themselves
 * (graphics/images/Texture_decode, blocking); here the
 * browser does it, from an <img>, which also means **asynchronously**:
 * setting img.src starts the download and returns at once. So a
 * texture goes through two steps, in two caches:
 *
 *  - [images]: src -> its <img>, created on first request (by
 *    preload_texture, or the first frame that draws the texture),
 *    before any GL context exists, so a preload can start early;
 *  - [gl_state.textures]: src -> its GL texture, created on first draw
 *    with the other backends' 1x1 magenta "missing texture" pixel, and
 *    replaced by the image, once, on the first frame after the image
 *    is complete. Until then (or forever, if the image can't be
 *    loaded) the faces are magenta.
 *
 * Checking [complete] every frame, rather than uploading from the
 * image's onload handler, keeps all GL calls inside [draw]: no GL
 * context yet when a preload's image arrives is then not a case to
 * handle. *)

let images : (string, Dom_html.imageElement Js.t) Hashtbl.t = Hashtbl.create 8

let is_http_url (src : string) : bool =
  String.starts_with ~prefix:"http://" src || String.starts_with ~prefix:"https://" src

(* the <img> of [src], its download started if this is the first request *)
let image_of (src : string) : Dom_html.imageElement Js.t =
  match Hashtbl.find_opt images src with
  | Some img -> img
  | None ->
      let img = Dom_html.createImg Dom_html.document in
      (* WebGL refuses to read the pixels of an image from another site
       * unless that site allows it (CORS), and the request must ask for
       * it; not for a relative path, where the attribute would instead
       * break loading from a file:// page. (Not in js_of_ocaml's
       * imageElement, hence Js.Unsafe.) *)
      if is_http_url src then Js.Unsafe.set img "crossOrigin" (Js.string "anonymous");
      (* claude: a texture the program carries with it
       * (Playground3d.embedded_texture) is already base64: a "data:"
       * URL is exactly that, and the browser decodes it itself, with no
       * file to find and no request to make *)
      let url = match Playground3d.embedded src with Some base64 -> "data:image/png;base64," ^ base64 | None -> src in
      img##.src := Js.string url;
      Hashtbl.replace images src img;
      img

type texture = { tex : WebGL.texture Js.t; mutable uploaded : bool }

let magenta_pixel () = new%js Typed_array.uint8Array_fromArray (Js.array [| 255; 0; 255; 255 |])

let create_texture (gl : WebGL.renderingContext Js.t) : WebGL.texture Js.t =
  let tex = gl##createTexture in
  gl##bindTexture gl##._TEXTURE_2D_ tex;
  gl##texImage2D_fromView gl##._TEXTURE_2D_ 0 gl##._RGBA 1 1 0 gl##._RGBA gl##._UNSIGNED_BYTE_ (magenta_pixel ());
  (* WebGL 1 can only sample a texture whose size isn't a power of 2
   * (e.g. a 100x60 image) with this wrap mode and no mipmaps (the
   * filters set in draw_group don't use any) *)
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_S_ gl##._CLAMP_TO_EDGE_;
  gl##texParameteri gl##._TEXTURE_2D_ gl##._TEXTURE_WRAP_T_ gl##._CLAMP_TO_EDGE_;
  tex

(* No v-flip: from an <img>, texImage2D puts the image's top row at
 * v = 0 (UNPACK_FLIP_Y_WEBGL is false by default), the convention of
 * the playground's UVs (see Playground3d.textured_quad), like the
 * OpenGL backend's upload of Texture_decode's rows.
 *
 * A complete image with no width failed to load (e.g. a 404): it stays
 * magenta. texImage2D itself can fail too, with a SecurityError, on an
 * image the browser considers from another origin, which includes any
 * image of a page opened as file:// in Chrome: then too, magenta, and
 * a message in the console (serve the page over http instead, e.g.
 * with 'make serve-build'). Either way the texture is marked uploaded, so each
 * problem is reported once, not every frame. *)
let try_upload (gl : WebGL.renderingContext Js.t) (src : string) (t : texture) : unit =
  let img = image_of src in
  (* claude: a plain int, not an optdef, since js_of_ocaml 6 *)
  let loaded = img##.naturalWidth > 0 in
  if Js.to_bool img##.complete then begin
    t.uploaded <- true;
    if loaded then begin
      gl##bindTexture gl##._TEXTURE_2D_ t.tex;
      try gl##texImage2D_fromImage gl##._TEXTURE_2D_ 0 gl##._RGBA gl##._RGBA gl##._UNSIGNED_BYTE_ img
      with exn ->
        Console.console##warn
          (Js.string
             (Printf.sprintf "playground3d webgl: can't use texture %s (%s)" src (Printexc.to_string exn)))
    end
    else Console.console##warn (Js.string (Printf.sprintf "playground3d webgl: can't load texture %s" src))
  end
