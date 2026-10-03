(* Webgl_textures: the WebGL backend's textures. The other backends
 * decode theirs themselves (Texture_decode, blocking); here the browser
 * does it, from an <img>, which means asynchronously: setting its src
 * starts the download and returns at once. So a texture goes through
 * two steps:
 *
 *   - its <img>, made at the first request (a preload, or the first
 *     frame that draws it), before any GL context exists, so that a
 *     preload can start early: [image_of];
 *   - its GL texture, made at the first draw as the 1 by 1 magenta
 *     pixel of a missing texture, and replaced by the image, once, the
 *     first frame after the image is complete: [create_texture], then
 *     [try_upload] each frame until done.
 *
 * Until then, or for ever if the image cannot be loaded, the faces are
 * magenta. The image's state is looked at each frame, not in its onload
 * handler: every GL call then stays inside the drawing, and an image
 * arriving before there is a GL context is not a case to handle.
 *
 * Which texture belongs to which source is the platform's to keep (its
 * GL state's table).
 *)

open Js_of_ocaml

(* a source's GL texture, and whether the image has replaced the
 * magenta pixel (or been given up on) *)
type texture = { tex : WebGL.texture Js.t; mutable uploaded : bool }

(* the <img> of a source, its download started if this is the first
 * request: a path, a URL (asked with CORS, which WebGL requires to read
 * another site's pixels), or an embedded picture as a data: URL *)
val image_of : string -> Dom_html.imageElement Js.t

(* a new texture holding the magenta pixel, clamped at its edges (its
 * filter is the drawing's to set, a rendering hint) *)
val create_texture : WebGL.renderingContext Js.t -> WebGL.texture Js.t

(* [try_upload gl src t]: if [src]'s image is complete, its pixels put
 * in [t], and [t] marked uploaded. Marked too when the image failed (a
 * 404) or the browser refuses its pixels (another origin; any image of
 * a file:// page in Chrome): magenta stays, and the console says why,
 * once *)
val try_upload : WebGL.renderingContext Js.t -> string -> texture -> unit
