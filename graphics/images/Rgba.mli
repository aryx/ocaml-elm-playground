(* Decoded images, whatever their channels, as 4-channel RGBA.
 *
 * Image files come with 1 (gray), 2 (gray + alpha), 3 (RGB) or 4 (RGBA)
 * channels per pixel; the code drawing them (graphics/core/Blit, the
 * Cairo backend's Image_native, the 3D samplers, OpenGL uploads) is
 * simpler with one layout: 4 bytes per pixel, R G B A, rows one after
 * the other, no offset.
 *
 * Why not just Stb_image.load ~channels:4? Because the OCaml binding of
 * the pinned stb_image (0.5) gets it wrong for an image with fewer
 * channels: stb_image lays the pixels out 4 bytes each, but the binding
 * allocates the buffer for the file's own channel count, and reports
 * that count. For a 64x64 RGB PNG: channels = 3, stride = 192, and a
 * 12288-byte buffer holding the first three quarters of the pixels, 4
 * bytes each (checked by hand, and in graphics/tests/Unit_rgba.ml).
 * Reading it as RGBA then reads past its end. So: load with the file's
 * own channels, and expand here, in OCaml. *)

(* [of_stb_image img]: [img] as RGBA (4 channels, stride = width * 4,
 * offset 0), a copy unless it's already so; gray becomes r = g = b, a
 * missing alpha becomes 255 (opaque) *)
val of_stb_image : Stb_image.int8 Stb_image.t -> Stb_image.int8 Stb_image.t
