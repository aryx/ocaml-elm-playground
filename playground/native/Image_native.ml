(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Loading of external images (e.g., from a URL) into Cairo surfaces, for
 * the native (SDL+Cairo) Playground backend. Pulled out of
 * Playground_platform.ml since it's independent of Playground/rendering.
 *
 * claude: survey of OCaml image-decoding libraries considered here, and
 * why we ended up on stb_image:
 *
 * - imagelib (what we used before): pure OCaml (PNG/GIF/BMP/PPM/... decoders
 *   written from scratch), so no C toolchain/system library needed, which
 *   is attractive for portability. In practice though its from-scratch GIF
 *   LZW decoder has a real correctness bug: [calc_clear_code] in
 *   src/imageGIF.ml derives the LZW "clear code" as [1 lsl (lzw_min_size-1)]
 *   instead of [1 lsl lzw_min_size], which desyncs the decoder on some
 *   perfectly valid real-world GIFs (e.g. elm-lang.org's Mario jump
 *   sprites) and raises [Image.Corrupted_image] instead of decoding them.
 *   Confirmed this is still unfixed as of the latest opam release
 *   (20221222) *and* the current upstream git master. It's the only pure-OCaml
 *   option, but "pure OCaml" isn't worth much if the decoder is wrong.
 * - stb_image (what we use now): OCaml bindings to Sean Barrett's
 *   stb_image.h, a public-domain single-header C library that's one of the
 *   most widely deployed image loaders in the game/graphics world (used by
 *   countless engines and tools), so its PNG/JPEG/GIF/BMP/... decoders are
 *   exercised by a vastly bigger and more adversarial corpus of real files
 *   than imagelib's. Decodes straight to a raw RGBA8 pixel buffer, which we
 *   convert to a Cairo surface ourselves (see [cairo_surface_of_stb_image]
 *   below) -- no intermediate PNG round-trip needed. Verified it correctly
 *   decodes the exact GIFs that broke imagelib. Downside: the OCaml binding
 *   package itself is old and builds via a plain Makefile rather than dune,
 *   which is a bit more fragile on non-Unix platforms (Windows) than a
 *   dune-native C-stub library would be -- though [tsdl], which this native
 *   backend already depends on for SDL2 windowing, has its own known
 *   Windows issues (see playground/native/dune), so this isn't a new
 *   platform-support regression.
 * - camlimages: broader format coverage and a long track record, but it's a
 *   heavier/older-style library: C bindings to system libjpeg/libpng/giflib
 *   etc, meaning more system library dependencies to install on every
 *   platform (and in the Dockerfile/CI) versus stb_image's self-contained,
 *   vendored C source.
 * - tsdl-image (SDL2_image bindings): would be a very natural fit since we
 *   already depend on [tsdl] for the window, but it requires a system
 *   SDL2_image install (conf-sdl2-image), another moving part for
 *   Dockerfile/CI/macOS/Windows setups that stb_image avoids entirely.
 * - bimage-unix: can decode via ImageMagick or stb_image under the hood,
 *   but pulls in the whole bimage stack for what we need here; simpler to
 *   depend on stb_image directly.
 *)

(*****************************************************************************)
(* Download *)
(*****************************************************************************)
(* from ocurl/examples/opar.ml *)
let writer accum data =
  Buffer.add_string accum data;
  String.length data

let save fname content =
  let fp = open_out_bin fname in
    Buffer.output_buffer fp content;
    close_out fp

let curl_url fname url =
  let result = Buffer.create 16384 in
  let conn = Curl.init () in
  Curl.set_writefunction conn (writer result);
  Curl.set_followlocation conn true;
  Curl.set_url conn url;
  Curl.perform conn;
  Curl.cleanup conn;
  save fname result

(*****************************************************************************)
(* Decoding *)
(*****************************************************************************)

(* claude: stb_image decodes to an interleaved, row-major, top-to-bottom
 * RGBA8 buffer (we force ~channels:4 below so this shape is always what we
 * get, even for e.g. an opaque JPEG). Cairo.Image.create_for_data32 wants a
 * (height, width) Bigarray.Array2.t of int32, each int32 being a
 * premultiplied-alpha ARGB32 pixel (alpha in the top byte); this is the
 * same layout/orientation, so we just need to repack/premultiply. *)
let cairo_surface_of_stb_image (img : Stb_image.int8 Stb_image.t) : Cairo.Surface.t =
  let w = img.width and h = img.height in
  let data = img.data in
  let pixels =
    Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout h w in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let o = (y * w + x) * 4 in
      let r = Bigarray.Array1.unsafe_get data o in
      let g = Bigarray.Array1.unsafe_get data (o + 1) in
      let b = Bigarray.Array1.unsafe_get data (o + 2) in
      let a = Bigarray.Array1.unsafe_get data (o + 3) in
      let premultiply c = c * a / 255 in
      let pixel =
        (a lsl 24) lor (premultiply r lsl 16)
        lor (premultiply g lsl 8) lor (premultiply b)
      in
      Bigarray.Array2.unsafe_set pixels y x (Int32.of_int pixel)
    done
  done;
  Cairo.Image.create_for_data32 pixels

let surface_of_url_exn url =
  let fn = Filename.temp_file "playground_img" (Filename.extension url) in
  curl_url fn url;
  match Stb_image.load ~channels:4 fn with
  | Ok img -> cairo_surface_of_stb_image img
  | Error (`Msg msg) ->
    failwith (Printf.sprintf "could not decode image %s: %s" url msg)

(*****************************************************************************)
(* Cairo surface cache *)
(*****************************************************************************)

(* url -> surface *)
let himages : (string, Cairo.Surface.t option) Hashtbl.t = Hashtbl.create 101

(* claude: cache None on decode/download failure so callers can degrade to
 * "skip this image" instead of crashing, and so we don't retry every
 * frame *)
let surface_of_url src =
  match Hashtbl.find_opt himages src with
  | Some surface_opt -> surface_opt
  | None ->
    let surface_opt =
      try Some (surface_of_url_exn src)
      with exn ->
        Logs.warn (fun m -> m "failed to load image %s: %s"
                      src (Printexc.to_string exn));
        None
    in
    Hashtbl.add himages src surface_opt;
    surface_opt
