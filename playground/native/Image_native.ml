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

(* claude: url -> downloaded file, so the "Animated GIFs" section below
 * can read the file again to extract all the frames *)
let hfiles : (string, string) Hashtbl.t = Hashtbl.create 101

let surface_of_url_exn url =
  Logs.info (fun m -> m "loading image %s" url);
  let fn = Filename.temp_file "playground_img" (Filename.extension url) in
  curl_url fn url;
  Hashtbl.replace hfiles url fn;
  match Stb_image.load ~channels:4 fn with
  | Ok img ->
    Logs.info (fun m -> m "loaded image %s (%dx%d)" url img.width img.height);
    cairo_surface_of_stb_image img
  | Error (`Msg msg) ->
    failwith (Printf.sprintf "could not decode image %s: %s" url msg)

(*****************************************************************************)
(* Cairo surface cache *)
(*****************************************************************************)

(* claude: surface_of_url_exn above does a synchronous network fetch
 * (curl_url) that can easily take several hundred ms, so calling it
 * lazily from render_image on a cache miss -- i.e., mid-game, the first
 * time a given sprite variant is actually needed -- freezes the whole
 * render+input loop for that long. Tried making that background
 * (Thread.create + a Mutex-protected cache, polled every frame) instead;
 * it avoided the freeze but the image would then pop in only ~1s later,
 * in the middle of gameplay, which isn't great either, and it added a
 * fair amount of complexity (thread lifecycle, cross-thread Cairo
 * surface creation, a mutex around every cache access) for what turned
 * out to be the wrong fix.
 *
 * What actual games do instead: load all needed assets up front, in a
 * loading phase, before the game loop starts -- see [preload] below and
 * Playground_platform.preload_image, called once per sprite in
 * examples/Mario.ml's init. That makes the cache purely synchronous
 * again: by the time the game loop runs, every url it'll ask for is
 * already cached, so the slow path here never runs during actual
 * gameplay. *)
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

(*****************************************************************************)
(* Animated GIFs *)
(*****************************************************************************)
(* claude: stb_image only returns the *first* frame of a GIF: the OCaml
 * binding (stb_image 0.5) only exposes load/decode, i.e., the C
 * stbi_load(), and the old stb_image.h it bundles does not even have
 * stbi_load_gif_from_memory() (the newer API returning all the frames).
 * So animated GIFs, e.g., elm-lang.org's Mario "walk" sprites (8 frames)
 * or turtle.gif (8 frames), were not animated natively, while the
 * browser animates them on the web.
 *
 * The approach here, which does not need another library:
 *  1) parse the GIF file format ourselves, just enough to cut the file
 *     into its frames (this is easy: a GIF is a sequence of blocks, and
 *     we don't need to decompress the pixel data, only to copy it);
 *  2) for each frame, build in memory a standalone, single-frame GIF
 *     file with the same colors and pixel data, and decode it with
 *     stb_image as before;
 *  3) compose the frames like a browser does: a frame is often just a
 *     small patch (e.g., Mario's frames 2-8 are about 16x26 pixels at
 *     some (x, y) offset in the 35x35 image) to draw over the previous
 *     full image, which must then be "disposed" of in a way specified
 *     in each frame;
 *  4) at render time, choose the frame to display according to the
 *     current time (see surface_of_url_at).
 *
 * Background: frames, delays, and the game's 60Hz
 * -----------------------------------------------
 * An animated GIF is like a flip book: one file containing several still
 * pictures (frames), each with a display duration (its delay). The
 * viewer shows frame 1, waits, shows frame 2, ..., and after the last
 * frame starts again from frame 1. For example mario/walk/left.gif
 * contains 8 frames (8 poses of Mario walking), each displayed 0.1s, so
 * one walk cycle lasts 8 * 0.1 = 0.8s and then repeats:
 *
 *   time (s)  0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9 ...
 *   frame      1    2    3    4    5    6    7    8    1    2  ...
 *             |<------------- one cycle = 0.8s ------------>|
 *
 * GIF delays are in 1/100s. Those Mario files actually say 0 (common in
 * old GIFs, whose tools often did not bother to set it). Taken
 * literally, 0 would mean "switch frames as fast as possible", which
 * would make the animation unwatchable and burn CPU, so browsers treat a
 * delay of 0 (or 1) as 10, i.e., 0.1s. We do the same, hence the 0.1s,
 * so that the native version looks like the web one.
 *
 * Frames are often just patches
 * -----------------------------
 * To make files smaller, a frame does not have to be a full picture: it
 * can be a smaller rectangle ("patch") drawn at some (x, y) position over
 * what is already displayed. In mario/walk/left.gif, frame 1 is the full
 * 35x35 image, but frames 2 to 8 are only the area around Mario's body
 * that changes, e.g., frame 2 is a 16x26 patch at (9, 5):
 *
 *        0         9                  25       35
 *      0 +-----------------------------------+
 *        |          full image (35x35)       |
 *      5 |         +----------------+        |
 *        |         |  frame 2 patch |        |
 *        |         |   (16x26)      |        |
 *     31 |         +----------------+        |
 *     35 +-----------------------------------+
 *
 * Each frame also says what to do with its patch once its delay is over,
 * before drawing the next frame (its "disposal method"):
 *  - 0 or 1: leave it, the next patch is drawn on top of it;
 *  - 2: erase the patch area (browsers make it transparent; the spec says
 *       "restore to background color", but browsers do not do that);
 *  - 3: restore what was there before the patch was drawn.
 * Mario's frames mostly use 2: each pose is drawn on an empty
 * (transparent) area, so the previous pose does not show through. Inside
 * a patch, transparent pixels also let what is below show through.
 * This is why animation_of_gif below keeps a "canvas" of the full image
 * size, draws each patch on it, takes a snapshot of it (the full picture
 * for that frame), then applies the disposal method.
 *
 * The GIF animation has its own clock, independent from the game loop.
 * The game loop (Playground_platform.run_app) runs 60 times per second
 * (every ~0.0167s): it updates the model and redraws the screen, and at
 * each redraw surface_of_url_at computes (time mod 0.8) to know which
 * frame should be visible at that moment. Since 0.1s = 6 redraws at 60Hz,
 * each GIF frame stays on screen for about 6 redraws:
 *
 *   redraws (60Hz):  | | | | | | | | | | | | | | | | | | ...
 *   GIF frame:       1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 ...
 *                   |<- 0.1s ->|<- 0.1s ->|<- 0.1s ->|
 *
 * Concretely, surface_of_url_at takes the current time in seconds
 * (Unix.gettimeofday (), e.g., 1789590431.53), keeps only the position
 * in the current cycle with Float.rem (e.g., 1789590431.53 mod 0.8 =
 * 0.33), then walks the frames subtracting their delays until the
 * remainder is smaller than the frame's delay: 0.33 -> frame 1 covers
 * [0, 0.1), frame 2 covers [0.1, 0.2), frame 3 [0.2, 0.3), frame 4
 * [0.3, 0.4), so frame 4 is displayed. All the full pictures were
 * computed once when the image was loaded (see load_queued), so this
 * is cheap enough to do at each redraw.
 *
 * The game only chooses *which* GIF to display (e.g., examples/Mario.ml's
 * to_gif picks walk/, stand/, or jump/ depending on Mario's state), the
 * walk GIF animates on its own. A faster screen (e.g., 120Hz on the web)
 * just shows each GIF frame during more redraws.
 *
 * Making your own animated sprites: see playground/making_sprites.mld
 * (published as docs/elm_playground/making_sprites.html by make website).
 *
 * The GIF format (GIF89a spec, https://www.w3.org/Graphics/GIF/spec-gif89a.txt):
 *
 *   "GIF89a"                             6 bytes header ("GIF87a" also ok)
 *   logical screen descriptor            7 bytes: width (2 bytes, little
 *                                        endian), height (2), flags (1),
 *                                        background color index (1),
 *                                        pixel aspect ratio (1)
 *   [global color table]                 3 * 2^(1 + (flags land 7)) bytes,
 *                                        if flags land 0x80
 *   blocks, each starting with a byte:
 *    0x21 = extension:                   0x21 label sub-blocks...
 *      label 0xF9 = graphic control ext. (applies to the next image):
 *                                        0x21 0xF9 0x04 packed delay(2)
 *                                        transparent_index 0x00
 *                                        where packed = disposal method
 *                                        (bits 2-4) and has_transparency
 *                                        (bit 0); delay in 1/100s
 *      other labels (comments, NETSCAPE looping, ...): ignored
 *    0x2C = image (a frame):             0x2C x(2) y(2) width(2) height(2)
 *                                        flags(1)
 *                                        [local color table] (same rule
 *                                        as the global one)
 *                                        lzw_min_code_size(1) sub-blocks...
 *    0x3B = end of file
 *   sub-blocks = sequence of (size byte, size bytes of data), ending with
 *                a 0 size byte
 *)

type gif_frame = {
  (* position and size of the frame patch in the full image *)
  fx: int; fy: int; fw: int; fh: int;
  (* how long to display this frame, in seconds *)
  delay: float;
  (* disposal method: what to do with the frame patch before drawing the
   * next frame: 0 or 1 = leave it, 2 = clear the patch area (to
   * transparent, like browsers do), 3 = restore what was there before *)
  disposal: int;
  (* a standalone GIF file containing just this frame, at (0, 0) *)
  gif: string;
}

(* Parse a GIF file content; returns the full image size and the frames,
 * or raises Failure/Invalid_argument if the content is not a valid GIF.
 *)
let gif_frames (s : string) : (int * int) * gif_frame list =
  let byte i = Char.code s.[i] in
  let u16 i = byte i lor (byte (i + 1) lsl 8) in
  if String.length s < 13 || String.sub s 0 3 <> "GIF"
  then failwith "not a GIF";
  let width = u16 6 and height = u16 8 in
  let color_table_size flags =
    if flags land 0x80 <> 0 then 3 * (1 lsl (1 + (flags land 7))) else 0 in
  (* the header, screen descriptor, and global color table are copied in
   * each single-frame GIF, with the screen size replaced by the frame
   * size *)
  let global_end = 13 + color_table_size (byte 10) in
  let global_table = String.sub s 13 (global_end - 13) in
  (* returns the position after the sub-blocks starting at i *)
  let rec skip_sub_blocks i =
    let size = byte i in
    if size = 0 then i + 1 else skip_sub_blocks (i + 1 + size)
  in
  (* the last graphic control extension seen, for the next frame:
   * (packed, delay, transparent_index) *)
  let rec loop i gce acc =
    match byte i with
    | 0x3B -> List.rev acc
    | 0x21 when byte (i + 1) = 0xF9 ->
        let gce = Some (byte (i + 3), u16 (i + 4), byte (i + 6)) in
        loop (skip_sub_blocks (i + 2)) gce acc
    | 0x21 ->
        loop (skip_sub_blocks (i + 2)) gce acc
    | 0x2C ->
        let fx = u16 (i + 1) and fy = u16 (i + 3) in
        let fw = u16 (i + 5) and fh = u16 (i + 7) in
        let flags = byte (i + 9) in
        let data_start = i + 10 + color_table_size flags in
        (* data_start is the lzw_min_code_size byte *)
        let data_end = skip_sub_blocks (data_start + 1) in
        let packed, delay_cs, transparent =
          match gce with
          | Some (p, d, t) -> p, d, t
          | None -> 0, 0, 0
        in
        let b = Buffer.create (data_end - i + 64) in
        let add_u16 n =
          Buffer.add_char b (Char.chr (n land 0xFF));
          Buffer.add_char b (Char.chr (n lsr 8)) in
        Buffer.add_string b "GIF89a";
        add_u16 fw; add_u16 fh;
        Buffer.add_string b (String.sub s 10 3);
        Buffer.add_string b global_table;
        (* keep only the transparency flag of the graphic control
         * extension (disposal and delay are handled by us) *)
        Buffer.add_string b "\x21\xF9\x04";
        Buffer.add_char b (Char.chr (packed land 1));
        add_u16 0;
        Buffer.add_char b (Char.chr transparent);
        Buffer.add_char b '\x00';
        (* the image descriptor, moved to (0, 0), then the local color
         * table and the compressed pixel data, copied as is *)
        Buffer.add_char b '\x2C';
        add_u16 0; add_u16 0;
        Buffer.add_string b (String.sub s (i + 5) (data_end - (i + 5)));
        Buffer.add_char b '\x3B';
        let frame = {
          fx; fy; fw; fh;
          (* like Chrome and Firefox: a delay of 0 or 1/100s (common in
           * old GIFs, including Mario's) means 1/10s *)
          delay = (if delay_cs <= 1 then 0.1 else float delay_cs /. 100.);
          disposal = (packed lsr 2) land 7;
          gif = Buffer.contents b;
        } in
        loop data_end None (frame :: acc)
    | c -> failwith (Printf.sprintf "unexpected GIF block 0x%02X at %d" c i)
  in
  (width, height), loop global_end None []

let decode_string (s : string) : Stb_image.int8 Stb_image.t =
  let buf = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
      (String.length s) in
  String.iteri (fun i c -> buf.{i} <- Char.code c) s;
  match Stb_image.decode ~channels:4 buf with
  | Ok img -> img
  | Error (`Msg msg) -> failwith msg

type animation = {
  (* each full image, and how long to display it (in seconds) *)
  frames: (Cairo.Surface.t * float) array;
  (* sum of the delays *)
  duration: float;
}

(* Compose the frames into full images; the "canvas" is an RGBA8 buffer
 * of the full image size, in the same layout as what stb_image returns,
 * so we can reuse cairo_surface_of_stb_image to convert snapshots of it.
 *)
let animation_of_gif (s : string) : animation =
  let (w, h), frames = gif_frames s in
  let new_canvas () =
    let c = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout
        (w * h * 4) in
    Bigarray.Array1.fill c 0;
    c
  in
  let copy_canvas c =
    let c' = new_canvas () in
    Bigarray.Array1.blit c c';
    c'
  in
  let canvas = new_canvas () in
  (* iterate over the pixels of the frame patch that are inside the
   * canvas; f gets the offset in the canvas, and in the patch *)
  let iter_patch (fr : gif_frame) f =
    for y = 0 to fr.fh - 1 do
      for x = 0 to fr.fw - 1 do
        let cx = fr.fx + x and cy = fr.fy + y in
        if cx < w && cy < h
        then f ((cy * w + cx) * 4) ((y * fr.fw + x) * 4)
      done
    done
  in
  let images =
    frames |> List.map (fun (fr : gif_frame) ->
      let before = if fr.disposal = 3 then Some (copy_canvas canvas) else None in
      (* draw the patch; its transparent pixels (alpha 0) let the
       * previous image show through *)
      let (patch : Stb_image.int8 Stb_image.t) = decode_string fr.gif in
      let data = patch.data in
      iter_patch fr (fun co po ->
        if data.{po + 3} <> 0 then
          for k = 0 to 3 do canvas.{co + k} <- data.{po + k} done
      );
      let snapshot =
        match Stb_image.image ~width:w ~height:h ~channels:4
                (copy_canvas canvas) with
        | Ok img -> cairo_surface_of_stb_image img
        | Error (`Msg msg) -> failwith msg
      in
      (* dispose of the patch before the next frame *)
      (match fr.disposal, before with
      | 2, _ -> iter_patch fr (fun co _ -> for k = 0 to 3 do canvas.{co + k} <- 0 done)
      | 3, Some c -> Bigarray.Array1.blit c canvas
      | _ -> ()
      );
      (snapshot, fr.delay)
    ) |> Array.of_list
  in
  { frames = images;
    duration = Array.fold_left (fun acc (_, d) -> acc +. d) 0. images }

let hanimations : (string, animation option) Hashtbl.t = Hashtbl.create 101

(* None if the image is not an animated GIF (or could not be loaded or
 * parsed, in which case we just display its first frame as before) *)
let animation_of_url (src : string) : animation option =
  match Hashtbl.find_opt hanimations src with
  | Some anim_opt -> anim_opt
  | None ->
    let anim_opt =
      (* make sure the file was downloaded *)
      match surface_of_url src, Hashtbl.find_opt hfiles src with
      | Some _, Some file ->
          let content =
            let ic = open_in_bin file in
            let len = in_channel_length ic in
            let buf = Bytes.create len in
            really_input ic buf 0 len;
            close_in ic;
            Bytes.unsafe_to_string buf
          in
          if String.length content >= 3 && String.sub content 0 3 = "GIF" then
            (try
              let anim = animation_of_gif content in
              if Array.length anim.frames > 1 then begin
                Logs.info (fun m -> m "animated image %s: %d frames, %.2fs"
                              src (Array.length anim.frames) anim.duration);
                Some anim
              end else None
            with exn ->
              Logs.warn (fun m -> m "could not extract GIF frames of %s: %s"
                            src (Printexc.to_string exn));
              None)
          else None
      | _ -> None
    in
    Hashtbl.add hanimations src anim_opt;
    anim_opt

(* Like surface_of_url, but for an animated GIF returns the frame to
 * display at [time] (in seconds, e.g., Unix.gettimeofday ()), the
 * animation looping forever (like in browsers, which is what
 * elm-playground relies on). *)
let surface_of_url_at ~(time : float) (src : string) : Cairo.Surface.t option =
  match animation_of_url src with
  | None -> surface_of_url src
  | Some anim ->
      let t = Float.rem time anim.duration in
      let n = Array.length anim.frames in
      let rec find i t =
        let (surface, delay) = anim.frames.(i) in
        if t < delay || i = n - 1 then Some surface else find (i + 1) (t -. delay)
      in
      find 0 t

(*****************************************************************************)
(* Preloading *)
(*****************************************************************************)

(* claude: preload just enqueues -- it doesn't touch the network itself,
 * so it has no ordering dependency on anything (CLI/logging setup, the
 * SDL window existing, ...) and is safe to call anytime, including
 * before Playground_platform.run_app has even started. run_app calls
 * [load_queued] once it has parsed argv, set up logging, and created its
 * window, so preloading only starts once there's a visible window and
 * -v/-debug would actually show progress. *)
let queued : string Queue.t = Queue.create ()

let preload src = Queue.push src queued

let load_queued () =
  Queue.iter (fun src ->
    ignore (surface_of_url src : Cairo.Surface.t option);
    (* claude: also extract the frames of animated GIFs now, rather than
     * on first use mid-game (see the "Animated GIFs" section) *)
    ignore (animation_of_url src : animation option))
    queued;
  Queue.clear queued
