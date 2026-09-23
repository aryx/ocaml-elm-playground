(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Loading of external images (e.g., from a URL) into RGBA8 pixel
 * buffers, for the native (SDL-based) Playground backends, which each
 * convert them to what they draw with (e.g., Cairo surfaces in
 * playground/native/Image_native.ml).
 *
 * claude: the decoders are our own, pure OCaml, one library a format
 * (png/, gif/, jpeg/, with deflate/ under PNG; see notes_images.md and
 * plan_images_teaching.md): code to read and learn from, like the rest
 * of graphics/, and no C library to build. The OCaml libraries
 * considered, and why none was kept:
 *
 * - imagelib: pure OCaml, but its GIF LZW decoder derives the clear
 *   code as [1 lsl (lzw_min_size-1)] instead of [1 lsl lzw_min_size]
 *   ([calc_clear_code] in src/imageGIF.ml, unfixed in the 20221222
 *   release and upstream), which desyncs it on valid GIFs such as
 *   elm-lang.org's Mario jump sprites (Lzw.mli has a test for it).
 * - stb_image: bindings to Sean Barrett's single-header C decoder, the
 *   game world's default, but the binding (0.5) is old, builds with a
 *   plain Makefile, and bundles an old stb_image.h: it truncates an RGB
 *   image asked for 4 channels, misdecodes interlaced 16-bit PNGs,
 *   ignores a PNG's tRNS color on gray and RGB pictures (Unit_png's
 *   PngSuite comment), and returns only a GIF's first frame.
 * - camlimages: C bindings to the system's libjpeg, libpng, giflib:
 *   more system libraries to install on every platform and in CI.
 * - tsdl-image (SDL2_image): a natural fit next to tsdl, but another
 *   system library (conf-sdl2-image) to install.
 * - bimage-unix: decodes through ImageMagick or stb_image, and brings
 *   the whole bimage stack.
 *)

(*****************************************************************************)
(* Decoding *)
(*****************************************************************************)

(* claude: an interleaved, row-major, top-to-bottom RGBA8 buffer, whatever
 * the file's channels (see Rgba_image.mli) *)
type image = Rgba_image.t

let read_file (file : string) : string =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  close_in ic;
  Bytes.unsafe_to_string buf

(* claude: the decoder is chosen by the file's first bytes, never by its
 * name (see notes_images.md section 1) *)
let decode_string (s : string) : image =
  let starts_with magic = String.length s >= String.length magic && String.sub s 0 (String.length magic) = magic in
  if starts_with Png.signature then Png.decode s
  else if starts_with "GIF8" then Gif.decode s
  else if starts_with "\xFF\xD8\xFF" then Jpeg.decode s
  else failwith "not a PNG, a GIF or a JPEG: an image format not read here"

(* claude: url -> local file (downloaded, for a URL), so the "Animated
 * GIFs" section below can read the file again to extract all the
 * frames *)
let hfiles : (string, string) Hashtbl.t = Hashtbl.create 101

let image_of_url_exn url : image =
  Logs.info (fun m -> m "loading image %s" url);
  let fn = Download.local_file ~prefix:"playground_img" url in
  Hashtbl.replace hfiles url fn;
  match decode_string (read_file fn) with
  | img ->
    Logs.info (fun m -> m "loaded image %s (%dx%d)" url img.width img.height);
    img
  | exception Failure msg ->
    failwith (Printf.sprintf "could not decode image %s: %s" url msg)

(*****************************************************************************)
(* Image cache *)
(*****************************************************************************)

(* claude: image_of_url_exn above does a synchronous network fetch
 * (Download.local_file) that can easily take several hundred ms, so calling it
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
let himages : (string, image option) Hashtbl.t = Hashtbl.create 101

(* claude: cache None on decode/download failure so callers can degrade to
 * "skip this image" instead of crashing, and so we don't retry every
 * frame *)
let image_of_url src =
  match Hashtbl.find_opt himages src with
  | Some image_opt -> image_opt
  | None ->
    let image_opt =
      try Some (image_of_url_exn src)
      with exn ->
        Logs.warn (fun m -> m "failed to load image %s: %s"
                      src (Printexc.to_string exn));
        None
    in
    Hashtbl.add himages src image_opt;
    image_opt

(*****************************************************************************)
(* Animated GIFs *)
(*****************************************************************************)
(* claude: an animated GIF, e.g., elm-lang.org's Mario "walk" sprites (8
 * frames) or turtle.gif (8 frames), is animated natively as the browser
 * animates it on the web: Gif.animation composes its frames into full
 * pictures once, when it's loaded, and at render time frame_at chooses
 * the one to display according to the current time.
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
 * A frame is often just a patch, drawn over the picture so far and then
 * disposed of (see Gif.mli): Mario's frames mostly clear their patch,
 * so the previous pose does not show through the next.
 *
 * The GIF animation has its own clock, independent from the game loop.
 * The game loop (Playground_platform.run_app) runs 60 times per second
 * (every ~0.0167s): it updates the model and redraws the screen, and at
 * each redraw frame_at computes (time mod 0.8) to know which
 * frame should be visible at that moment. Since 0.1s = 6 redraws at 60Hz,
 * each GIF frame stays on screen for about 6 redraws:
 *
 *   redraws (60Hz):  | | | | | | | | | | | | | | | | | | ...
 *   GIF frame:       1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 ...
 *                   |<- 0.1s ->|<- 0.1s ->|<- 0.1s ->|
 *
 * Concretely, frame_at takes the current time in seconds
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
 *)

(* claude: polymorphic in the frame type so a backend can convert every
 * frame once (see [map_animation]) to whatever it draws with, e.g. a
 * Cairo surface, and still use [frame_at] to pick the right one *)
type 'a animation = {
  (* each full image, and how long to display it (in seconds) *)
  frames: ('a * float) array;
  (* sum of the delays *)
  duration: float;
}

let animation_of_gif (s : string) : image animation =
  let images = Array.of_list (Gif.animation s) in
  { frames = images;
    duration = Array.fold_left (fun acc (_, d) -> acc +. d) 0. images }

let hanimations : (string, image animation option) Hashtbl.t = Hashtbl.create 101

(* None if the image is not an animated GIF (or could not be loaded or
 * parsed, in which case we just display its first frame as before) *)
let animation_of_url (src : string) : image animation option =
  match Hashtbl.find_opt hanimations src with
  | Some anim_opt -> anim_opt
  | None ->
    let anim_opt =
      (* make sure the file was downloaded *)
      match image_of_url src, Hashtbl.find_opt hfiles src with
      | Some _, Some file ->
          let content = read_file file in
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

let map_animation (f : 'a -> 'b) (anim : 'a animation) : 'b animation =
  { anim with frames = Array.map (fun (x, delay) -> (f x, delay)) anim.frames }

(* The frame to display at [time] (in seconds, e.g., Unix.gettimeofday ()),
 * the animation looping forever (like in browsers, which is what
 * elm-playground relies on). *)
let frame_at ~(time : float) (anim : 'a animation) : 'a =
  let t = Float.rem time anim.duration in
  let n = Array.length anim.frames in
  let rec find i t =
    let (frame, delay) = anim.frames.(i) in
    if t < delay || i = n - 1 then frame else find (i + 1) (t -. delay)
  in
  find 0 t

(* Like image_of_url, but for an animated GIF returns the frame to
 * display at [time] *)
let image_of_url_at ~(time : float) (src : string) : image option =
  match animation_of_url src with
  | None -> image_of_url src
  | Some anim -> Some (frame_at ~time anim)

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
  let srcs = List.of_seq (Queue.to_seq queued) in
  srcs |> List.iter (fun src ->
    ignore (image_of_url src : image option);
    (* claude: also extract the frames of animated GIFs now, rather than
     * on first use mid-game (see the "Animated GIFs" section) *)
    ignore (animation_of_url src : image animation option));
  Queue.clear queued;
  srcs
