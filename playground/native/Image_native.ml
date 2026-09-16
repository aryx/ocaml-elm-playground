(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Loading of external images (e.g., from a URL) into Cairo surfaces, for
 * the native (SDL+Cairo) Playground backend. Pulled out of
 * Playground_platform.ml since it's independent of Playground/rendering.
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

(* claude: imagelib's GIF LZW decoder (imageGIF.ml) has an off-by-one bug in
 * how it derives the clear code from the LZW minimum code size, which
 * desyncs the decoder on some real-world GIFs (e.g., the elm-lang.org mario
 * jump sprites, https://elm-lang.org/images/mario/jump/{left,right}.gif),
 * raising Image.Corrupted_image. When that happens, shell out to
 * ImageMagick's 'convert', which decodes those GIFs correctly. *)
let convert_with_imagemagick src_file dst_file =
  let cmd = Printf.sprintf "convert %s %s"
      (Filename.quote src_file) (Filename.quote dst_file) in
  Sys.command cmd = 0

let png_file_of_url url =
  (* copy of ImageLib_unix.openfile url but not always falling back for GIF
   * to convert, which does not work well on my mac at least  *)
  let ext = ImageUtil_unix.get_extension' url in
  let fn = Filename.temp_file "imagelib1" ("." ^ ext) in
  curl_url fn url;
  let tmpfile =
    Filename.temp_file "imagelib2" ".png"
    (* "/tmp/imagelib.png" *)
  in
  (try
    let ich = ImageUtil_unix.chunk_reader_of_path fn in
    let extension = ImageUtil_unix.get_extension' fn in
    Logs.debug (fun m -> m "reading png_file_of_url");
    let image =
      try ImageLib.openfile ~extension ich
      with Image.Not_yet_implemented _ -> failwith (Printf.sprintf "PB with %s" fn)
    in
    Logs.debug (fun m -> m "saving png_file_of_url");
    ImageLib_unix.writefile tmpfile image
  with exn ->
    Logs.warn (fun m -> m "imagelib failed to decode %s (%s), falling back to convert"
                  url (Printexc.to_string exn));
    if not (convert_with_imagemagick fn tmpfile) then
      failwith (Printf.sprintf
                  "could not decode image %s (imagelib failed, and 'convert' fallback also failed)"
                  url));
  tmpfile

(*****************************************************************************)
(* Cairo surface cache *)
(*****************************************************************************)

(* url -> surface, via Cairo.PNG.create *)
let himages : (string, Cairo.Surface.t option) Hashtbl.t = Hashtbl.create 101

(* claude: cache None on decode/download failure (e.g., a GIF that trips
 * an imagelib decoder bug) so callers can degrade to "skip this image"
 * instead of crashing, and so we don't retry every frame *)
let surface_of_url src =
  match Hashtbl.find_opt himages src with
  | Some surface_opt -> surface_opt
  | None ->
    let surface_opt =
      try Some (Cairo.PNG.create (png_file_of_url src))
      with exn ->
        Logs.warn (fun m -> m "failed to load image %s: %s"
                      src (Printexc.to_string exn));
        None
    in
    Hashtbl.add himages src surface_opt;
    surface_opt
