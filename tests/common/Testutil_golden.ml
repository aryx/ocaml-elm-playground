(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Testutil_golden.mli *)

let t = Testo.create

type scene = string * string * int
type scripted = string * string * int * string

(* Rendering frame n means playing the game for n frames, so a scene
 * deep into a game costs real seconds of CPU, and dune runs them all at
 * once. The heavy ones are skipped unless GOLDEN_ALL is set: 'make
 * test' keeps the cheap frames (every example, and one of each game),
 * 'make test-golden-all' runs the lot. *)
let heavy_frames = 100
let run_heavy = Sys.getenv_opt "GOLDEN_ALL" <> None
let skip_heavy = "heavy (deep into a game): make test-golden-all"

(* the tests run in _build/default/<dir>/, e.g. tests/3d/; the
 * examples, from _build/default/, the root their image and texture
 * paths are relative to *)
let build_root ~dir = String.split_on_char '/' dir |> List.map (fun _ -> "..") |> String.concat "/"
let golden_dir = "golden"
let actual_dir = "actual"

(* where to look at a new frame, from the project's root *)
let shown ~dir (file : string) = Filename.concat (Filename.concat "_build/default" dir) file

(*****************************************************************************)
(* Rendering a frame *)
(*****************************************************************************)

(* an RGB frame, 3 bytes per pixel, row by row *)
type frame = { width : int; height : int; rgb : Bytes.t }

(* the binary PPM the examples dump: "P6\n<w> <h>\n255\n", then the bytes *)
let read_ppm (file : string) : frame =
  let ic = open_in_bin file in
  let width, height = Scanf.bscanf (Scanf.Scanning.from_channel ic) "P6 %d %d 255%c" (fun w h _ -> (w, h)) in
  (* the Scanf buffer read ahead: seek to the pixels, right after the header *)
  let header = Printf.sprintf "P6\n%d %d\n255\n" width height in
  seek_in ic (String.length header);
  let rgb = really_input_string ic (width * height * 3) |> Bytes.of_string in
  close_in ic;
  { width; height; rgb }

let render ~dir ~(exe : string) ~(keys : string) ~(script : string option) ~(frame : int) : frame =
  let ppm = Filename.temp_file "golden3d" ".ppm" in
  (* claude: seed=1, a flag (see Playground.flags) for the games drawing
   * random numbers (Snake, Tetris, StarCollector3d): the same numbers
   * every run; the other programs ignore it *)
  let args = [| exe; "-fixed-time"; "1000"; "-keys"; keys; "-dump-frame"; string_of_int frame; ppm; "seed=1" |] in
  let args = match script with Some s -> Array.append args [| "-script"; s |] | None -> args in
  (* claude: SDL's dummy video driver: the window is only a surface in
   * memory, never shown, which the software backends draw into as into
   * a real one; no display needed, nothing popping up on the screen,
   * and scenes can run in parallel (see notes_debugging_techniques.md,
   * section 9) *)
  (* claude: and a store of documents of its own, empty (see
   * Playground_platform.store): an app's scene that saves and opens
   * again finds only what it saved, never another scene's -- they run
   * in parallel -- nor the user's own documents *)
  let store = Filename.temp_file "golden_store" "" in
  Sys.remove store;
  Sys.mkdir store 0o755;
  let env = Array.append [| "SDL_VIDEODRIVER=dummy"; "ELM_PLAYGROUND_STORE=" ^ store |] (Unix.environment ()) in
  (match Unix.fork () with
  | 0 -> (
      try
        Unix.chdir (build_root ~dir);
        Unix.execve exe args env
      with _ -> exit 127)
  | pid -> (
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> ()
      | _ -> Alcotest.failf "%s -keys %S failed" exe keys));
  let result = read_ppm ppm in
  Sys.remove ppm;
  Array.iter (fun f -> Sys.remove (Filename.concat store f)) (Sys.readdir store);
  Sys.rmdir store;
  result

(*****************************************************************************)
(* PNG files *)
(*****************************************************************************)

let read_png (file : string) : frame =
  let image = ImageLib_unix.openfile file in
  let width = image.width and height = image.height in
  let rgb = Bytes.create (width * height * 3) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Image.read_rgb image x y (fun r g b ->
          let i = ((y * width) + x) * 3 in
          Bytes.set_uint8 rgb i r;
          Bytes.set_uint8 rgb (i + 1) g;
          Bytes.set_uint8 rgb (i + 2) b)
    done
  done;
  { width; height; rgb }

let write_png (file : string) (frame : frame) : unit =
  let image = Image.create_rgb frame.width frame.height in
  for y = 0 to frame.height - 1 do
    for x = 0 to frame.width - 1 do
      let i = ((y * frame.width) + x) * 3 in
      Image.write_rgb image x y (Bytes.get_uint8 frame.rgb i) (Bytes.get_uint8 frame.rgb (i + 1))
        (Bytes.get_uint8 frame.rgb (i + 2))
    done
  done;
  ImageLib_unix.writefile file image

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

(* the number of differing pixels, and the first one's (x, y) *)
let compare_frames (golden : frame) (actual : frame) : int * (int * int) option =
  let count = ref 0 and first = ref None in
  for i = 0 to (golden.width * golden.height) - 1 do
    let same k = Bytes.get golden.rgb ((i * 3) + k) = Bytes.get actual.rgb ((i * 3) + k) in
    if not (same 0 && same 1 && same 2) then begin
      incr count;
      if !first = None then first := Some (i mod golden.width, i / golden.width)
    end
  done;
  (!count, !first)

(* [name]: the golden file's, see Testutil_golden.mli *)
let test_scene ~dir ~approve ~name ~exe ~keys ~script ~frame () =
  let golden_file = Filename.concat golden_dir (name ^ ".png") in
  let actual_file = Filename.concat actual_dir (name ^ ".png") in
  let actual = render ~dir ~exe:(exe ^ ".exe") ~keys ~script ~frame in
  let save_actual () =
    if not (Sys.file_exists actual_dir) then Sys.mkdir actual_dir 0o755;
    write_png actual_file actual
  in
  if not (Sys.file_exists golden_file) then begin
    save_actual ();
    Alcotest.failf "no golden frame %s yet; the new frame is %s ('make %s')" golden_file (shown ~dir actual_file)
      approve
  end;
  let golden = read_png golden_file in
  if (golden.width, golden.height) <> (actual.width, actual.height) then begin
    save_actual ();
    Alcotest.failf "%s: %dx%d, the golden frame is %dx%d" name actual.width actual.height golden.width golden.height
  end;
  match compare_frames golden actual with
  | 0, _ ->
      (* a new frame from an earlier run, no longer different *)
      if Sys.file_exists actual_file then Sys.remove actual_file
  | n, first ->
      save_actual ();
      let x, y = Option.get first in
      Alcotest.failf "%s: %d pixels differ from the golden frame, the first at (%d, %d); the new frame is %s ('make %s')"
        name n x y (shown ~dir actual_file) approve

let tests ~dir ~approve ?(scripted : scripted list = []) (scenes : scene list) : Testo.t list =
  let one ~frame title body = if frame > heavy_frames && not run_heavy then t ~skipped:skip_heavy title body else t title body in
  let plain =
    scenes
    |> List.map (fun (exe, keys, frame) ->
           let name = Filename.basename exe ^ if keys = "" then "" else "_" ^ keys in
           let title = Filename.basename exe ^ if keys = "" then "" else " -keys " ^ keys in
           one ~frame title (test_scene ~dir ~approve ~name ~exe ~keys ~script:None ~frame))
  in
  let with_script =
    scripted
    |> List.map (fun (exe, label, frame, script) ->
           let name = Filename.basename exe ^ "_" ^ label in
           one ~frame
             (Filename.basename exe ^ " -script " ^ label)
             (test_scene ~dir ~approve ~name ~exe ~keys:"" ~script:(Some script) ~frame))
  in
  Testo.categorize "golden frames" (plain @ with_script)
