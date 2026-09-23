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
type flagged = string * string * int * string list

(* Rendering frame n means playing the game for n frames, so a scene
 * deep into a game costs real seconds of CPU, and dune runs them all at
 * once. The heavy ones are skipped unless GOLDEN=all: 'make
 * test' keeps the cheap frames (every example, and one of each game),
 * 'make test-golden-all' runs the lot. And GOLDEN=none skips them all
 * ('make test-lite', for a change that only moves things around). *)
let heavy_frames = 100
let golden = Sys.getenv_opt "GOLDEN"
let run_heavy = golden = Some "all"
let run_none = golden = Some "none"
let skip_heavy = "heavy (deep into a game): make test-golden-all"
let skip_none = "GOLDEN=none (make test-lite)"

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

let render ~dir ~(exe : string) ~(keys : string) ~(script : string option) ~(flags : string list) ~(frame : int) :
    frame =
  let ppm = Filename.temp_file "golden3d" ".ppm" in
  (* claude: seed=1, a flag (see Playground.flags) for the games drawing
   * random numbers (Snake, Tetris, StarCollector3d): the same numbers
   * every run; the other programs ignore it *)
  let args = [| exe; "-fixed-time"; "1000"; "-keys"; keys; "-dump-frame"; string_of_int frame; ppm; "seed=1" |] in
  let args = match script with Some s -> Array.append args [| "-script"; s |] | None -> args in
  (* claude: the program's own flags, e.g. artwork=shapes: a game with
   * two looks, frozen in both *)
  let args = Array.append args (Array.of_list flags) in
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

(* claude: by our own Png (graphics/images/png/), in RGB: the frames
 * have no alpha *)
let read_png (file : string) : frame =
  let ic = open_in_bin file in
  let img = Png.decode (really_input_string ic (in_channel_length ic)) in
  close_in ic;
  let rgb = Bytes.create (img.width * img.height * 3) in
  for i = 0 to (img.width * img.height) - 1 do
    for k = 0 to 2 do
      Bytes.set_uint8 rgb ((i * 3) + k) img.rgba.{(i * 4) + k}
    done
  done;
  { width = img.width; height = img.height; rgb }

let write_png (file : string) (frame : frame) : unit =
  let img = Rgba_image.create ~width:frame.width ~height:frame.height in
  for i = 0 to (frame.width * frame.height) - 1 do
    for k = 0 to 2 do
      img.rgba.{(i * 4) + k} <- Bytes.get_uint8 frame.rgb ((i * 3) + k)
    done
  done;
  let oc = open_out_bin file in
  output_string oc (Png.encode ~alpha:false img);
  close_out oc

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
let test_scene ~dir ~approve ~name ~exe ~keys ~script ?(flags = []) ~frame () =
  let golden_file = Filename.concat golden_dir (name ^ ".png") in
  let actual_file = Filename.concat actual_dir (name ^ ".png") in
  let actual = render ~dir ~exe:(exe ^ ".exe") ~keys ~script ~flags ~frame in
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

let tests ~dir ~approve ?(scripted : scripted list = []) ?(flagged : flagged list = []) (scenes : scene list) :
    Testo.t list =
  let one ~frame title body =
    if run_none then t ~skipped:skip_none title body
    else if frame > heavy_frames && not run_heavy then t ~skipped:skip_heavy title body
    else t title body
  in
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
  let with_flags =
    flagged
    |> List.map (fun (exe, label, frame, flags) ->
           let name = Filename.basename exe ^ "_" ^ label in
           one ~frame
             (Filename.basename exe ^ " " ^ String.concat " " flags)
             (test_scene ~dir ~approve ~name ~exe ~keys:"" ~script:None ~flags ~frame))
  in
  Testo.categorize "golden frames" (plain @ with_script @ with_flags)
