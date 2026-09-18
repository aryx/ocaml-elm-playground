(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Golden frames for the 3D software rasterizer: one frame of each 3D
 * example, some with debug keys pressed first, compared pixel by pixel
 * with the committed tests/3d/golden/<scene>.png.
 *
 * Each example runs with its clock frozen (-fixed-time), the keys
 * pressed before its first frame (-keys), and frame n dumped as a PPM
 * (-dump-frame n, see Native_loop), under SDL's "dummy" video driver:
 * the window gets an in-memory surface, so no display is needed, and
 * the pixels are the same as on a real window.
 *
 * When a frame differs, the test fails saying how many pixels differ
 * and where, and writes the new frame to tests/3d/actual/<scene>.png
 * (in _build/default/). If the change is intended (look at it!),
 * 'make approve-golden3d' makes the new frames the golden ones.
 *
 * Not included: games3d/Minecraft3d (slow, and a 1.5 MB frame; see
 * scripts/ref_frames_3d.sh for a manual check), games3d/StarCollector3d
 * (random stars). *)

let t = Testo.create

(* executable, debug keys ("" for none), frame; the keys are
 * f wireframe, z painter's algorithm, b culling off, m the next shading
 * mode, p linear interpolation, i nearest texture filtering *)
let scenes =
  [
    ("examples3d/Cube3d", "", 3);
    ("examples3d/Cubes3d", "", 3);
    ("examples3d/Cubes3d", "f", 3);
    ("examples3d/Cubes3d", "z", 3);
    ("examples3d/Cubes3d", "b", 3);
    ("examples3d/Cubes3d", "bf", 3);
    ("examples3d/Spheres3d", "", 3);
    ("examples3d/Spheres3d", "m", 3);
    ("examples3d/Spheres3d", "mm", 3);
    ("examples3d/Spheres3d", "mmm", 3);
    ("examples3d/TexturedCube3d", "", 3);
    ("examples3d/TexturedCube3d", "p", 3);
    ("examples3d/TexturedCube3d", "i", 3);
    ("examples3d/InteractiveCube3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "z", 3);
    ("examples3d/FloatingCity3d", "", 3);
  ]

(* the tests run in _build/default/tests/3d/; the examples, from
 * _build/default/, the root their texture paths are relative to *)
let build_root = "../.."
let golden_dir = "golden"
let actual_dir = "actual"

(* where to look at a new frame, from the project's root *)
let shown (file : string) = Filename.concat "_build/default/tests/3d" file

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

let render ~(exe : string) ~(keys : string) ~(frame : int) : frame =
  let ppm = Filename.temp_file "golden3d" ".ppm" in
  let args = [| exe; "-fixed-time"; "1000"; "-keys"; keys; "-dump-frame"; string_of_int frame; ppm |] in
  let env = Array.append [| "SDL_VIDEODRIVER=dummy" |] (Unix.environment ()) in
  (match Unix.fork () with
  | 0 -> (
      try
        Unix.chdir build_root;
        Unix.execve exe args env
      with _ -> exit 127)
  | pid -> (
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 0 -> ()
      | _ -> Alcotest.failf "%s -keys %S failed" exe keys));
  let result = read_ppm ppm in
  Sys.remove ppm;
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

let test_scene (exe, keys, frame) () =
  let name = Filename.basename exe ^ if keys = "" then "" else "_" ^ keys in
  let golden_file = Filename.concat golden_dir (name ^ ".png") in
  let actual_file = Filename.concat actual_dir (name ^ ".png") in
  let actual = render ~exe:(exe ^ ".exe") ~keys ~frame in
  let save_actual () =
    if not (Sys.file_exists actual_dir) then Sys.mkdir actual_dir 0o755;
    write_png actual_file actual
  in
  if not (Sys.file_exists golden_file) then begin
    save_actual ();
    Alcotest.failf "no golden frame %s yet; the new frame is in %s ('make approve-golden3d')" golden_file (shown actual_file)
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
      Alcotest.failf "%s: %d pixels differ from the golden frame, the first at (%d, %d); the new frame is %s" name
        n x y (shown actual_file)

let tests =
  scenes
  |> List.map (fun ((exe, keys, _) as scene) ->
         let name = Filename.basename exe ^ if keys = "" then "" else " -keys " ^ keys in
         t name (test_scene scene))
  |> Testo.categorize "golden frames"
