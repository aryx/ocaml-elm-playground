(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Textures for the 3D backends, like Image_decode for 2D images (as
 * RGBA too, see Rgba), and independent of rendering too; the
 * difference with Image_decode: no GIF animation, and local file paths
 * are the common case. *)

(*****************************************************************************)
(* Load + cache *)
(*****************************************************************************)

let load_exn (src : string) : Stb_image.int8 Stb_image.t =
  let path = Download.local_file ~prefix:"playground3d_texture" src in
  match Stb_image.load path with
  | Ok img -> Rgba.of_stb_image img
  | Error (`Msg msg) -> failwith (Printf.sprintf "could not decode texture %s: %s" src msg)

let cache : (string, Stb_image.int8 Stb_image.t option) Hashtbl.t = Hashtbl.create 16

let load (src : string) : Stb_image.int8 Stb_image.t option =
  match Hashtbl.find_opt cache src with
  | Some result -> result
  | None ->
      let result =
        try Some (load_exn src)
        with exn ->
          Printf.eprintf "playground3d: failed to load texture %s: %s\n%!" src
            (Printexc.to_string exn);
          None
      in
      Hashtbl.add cache src result;
      result

(*****************************************************************************)
(* Preloading (same rationale as Image_decode.ml's preload/load_queued) *)
(*****************************************************************************)

let queued : string Queue.t = Queue.create ()
let preload src = Queue.push src queued

let load_queued () =
  Queue.iter (fun src -> ignore (load src : Stb_image.int8 Stb_image.t option)) queued;
  Queue.clear queued
