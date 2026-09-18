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

(* Pulled out of Playground3d_platform.ml, same reasoning as
 * graphics/images/Image_decode.ml: independent of rendering. Not
 * forcing a channel count in Stb_image.load -- see the note in
 * Playground3d_platform.ml about the corrupted-buffer bug that causes. *)

(*****************************************************************************)
(* Download (same approach as Image_decode.ml's curl_url) *)
(*****************************************************************************)

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

let is_url (src : string) : bool =
  let has_prefix p =
    String.length src >= String.length p && String.sub src 0 (String.length p) = p
  in
  has_prefix "http://" || has_prefix "https://"

(*****************************************************************************)
(* Load + cache *)
(*****************************************************************************)

let load_exn (src : string) : Stb_image.int8 Stb_image.t =
  let path =
    if is_url src then begin
      let fn = Filename.temp_file "playground3d_texture" (Filename.extension src) in
      curl_url fn src;
      fn
    end
    else src
  in
  match Stb_image.load path with
  | Ok img -> img
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
