(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Gif.mli *)

type frame = {
  x : int;
  y : int;
  patch : Rgba_image.t;
  delay : float;
  disposal : int;
}

(*****************************************************************************)
(* Frames *)
(*****************************************************************************)

(* the rows of an interlaced frame of [h] rows, in the order they are
 * stored: every 8th from 0, every 8th from 4, every 4th from 2, every
 * 2nd from 1 *)
let interlaced_rows (h : int) : int array =
  let passes = [ (0, 8); (4, 8); (2, 4); (1, 2) ] in
  passes
  |> List.map (fun (start, step) -> List.init (max 0 ((h - start + step - 1) / step)) (fun k -> start + (k * step)))
  |> List.concat |> Array.of_list

let frames (s : string) : (int * int) * frame list =
  let len = String.length s in
  let byte i = if i < len then Char.code s.[i] else failwith "GIF: the file ends early" in
  let u16 i = byte i lor (byte (i + 1) lsl 8) in
  if len < 13 || (String.sub s 0 6 <> "GIF87a" && String.sub s 0 6 <> "GIF89a") then failwith "GIF: not a GIF";
  let width = u16 6 and height = u16 8 in
  let palette_size flags = if flags land 0x80 <> 0 then 3 * (1 lsl (1 + (flags land 7))) else 0 in
  let global_end = 13 + palette_size (byte 10) in
  let global = if global_end > len then failwith "GIF: the file ends early" else String.sub s 13 (global_end - 13) in
  (* the sub-blocks starting at [i], joined, and the position after them *)
  let sub_blocks i =
    let b = Buffer.create 256 in
    let rec loop i =
      let size = byte i in
      if size = 0 then i + 1
      else begin
        if i + 1 + size > len then failwith "GIF: the file ends early";
        Buffer.add_substring b s (i + 1) size;
        loop (i + 1 + size)
      end
    in
    let after = loop i in
    (Buffer.contents b, after)
  in
  (* the graphic control extension seen last, for the next frame:
   * (packed, delay, transparent index) *)
  let rec loop i gce acc =
    if i >= len then List.rev acc (* no 0x3B: accepted, as browsers do *)
    else
      match byte i with
      | 0x3B -> List.rev acc
      | 0x21 when byte (i + 1) = 0xF9 ->
          let gce = (byte (i + 3), u16 (i + 4), byte (i + 6)) in
          loop (snd (sub_blocks (i + 2))) gce acc
      | 0x21 -> loop (snd (sub_blocks (i + 2))) gce acc
      | 0x2C ->
          let x = u16 (i + 1) and y = u16 (i + 3) and w = u16 (i + 5) and h = u16 (i + 7) in
          let flags = byte (i + 9) in
          let local_end = i + 10 + palette_size flags in
          let palette = if flags land 0x80 <> 0 then String.sub s (i + 10) (local_end - i - 10) else global in
          if palette = "" then failwith "GIF: a frame without a palette";
          let data, after = sub_blocks (local_end + 1) in
          let indices = Lzw.decode ~min_code_size:(byte local_end) data ~npixels:(w * h) in
          let packed, delay, transparent = gce in
          let transparent = if packed land 1 = 1 then transparent else -1 in
          let rows = if flags land 0x40 <> 0 then interlaced_rows h else Array.init h (fun j -> j) in
          let patch = Rgba_image.create ~width:w ~height:h in
          for k = 0 to h - 1 do
            (* the [k]th row stored is row [rows.(k)] of the patch *)
            for col = 0 to w - 1 do
              let index = Char.code (Bytes.get indices ((k * w) + col)) in
              let o = ((rows.(k) * w) + col) * 4 in
              if index <> transparent then begin
                let color c = if (3 * index) + 2 < String.length palette then Char.code palette.[(3 * index) + c] else 0 in
                patch.rgba.{o} <- color 0;
                patch.rgba.{o + 1} <- color 1;
                patch.rgba.{o + 2} <- color 2;
                patch.rgba.{o + 3} <- 255
              end
            done
          done;
          let frame =
            { x; y; patch; delay = (if delay <= 1 then 0.1 else float delay /. 100.); disposal = (packed lsr 2) land 7 }
          in
          loop after (0, 0, 0) (frame :: acc)
      | c -> failwith (Printf.sprintf "GIF: unexpected block 0x%02X at %d" c i)
  in
  ((width, height), loop global_end (0, 0, 0) [])

(*****************************************************************************)
(* Composition *)
(*****************************************************************************)

let animation (s : string) : (Rgba_image.t * float) list =
  let (w, h), frames = frames s in
  let canvas = Rgba_image.create ~width:w ~height:h in
  let copy (img : Rgba_image.t) : Rgba_image.t =
    let c = Rgba_image.create ~width:img.width ~height:img.height in
    Bigarray.Array1.blit img.rgba c.rgba;
    c
  in
  (* the pixels of the frame's patch that are inside the canvas; [f]
   * gets the offset in the canvas, and in the patch *)
  let iter_patch (fr : frame) f =
    for j = 0 to fr.patch.height - 1 do
      for i = 0 to fr.patch.width - 1 do
        let cx = fr.x + i and cy = fr.y + j in
        if cx < w && cy < h then f (((cy * w) + cx) * 4) (((j * fr.patch.width) + i) * 4)
      done
    done
  in
  frames
  |> List.map (fun (fr : frame) ->
         let before = if fr.disposal = 3 then Some (copy canvas) else None in
         (* the patch's transparent pixels (alpha 0) let the picture
          * below show through *)
         let p = fr.patch.rgba in
         iter_patch fr (fun co po -> if p.{po + 3} <> 0 then for k = 0 to 3 do canvas.rgba.{co + k} <- p.{po + k} done);
         let snapshot = copy canvas in
         (match (fr.disposal, before) with
         | 2, _ -> iter_patch fr (fun co _ -> for k = 0 to 3 do canvas.rgba.{co + k} <- 0 done)
         | 3, Some c -> Bigarray.Array1.blit c.rgba canvas.rgba
         | _ -> ());
         (snapshot, fr.delay))

let decode (s : string) : Rgba_image.t =
  match animation s with
  | (first, _) :: _ -> first
  | [] -> failwith "GIF: no frame"
