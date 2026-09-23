(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Y4m.mli *)

type header = { width : int; height : int; rate : int * int; chroma : Yuv.chroma; range : Yuv.range }

let frame_bytes (h : header) : int =
  let cw, ch = Yuv.chroma_size h.chroma ~width:h.width ~height:h.height in
  (h.width * h.height) + (2 * cw * ch)

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

let magic = "YUV4MPEG2 "

(* the line starting at [at], and where the next one starts *)
let line (s : string) (at : int) : string * int =
  match String.index_from_opt s at '\n' with Some nl -> (String.sub s at (nl - at), nl + 1) | None -> failwith "Y4M: a line without its end"

let parse_header (text : string) : header =
  let h = ref { width = 0; height = 0; rate = (25, 1); chroma = C420; range = Studio } in
  List.iter
    (fun field ->
      if field <> "" then
        let v = String.sub field 1 (String.length field - 1) in
        match field.[0] with
        | 'W' -> h := { !h with width = int_of_string v }
        | 'H' -> h := { !h with height = int_of_string v }
        | 'F' -> (
            match String.split_on_char ':' v with
            | [ n; d ] -> h := { !h with rate = (int_of_string n, int_of_string d) }
            | _ -> failwith ("Y4M: a frame rate of " ^ v))
        | 'I' -> if v <> "p" && v <> "?" then failwith "Y4M: interlaced, not read here"
        | 'C' ->
            if String.starts_with ~prefix:"420" v then h := { !h with chroma = C420 }
            else if v = "444" then h := { !h with chroma = C444 }
            else failwith ("Y4M: the color sampling " ^ v ^ ", not read here")
        | 'X' -> if v = "COLORRANGE=FULL" then h := { !h with range = Full }
        | _ -> () (* A, the aspect: the pixels are drawn square *))
    (String.split_on_char ' ' text);
  if !h.width <= 0 || !h.height <= 0 then failwith "Y4M: no size";
  !h

let of_string (s : string) : header * Movie.t =
  if not (String.starts_with ~prefix:magic s) then failwith "Y4M: not a YUV4MPEG2 file";
  let text, at = line s (String.length magic) in
  let h = parse_header text in
  let size = frame_bytes h in
  (* where each frame's planes start: after its FRAME line *)
  let rec starts at acc =
    if at >= String.length s then List.rev acc
    else
      let frame, data = line s at in
      if not (String.starts_with ~prefix:"FRAME" frame) then failwith "Y4M: a frame without FRAME";
      if data + size > String.length s then List.rev acc (* cut short: the frames before it *)
      else starts (data + size) (data :: acc)
  in
  let starts = Array.of_list (starts at []) in
  if starts = [||] then failwith "Y4M: no frames";
  let num, den = h.rate in
  let period = float_of_int den /. float_of_int num in
  let cw, ch = Yuv.chroma_size h.chroma ~width:h.width ~height:h.height in
  (* a frame decoded when asked: its planes cut out, the color converted *)
  let frame i =
    let at = starts.(i) and n = h.width * h.height in
    let plane off len = Bytes.of_string (String.sub s (at + off) len) in
    Yuv.to_image h.range
      { width = h.width; height = h.height; chroma = h.chroma; y = plane 0 n; cb = plane n (cw * ch); cr = plane (n + (cw * ch)) (cw * ch) }
  in
  let count = Array.length starts in
  (h, { Movie.width = h.width; height = h.height; times = Array.init count (fun i -> float_of_int i *. period); duration = float_of_int count *. period; frame })

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let to_string ?(chroma = Yuv.C420) ?(range = Yuv.Studio) ~(rate : int * int) (frames : Rgba_image.t list) : string =
  match frames with
  | [] -> invalid_arg "Y4m.to_string: no frames"
  | first :: _ ->
      let h = { width = first.width; height = first.height; rate; chroma; range } in
      let b = Buffer.create (List.length frames * (frame_bytes h + 6)) in
      Printf.bprintf b "%sW%d H%d F%d:%d Ip A1:1 %s XCOLORRANGE=%s\n" magic h.width h.height (fst rate) (snd rate)
        (match chroma with C420 -> "C420jpeg" | C444 -> "C444")
        (match range with Full -> "FULL" | Studio -> "LIMITED");
      List.iter
        (fun (img : Rgba_image.t) ->
          if img.width <> h.width || img.height <> h.height then invalid_arg "Y4m.to_string: frames of different sizes";
          let p = Yuv.of_image range chroma img in
          Buffer.add_string b "FRAME\n";
          Buffer.add_bytes b p.y;
          Buffer.add_bytes b p.cb;
          Buffer.add_bytes b p.cr)
        frames;
      Buffer.contents b
