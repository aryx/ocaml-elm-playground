(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Avi.mli *)

type header = { width : int; height : int; rate : int * int; codec : string }

(*****************************************************************************)
(* RIFF *)
(*****************************************************************************)

let u16 (s : string) (i : int) : int = String.get_uint16_le s i
let u32 (s : string) (i : int) : int = Int32.to_int (String.get_int32_le s i) land 0xFFFFFFFF

(* the chunks between [from] and [upto]: (name, where its bytes start,
 * their size); a LIST's name is its type ("hdrl", "movi"), its bytes
 * after the type *)
let chunks (s : string) (from : int) (upto : int) : (string * int * int) list =
  let upto = min upto (String.length s) in
  let rec go i acc =
    if i + 8 > upto then List.rev acc
    else
      let id = String.sub s i 4 and size = u32 s (i + 4) in
      let size = min size (upto - i - 8) in
      let entry = if id = "LIST" && size >= 4 then (String.sub s (i + 8) 4, i + 12, size - 4) else (id, i + 8, size) in
      go (i + 8 + size + (size land 1)) (entry :: acc)
  in
  go from []

let find (name : string) (cs : (string * int * int) list) : (int * int) option =
  List.find_map (fun (n, at, size) -> if n = name then Some (at, size) else None) cs

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

type stream = { kind : string; handler : string; scale : int; rate : int; format : int * int (* strf *) }

let of_string (s : string) : header * Movie.t * Signal.t option =
  if String.length s < 12 || String.sub s 0 4 <> "RIFF" || String.sub s 8 4 <> "AVI " then failwith "AVI: not an AVI file";
  let top = chunks s 12 (String.length s) in
  let hdrl = match find "hdrl" top with Some (at, size) -> chunks s at (at + size) | None -> failwith "AVI: no headers (hdrl)" in
  let movi = match find "movi" top with Some (at, size) -> (at, size) | None -> failwith "AVI: no data (movi)" in
  (* the streams, in order: stream n's chunks are named "0n.." *)
  let streams =
    List.filter_map
      (fun (n, at, size) ->
        if n <> "strl" then None
        else
          let cs = chunks s at (at + size) in
          match (find "strh" cs, find "strf" cs) with
          | Some (h, _), Some f -> Some { kind = String.sub s h 4; handler = String.sub s (h + 4) 4; scale = u32 s (h + 20); rate = u32 s (h + 24); format = f }
          | _ -> None)
      hdrl
  in
  let numbered = List.mapi (fun i st -> (i, st)) streams in
  let video = match List.find_opt (fun (_, st) -> st.kind = "vids") numbered with Some v -> v | None -> failwith "AVI: no video stream" in
  let vn, vs = video in
  let bitmap, _ = vs.format in
  let width = u32 s (bitmap + 4) and height = abs (Int32.to_int (String.get_int32_le s (bitmap + 8))) in
  let codec = String.sub s (bitmap + 16) 4 in
  if String.uppercase_ascii codec <> "MJPG" then failwith ("AVI: the codec " ^ String.escaped codec ^ ", not read here (only Motion JPEG)");
  (* the data: every chunk of "movi", "rec " lists opened *)
  let rec data at size = List.concat_map (fun (n, a, sz) -> if n = "rec " then data a sz else [ (n, a, sz) ]) (chunks s at (at + size)) in
  let data = data (fst movi) (snd movi) in
  let of_stream n = List.filter (fun (name, _, _) -> String.length name = 4 && int_of_string_opt (String.sub name 0 2) = Some n) data in
  let frames = Array.of_list (List.filter (fun (name, _, sz) -> (String.sub name 2 2 = "dc" || String.sub name 2 2 = "db") && sz > 0) (of_stream vn)) in
  if frames = [||] then failwith "AVI: no frames";
  let rate = if vs.rate > 0 && vs.scale > 0 then (vs.rate, vs.scale) else (1_000_000, max 1 (match find "avih" hdrl with Some (a, _) -> u32 s a | None -> 40_000)) in
  let period = float_of_int (snd rate) /. float_of_int (fst rate) in
  let count = Array.length frames in
  (* the last frame decoded, kept: a player shows each 2 or 3 times (60
   * screens a second for 25 frames) *)
  let last = ref (-1, Rgba_image.create ~width:0 ~height:0) in
  let decode i =
    if fst !last <> i then (let _, at, size = frames.(i) in last := (i, Jpeg.decode (String.sub s at size)));
    snd !last
  in
  let movie : Movie.t =
    {
      width;
      height;
      times = Array.init count (fun i -> float_of_int i *. period);
      duration = float_of_int count *. period;
      (* each frame a JPEG of its own: decoded alone *)
      frame = decode;
    }
  in
  (* the sound: the first audio stream's chunks, end to end *)
  let sound =
    match List.find_opt (fun (_, st) -> st.kind = "auds") numbered with
    | None -> None
    | Some (an, st) ->
        let fmt, _ = st.format in
        let tag = u16 s fmt and channels = u16 s (fmt + 2) and rate = u32 s (fmt + 4) and bits = u16 s (fmt + 14) in
        if tag <> 1 || (bits <> 8 && bits <> 16) || (channels <> 1 && channels <> 2) then failwith "AVI: sound not 8 or 16-bit PCM, mono or stereo";
        let bytes = String.concat "" (List.map (fun (_, at, size) -> String.sub s at size) (of_stream an)) in
        let width = bits / 8 * channels in
        let sample i c =
          let at = (i * width) + (c * bits / 8) in
          if bits = 16 then float_of_int (String.get_int16_le bytes at) /. 32767. else float_of_int (Char.code bytes.[at] - 128) /. 127.
        in
        let mono = Array.init (String.length bytes / width) (fun i -> if channels = 1 then sample i 0 else (sample i 0 +. sample i 1) /. 2.) in
        Some (Resample.to_rate Linear rate mono)
  in
  ({ width; height; rate; codec }, movie, sound)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

(* a chunk, its bytes padded to even; a list, its type then its chunks *)
let chunk (b : Buffer.t) (id : string) (body : string) : unit =
  Buffer.add_string b id;
  Buffer.add_int32_le b (Int32.of_int (String.length body));
  Buffer.add_string b body;
  if String.length body land 1 = 1 then Buffer.add_char b '\000'

let list (b : Buffer.t) (typ : string) (body : string) : unit = chunk b "LIST" (typ ^ body)

(* little-endian numbers into a string *)
let le (fields : [ `U16 of int | `U32 of int | `S of string ] list) : string =
  let b = Buffer.create 64 in
  List.iter (function `U16 v -> Buffer.add_uint16_le b v | `U32 v -> Buffer.add_int32_le b (Int32.of_int v) | `S s -> Buffer.add_string b s) fields;
  Buffer.contents b

let to_string ?(quality = 75) ?(sound : Signal.t option) ~(rate : int * int) (frames : Rgba_image.t list) : string =
  let first = match frames with [] -> invalid_arg "Avi.to_string: no frames" | f :: _ -> f in
  let w = first.width and h = first.height and num, den = rate in
  let n = List.length frames in
  (* the data, interleaved: each frame, then the sound that plays with
   * it; each chunk's name, offset (from "movi") and size, for the index *)
  let movi = Buffer.create (n * 8192) and index = ref [] in
  let add id body =
    index := (id, 4 + Buffer.length movi, String.length body) :: !index;
    chunk movi id body
  in
  let sample_at i = i * Signal.rate * den / num in
  List.iteri
    (fun i (img : Rgba_image.t) ->
      if img.width <> w || img.height <> h then invalid_arg "Avi.to_string: frames of different sizes";
      add "00dc" (Jpeg_encode.encode ~quality img);
      Option.iter
        (fun (samples : Signal.t) ->
          let a = min (Array.length samples) (sample_at i) and z = if i = n - 1 then Array.length samples else min (Array.length samples) (sample_at (i + 1)) in
          if z > a then add "01wb" (String.init (2 * (z - a)) (fun k -> let v = Signal.to_int16 samples.(a + (k / 2)) land 0xFFFF in Char.chr (if k land 1 = 0 then v land 0xFF else v lsr 8))))
        sound)
    frames;
  let largest = List.fold_left (fun m (_, _, size) -> max m size) 0 !index in
  let streams = if sound = None then 1 else 2 in
  let avih =
    le [ `U32 (1_000_000 * den / num); `U32 (largest * num / den); `U32 0; `U32 0x110 (* an index, interleaved *); `U32 n; `U32 0; `U32 streams; `U32 largest; `U32 w; `U32 h; `S (String.make 16 '\000') ]
  in
  let video_strl =
    let b = Buffer.create 128 in
    chunk b "strh" (le [ `S "vidsMJPG"; `U32 0; `U16 0; `U16 0; `U32 0; `U32 den; `U32 num; `U32 0; `U32 n; `U32 largest; `U32 0xFFFFFFFF; `U32 0; `U16 0; `U16 0; `U16 w; `U16 h ]);
    chunk b "strf" (le [ `U32 40; `U32 w; `U32 h; `U16 1; `U16 24; `S "MJPG"; `U32 (w * h * 3); `U32 0; `U32 0; `U32 0; `U32 0 ]);
    Buffer.contents b
  in
  let audio_strl (samples : Signal.t) =
    let b = Buffer.create 128 in
    (* PCM's rate/scale: blocks of 2 bytes (one 16-bit sample), 2 x rate bytes a second *)
    chunk b "strh" (le [ `S "auds"; `U32 0; `U32 0; `U16 0; `U16 0; `U32 0; `U32 2; `U32 (2 * Signal.rate); `U32 0; `U32 (Array.length samples); `U32 largest; `U32 0xFFFFFFFF; `U32 2; `U16 0; `U16 0; `U16 0; `U16 0 ]);
    chunk b "strf" (le [ `U16 1; `U16 1; `U32 Signal.rate; `U32 (2 * Signal.rate); `U16 2; `U16 16 ]);
    Buffer.contents b
  in
  let hdrl = Buffer.create 512 in
  chunk hdrl "avih" avih;
  list hdrl "strl" video_strl;
  Option.iter (fun samples -> list hdrl "strl" (audio_strl samples)) sound;
  let idx1 = Buffer.create (16 * List.length !index) in
  List.iter (fun (id, offset, size) -> Buffer.add_string idx1 (le [ `S id; `U32 0x10 (* a key frame *); `U32 offset; `U32 size ])) (List.rev !index);
  let body = Buffer.create (Buffer.length movi + 1024) in
  list body "hdrl" (Buffer.contents hdrl);
  list body "movi" (Buffer.contents movi);
  chunk body "idx1" (Buffer.contents idx1);
  let file = Buffer.create (Buffer.length body + 12) in
  chunk file "RIFF" ("AVI " ^ Buffer.contents body);
  Buffer.contents file
