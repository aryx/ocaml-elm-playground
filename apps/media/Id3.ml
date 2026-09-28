(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Id3.mli *)

type t = { title : string; artist : string; album : string; year : string; track : int option }

let empty = { title = ""; artist = ""; album = ""; year = ""; track = None }

(*****************************************************************************)
(* ID3v1: 128 bytes at the end *)
(*****************************************************************************)

(* a field without its padding, zeros or spaces, and what follows a zero *)
let field (s : string) (at : int) (len : int) : string =
  let f = String.sub s at len in
  let f = match String.index_opt f '\000' with Some i -> String.sub f 0 i | None -> f in
  String.trim f

let v1 (s : string) : t option =
  let n = String.length s in
  if n < 128 || String.sub s (n - 128) 3 <> "TAG" then None
  else
    let at = n - 128 in
    (* v1.1: a zero at the comment's byte 28, the track in its byte 29 *)
    let track = if s.[at + 125] = '\000' && s.[at + 126] <> '\000' then Some (Char.code s.[at + 126]) else None in
    Some { title = field s (at + 3) 30; artist = field s (at + 33) 30; album = field s (at + 63) 30; year = field s (at + 93) 4; track }

let v1_to_string (t : t) : string =
  let b = Bytes.make 128 '\000' in
  let put at len v = Bytes.blit_string v 0 b at (min len (String.length v)) in
  put 0 3 "TAG";
  put 3 30 t.title;
  put 33 30 t.artist;
  put 63 30 t.album;
  put 93 4 t.year;
  Option.iter (fun k -> Bytes.set b 126 (Char.chr (k land 255))) t.track;
  Bytes.set b 127 '\255';
  Bytes.to_string b

(*****************************************************************************)
(* ID3v2: frames at the start *)
(*****************************************************************************)

(* 7 bits a byte: no byte of it has its high bit, so none is FF *)
let syncsafe (s : string) (at : int) : int =
  (Char.code s.[at] lsl 21) lor (Char.code s.[at + 1] lsl 14) lor (Char.code s.[at + 2] lsl 7) lor Char.code s.[at + 3]

let plain (s : string) (at : int) (len : int) : int =
  let v = ref 0 in
  for i = 0 to len - 1 do
    v := (!v lsl 8) lor Char.code s.[at + i]
  done;
  !v

(* a text frame's data into UTF-8: its first byte says how it is coded *)
let text (d : string) : string =
  let b = Buffer.create (String.length d) in
  let n = String.length d in
  let utf16 ~big from =
    let i = ref from in
    while !i + 1 < n do
      let u = if big then (Char.code d.[!i] lsl 8) lor Char.code d.[!i + 1] else (Char.code d.[!i + 1] lsl 8) lor Char.code d.[!i] in
      (* a surrogate pair's halves (beyond the BMP) shown as '?' *)
      if u <> 0 then Buffer.add_utf_8_uchar b (if u >= 0xD800 && u < 0xE000 then Uchar.of_char '?' else Uchar.of_int u);
      i := !i + 2
    done
  in
  (if n > 0 then
     match d.[0] with
     | '\001' when n >= 3 -> utf16 ~big:(d.[1] = '\254') 3 (* FE FF: big-endian *)
     | '\002' -> utf16 ~big:true 1
     | '\003' -> Buffer.add_string b (String.sub d 1 (n - 1))
     | _ -> String.iteri (fun i c -> if i > 0 && c <> '\000' then Buffer.add_utf_8_uchar b (Uchar.of_char c)) d);
  (* a zero ends it (v2.4's lists of strings: the first one) *)
  let s = Buffer.contents b in
  match String.index_opt s '\000' with Some i -> String.sub s 0 i | None -> s

let v2_frames (s : string) : (string * string) list =
  if String.length s < 10 || String.sub s 0 3 <> "ID3" then []
  else
    let major = Char.code s.[3] and flags = Char.code s.[5] in
    let size = syncsafe s 6 in
    let stop = min (String.length s) (10 + size) in
    let id_len, head = if major = 2 then (3, 6) else (4, 10) in
    (* an extended header (flag 0x40) skipped: v2.4's size counts itself,
     * v2.3's doesn't *)
    let start =
      if flags land 0x40 = 0 || major = 2 then 10 else if major = 4 then 10 + syncsafe s 10 else 10 + 4 + plain s 10 4
    in
    let rec frames at acc =
      if at + head > stop || s.[at] = '\000' (* the padding *) then List.rev acc
      else
        let id = String.sub s at id_len in
        let len = if major = 2 then plain s (at + 3) 3 else if major = 4 then syncsafe s (at + 4) else plain s (at + 4) 4 in
        let data_at = at + head in
        if len <= 0 || data_at + len > stop then List.rev acc
        else
          let acc = if id.[0] = 'T' && id <> "TXXX" && id <> "TXX" then (id, text (String.sub s data_at len)) :: acc else acc in
          frames (data_at + len) acc
    in
    frames start []

(*****************************************************************************)
(* Both *)
(*****************************************************************************)

let read (s : string) : t option =
  let frames = v2_frames s in
  let old = v1 s in
  if frames = [] && old = None then None
  else
    let old = Option.value old ~default:empty in
    (* v2.3's names, then v2.2's *)
    let get ids fallback =
      match List.find_map (fun id -> List.assoc_opt id frames) ids with Some v when v <> "" -> v | _ -> fallback
    in
    let track =
      (* "3/12": the third of twelve *)
      match int_of_string_opt (List.hd (String.split_on_char '/' (get [ "TRCK"; "TRK" ] ""))) with
      | Some k -> Some k
      | None -> old.track
    in
    Some
      {
        title = get [ "TIT2"; "TT2" ] old.title;
        artist = get [ "TPE1"; "TP1" ] old.artist;
        album = get [ "TALB"; "TAL" ] old.album;
        year = get [ "TDRC"; "TYER"; "TYE" ] old.year;
        track;
      }

let display ~(name : string) (tag : t option) : string =
  match tag with
  | Some { artist; title; _ } when artist <> "" && title <> "" -> artist ^ " - " ^ title
  | Some { title; _ } when title <> "" -> title
  | _ -> Filename.remove_extension (Filename.basename name)
