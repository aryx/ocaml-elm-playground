(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mpeg_system.mli *)

type stream = { id : int; bytes : string; first_pts : float option }

let byte (s : string) (i : int) : int = Char.code s.[i]

let pts (s : string) (at : int) : int =
  ((byte s at lsr 1) land 7) lsl 30
  lor (byte s (at + 1) lsl 22)
  lor ((byte s (at + 2) lsr 1) lsl 15)
  lor (byte s (at + 3) lsl 7)
  lor (byte s (at + 4) lsr 1)

let start_code (s : string) (at : int) : bool = at + 4 <= String.length s && s.[at] = '\000' && s.[at + 1] = '\000' && s.[at + 2] = '\001'

(* where a packet's contents start, past its stuffing (FF bytes), its
 * buffer size (2 bytes starting 01), and its timestamps: 0010 a PTS,
 * 0011 a PTS and a DTS, 0F none; and the PTS if any *)
let payload (s : string) (at : int) (last : int) : int * int option =
  let i = ref at in
  while !i < last && byte s !i = 0xFF do
    incr i
  done;
  if !i < last && byte s !i land 0xC0 = 0x40 then i := !i + 2;
  let c = if !i < last then byte s !i else 0x0F in
  if c land 0xF0 = 0x20 && !i + 5 <= last then (!i + 5, Some (pts s !i))
  else if c land 0xF0 = 0x30 && !i + 10 <= last then (!i + 10, Some (pts s !i))
  else (!i + 1, None)

let of_string (s : string) : stream list =
  if not (start_code s 0 && byte s 3 = 0xBA) then failwith "not an MPEG system stream (no pack first)";
  if byte s 4 land 0xF0 <> 0x20 then failwith "an MPEG-2 program stream: not read";
  let n = String.length s in
  (* each stream's contents, and first PTS, in the order they come *)
  let order = ref [] and contents = Hashtbl.create 4 and firsts = Hashtbl.create 4 in
  let rec walk at =
    if at + 4 > n then ()
    else if not (start_code s at) then walk (at + 1) (* junk: to the next start code *)
    else
      match byte s (at + 3) with
      | 0xB9 -> () (* the end *)
      | 0xBA -> walk (at + 12)
      | id when id >= 0xBB && at + 6 <= n ->
          let length = (byte s (at + 4) lsl 8) lor byte s (at + 5) in
          let last = min n (at + 6 + length) in
          (* the system header, padding and private streams skipped *)
          if (id >= 0xC0 && id <= 0xEF) && at + 6 < last then (
            let start, pts = payload s (at + 6) last in
            if not (Hashtbl.mem contents id) then (
              Hashtbl.add contents id (Buffer.create 65536);
              order := id :: !order);
            (match pts with Some p when not (Hashtbl.mem firsts id) -> Hashtbl.add firsts id p | _ -> ());
            if start < last then Buffer.add_substring (Hashtbl.find contents id) s start (last - start));
          walk last
      | _ -> walk (at + 1)
  in
  walk 0;
  List.rev_map
    (fun id ->
      { id; bytes = Buffer.contents (Hashtbl.find contents id);
        first_pts = Option.map (fun p -> float_of_int p /. 90000.) (Hashtbl.find_opt firsts id) })
    !order

let video (streams : stream list) : stream option = List.find_opt (fun st -> st.id >= 0xE0 && st.id <= 0xEF) streams
let audio (streams : stream list) : stream option = List.find_opt (fun st -> st.id >= 0xC0 && st.id <= 0xDF) streams
