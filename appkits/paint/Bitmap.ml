(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type t = { width : int; height : int; rowbytes : int; bits : Bytes.t }

let create ~width ~height =
  let rowbytes = (width + 7) / 8 in
  { width; height; rowbytes; bits = Bytes.make (rowbytes * height) '\000' }

let width b = b.width
let height b = b.height
let inside b x y = x >= 0 && y >= 0 && x < b.width && y < b.height

(* the byte a dot is in, and its bit there: the leftmost dot highest *)
let byte b x y = (y * b.rowbytes) + (x lsr 3)
let mask x = 0x80 lsr (x land 7)

let get b x y = inside b x y && Char.code (Bytes.get b.bits (byte b x y)) land mask x <> 0

let set b x y black =
  if inside b x y then
    let i = byte b x y in
    let c = Char.code (Bytes.get b.bits i) in
    Bytes.set b.bits i (Char.chr (if black then c lor mask x else c land lnot (mask x)))

let copy b = { b with bits = Bytes.copy b.bits }

let change b f =
  let b = copy b in
  f b;
  b

let sub b ~x ~y ~w ~h =
  let s = create ~width:w ~height:h in
  for j = 0 to h - 1 do
    for i = 0 to w - 1 do
      if get b (x + i) (y + j) then set s i j true
    done
  done;
  s

let blit ~src ~dst ~x ~y =
  for j = 0 to src.height - 1 do
    for i = 0 to src.width - 1 do
      set dst (x + i) (y + j) (get src i j)
    done
  done

let count b =
  let n = ref 0 in
  for y = 0 to b.height - 1 do
    for x = 0 to b.width - 1 do
      if get b x y then incr n
    done
  done;
  !n

(* the runs of black of row y, as (start, length) *)
let runs b y =
  let rec go x acc =
    if x >= b.width then List.rev acc
    else if not (get b x y) then go (x + 1) acc
    else
      let rec stop e = if e < b.width && get b e y then stop (e + 1) else e in
      let e = stop x in
      go e ((x, e - x) :: acc)
  in
  go 0 []

let rectangles b =
  (* the rectangles still growing, by their run: (start, length) ->
     (top row, rows so far) *)
  let growing = Hashtbl.create 64 in
  let done_ = ref [] in
  let close (x, w) (top, h) = done_ := (x, top, w, h) :: !done_ in
  for y = 0 to b.height - 1 do
    let row = runs b y in
    (* a rectangle not continued by this row is finished *)
    Hashtbl.filter_map_inplace
      (fun run (top, h) -> if List.mem run row then Some (top, h) else (close run (top, h); None))
      growing;
    List.iter
      (fun run ->
        match Hashtbl.find_opt growing run with
        | Some (top, h) -> Hashtbl.replace growing run (top, h + 1)
        | None -> Hashtbl.replace growing run (y, 1))
      row
  done;
  Hashtbl.iter close growing;
  List.sort compare !done_

let row b y = Bytes.sub b.bits (y * b.rowbytes) b.rowbytes

let to_string b =
  let out = Buffer.create (b.rowbytes * b.height / 4) in
  Buffer.add_string out (Printf.sprintf "PAINT %d %d\n" b.width b.height);
  for y = 0 to b.height - 1 do
    Buffer.add_bytes out (Packbits.encode (row b y))
  done;
  Buffer.contents out

let of_string s =
  Scanf.sscanf s "PAINT %d %d\n%n" (fun width height start ->
      let b = create ~width ~height in
      let data = Bytes.unsafe_of_string s in
      let pos = ref start in
      for y = 0 to height - 1 do
        let r, next = Packbits.decode data ~pos:!pos ~len:b.rowbytes in
        Bytes.blit r 0 b.bits (y * b.rowbytes) b.rowbytes;
        pos := next
      done;
      b)
