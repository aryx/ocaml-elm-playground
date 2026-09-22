(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Zobrist.mli *)

type t = { numbers : int64 array; squares : int; side : int64 }

(* a 64-bit number from OCaml's generator, which gives 30 bits at a
 * time *)
let random64 (st : Random.State.t) : int64 =
  let bits () = Int64.of_int (Random.State.bits st) in
  Int64.logxor (bits ()) (Int64.logxor (Int64.shift_left (bits ()) 30) (Int64.shift_left (bits ()) 60))

let make ~(pieces : int) ~(squares : int) ~(seed : int) : t =
  let st = Random.State.make [| seed |] in
  { numbers = Array.init (pieces * squares) (fun _ -> random64 st); squares; side = random64 st }

let number (z : t) ~(piece : int) ~(square : int) : int64 = z.numbers.((piece * z.squares) + square)
let side (z : t) : int64 = z.side

let of_board (z : t) (pieces : (int * int) list) : int64 =
  List.fold_left (fun key (piece, square) -> Int64.logxor key (number z ~piece ~square)) 0L pieces

type bound = Exact | Lower | Upper
type 'move entry = { value : float; depth : int; bound : bound; best : 'move option }
type 'move table = { entries : (int64, 'move entry) Hashtbl.t; mutable hits : int }

let table () : 'move table = { entries = Hashtbl.create 4096; hits = 0 }

let find (t : 'move table) (key : int64) : 'move entry option =
  match Hashtbl.find_opt t.entries key with
  | Some e ->
      t.hits <- t.hits + 1;
      Some e
  | None -> None

let remember (t : 'move table) (key : int64) (e : 'move entry) : unit =
  match Hashtbl.find_opt t.entries key with
  | Some old when old.depth >= e.depth -> ()
  | _ -> Hashtbl.replace t.entries key e

let size (t : 'move table) : int = Hashtbl.length t.entries
let hits (t : 'move table) : int = t.hits

let forget (t : 'move table) : unit =
  Hashtbl.reset t.entries;
  t.hits <- 0
