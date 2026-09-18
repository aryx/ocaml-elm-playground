(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Sprite.mli *)

(*****************************************************************************)
(* Pixel art *)
(*****************************************************************************)

let width (rows : string list) : int = List.fold_left (fun acc row -> max acc (String.length row)) 0 rows

let pad (cols : int) (row : string) : string = row ^ String.make (cols - String.length row) '.'

let runs (row : string) : (int * int * char) list =
  let n = String.length row in
  (* [start] the first column of the current run *)
  let rec go start i acc =
    if i = n then List.rev (if n = 0 then acc else (start, i - start, row.[start]) :: acc)
    else if row.[i] = row.[start] then go start (i + 1) acc
    else go i (i + 1) ((start, i - start, row.[start]) :: acc)
  in
  go 0 0 []

(* the sprite's pixels, each row cut in rectangles by [split] (first
 * column, length, character); the sprite is [cols] x [nrows] pixels of
 * [size], centered on (0, 0) *)
let draw (split : string -> (int * int * char) list) size palette rows : shape =
  let cols = width rows in
  let nrows = List.length rows in
  let left = -.(float_of_int cols *. size /. 2.) in
  let top = float_of_int nrows *. size /. 2. in
  rows
  |> List.mapi (fun r row ->
         split row
         |> List.filter_map (fun (c, len, ch) ->
                match List.assoc_opt ch palette with
                | None -> None
                | Some color ->
                    let w = float_of_int len *. size in
                    Some
                      (rectangle color w size
                      |> move (left +. (float_of_int c *. size) +. (w /. 2.)) (top -. ((float_of_int r +. 0.5) *. size)))))
  |> List.concat |> group

let pixels (size : number) (palette : (char * color) list) (rows : string list) : shape =
  draw runs size palette rows

let pixels_squares (size : number) (palette : (char * color) list) (rows : string list) : shape =
  let one_per_pixel row = List.init (String.length row) (fun c -> (c, 1, row.[c])) in
  draw one_per_pixel size palette rows

let flip (rows : string list) : string list =
  let cols = width rows in
  rows |> List.map (fun row ->
    let row = pad cols row in
    String.init cols (fun i -> row.[cols - 1 - i]))

(*****************************************************************************)
(* Animation *)
(*****************************************************************************)

let cycle (n : int) (frames : 'a list) : 'a =
  let len = List.length frames in
  List.nth frames (((n mod len) + len) mod len)

(* claude: the time is seconds since 1970 (about 1.8e9), times [fps]:
 * too big for js_of_ocaml's 32-bit ints, hence the modulo on floats
 * before int_of_float (see Playground.to_frac for the same problem) *)
let frame (fps : number) (Time t : time) (frames : 'a list) : 'a =
  let len = float_of_int (List.length frames) in
  List.nth frames (int_of_float (Float.rem (floor (t *. fps)) len))
