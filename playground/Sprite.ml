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
(* Two looks: the artwork flag *)
(*****************************************************************************)

let artwork ~(default : bool) (flags : flags) : bool =
  match List.assoc_opt "artwork" flags with Some "sprites" -> true | Some "shapes" -> false | Some _ | None -> default

(*****************************************************************************)
(* Files *)
(*****************************************************************************)

let of_xpm (text : string) : (char * color) list * string list =
  let xpm = Xpm.parse text in
  (List.filter_map (fun (c, v) -> Option.map (fun (r, g, b) -> (c, rgb r g b)) v) xpm.colors, xpm.rows)

(* the color as red, green, blue: the playground's colors are either *)
let triple (c : color) : int * int * int =
  match c with
  | Rgb (r, g, b) -> (r, g, b)
  | Hex s ->
      let byte i = int_of_string ("0x" ^ String.sub s i 2) in
      (byte 1, byte 3, byte 5)

let to_xpm (name : string) (palette : (char * color) list) (rows : string list) : string =
  let rows = List.map (pad (width rows)) rows in
  let transparent =
    List.concat_map (fun r -> List.init (String.length r) (String.get r)) rows
    |> List.filter (fun c -> not (List.mem_assoc c palette))
    |> List.sort_uniq compare
  in
  Xpm.print
    { name; colors = List.map (fun c -> (c, None)) transparent @ List.map (fun (c, col) -> (c, Some (triple col))) palette; rows }

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
