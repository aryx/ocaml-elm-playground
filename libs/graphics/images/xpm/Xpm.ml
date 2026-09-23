(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Xpm.mli *)

type color = (int * int * int) option

type t = { name : string; colors : (char * color) list; rows : string list }

let fail fmt = Printf.ksprintf (fun s -> failwith ("XPM: " ^ s)) fmt

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

(* the position of [sub] in [s] from [from], if any *)
let rec find (s : string) (sub : string) (from : int) : int option =
  if from + String.length sub > String.length s then None
  else if String.sub s from (String.length sub) = sub then Some from
  else find s sub (from + 1)

(* the C strings of the file, in order, outside the comments *)
let strings (text : string) : string list =
  let n = String.length text in
  let rec go i acc =
    if i >= n then List.rev acc
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' then
      match find text "*/" (i + 2) with Some j -> go (j + 2) acc | None -> List.rev acc
    else if text.[i] = '"' then (
      let b = Buffer.create 16 in
      let rec str j =
        if j >= n then fail "a string is not closed"
        else if text.[j] = '"' then j + 1
        else if text.[j] = '\\' && j + 1 < n then (Buffer.add_char b text.[j + 1]; str (j + 2))
        else (Buffer.add_char b text.[j]; str (j + 1))
      in
      let next = str (i + 1) in
      go next (Buffer.contents b :: acc))
    else go (i + 1) acc
  in
  go 0 []

(* the array's name: the identifier before the first "[]" *)
let name (text : string) : string =
  match find text "[]" 0 with
  | None -> ""
  | Some j ->
      let is_ident c = c = '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') in
      let stop = ref j in
      while !stop > 0 && text.[!stop - 1] = ' ' do decr stop done;
      let start = ref !stop in
      while !start > 0 && is_ident text.[!start - 1] do decr start done;
      String.sub text !start (!stop - !start)

let words (s : string) : string list = String.split_on_char ' ' s |> List.concat_map (String.split_on_char '\t') |> List.filter (( <> ) "")

let hex (s : string) : int = match int_of_string_opt ("0x" ^ s) with Some v -> v | None -> fail "bad color #%s" s

let color_value (v : string) : color =
  let n = String.length v in
  match String.lowercase_ascii v with
  | "none" -> None
  | "black" -> Some (0, 0, 0)
  | "white" -> Some (255, 255, 255)
  | "red" -> Some (255, 0, 0)
  | "green" -> Some (0, 255, 0)
  | "blue" -> Some (0, 0, 255)
  | _ when n = 7 && v.[0] = '#' -> Some (hex (String.sub v 1 2), hex (String.sub v 3 2), hex (String.sub v 5 2))
  (* 16 bits a channel: the high byte of each *)
  | _ when n = 13 && v.[0] = '#' -> Some (hex (String.sub v 1 2), hex (String.sub v 5 2), hex (String.sub v 9 2))
  | _ -> fail "color %S not read (only #rrggbb, None and a few names)" v

(* "R c #dc281e": the character, then keys and values; the 'c' one *)
let color_line (line : string) : char * color =
  if line = "" then fail "an empty color line";
  let rec value = function
    | "c" :: v :: _ -> color_value v
    | _ :: rest -> value rest
    | [] -> fail "no 'c' color for %C" line.[0]
  in
  (line.[0], value (words (String.sub line 1 (String.length line - 1))))

let parse (text : string) : t =
  match strings text with
  | [] -> fail "no strings: not an XPM file"
  | header :: rest -> (
      match List.map int_of_string_opt (words header) with
      | Some w :: Some h :: Some ncolors :: Some cpp :: _ ->
          if cpp <> 1 then fail "%d characters per pixel, only 1 read" cpp;
          if List.length rest < ncolors + h then fail "%d strings, %d expected" (List.length rest) (ncolors + h);
          let colors = List.filteri (fun i _ -> i < ncolors) rest |> List.map color_line in
          let rows = List.filteri (fun i _ -> i >= ncolors && i < ncolors + h) rest in
          rows
          |> List.iteri (fun r row ->
                 if String.length row <> w then fail "row %d is %d wide, not %d" r (String.length row) w;
                 String.iter (fun c -> if not (List.mem_assoc c colors) then fail "%C in row %d is not in the palette" c r) row);
          { name = name text; colors; rows }
      | _ -> fail "bad header %S, expected: width height colors characters-per-pixel" header)

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let print (t : t) : string =
  let w = List.fold_left (fun acc r -> max acc (String.length r)) 0 t.rows in
  let color (c, v) =
    match v with None -> Printf.sprintf "%c c None" c | Some (r, g, b) -> Printf.sprintf "%c c #%02x%02x%02x" c r g b
  in
  let lines = Printf.sprintf "%d %d %d 1" w (List.length t.rows) (List.length t.colors) :: List.map color t.colors @ t.rows in
  Printf.sprintf "/* XPM */\nstatic char *%s[] = {\n%s\n};\n" t.name
    (lines |> List.map (fun l -> "\"" ^ l ^ "\"") |> String.concat ",\n")
