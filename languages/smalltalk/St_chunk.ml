(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_chunk.mli *)

type item =
  | Doit of string * int
  | Methods of { class_name : string; meta : bool; category : string; methods : (string * int) list }

exception Error of int * string

(* the chunks, "!!" undone, each with where its text starts (after the
 * blanks that begin it) *)
let chunks (s : string) : (string * int) list =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else begin
      let b = Buffer.create 256 in
      let rec scan j =
        if j >= n then j
        else if s.[j] = '!' then
          if j + 1 < n && s.[j + 1] = '!' then begin
            Buffer.add_char b '!';
            scan (j + 2)
          end
          else j + 1
        else begin
          Buffer.add_char b s.[j];
          scan (j + 1)
        end
      in
      let next = scan i in
      let text = Buffer.contents b in
      let blanks = ref 0 in
      while !blanks < String.length text && String.contains " \t\r\n" text.[!blanks] do incr blanks done;
      let text = String.sub text !blanks (String.length text - !blanks) in
      let rec rstrip t =
        let l = String.length t in
        if l > 0 && String.contains " \t\r\n" t.[l - 1] then rstrip (String.sub t 0 (l - 1)) else t
      in
      (* claude: a last piece with no "!" after it is not a chunk *)
      if next >= n && (next = n && (n = 0 || s.[n - 1] <> '!')) && String.trim text = "" then List.rev acc
      else go next ((rstrip text, i + !blanks) :: acc)
    end
  in
  go 0 []

(* "Point methodsFor: 'accessing'" or "Point class methodsFor: '...'" *)
let reader (text : string) (pos : int) : string * bool * string =
  let words = String.split_on_char ' ' text |> List.concat_map (String.split_on_char '\n') |> List.filter (( <> ) "") in
  let category () =
    match String.index_opt text '\'' with
    | Some a -> ( match String.index_from_opt text (a + 1) '\'' with Some b -> String.sub text (a + 1) (b - a - 1) | None -> "")
    | None -> ""
  in
  match words with
  | cls :: "methodsFor:" :: _ -> (cls, false, category ())
  | cls :: "class" :: "methodsFor:" :: _ -> (cls, true, category ())
  | _ -> raise (Error (pos, "methodsFor: expected"))

let read (s : string) : item list =
  let rec go chunks acc =
    match chunks with
    | [] -> List.rev acc
    | ("", _) :: (header, pos) :: rest ->
        let class_name, meta, category = reader header pos in
        let rec methods chunks ms =
          match chunks with
          | [] -> ([], List.rev ms)
          | ("", _) :: rest -> (rest, List.rev ms)
          | m :: rest -> methods rest (m :: ms)
        in
        let rest, ms = methods rest [] in
        go rest (Methods { class_name; meta; category; methods = ms } :: acc)
    | ("", _) :: rest -> go rest acc
    | (text, pos) :: rest -> go rest (Doit (text, pos) :: acc)
  in
  go (chunks s) []

let chunk (text : string) : string =
  let b = Buffer.create (String.length text + 2) in
  String.iter (fun c -> if c = '!' then Buffer.add_string b "!!" else Buffer.add_char b c) text;
  Buffer.add_char b '!';
  Buffer.contents b

let methods_chunk ~(class_name : string) ~(meta : bool) ~(category : string) (methods : string list) : string =
  let b = Buffer.create 1024 in
  Buffer.add_string b
    (Printf.sprintf "!%s%s methodsFor: '%s'!\n" class_name (if meta then " class" else "") category);
  List.iter
    (fun m ->
      Buffer.add_string b (chunk m);
      Buffer.add_string b "\n\n")
    methods;
  Buffer.add_string b " !\n";
  Buffer.contents b
