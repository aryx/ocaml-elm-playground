(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Weights.mli *)

type t = {
  notes : (string * string) list;
  matrices : (string * Matrix.t) list;
}

let note (w : t) (word : string) : string option = List.assoc_opt word w.notes
let matrix (w : t) (name : string) : Matrix.t option = List.assoc_opt name w.matrices

(*****************************************************************************)
(* Writing *)
(*****************************************************************************)

let to_string (w : t) : string =
  let b = Buffer.create 1024 in
  Buffer.add_string b "weights 1\n";
  List.iter (fun (word, said) -> Buffer.add_string b (Printf.sprintf "note %s %s\n" word said)) w.notes;
  List.iter
    (fun (name, (m : Matrix.t)) -> Buffer.add_string b (Printf.sprintf "matrix %s %d %d\n" name m.rows m.cols))
    w.matrices;
  Buffer.add_string b "end\n";
  List.iter
    (fun (_, (m : Matrix.t)) -> Array.iter (fun x -> Buffer.add_int32_le b (Int32.bits_of_float x)) m.data)
    w.matrices;
  Buffer.contents b

(*****************************************************************************)
(* Reading *)
(*****************************************************************************)

(* the header's lines up to "end", and where the numbers start *)
let rec lines (s : string) (from : int) (acc : string list) : (string list * int, string) result =
  match String.index_from_opt s from '\n' with
  | None -> Error "the header has no end"
  | Some stop ->
      let line = String.sub s from (stop - from) in
      if line = "end" then Ok (List.rev acc, stop + 1) else lines s (stop + 1) (line :: acc)

(* a header line: a note, or a matrix's name and shape *)
type line = Note of string * string | Shape of string * int * int

let line (l : string) : (line, string) result =
  match String.split_on_char ' ' l with
  | "note" :: word :: said -> Ok (Note (word, String.concat " " said))
  | [ "matrix"; name; rows; cols ] -> (
      match (int_of_string_opt rows, int_of_string_opt cols) with
      | (Some r, Some c) when r >= 0 && c >= 0 -> Ok (Shape (name, r, c))
      | _ -> Error (Printf.sprintf "a matrix's size is not two numbers: %S" l))
  | _ -> Error (Printf.sprintf "a line that is neither a note nor a matrix: %S" l)

let of_string (s : string) : (t, string) result =
  let ( let* ) = Result.bind in
  let* (header, numbers) = lines s 0 [] in
  let* header =
    match header with
    | "weights 1" :: rest -> Ok rest
    | _ -> Error "not a weights file (its first line is not \"weights 1\")"
  in
  let* parsed =
    List.fold_left
      (fun acc l ->
        let* acc = acc in
        let* l = line l in
        Ok (l :: acc))
      (Ok []) header
  in
  let parsed = List.rev parsed in
  let notes = List.filter_map (function Note (w, said) -> Some (w, said) | Shape _ -> None) parsed in
  let shapes = List.filter_map (function Shape (n, r, c) -> Some (n, r, c) | Note _ -> None) parsed in
  let announced = List.fold_left (fun n (_, r, c) -> n + (4 * r * c)) 0 shapes in
  let present = String.length s - numbers in
  if announced <> present then
    Error (Printf.sprintf "the header announces %d bytes of numbers, the file has %d" announced present)
  else
    let at = ref numbers in
    let matrices =
      List.map
        (fun (name, rows, cols) ->
          let m =
            Matrix.init rows cols (fun r c -> Int32.float_of_bits (String.get_int32_le s (!at + (4 * ((r * cols) + c)))))
          in
          at := !at + (4 * rows * cols);
          (name, m))
        shapes
    in
    Ok { notes; matrices }
