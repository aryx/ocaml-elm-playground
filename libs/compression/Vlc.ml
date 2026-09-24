(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Vlc.mli *)

type 'a t = Leaf of 'a | Node of 'a t option * 'a t option

let bits_of (code : string) : int list =
  List.filter_map (function '0' -> Some 0 | '1' -> Some 1 | _ -> None) (List.init (String.length code) (String.get code))

let of_list (codes : (string * 'a) list) : 'a t =
  let rec insert (node : 'a t option) (bits : int list) (v : 'a) : 'a t =
    match (node, bits) with
    | None, [] -> Leaf v
    | Some _, [] | Some (Leaf _), _ -> invalid_arg "Vlc.of_list: a code is a prefix of another"
    | None, b :: rest -> if b = 0 then Node (Some (insert None rest v), None) else Node (None, Some (insert None rest v))
    | Some (Node (l, r)), b :: rest -> if b = 0 then Node (Some (insert l rest v), r) else Node (l, Some (insert r rest v))
  in
  match List.fold_left (fun tree (code, v) -> Some (insert tree (bits_of code) v)) None codes with
  | Some t -> t
  | None -> invalid_arg "Vlc.of_list: no codes"

let read (b : Bits.t) (table : 'a t) : 'a =
  let rec go = function
    | Leaf v -> v
    | Node (l, r) -> (
        match if Bits.read b 1 = 0 then l else r with Some n -> go n | None -> failwith "Vlc: not a code of the table")
  in
  go table

let kraft (codes : (string * 'a) list) : float =
  List.fold_left (fun acc (code, _) -> acc +. (2. ** -.float_of_int (List.length (bits_of code)))) 0. codes
