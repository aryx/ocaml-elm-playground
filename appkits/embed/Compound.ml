(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type t = Part of Component.part | Column of t list | Row of t list
type path = int list

let gap = 14.

(* the room each child of a node takes along its axis, and how tall
   the node is -- both at [width] *)
let rec height node width =
  match node with
  | Part p -> p.height width
  | Column kids -> List.fold_left (fun acc k -> acc +. height k width) 0. kids +. (gap *. float_of_int (max 0 (List.length kids - 1)))
  | Row kids ->
      let w = share width kids in
      List.fold_left (fun acc k -> Float.max acc (height k w)) 0. kids

and share width kids =
  let n = float_of_int (max 1 (List.length kids)) in
  (width -. (gap *. (n -. 1.))) /. n

let layout doc ~left ~top ~width =
  let rec go path node left top width acc =
    match node with
    | Part p ->
        let h = p.height width in
        (List.rev path, { Widget.x = left +. (width /. 2.); y = top -. (h /. 2.); w = width; h }) :: acc
    | Column kids ->
        let _, _, acc =
          List.fold_left
            (fun (i, top, acc) k -> (i + 1, top -. height k width -. gap, go (i :: path) k left top width acc))
            (0, top, acc) kids
        in
        acc
    | Row kids ->
        let w = share width kids in
        snd
          (List.fold_left
             (fun (i, acc) k -> (i + 1, go (i :: path) k (left +. (float_of_int i *. (w +. gap))) top w acc))
             (0, acc) kids)
  in
  (List.rev (go [] doc left top width []), height doc width)

let at_point boxes (x, y) = Option.map fst (List.find_opt (fun (_, b) -> Widget.contains b x y) boxes)

let rec get doc path =
  match (doc, path) with
  | Part p, [] -> Some p
  | (Column kids | Row kids), i :: rest -> Option.bind (List.nth_opt kids i) (fun k -> get k rest)
  | _ -> None

(* the same kind of node, with other children *)
let rebuild node kids = match node with Row _ -> Row kids | _ -> Column kids

let rec set doc path part =
  match (doc, path) with
  | Part _, [] -> Part part
  | (Column kids | Row kids), i :: rest -> rebuild doc (List.mapi (fun j k -> if j = i then set k rest part else k) kids)
  | _ -> doc

let rec insert_after doc path node =
  match (doc, path) with
  | (Column kids | Row kids), [] -> rebuild doc (kids @ [ node ])
  | (Column kids | Row kids), [ i ] -> rebuild doc (List.concat (List.mapi (fun j k -> if j = i then [ k; node ] else [ k ]) kids))
  | (Column kids | Row kids), i :: rest -> rebuild doc (List.mapi (fun j k -> if j = i then insert_after k rest node else k) kids)
  (* a lone part becomes a column, so that it has somewhere to go *)
  | Part _, _ -> Column [ doc; node ]

(* a node without the part at path: None when nothing is left of it,
   and a node left with one child is that child *)
let rec remove_in node path =
  match (node, path) with
  | Part _, [] -> None
  | (Column kids | Row kids), i :: rest -> (
      match without kids i rest with [] -> None | [ only ] -> Some only | kids -> Some (rebuild node kids))
  | _ -> Some node

and without kids i rest = List.concat (List.mapi (fun j k -> if j <> i then [ k ] else Option.to_list (remove_in k rest)) kids)

(* the root stays what it is, even with one child or none *)
let remove doc path =
  match (doc, path) with (Column kids | Row kids), i :: rest -> rebuild doc (without kids i rest) | _ -> doc

(* ---- saving: one line per node, each part's text counted ---- *)

let save doc =
  let out = Buffer.create 256 in
  let rec go = function
    | Part p ->
        let text = p.save () in
        Printf.bprintf out "part %s %d\n%s\n" p.kind (String.length text) text
    | Column kids ->
        Printf.bprintf out "column %d\n" (List.length kids);
        List.iter go kids
    | Row kids ->
        Printf.bprintf out "row %d\n" (List.length kids);
        List.iter go kids
  in
  go doc;
  Buffer.contents out

let load registry s =
  (* the line starting at pos, and where the next one starts *)
  let line pos =
    let e = String.index_from s pos '\n' in
    (String.sub s pos (e - pos), e + 1)
  in
  let rec node pos =
    let l, pos = line pos in
    match String.split_on_char ' ' l with
    | [ "part"; kind; n ] ->
        let n = int_of_string n in
        (Part (Component.load registry ~kind (String.sub s pos n)), pos + n + 1)
    | [ ("column" | "row") as what; n ] ->
        let rec kids k pos acc = if k = 0 then (List.rev acc, pos) else let kid, pos = node pos in kids (k - 1) pos (kid :: acc) in
        let kids, pos = kids (int_of_string n) pos [] in
        ((if what = "row" then Row kids else Column kids), pos)
    | _ -> failwith ("Compound.load: " ^ l)
  in
  fst (node 0)
