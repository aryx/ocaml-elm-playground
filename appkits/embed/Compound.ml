(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type sizing = { height : float option; share : float; scaled : bool }
type t = Part of Component.part | Column of t list | Row of t list | Sized of sizing * t
type path = int list

let gap = 14.
let unsized = { height = None; share = 1.; scaled = false }

(* a node's sizing, and what it wraps *)
let rec split = function Sized (s, n) -> (s, snd (split n)) | n -> (unsized, n)
let share_of n = (fst (split n)).share

(* how tall a node is at [width]; a height a person gave is kept, but
   never less than the node asks for: OpenDoc's frame negotiation, the
   container proposing and the part insisting on what it needs -- unless
   the part is scaled, and then there is nothing to negotiate: it is as
   tall as its proportions make it at that width, or as it was given,
   and drawn to fit *)
let rec height node width =
  match node with
  | Part p -> p.height width
  | Sized ({ scaled = true; height = h; _ }, Part p) -> (
      match h with Some h -> h | None -> Component.fitted_height ~scaled:true p width)
  | Sized (s, n) -> ( match s.height with Some h -> Float.max h (height n width) | None -> height n width)
  | Column kids -> List.fold_left (fun acc k -> acc +. height k width) 0. kids +. (gap *. float_of_int (max 0 (List.length kids - 1)))
  | Row kids -> List.fold_left2 (fun acc k w -> Float.max acc (height k w)) 0. kids (widths width kids)

(* a row's width, shared out by its children's shares *)
and widths width kids =
  let n = float_of_int (max 1 (List.length kids)) in
  let room = width -. (gap *. (n -. 1.)) in
  let total = List.fold_left (fun acc k -> acc +. share_of k) 0. kids in
  List.map (fun k -> room *. share_of k /. total) kids

type splitter = { row : path; index : int; grip : Widget.box; span : float * float }

(* one walk for both: where the parts go, and where a row's children
   meet -- the gaps a person can drag *)
let walk doc ~left ~top ~width =
  let parts = ref [] and splitters = ref [] in
  let rec go ?forced path node left top width =
    match node with
    | Part p ->
        let h = match forced with Some h -> h | None -> p.height width in
        parts := (List.rev path, { Widget.x = left +. (width /. 2.); y = top -. (h /. 2.); w = width; h }) :: !parts
    | Sized (_, n) -> go ~forced:(height node width) path n left top width
    | Column kids ->
        ignore
          (List.fold_left
             (fun (i, top) k ->
               go (i :: path) k left top width;
               (i + 1, top -. height k width -. gap))
             (0, top) kids)
    | Row kids ->
        let ws = widths width kids in
        let h = height node width in
        let lefts = List.rev (snd (List.fold_left (fun (x, acc) w -> (x +. w +. gap, x :: acc)) (left, []) ws)) in
        List.iteri
          (fun i (k, (l, w)) ->
            go (i :: path) k l top w;
            if i + 1 < List.length kids then
              let r = List.nth lefts (i + 1) +. List.nth ws (i + 1) in
              splitters :=
                { row = List.rev path; index = i; grip = { Widget.x = l +. w +. (gap /. 2.); y = top -. (h /. 2.); w = gap; h }; span = (l, r) }
                :: !splitters)
          (List.combine kids (List.combine lefts ws))
  in
  go [] doc left top width;
  (List.rev !parts, List.rev !splitters)

let layout doc ~left ~top ~width = (fst (walk doc ~left ~top ~width), height doc width)
let splitters doc ~left ~top ~width = snd (walk doc ~left ~top ~width)
let at_point boxes (x, y) = Option.map fst (List.find_opt (fun (_, b) -> Widget.contains b x y) boxes)

(* the wrappers are not in the paths: a path goes through a Sized to
   what it wraps, and the Sized goes where its node goes *)
let rec get doc path =
  match (doc, path) with
  | Sized (_, n), _ -> get n path
  | Part p, [] -> Some p
  | (Column kids | Row kids), i :: rest -> Option.bind (List.nth_opt kids i) (fun k -> get k rest)
  | _ -> None

(* the same kind of node, with other children *)
let rebuild node kids = match node with Row _ -> Row kids | _ -> Column kids

(* [update doc path f]: the node at path -- its wrapper included --
   given to f *)
let rec update doc path f =
  match (doc, path) with
  | _, [] -> f doc
  | Sized (s, n), _ -> Sized (s, update n path f)
  | (Column kids | Row kids), i :: rest -> rebuild doc (List.mapi (fun j k -> if j = i then update k rest f else k) kids)
  | _ -> doc

let rec set doc path part =
  match (doc, path) with
  | Sized (s, n), _ -> Sized (s, set n path part)
  | Part _, [] -> Part part
  | (Column kids | Row kids), i :: rest -> rebuild doc (List.mapi (fun j k -> if j = i then set k rest part else k) kids)
  | _ -> doc

(* whether the node at [path] is scaled rather than negotiated with *)
let scaled doc path =
  let rec go doc path =
    match (doc, path) with
    | Sized (s, n), [] -> s.scaled || go n []
    | Sized (_, n), _ -> go n path
    | (Column kids | Row kids), i :: rest -> ( match List.nth_opt kids i with Some k -> go k rest | None -> false)
    | _ -> false
  in
  go doc path

let set_scaled doc path b =
  update doc path (fun n ->
      let s, inner = split n in
      Sized ({ s with scaled = b }, inner))

let set_height doc path h =
  update doc path (fun n ->
      let s, inner = split n in
      Sized ({ s with height = h }, inner))

let resize_row doc path i fraction =
  update doc path (fun n ->
      let s, row = split n in
      let row =
        match row with
        | Row kids when i + 1 < List.length kids ->
            let a = List.nth kids i and b = List.nth kids (i + 1) in
            let total = share_of a +. share_of b in
            let f = Float.max 0.1 (Float.min 0.9 fraction) in
            let resize k share =
              let ks, inner = split k in
              Sized ({ ks with share }, inner)
            in
            Row (List.mapi (fun j k -> if j = i then resize a (f *. total) else if j = i + 1 then resize b ((1. -. f) *. total) else k) kids)
        | row -> row
      in
      if s = unsized then row else Sized (s, row))

let rec insert_after doc path node =
  match (doc, path) with
  | Sized (s, n), _ -> Sized (s, insert_after n path node)
  | (Column kids | Row kids), [] -> rebuild doc (kids @ [ node ])
  | (Column kids | Row kids), [ i ] -> rebuild doc (List.concat (List.mapi (fun j k -> if j = i then [ k; node ] else [ k ]) kids))
  | (Column kids | Row kids), i :: rest -> rebuild doc (List.mapi (fun j k -> if j = i then insert_after k rest node else k) kids)
  (* a lone part becomes a column, so that it has somewhere to go *)
  | Part _, _ -> Column [ doc; node ]

(* a node without the part at path: None when nothing is left of it,
   and a node left with one child is that child *)
let rec remove_in node path =
  match (node, path) with
  | Sized (s, n), _ -> Option.map (fun n -> Sized (s, n)) (remove_in n path)
  | Part _, [] -> None
  | (Column kids | Row kids), i :: rest -> (
      match without kids i rest with [] -> None | [ only ] -> Some only | kids -> Some (rebuild node kids))
  | _ -> Some node

and without kids i rest = List.concat (List.mapi (fun j k -> if j <> i then [ k ] else Option.to_list (remove_in k rest)) kids)

(* the root stays what it is, even with one child or none *)
let rec remove doc path =
  match (doc, path) with
  | Sized (s, n), _ -> Sized (s, remove n path)
  | (Column kids | Row kids), i :: rest -> rebuild doc (without kids i rest)
  | _ -> doc

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
    | Sized (sz, n) ->
        Printf.bprintf out "sized %s %g%s\n"
          (match sz.height with Some h -> Printf.sprintf "%g" h | None -> "-")
          sz.share
          (if sz.scaled then " scaled" else "");
        go n
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
    | "sized" :: h :: share :: rest ->
        let n, pos = node pos in
        let height = if h = "-" then None else Some (float_of_string h) in
        (Sized ({ height; share = float_of_string share; scaled = rest = [ "scaled" ] }, n), pos)
    | _ -> failwith ("Compound.load: " ^ l)
  in
  fst (node 0)
