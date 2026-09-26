(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bsp.mli *)

type 'a poly = { corners : (Vec3.t * bool) list; data : 'a }
type plane = Vec3.t * float

(* a hundredth of a millimetre, the model being in metres *)
let eps = 1e-5

(* Newell's normal (Vec3.face_normal): right for a concave polygon or
   one with a keyhole, where the cross product of two sides is not *)
let plane_of corners =
  let ps = List.map fst corners in
  let n = Vec3.face_normal ps in
  if Vec3.length n < 0.5 then None else Some (n, Vec3.dot n (List.hd ps))

let side ((n, d) : plane) p =
  let s = Vec3.dot n p -. d in
  if s > eps then 1 else if s < -.eps then -1 else 0

(* Sutherland-Hodgman, both halves at once. Going round the corners, a
   side from p to q (drawn or not): p goes to its own half (to both if
   on the plane); if the side crosses the plane, the crossing point
   goes to both, and the side's two pieces keep its flag, while the
   side the cut makes along the plane, from where the polygon leaves a
   half to where it comes back, is not drawn *)
let split ((n, d) as plane : plane) corners =
  match corners with
  | [] -> ([], [])
  | first :: _ ->
      let rec sides acc = function
        | a :: (b :: _ as rest) -> sides ((a, b) :: acc) rest
        | [ a ] -> List.rev ((a, first) :: acc)
        | [] -> List.rev acc
      in
      let dist p = Vec3.dot n p -. d in
      let front, back =
        List.fold_left
          (fun (front, back) ((p, drawn), (q, _)) ->
            let sp = side plane p and sq = side plane q in
            let cut () =
              let t = dist p /. (dist p -. dist q) in
              Vec3.add p (Vec3.scale t (Vec3.sub q p))
            in
            match (sp, sq) with
            | 1, -1 ->
                let i = cut () in
                ((i, false) :: (p, drawn) :: front, (i, drawn) :: back)
            | -1, 1 ->
                let i = cut () in
                ((i, drawn) :: front, (i, false) :: (p, drawn) :: back)
            | 1, _ -> ((p, drawn) :: front, back)
            | -1, _ -> (front, (p, drawn) :: back)
            | _ ->
                (* p on the plane: in both, its side drawn only in the
                   half the side goes into (in both if it runs along) *)
                ((p, drawn && sq >= 0) :: front, (p, drawn && sq <= 0) :: back))
          ([], []) (sides [] corners)
      in
      let keep l = if List.length l >= 3 then List.rev l else [] in
      (keep front, keep back)

type 'a t = Empty | Node of { plane : plane; here : 'a poly list; front : 'a t; back : 'a t }

(* half the length of the sum of the cross products of the corners
   taken in turn: the area of a flat polygon *)
let area corners =
  let ps = List.map fst corners in
  match ps with
  | [] -> 0.
  | first :: _ ->
      let rec go acc = function a :: (b :: _ as rest) -> go (Vec3.add acc (Vec3.cross a b)) rest | [ a ] -> Vec3.add acc (Vec3.cross a first) | [] -> acc in
      Vec3.length (go (0., 0., 0.) ps) /. 2.

let rec build_in_order polys =
  match polys with
  | [] -> Empty
  | p :: rest -> (
      match plane_of p.corners with
      | None -> build_in_order rest
      | Some plane ->
          let here, front, back =
            List.fold_left
              (fun (here, front, back) q ->
                let sides = List.map (fun (c, _) -> side plane c) q.corners in
                if List.for_all (( = ) 0) sides then (q :: here, front, back)
                else if List.for_all (fun s -> s >= 0) sides then (here, q :: front, back)
                else if List.for_all (fun s -> s <= 0) sides then (here, front, q :: back)
                else
                  let f, b = split plane q.corners in
                  let piece c l = if c = [] then l else { q with corners = c } :: l in
                  (here, piece f front, piece b back))
              ([ p ], [], []) rest
          in
          Node { plane; here = List.rev here; front = build_in_order (List.rev front); back = build_in_order (List.rev back) })

(* the largest first: a wall seldom cuts the small faces around it (the
   sides of a window's recess are all behind it), while a small face
   often cuts a large one -- fewer cuts, fewer pieces, and fewer
   seams where the pieces meet *)
let build polys = build_in_order (List.stable_sort (fun p q -> compare (area q.corners) (area p.corners)) polys)

let back_to_front ~eye t =
  let rec walk t acc =
    match t with
    | Empty -> acc
    | Node { plane; here; front; back } ->
        (* acc is built from the nearest backwards: the far half last *)
        if side plane eye >= 0 then walk back (here @ walk front acc) else walk front (here @ walk back acc)
  in
  walk t []

let rec size = function Empty -> 0 | Node { here; front; back; _ } -> List.length here + size front + size back
