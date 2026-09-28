(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Csg.mli *)

type 'a boundary = { t : float; leaf : 'a; flipped : bool }
type 'a interval = 'a boundary * 'a boundary
type op = Union | Inter | Diff

let flip (b : 'a boundary) : 'a boundary = { b with flipped = not b.flipped }

let combine (op : op) (a : 'a interval list) (b : 'a interval list) : 'a interval list =
  let holds in_a in_b = match op with Union -> in_a || in_b | Inter -> in_a && in_b | Diff -> in_a && not in_b in
  (* the boundaries as events, (boundary, from A?, an entry?), by t;
   * at the same t the entries first, so that [1, 2] and [2, 3] touch
   * rather than part *)
  let events side l = List.concat_map (fun (i, o) -> [ (i, side, true); (o, side, false) ]) l in
  let sorted =
    List.stable_sort
      (fun ((b1 : 'a boundary), _, e1) ((b2 : 'a boundary), _, e2) ->
        match Float.compare b1.t b2.t with 0 -> compare e2 e1 | c -> c)
      (events true a @ events false b)
  in
  let in_a = ref false and in_b = ref false and start = ref None and out = ref [] in
  List.iter
    (fun (bnd, from_a, entry) ->
      let before = holds !in_a !in_b in
      if from_a then in_a := entry else in_b := entry;
      let after = holds !in_a !in_b in
      (* the rule: an operand's exit as the result's entry, or its entry
       * as the result's exit, turns the normal round *)
      if (not before) && after then start := Some (if entry then bnd else flip bnd)
      else if before && not after then
        match !start with
        | Some s ->
            out := (s, if entry then flip bnd else bnd) :: !out;
            start := None
        | None -> ())
    sorted;
  List.rev !out

let inside (l : 'a interval list) (t : float) : bool = List.exists (fun ((i : 'a boundary), (o : 'a boundary)) -> i.t <= t && t <= o.t) l
