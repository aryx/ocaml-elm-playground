(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Transition.mli *)

type rect = { x : float; y : float; w : float; h : float }
type 'k frame = { key : 'k; rect : rect; alpha : float }
type 'k t = { moves : ('k * rect * rect * float * float) list (* from, to, alpha from, alpha to *) }

let lerp (a : float) (b : float) (p : float) = a +. ((b -. a) *. p)
let lerp_rect (a : rect) (b : rect) (p : float) : rect = { x = lerp a.x b.x p; y = lerp a.y b.y p; w = lerp a.w b.w p; h = lerp a.h b.h p }
let centre (r : rect) : rect = { x = r.x +. (r.w /. 2.); y = r.y +. (r.h /. 2.); w = 0.; h = 0. }

let make ?(enter = fun _ r -> centre r) ?(leave = fun _ r -> centre r) ~(before : ('k * rect) list) ~(after : ('k * rect) list) () : 'k t =
  let old = Hashtbl.create 64 in
  List.iter (fun (k, r) -> Hashtbl.replace old k r) before;
  let now = Hashtbl.create 64 in
  List.iter (fun (k, r) -> Hashtbl.replace now k r) after;
  let moving = List.map (fun (k, r) -> match Hashtbl.find_opt old k with Some r0 -> (k, r0, r, 1., 1.) | None -> (k, enter k r, r, 0., 1.)) after in
  let leaving = List.filter_map (fun (k, r) -> if Hashtbl.mem now k then None else Some (k, r, leave k r, 1., 0.)) before in
  { moves = moving @ leaving }

let at (t : 'k t) (p : float) : 'k frame list =
  List.map (fun (key, a, b, fa, fb) -> { key; rect = lerp_rect a b p; alpha = Float.max 0. (Float.min 1. (lerp fa fb p)) }) t.moves
