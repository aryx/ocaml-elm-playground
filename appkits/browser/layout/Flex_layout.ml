(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Flex_layout.mli *)

type item = {
  base : float;
  grow : float;
  shrink : float;
  min_size : float;
  max_size : float;
  auto_before : bool;
  auto_after : bool;
}

let clamp (it : item) (size : float) : float = Float.max it.min_size (Float.min it.max_size size)

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

let lines ~(wrap : bool) ~(room : float) ~(gap : float) (items : item array) : (int * int) list =
  let n = Array.length items in
  if n = 0 then []
  else if not wrap then [ (0, n - 1) ]
  else
    (* each line as many as fit, at least one *)
    let rec go start acc =
      if start >= n then List.rev acc
      else
        let rec extend j used =
          if j + 1 < n && used +. gap +. clamp items.(j + 1) items.(j + 1).base <= room then extend (j + 1) (used +. gap +. clamp items.(j + 1) items.(j + 1).base)
          else j
        in
        let j = extend start (clamp items.(start) items.(start).base) in
        go (j + 1) ((start, j) :: acc)
    in
    go 0 []

(*****************************************************************************)
(* Resolving flexible lengths (section 9.7) *)
(*****************************************************************************)

let resolve ~(room : float) ~(gap : float) (items : item array) : float array =
  let n = Array.length items in
  let gaps = gap *. float_of_int (max 0 (n - 1)) in
  let growing = room -. gaps -. Array.fold_left (fun s it -> s +. clamp it it.base) 0. items >= 0. in
  (* an item that cannot flex this way is frozen at its base from the
   * start *)
  let frozen = Array.map (fun it -> if growing then it.grow = 0. else it.shrink = 0.) items in
  let sizes = Array.map (fun it -> clamp it it.base) items in
  let rec loop () =
    if Array.exists not frozen then (
      let free = ref (room -. gaps) and grow = ref 0. and scaled = ref 0. in
      Array.iteri
        (fun i it ->
          if frozen.(i) then free := !free -. sizes.(i)
          else (
            free := !free -. it.base;
            grow := !grow +. it.grow;
            scaled := !scaled +. (it.shrink *. it.base)))
        items;
      (* grow factors summing to less than 1 take only that part *)
      let free = if growing && !grow < 1. then !free *. !grow else !free in
      let target =
        Array.mapi
          (fun i it ->
            if frozen.(i) then sizes.(i)
            else if growing then if !grow > 0. then it.base +. (free *. it.grow /. !grow) else it.base
            else if !scaled > 0. then it.base +. (free *. it.shrink *. it.base /. !scaled)
            else it.base)
          items
      in
      (* clamped by min and max: the violations decide who is frozen *)
      let total = ref 0. in
      let clamped = Array.mapi (fun i it -> if frozen.(i) then sizes.(i) else clamp it target.(i)) items in
      Array.iteri (fun i _ -> if not frozen.(i) then total := !total +. (clamped.(i) -. target.(i))) items;
      if Float.abs !total < 1e-9 then Array.iteri (fun i _ -> if not frozen.(i) then (sizes.(i) <- clamped.(i); frozen.(i) <- true)) items
      else (
        Array.iteri
          (fun i _ ->
            let v = clamped.(i) -. target.(i) in
            if (not frozen.(i)) && ((!total > 0. && v > 0.) || (!total < 0. && v < 0.)) then (
              sizes.(i) <- clamped.(i);
              frozen.(i) <- true))
          items;
        loop ()))
  in
  loop ();
  sizes

(*****************************************************************************)
(* Placing *)
(*****************************************************************************)

let place ~(justify : Computed.align) ~(room : float) ~(gap : float) (items : item array) (sizes : float array) : float array =
  let n = Array.length items in
  let used = Array.fold_left ( +. ) 0. sizes +. (gap *. float_of_int (max 0 (n - 1))) in
  let left = Float.max 0. (room -. used) in
  let autos = Array.fold_left (fun k it -> k + (if it.auto_before then 1 else 0) + if it.auto_after then 1 else 0) 0 items in
  (* the room left before the first, and between two *)
  let first, between =
    if autos > 0 then (0., 0.)
    else
      match justify with
      | End -> (left, 0.)
      | Center -> (left /. 2., 0.)
      | Space_between -> (0., if n > 1 then left /. float_of_int (n - 1) else 0.)
      | Space_around -> (left /. float_of_int (2 * n), left /. float_of_int n)
      | Space_evenly -> (left /. float_of_int (n + 1), left /. float_of_int (n + 1))
      | Start | Stretch | Align_baseline -> (0., 0.)
  in
  let per_auto = if autos > 0 then left /. float_of_int autos else 0. in
  let pos = ref first in
  Array.mapi
    (fun i it ->
      if it.auto_before then pos := !pos +. per_auto;
      let at = !pos in
      pos := !pos +. sizes.(i) +. gap +. between +. if it.auto_after then per_auto else 0.;
      at)
    items

let cross ~(align : Computed.align) ~(line : float) ~(size : float) : float * float =
  match align with
  | End -> (line -. size, size)
  | Center -> ((line -. size) /. 2., size)
  | Stretch -> (0., Float.max size line)
  | Start | Align_baseline | Space_between | Space_around | Space_evenly -> (0., size)
