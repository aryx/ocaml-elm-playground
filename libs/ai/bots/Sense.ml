(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sense.mli *)

type 'v target = { visible : bool; audible : bool; position : 'v option; age : int; seen_for : int }

let unknown : 'v target = { visible = false; audible = false; position = None; age = 0; seen_for = 0 }

let update ?(sight = Float.infinity) ?(hearing = 0.) ~(distance : float) ~(clear : bool) ~(position : 'v)
    (t : 'v target) : 'v target =
  let visible = clear && distance <= sight in
  {
    visible;
    audible = distance <= hearing;
    position = (if visible then Some position else t.position);
    age = (if visible then 0 else t.age + 1);
    seen_for = (if visible then t.seen_for + 1 else 0);
  }

let lost (t : 'v target) : bool = (not t.visible) && t.position = None
let forget ~(after : int) (t : 'v target) : 'v target = if (not t.visible) && t.age > after then { t with position = None } else t

let nearest (targets : (float * 'v target) list) : 'v target option =
  List.filter (fun (_, t) -> t.visible) targets
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> function
  | (_, t) :: _ -> Some t
  | [] -> None

let focus (targets : (float * 'v target) list) : 'v target option =
  match nearest targets with
  | Some t -> Some t
  | None -> (
      List.filter (fun (_, t) -> t.position <> None) targets
      |> List.sort (fun (_, a) (_, b) -> compare a.age b.age)
      |> function
      | (_, t) :: _ -> Some t
      | [] -> None)
