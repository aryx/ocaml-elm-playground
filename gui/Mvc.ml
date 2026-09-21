(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mvc.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'model t = {
  mutable model : 'model;
  mutable observers : (unit -> unit) list;
  mutable notifications : int;
}

(*****************************************************************************)
(* Functions *)
(*****************************************************************************)

let create model = { model; observers = []; notifications = 0 }
let get t = t.model
let on_change t f = t.observers <- t.observers @ [ f ]
let notifications t = t.notifications

let change t f =
  t.model <- f t.model;
  (* everyone, every time: a view that did not care is woken all the
   * same, since the model knows only that it changed *)
  t.notifications <- t.notifications + List.length t.observers;
  List.iter (fun f -> f ()) t.observers
