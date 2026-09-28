(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type architecture = Immediate | Callbacks | Mvc | Mvu

let architectures = [ (Immediate, "immediate"); (Callbacks, "callbacks"); (Mvc, "MVC"); (Mvu, "MVU") ]

type runner = { step : Widget.input -> Widget.paint list; summary : unit -> string }

let label_size (th : Theme.t) s = (Widget.text_width ~size:th.text_size s, th.row)

let places panel layout =
  let at = Layout.arrange panel layout in
  fun slot -> List.assoc slot at

let immediate theme frame =
  let ui = ref (Immediate.set_theme theme Immediate.empty) in
  fun i ->
    let u = frame (Immediate.frame i !ui) in
    ui := u;
    Immediate.paint u

let retained ?(before = fun () -> ()) theme ui i =
  before ();
  Retained.handle i ui;
  Retained.paint theme ui
