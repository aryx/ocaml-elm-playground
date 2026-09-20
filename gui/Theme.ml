(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Theme.mli *)

type t = {
  background : Color.t;
  face : Color.t;
  face_hot : Color.t;
  face_down : Color.t;
  edge : Color.t;
  border : float;
  text : Color.t;
  accent : Color.t;
  text_size : float;
  row : float;
  padding : float;
  slider_width : float;
  knob : float;
}

let default =
  {
    background = Color.rgb 245 245 240;
    face = Color.rgb 220 220 215;
    face_hot = Color.rgb 235 235 225;
    face_down = Color.rgb 195 195 190;
    edge = Color.rgb 120 120 115;
    border = 2.;
    text = Color.black;
    accent = Color.rgb 52 101 164 (* Color.blue *);
    text_size = 16.;
    row = 36.;
    padding = 12.;
    slider_width = 220.;
    knob = 18.;
  }
