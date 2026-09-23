(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Widget.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type box = { x : float; y : float; w : float; h : float }

let left (b : box) = b.x -. (b.w /. 2.)
let right (b : box) = b.x +. (b.w /. 2.)
let bottom (b : box) = b.y -. (b.h /. 2.)
let top (b : box) = b.y +. (b.h /. 2.)

let contains (b : box) px py =
  px >= left b && px <= right b && py >= bottom b && py <= top b

type id = float * float

let id (b : box) : id = (b.x, b.y)

let inset d (b : box) =
  { b with w = max 0. (b.w -. (2. *. d)); h = max 0. (b.h -. (2. *. d)) }

type input = {
  mx : float;
  my : float;
  mdown : bool;
  mclick : bool;
  mrdown : bool;
  typed : string;
  wheel : float;
  keys : string list;
}

let no_input =
  { mx = 0.; my = 0.; mdown = false; mclick = false; mrdown = false; typed = ""; wheel = 0.; keys = [] }

type canvas_event = Hover of (float * float) | Press of (float * float) | Right_press of (float * float)

(*****************************************************************************)
(* What a widget draws *)
(*****************************************************************************)

type paint =
  | Fill of Color.t * box
  | Text of Color.t * box * string
  | Disc of Color.t * float * float * float
  | Segment of Color.t * float * float * float * float * float

let frame color thickness (b : box) =
  let t = min thickness (min (b.w /. 2.) (b.h /. 2.)) in
  [
    Fill (color, { b with y = top b -. (t /. 2.); h = t });
    Fill (color, { b with y = bottom b +. (t /. 2.); h = t });
    (* claude: the sides stop at the bars above, so no pixel is filled
     * twice -- it would show through a translucent color *)
    Fill (color, { b with x = left b +. (t /. 2.); w = t; h = b.h -. (2. *. t) });
    Fill (color, { b with x = right b -. (t /. 2.); w = t; h = b.h -. (2. *. t) });
  ]

let text_width ~size s = 0.6 *. size *. float_of_int (String.length s)
