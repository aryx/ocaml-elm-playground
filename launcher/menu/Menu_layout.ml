(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Menu_layout.mli *)

open Playground
open Menu_model

(*****************************************************************************)
(* Layout *)
(*****************************************************************************)

(* claude: the menu's screen, 16:9 (run_app's ~screen): the grid on the
 * left, the chosen one on the right *)
let screen_w = 1778
let screen_h = 1000
let left_edge = -869. (* the left margin's end *)

let cols = 5
let rows = 4
let thumb = 132.
let cell_w = 160.
let cell_h = 172.
let grid_left = left_edge (* the first column's left edge *)
let grid_top = 285. (* the first row's top edge *)
let shot = 400. (* the chosen one's screenshot *)
let shot_x = 160.
let shot_y = 230.
let text_x = 385. (* what the catalogue says, right of the screenshot *)
let text_w = 480.

(* its code, under both: where Code_map draws it (its top left, pixels) *)
let code_area = (-40., 10., 909, 440)

let in_code_area ((mx, my) : number * number) : bool =
  let x, y, w, h = code_area in
  mx >= x && mx <= x +. float_of_int w && my <= y && my >= y -. float_of_int h

(* the first row shown: the chosen one's row kept in view *)
let first_row (m : model) : int = max 0 ((m.pos / cols) - rows + 1)

(* the centre of the thumbnail of the program at [i] in [shown], if it
 * is in view *)
let cell_centre (m : model) (i : int) : (number * number) option =
  let r = (i / cols) - first_row m in
  if r < 0 || r >= rows then None
  else
    let c = i mod cols in
    Some (grid_left +. (float_of_int c *. cell_w) +. (thumb /. 2.), grid_top -. (float_of_int r *. cell_h) -. (thumb /. 2.))

(* the buttons at the top: the two shelves, the section's arrows *)
let games_tab = (-560., 455.)
let apps_tab = (-460., 455.)
let prev_arrow = (-859., 400.)
let next_arrow = (-499., 400.)

(* the filter bar: each word's left end, and its key *)
let bar_y = 322.
let bar = [ ("b", -869.); ("p", -709.); ("e", -559.); ("m", -434.); ("l", -284.); ("c", -164.) ]
let bar_width = 120.

let near ((x, y) : number * number) ((mx, my) : number * number) ~(w : number) ~(h : number) : bool =
  Float.abs (mx -. x) <= w /. 2. && Float.abs (my -. y) <= h /. 2.
