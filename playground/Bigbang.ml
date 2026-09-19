(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Bigbang.mli *)

(*****************************************************************************)
(* Images *)
(*****************************************************************************)

type image = { width : number; height : number; shape : shape (* centered on (0, 0) *) }
type mode = Solid | Outline

let width (i : image) : number = i.width
let height (i : image) : number = i.height
let to_shape (i : image) : shape = i.shape

(* a line from a to b, 2 wide: outlines are made of them *)
let line (color : color) ((x0, y0) : number * number) ((x1, y1) : number * number) : shape =
  rectangle color (Float.hypot (x1 -. x0) (y1 -. y0) +. 2.) 2. |> rotate (atan2 (y1 -. y0) (x1 -. x0) *. 180. /. Float.pi) |> move ((x0 +. x1) /. 2.) ((y0 +. y1) /. 2.)

let outline (color : color) (points : (number * number) list) : shape =
  match points with [] -> group [] | first :: _ -> group (List.map2 (line color) points (List.tl points @ [ first ]))

let around (n : int) (rx : number) (ry : number) : (number * number) list =
  List.init n (fun i -> let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in (rx *. cos a, ry *. sin a))

let circle (r : number) (m : mode) (c : color) : image =
  { width = 2. *. r; height = 2. *. r; shape = (match m with Solid -> Playground.circle c r | Outline -> outline c (around 48 r r)) }

let ellipse (w : number) (h : number) (m : mode) (c : color) : image =
  { width = w; height = h; shape = (match m with Solid -> oval c w h | Outline -> outline c (around 48 (w /. 2.) (h /. 2.))) }

let rectangle (w : number) (h : number) (m : mode) (c : color) : image =
  let corners = [ (-.w /. 2., -.h /. 2.); (w /. 2., -.h /. 2.); (w /. 2., h /. 2.); (-.w /. 2., h /. 2.) ] in
  { width = w; height = h; shape = (match m with Solid -> Playground.rectangle c w h | Outline -> outline c corners) }

let square (s : number) (m : mode) (c : color) : image = rectangle s s m c

let triangle (s : number) (m : mode) (c : color) : image =
  let h = s *. sqrt 3. /. 2. in
  let points = [ (-.s /. 2., -.h /. 2.); (s /. 2., -.h /. 2.); (0., h /. 2.) ] in
  { width = s; height = h; shape = (match m with Solid -> polygon c points | Outline -> outline c points) }

let text (str : string) (size : number) (c : color) : image =
  { width = 0.6 *. size *. float_of_int (String.length str); height = size; shape = words c str |> scale (size /. words_font_size) }

let empty_scene (w : number) (h : number) : image =
  { width = w; height = h; shape = group [ Playground.rectangle white w h; outline black [ (-.w /. 2., -.h /. 2.); (w /. 2., -.h /. 2.); (w /. 2., h /. 2.); (-.w /. 2., h /. 2.) ] ] }

let overlay (top : image) (bottom : image) : image =
  { width = Float.max top.width bottom.width; height = Float.max top.height bottom.height; shape = group [ bottom.shape; top.shape ] }

let beside (a : image) (b : image) : image =
  let w = a.width +. b.width in
  { width = w; height = Float.max a.height b.height; shape = group [ a.shape |> move_x ((a.width -. w) /. 2.); b.shape |> move_x ((w -. b.width) /. 2.) ] }

let above (a : image) (b : image) : image =
  let h = a.height +. b.height in
  { width = Float.max a.width b.width; height = h; shape = group [ a.shape |> move_y ((h -. a.height) /. 2.); b.shape |> move_y ((b.height -. h) /. 2.) ] }

let place_image (i : image) (x : number) (y : number) (scene : image) : image =
  { scene with shape = group [ scene.shape; i.shape |> move (x -. (scene.width /. 2.)) ((scene.height /. 2.) -. y) ] }

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* the keys' names, HtDP's: the arrows "left", "right", "up", "down",
 * space " ", enter "\r", the others as the playground names them ("a") *)
let key_name (k : string) : string =
  match k with
  | "ArrowLeft" -> "left"
  | "ArrowRight" -> "right"
  | "ArrowUp" -> "up"
  | "ArrowDown" -> "down"
  | "space" -> " "
  | "Enter" | "return" -> "\r"
  | "Escape" | "escape" -> "escape"
  | k -> k

let key_events (before : keyboard) (now : keyboard) : string list * string list =
  (List.map key_name (Set_.elements (Set_.diff now.keys before.keys)), List.map key_name (Set_.elements (Set_.diff before.keys now.keys)))

let mouse_event (before : mouse) (now : mouse) : string option =
  if now.mdown && not before.mdown then Some "button-down"
  else if before.mdown && not now.mdown then Some "button-up"
  else if now.mx <> before.mx || now.my <> before.my then Some (if now.mdown then "drag" else "move")
  else None

type 'w world = { w : 'w; keys_before : keyboard; mouse_before : mouse; frames : int; stopped : bool }

let big_bang (init : 'w) ~(to_draw : 'w -> image) ?(on_tick : ('w -> 'w) option) ?(tick_rate = 1. /. 60.) ?(on_key : ('w -> string -> 'w) option)
    ?(on_release : ('w -> string -> 'w) option) ?(on_mouse : ('w -> number -> number -> string -> 'w) option) ?(stop_when : ('w -> bool) option)
    ?(last_picture : ('w -> image) option) () : ('w world game, msg) app =
  let view (_ : computer) (world : 'w world) : shape list =
    let draw = match last_picture with Some f when world.stopped -> f | _ -> to_draw in
    [ (draw world.w).shape ]
  in
  let update (computer : computer) (world : 'w world) : 'w world =
    if world.stopped then world
    else
      let w = world.w in
      (* the events since the last frame: keys pressed and released, the
       * mouse, in that order; then a tick, if one is due *)
      let pressed, released = key_events world.keys_before computer.keyboard in
      let w = match on_key with Some f -> List.fold_left f w pressed | None -> w in
      let w = match on_release with Some f -> List.fold_left f w released | None -> w in
      let w =
        match (on_mouse, mouse_event world.mouse_before computer.mouse) with
        | Some f, Some e ->
            (* the mouse in the scene's coordinates: from its top-left
             * corner, y going down *)
            let scene = to_draw w in
            f w (computer.mouse.mx +. (scene.width /. 2.)) ((scene.height /. 2.) -. computer.mouse.my) e
        | _ -> w
      in
      let every = max 1 (int_of_float (Float.round (tick_rate *. 60.))) in
      let w = match on_tick with Some f when (world.frames + 1) mod every = 0 -> f w | _ -> w in
      let stopped = match stop_when with Some f -> f w | None -> false in
      { w; keys_before = computer.keyboard; mouse_before = computer.mouse; frames = world.frames + 1; stopped }
  in
  game view update { w = init; keys_before = initial_computer.keyboard; mouse_before = initial_computer.mouse; frames = 0; stopped = false }
