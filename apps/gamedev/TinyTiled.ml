(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tiled (Thorbjorn Lindeijer, 2008), the map editor
 * most 2D games are laid out in: a level is a grid of cells, each a
 * character, which the game draws its own way. It edits TinyMario's
 * level, an XPM file beside the game (mario_level.xpm), and writes it
 * back.
 *
 * Type a character of the palette and it is written at the cursor,
 * which moves on as in a text editor (space empties a cell), or paint
 * with the mouse (the right button empties). The arrows move the
 * cursor and the camera follows it, since a level is wider than the
 * screen; '+' and '-' add and take away a column on the right, where a
 * level grows; e exports the file: natively in the current directory,
 * in the browser as a download. Copied over the game's, it is the
 * game's level at the next build.
 *
 * The editor draws each cell as its color in the file's palette, with
 * its character on it, and not as the game draws it: it knows the
 * level, not the game. That is the whole trick of a generic map editor
 * -- Tiled's answer is to be given the game's tile *set*, an image cut
 * into cells (an exercise below) -- and the reason the file keeps a
 * color per character: the palette is for whoever edits the map, the
 * characters are for the game (Tilemap.of_xpm).
 *
 * The map and the sprites are the same file format, XPM, for the same
 * reason: one character per cell, a palette beside it, and a picture
 * you can read in a diff (see TinyAseprite.ml, and Xpm.mli). A level
 * can therefore be drawn in GIMP too, a pixel per cell. Tiled's own
 * format, TMX, is XML with the layers as base64 or CSV, and carries
 * what this one has no room for: several layers, objects placed off the
 * grid, properties.
 *
 * Uses: Tilemap (the level, and view_visible: only the cells on
 * screen), Camera2d (the camera following the cursor, and clamped to
 * the level), Sprite (of_xpm, to_xpm, and pixels for the minimap),
 * Scene2d (keys pressed), Playground_platform.export with its
 * capability (Cap.open_out). Not: gui/.
 *
 * Exercises: several layers (a background behind the level, as Tiled
 * has); a tile set drawn by the game, so the editor shows the map as
 * the player sees it (an image cut into cells, or a game's [tile]
 * function given to a library); the level grown at the top and bottom
 * too; a fill bucket; the cells a game cannot reach, found by a flood
 * fill from the start ('@'), drawn in red.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let tile_size = 50.
let empty = ' '

type editor = {
  palette : (char * color) list;
  map : Tilemap.t;
  cursor : int * int;
  cam : Camera2d.t;
  brush : char;
  (* frames an arrow has been held, for the repeat: a level is wider
   * than a sprite, and pressing a key per cell would not do *)
  held : int;
  said : string;
}

type model = editor Scene2d.t

let initial_model : model =
  let palette, rows = Sprite.of_xpm Mario_xpm.level in
  Scene2d.start
    { palette; map = Tilemap.of_strings tile_size rows; cursor = (2, 22); cam = Camera2d.origin; brush = '#'; held = 0; said = "" }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

let paint (e : editor) (c : char) : editor =
  let col, row = e.cursor in
  { e with map = Tilemap.set e.map col row c; brush = c; said = "" }

let move_cursor (e : editor) ((dc, dr) : int * int) : editor =
  let col, row = e.cursor in
  let clamp n hi = max 0 (min (hi - 1) n) in
  { e with cursor = (clamp (col + dc) (Tilemap.cols e.map), clamp (row + dr) (Tilemap.rows e.map)) }

(* [resize e d]: [d] columns more (or fewer) on the right, where a level
 * grows; the rows stay, a screen being as tall as it is *)
let resize (e : editor) (d : int) : editor =
  let w = max 1 (Tilemap.cols e.map + d) in
  let row (r : string) = if String.length r >= w then String.sub r 0 w else r ^ String.make (w - String.length r) empty in
  let e = { e with map = Tilemap.of_strings tile_size (List.map row (Tilemap.to_strings e.map)); said = Printf.sprintf "%d columns" w } in
  move_cursor e (0, 0)

let export (caps : < Cap.open_out >) (e : editor) : editor =
  let bytes = Sprite.to_xpm "mario_level" e.palette (Tilemap.to_strings e.map) in
  Playground_platform.export caps "mario_level.xpm" bytes;
  { e with said = Printf.sprintf "exported mario_level.xpm, %d bytes: copy it over the game's" (String.length bytes) }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* The map has the screen less the two strips: the title and the
 * minimap above, the palette and the help below. The camera is clamped
 * to that area, and the map drawn moved into it, so that the level's
 * edges land at its edges and not under a strip. *)
let above = 190.
let below = 150.
let area (screen : screen) : screen = { screen with height = screen.height -. above -. below }
let area_y = (below -. above) /. 2.

(* the palette, along the bottom of the screen: the empty cell first *)
let brushes (e : editor) : char list = empty :: List.map fst e.palette
let palette_x (i : int) : number = -400. +. (float_of_int i *. 70.)
let palette_y = -400.

let update_mouse (computer : computer) (e : editor) : editor =
  let m = computer.mouse in
  let wx, wy = Camera2d.to_world e.cam m.mx (m.my -. area_y) in
  let col, row = Tilemap.cell e.map wx wy in
  let inside =
    Tilemap.get e.map col row <> None
    && m.my < computer.screen.top -. above
    && m.my > computer.screen.bottom +. below
  in
  if inside && m.mdown then paint { e with cursor = (col, row) } e.brush
  else if inside && m.mrdown then { (paint { e with cursor = (col, row) } empty) with brush = e.brush }
  else if not m.mclick then e
  else
    match List.find_opt (fun (i, _) -> abs_float (m.mx -. palette_x i) < 30. && abs_float (m.my -. palette_y) < 30.) (List.mapi (fun i b -> (i, b)) (brushes e)) with
    | Some (_, b) -> { e with brush = b; said = "" }
    | None -> e

let update (caps : < Cap.open_out >) (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let e = s.scene in
  let typed = computer.keyboard.typed in
  (* a letter is a command unless it is a character of the palette *)
  let command (c : char) = String.contains typed c && not (List.mem_assoc c e.palette) in
  (* the characters typed, each written at the cursor, which moves on *)
  let e =
    String.fold_left (fun e c -> if c = empty || List.mem_assoc c e.palette then move_cursor (paint e c) (1, 0) else e) e typed
  in
  (* An arrow: one cell as it goes down, then, once it has been held
   * for 12 frames, one every 4 -- a keyboard's own repeat, which a
   * game does not get (it sees the keys held, [keyboard], not the
   * presses the system repeats). *)
  let k = computer.keyboard in
  let arrow =
    if k.kup then Some (0, -1) else if k.kdown then Some (0, 1) else if k.kleft then Some (-1, 0) else if k.kright then Some (1, 0) else None
  in
  let step = match arrow with Some d when e.held = 0 || (e.held >= 12 && e.held mod 4 = 0) -> Some d | _ -> None in
  let e = { e with held = (match arrow with None -> 0 | Some _ -> e.held + 1) } in
  let e =
    match step with
    | Some d -> move_cursor e d
    | None ->
    if arrow <> None then e
    else if command '+' || command '=' then resize e 1
    else if command '-' then resize e (-1)
    else if command 'e' then export caps e
    else update_mouse computer e
  in
  (* the camera on the cursor, kept inside the level *)
  let cx, cy = Tilemap.center e.map (fst e.cursor) (snd e.cursor) in
  { s with scene = { e with cam = e.cam |> Camera2d.look_at cx cy |> Camera2d.clamp (area computer.screen) (Tilemap.bounds e.map) } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let yellow = rgb 240 200 40

(* a cell: its color in the palette, and its character on it -- what the
 * file says, not what the game draws *)
let cell (e : editor) (c : char) : shape =
  match List.assoc_opt c e.palette with
  | None -> square (rgb 52 52 64) tile_size
  | Some color -> group [ square color tile_size; text (rgb 20 20 26) 1.6 (String.make 1 c) ]

(* the whole level, small, and the part of it on screen: a minimap is a
 * second camera (as TinyDefender's scanner is) *)
let minimap (computer : computer) (e : editor) : shape list =
  let pixel = 4. in
  let visible = Camera2d.visible computer.screen e.cam in
  let w = (visible.right -. visible.left) /. tile_size *. pixel
  and h = (visible.top -. visible.bottom) /. tile_size *. pixel in
  let mx = 0. and my = 400. in
  let cx = ((visible.left +. visible.right) /. 2. -. (Tilemap.bounds e.map).left) /. tile_size *. pixel in
  let cy = ((visible.top +. visible.bottom) /. 2. -. (Tilemap.bounds e.map).top) /. tile_size *. pixel in
  [ Sprite.pixels pixel e.palette (Tilemap.to_strings e.map) |> move mx my;
    rectangle yellow w 2. |> move (mx +. cx) (my +. cy +. (h /. 2.)) |> fade 0.8;
    rectangle yellow w 2. |> move (mx +. cx) (my +. cy -. (h /. 2.)) |> fade 0.8;
    rectangle yellow 2. h |> move (mx +. cx -. (w /. 2.)) (my +. cy) |> fade 0.8;
    rectangle yellow 2. h |> move (mx +. cx +. (w /. 2.)) (my +. cy) |> fade 0.8 ]

let view (computer : computer) (s : model) : shape list =
  let e = s.scene in
  let cx, cy = Tilemap.center e.map (fst e.cursor) (snd e.cursor) in
  [ rectangle (rgb 40 40 50) computer.screen.width computer.screen.height;
    Camera2d.view e.cam
      [ Tilemap.view_visible (Camera2d.visible (area computer.screen) e.cam) (cell e) e.map;
        square yellow tile_size |> fade 0.35 |> move cx cy ]
    |> move_y area_y ]
  (* the strips the map does not reach: a level under the palette would
   * be unreadable *)
  @ [ rectangle (rgb 40 40 50) computer.screen.width above |> move_y (computer.screen.top -. (above /. 2.));
      rectangle (rgb 40 40 50) computer.screen.width below |> move_y (computer.screen.bottom +. (below /. 2.)) ]
  @ minimap computer e
  @ [ text white 2.5
        (Printf.sprintf "TINY TILED   mario_level   %d x %d   CELL %d,%d" (Tilemap.cols e.map) (Tilemap.rows e.map) (fst e.cursor)
           (snd e.cursor))
      |> move_y 470.;
      text yellow 1.6 e.said |> move_y 330. ]
  @ List.mapi
      (fun i b ->
        group
          ((if b = e.brush then [ square yellow 60. |> fade 0.5 ] else [])
          @ [ cell e b; text white 1.4 (if b = empty then "space" else String.make 1 b) |> move_y (-40.) ])
        |> move (palette_x i) palette_y)
      (brushes e)
  @ [ text gray 1.6 "type a palette character, or paint with the mouse (right: empty)   arrows: cursor" |> move_y (-460.) ]

let app (caps : < Cap.open_out >) = game view (update caps) initial_model

let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_out >)))
