(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Aseprite (David Capello, 2001), the sprite editor of
 * today's pixel artists: a sprite is a few frames of pixel art, drawn a
 * pixel at a time, the one before shown faintly under the one drawn
 * (the onion skin, from the cel animators' light table), and played
 * back as it is drawn. It edits TinyMario's hero, whose four poses are
 * XPM files (mario_stand.xpm, ...), and writes them back.
 *
 * Type a character of the palette and it is painted at the cursor,
 * which moves on as in a text editor ('.' is transparent): the keys are
 * the file's own characters, as in TinySokobanEd. The arrows move the
 * cursor; the mouse paints with the brush (the last character typed,
 * or the one clicked in the palette), the right button erases. Tab goes
 * to the next frame (with shift, the one before), n adds a copy of the
 * frame after it, f flips it left-right (Sprite.flip), o shows or hides
 * the onion skin, and e exports every frame as its XPM file: natively
 * in the current directory, in the browser as downloads. Copied over
 * the game's files, they are its sprites at the next build.
 *
 * Every home computer had its sprite editor, since its sprites were a
 * chip's: the Commodore 64's were 24x21 pixels, typed as DATA lines
 * until editors drew them; STOS (1988) and AMOS (1990) came with one,
 * next to their map and music editors; Deluxe Paint (Dan Silva, 1985)
 * drew most of the rest, animation included. Aseprite made the sprite
 * editor a program of its own again, for the pixel art of indie games
 * (Celeste's among them).
 *
 * A tool for any game, so in apps/gamedev/ (games/README-tools.md): what
 * it writes is read by a playground layer, Sprite, in a format of the
 * world's, XPM (graphics/images/xpm/), which GIMP and ImageMagick open
 * too, and which a game embeds at build time (TinyMario's Mario_xpm, see
 * games/platform/dune; apps/gamedev/dune embeds the same files, to start
 * from them).
 *
 * Uses: Sprite (of_xpm, to_xpm, pixels, flip, frame), Scene2d (keys
 * pressed), Playground_platform.export with its capability
 * (Cap.open_out). Not: the paint appkit (a sprite is a few hundred
 * pixels, as characters, not a bitmap), gui/.
 *
 * Exercises: a color picker, to change the palette (the characters stay,
 * their colors change: a palette swap, the NES's way of making Luigi out
 * of Mario); a bigger or smaller canvas; the fill bucket (TinyMacPaint's
 * seed fill, over characters); undo (the puzzle kit's Undo, over the
 * frames); frames of different speeds, as Aseprite's timeline has.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a frame: its name (the XPM array's, and its file's) and its rows *)
type frame = { name : string; rows : string list }

type editor = {
  palette : (char * color) list;
  frames : frame list;
  current : int;
  cursor : int * int;
  brush : char; (* what the mouse paints *)
  onion : bool;
  said : string; (* what the last command did *)
}

type model = editor Scene2d.t

let transparent = '.'

let initial_model : model =
  let load (name, text) = (name, Sprite.of_xpm text) in
  let files =
    List.map load
      [ ("mario_stand", Mario_xpm.stand); ("mario_walk1", Mario_xpm.walk1); ("mario_walk2", Mario_xpm.walk2); ("mario_jump", Mario_xpm.jump) ]
  in
  let palette = match files with (_, (p, _)) :: _ -> p | [] -> [] in
  Scene2d.start
    { palette; frames = List.map (fun (name, (_, rows)) -> { name; rows }) files; current = 0; cursor = (0, 0);
      brush = transparent; onion = true; said = "" }

let frame (e : editor) : frame = List.nth e.frames e.current
let cols (e : editor) : int = List.fold_left (fun acc r -> max acc (String.length r)) 0 (frame e).rows
let nrows (e : editor) : int = List.length (frame e).rows

let set_frame (e : editor) (f : frame) : editor = { e with frames = List.mapi (fun i g -> if i = e.current then f else g) e.frames }

(*****************************************************************************)
(* Editing *)
(*****************************************************************************)

(* [paint e c]: the pixel under the cursor becomes [c] *)
let paint (e : editor) (c : char) : editor =
  let col, row = e.cursor in
  let f = frame e in
  let rows = List.mapi (fun r line -> if r <> row then line else String.mapi (fun i old -> if i = col then c else old) line) f.rows in
  set_frame { e with brush = c; said = "" } { f with rows }

let move_cursor (e : editor) ((dc, dr) : int * int) : editor =
  let col, row = e.cursor in
  let clamp n hi = max 0 (min (hi - 1) n) in
  { e with cursor = (clamp (col + dc) (cols e), clamp (row + dr) (nrows e)) }

let export (caps : < Cap.open_out >) (e : editor) : editor =
  e.frames |> List.iter (fun f -> Playground_platform.export caps (f.name ^ ".xpm") (Sprite.to_xpm f.name e.palette f.rows));
  { e with said = Printf.sprintf "exported %s: copy them over the game's" (String.concat ", " (List.map (fun f -> f.name ^ ".xpm") e.frames)) }

(* a copy of this frame, after it *)
let add_frame (e : editor) : editor =
  let f = frame e in
  let copy = { f with name = Printf.sprintf "%s_%d" f.name (List.length e.frames + 1) } in
  let before = List.filteri (fun i _ -> i <= e.current) e.frames and after = List.filteri (fun i _ -> i > e.current) e.frames in
  { e with frames = before @ [ copy ] @ after; current = e.current + 1; said = "a new frame, " ^ copy.name }

(*****************************************************************************)
(* The layout *)
(*****************************************************************************)

(* the canvas: one pixel of the sprite, [cell] on the screen *)
let canvas_x = -130.
let canvas_y = 60.
let cell (e : editor) : number = 400. /. float_of_int (max (cols e) (nrows e))

(* the center of the pixel (col, row) on the screen *)
let pixel_center (e : editor) ((col, row) : int * int) : number * number =
  let c = cell e in
  ( canvas_x +. ((float_of_int col +. 0.5 -. (float_of_int (cols e) /. 2.)) *. c),
    canvas_y -. ((float_of_int row +. 0.5 -. (float_of_int (nrows e) /. 2.)) *. c) )

(* the pixel under the mouse, if on the canvas *)
let pixel_at (e : editor) (x : number) (y : number) : (int * int) option =
  let c = cell e in
  let col = int_of_float (floor (((x -. canvas_x) /. c) +. (float_of_int (cols e) /. 2.))) in
  let row = int_of_float (floor (((canvas_y -. y) /. c) +. (float_of_int (nrows e) /. 2.))) in
  if col >= 0 && col < cols e && row >= 0 && row < nrows e then Some (col, row) else None

(* the palette, under the canvas: the transparent first *)
let palette_y = -240.
let brushes (e : editor) : char list = transparent :: List.map fst e.palette
let palette_x (i : int) : number = canvas_x -. 160. +. (float_of_int i *. 70.)

let update_mouse (m : mouse) (e : editor) : editor =
  match pixel_at e m.mx m.my with
  | Some p when m.mdown -> paint { e with cursor = p } e.brush
  | Some p when m.mrdown -> { (paint { e with cursor = p } transparent) with brush = e.brush }
  | _ when m.mclick -> (
      match List.find_opt (fun (i, _) -> abs_float (m.mx -. palette_x i) < 30. && abs_float (m.my -. palette_y) < 30.) (List.mapi (fun i b -> (i, b)) (brushes e)) with
      | Some (_, b) -> { e with brush = b }
      | None -> e)
  | _ -> e

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (caps : < Cap.open_out >) (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed (key : keyboard -> bool) = Scene2d.pressed key s in
  let e = s.scene in
  let typed = computer.keyboard.typed in
  (* a letter is a command unless it is a character of the palette *)
  let command (c : char) = String.contains typed c && not (List.mem_assoc c e.palette) in
  (* the characters typed, each painted at the cursor, which moves on *)
  let e =
    String.fold_left
      (fun e c -> if c = transparent || List.mem_assoc c e.palette then move_cursor (paint e c) (1, 0) else e)
      e typed
  in
  let n = List.length e.frames in
  let e =
    if pressed (fun k -> k.kup) then move_cursor e (0, -1)
    else if pressed (fun k -> k.kdown) then move_cursor e (0, 1)
    else if pressed (fun k -> k.kleft) then move_cursor e (-1, 0)
    else if pressed (fun k -> k.kright) then move_cursor e (1, 0)
    else if pressed (fun k -> Set_.mem "Tab" k.keys) then
      move_cursor { e with current = (if computer.keyboard.kshift then e.current + n - 1 else e.current + 1) mod n; said = "" } (0, 0)
    else if command 'n' then add_frame e
    else if command 'f' then set_frame { e with said = "" } { (frame e) with rows = Sprite.flip (frame e).rows }
    else if command 'o' then { e with onion = not e.onion }
    else if command 'e' then export caps e
    else update_mouse computer.mouse e
  in
  { s with scene = e }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let yellow = rgb 240 200 40

(* transparent pixels over a checkerboard, as paint programs show them *)
let checkerboard (e : editor) : shape =
  let c = cell e in
  List.init (nrows e) (fun row ->
      List.init (cols e) (fun col ->
          let x, y = pixel_center e (col, row) in
          square (if (col + row) mod 2 = 0 then rgb 70 70 84 else rgb 58 58 70) c |> move x y))
  |> List.concat |> group

let view_canvas (e : editor) : shape list =
  let c = cell e in
  let cx, cy = pixel_center e e.cursor in
  [ checkerboard e ]
  @ (if e.onion && e.current > 0 then
       [ Sprite.pixels c e.palette (List.nth e.frames (e.current - 1)).rows |> fade 0.25 |> move canvas_x canvas_y ]
     else [])
  @ [ Sprite.pixels c e.palette (frame e).rows |> move canvas_x canvas_y; square yellow c |> fade 0.35 |> move cx cy ]

(* every frame small, the current one marked; and the sprite played *)
let view_frames (computer : computer) (e : editor) : shape list =
  List.mapi
    (fun i f ->
      let x = canvas_x -. 160. +. (float_of_int i *. 110.) in
      group
        ((if i = e.current then [ square yellow 76. |> fade 0.4 ] else [])
        @ [ Sprite.pixels 6. e.palette f.rows; text gray 1.2 f.name |> move_y (-50.) ])
      |> move x (-340.))
    e.frames
  @ [ text gray 1.6 "played, 4 frames a second" |> move 300. 250.;
      Sprite.pixels 12. e.palette (Sprite.frame 4. computer.time (List.map (fun f -> f.rows) e.frames)) |> move 300. 150.;
      text gray 1.6 "the game's size" |> move 300. 40.;
      Sprite.pixels 4. e.palette (frame e).rows |> move 300. (-10.) ]

let view_palette (e : editor) : shape list =
  List.mapi
    (fun i b ->
      let swatch = match List.assoc_opt b e.palette with Some col -> square col 40. | None -> group [ square (rgb 70 70 84) 40.; square (rgb 58 58 70) 20. |> move 10. 10.; square (rgb 58 58 70) 20. |> move (-10.) (-10.) ] in
      group ((if b = e.brush then [ square yellow 56. |> fade 0.5 ] else []) @ [ swatch; text white 1.6 (String.make 1 b) |> move_y (-40.) ])
      |> move (palette_x i) palette_y)
    (brushes e)

let view (computer : computer) (s : model) : shape list =
  let e = s.scene in
  [ rectangle (rgb 40 40 50) computer.screen.width computer.screen.height;
    text white 3. (Printf.sprintf "TINY ASEPRITE   %s   FRAME %d/%d" (frame e).name (e.current + 1) (List.length e.frames)) |> move_y 440.;
    text yellow 1.6 e.said |> move_y 395. ]
  @ view_canvas e @ view_palette e @ view_frames computer e
  @ [ text gray 1.6 "type a palette character, or paint with the mouse (right: erase)   arrows: cursor" |> move_y (-420.);
      text gray 1.6 "tab: next frame   n: new frame   f: flip   o: onion skin   e: export" |> move_y (-450.) ]

let app (caps : < Cap.open_out >) = game view (update caps) initial_model

let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_out >)))
