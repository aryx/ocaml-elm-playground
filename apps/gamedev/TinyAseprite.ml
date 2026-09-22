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
 * the onion skin, '+' and '-' grow and shrink the canvas (every frame:
 * a sprite's frames are one size), and e exports every frame as its
 * XPM file: natively
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
 * The colors come from a fixed palette of sixteen, PICO-8's: clicking
 * one gives it to the brush's character (the others keep theirs: a
 * palette swap, the NES's way of making Luigi out of Mario), or, with
 * the transparent brush, adds a character for it.
 *
 * Exercises: a color of your own (three sliders, or a hexadecimal
 * typed); a character taken out of the palette again; the fill bucket (TinyMacPaint's
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

(* The colors to choose from: PICO-8's sixteen (Joseph White,
 * Lexaloffle, 2015), the fixed palette of a fantasy console, as the
 * home computers had theirs (the Commodore 64's sixteen, 1982). A
 * palette that small is a style: the colors go together whatever you
 * pick, which is why pixel artists still draw in it. *)
let colors : color list =
  [ rgb 0 0 0; rgb 29 43 83; rgb 126 37 83; rgb 0 135 81; rgb 171 82 54; rgb 95 87 79; rgb 194 195 199; rgb 255 241 232;
    rgb 255 0 77; rgb 255 163 0; rgb 255 236 39; rgb 0 228 54; rgb 41 173 255; rgb 131 118 156; rgb 255 119 168; rgb 255 204 170 ]

(* the characters a palette can use, the sprite's own first *)
let free_char (e : editor) : char option =
  "RSBKGYWPOCMabcdefghijklmnopqrstuvwxyz0123456789"
  |> String.to_seq |> List.of_seq
  |> List.find_opt (fun c -> not (List.mem_assoc c e.palette))

(* [pick e color]: the color chosen in the strip. The brush's character
 * takes it -- a palette swap, how the NES made Luigi out of Mario --
 * or, with the transparent brush, a new character takes it. *)
let pick (e : editor) (color : color) : editor =
  if e.brush <> transparent then
    { e with palette = List.map (fun (c, col) -> if c = e.brush then (c, color) else (c, col)) e.palette; said = Printf.sprintf "%C recolored" e.brush }
  else
    match free_char e with
    | None -> { e with said = "no character left for another color" }
    | Some c -> { e with palette = e.palette @ [ (c, color) ]; brush = c; said = Printf.sprintf "%C added to the palette" c }

(* [resize e d]: [d] columns and rows more (or fewer), on the right and
 * at the bottom, in every frame: a sprite's frames are one size *)
let resize (e : editor) (d : int) : editor =
  let w = max 1 (cols e + d) and h = max 1 (nrows e + d) in
  let row (r : string) = if String.length r >= w then String.sub r 0 w else r ^ String.make (w - String.length r) transparent in
  let rows (rs : string list) = List.init h (fun i -> match List.nth_opt rs i with Some r -> row r | None -> String.make w transparent) in
  let e = { e with frames = List.map (fun f -> { f with rows = rows f.rows }) e.frames; said = Printf.sprintf "%d x %d" w h } in
  move_cursor e (0, 0)

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

(* the colors to choose from, two rows of eight on the right *)
let pick_center (i : int) : number * number = (290. +. ((float_of_int (i mod 8) -. 3.5) *. 46.), if i < 8 then -120. else -172.)

let update_mouse (m : mouse) (e : editor) : editor =
  match pixel_at e m.mx m.my with
  | Some p when m.mdown -> paint { e with cursor = p } e.brush
  | Some p when m.mrdown -> { (paint { e with cursor = p } transparent) with brush = e.brush }
  | _ when m.mclick -> (
      let near (x, y) = abs_float (m.mx -. x) < 23. && abs_float (m.my -. y) < 23. in
      match List.find_opt (fun (i, _) -> near (palette_x i, palette_y)) (List.mapi (fun i b -> (i, b)) (brushes e)) with
      | Some (_, b) -> { e with brush = b; said = "" }
      | None -> (
          match List.find_opt (fun (i, _) -> near (pick_center i)) (List.mapi (fun i c -> (i, c)) colors) with
          | Some (_, color) -> pick e color
          | None -> e))
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
    else if command '+' || command '=' then resize e 1
    else if command '-' then resize e (-1)
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
      Sprite.pixels 4. e.palette (frame e).rows |> move 300. (-10.);
      text gray 1.6 "click a color: the brush's, or a new one" |> move 300. (-70.) ]
  @ List.mapi
      (fun i color ->
        let x, y = pick_center i in
        square color 40. |> move x y)
      colors

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
      text gray 1.6 "tab: next frame   n: new frame   f: flip   o: onion skin   + -: canvas   e: export" |> move_y (-450.) ]

let app (caps : < Cap.open_out >) = game view (update caps) initial_model

let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_out >)))
