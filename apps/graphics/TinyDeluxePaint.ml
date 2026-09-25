(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyDeluxePaint: a picture of 32 colours on the Amiga (Deluxe Paint,
 * Dan Silva, Electronic Arts, 1985).
 *
 *   the tools at the right: dotted and continuous freehand, line, fill,
 *   airbrush, rectangles and ellipses (outlined, filled), the brush cut
 *   from the picture (drag a rectangle: it becomes the brush, its own
 *   colours, the background colour transparent), symmetry (each stroke six times,
 *   turned around the middle); the four built-in brushes above them;
 *   the palette below (a click: the colour to paint with; a right
 *   click: the background's); Tab starts and stops the colour cycling;
 *   the Picture menu for Undo, the palette's editor, Export ILBM
 *
 * The picture is 320 by 200 dots, each the *number* of a colour, and
 * the palette of 32 says what each number looks like, in the Amiga's 12
 * bits (4 a channel, 4096 colours to choose 32 from). So a colour can
 * change without a dot changing -- and Deluxe Paint made that its
 * animation: a range of the palette turning, each colour moving to the
 * next number (Cycling.mli). The picture it starts with has a waterfall
 * and a sea painted in stripes of the blues 16 to 23 and a fire in the
 * reds 24 to 31: Tab, and they run, flow and flicker, not one dot of
 * the picture redrawn.
 *
 * What MacPaint (1984, beside this) could not: colour, and the brush
 * cut from the picture -- a tree drawn once and stamped into a forest,
 * Deluxe Paint's own idea. What Photoshop (1990, beside this too) has
 * that this hasn't: 24 bits a dot and no palette; what this has that it
 * hasn't: the palette as a thing to play with.
 *
 * The file is Electronic Arts' own format, IFF ILBM (Ilbm.mli): chunks
 * (the ancestor of RIFF, and so of WAV and AVI), the pixels as
 * bitplanes as the Amiga's chips read them, ByteRun1 (MacPaint's
 * PackBits) to compress them, and the cycling ranges in a CRNG chunk.
 * Export writes one; the File menu saves the same picture as a
 * document.
 *
 * What it uses: appkits/indexed (Indexed, Cycling), libs/graphics/
 * images/ilbm (the picture's type and its file), appkits/document
 * (Undo), the File menu, and gui/'s immediate widgets. The picture is
 * drawn as one bitmap, enlarged without smoothing: the dots as blocks.
 *
 * Left undone, exercises: text; the curve tool; stencils (colours that
 * painting leaves alone); the animation frames of Deluxe Paint III
 * (1988); perspective; the magnifier; editing a cycling range's
 * colours and rate from the palette's editor; importing an ILBM file.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool = Dotted | Freehand | Line_tool | Bucket | Airbrush | Rect | Filled_rect | Ellipse | Filled_ellipse | Cut

let tools = [ Dotted; Freehand; Line_tool; Bucket; Airbrush; Rect; Filled_rect; Ellipse; Filled_ellipse; Cut ]

let tool_name = function
  | Dotted -> "Dotted Freehand"
  | Freehand -> "Continuous Freehand"
  | Line_tool -> "Straight Line"
  | Bucket -> "Fill"
  | Airbrush -> "Airbrush"
  | Rect -> "Rectangle"
  | Filled_rect -> "Filled Rectangle"
  | Ellipse -> "Ellipse"
  | Filled_ellipse -> "Filled Ellipse"
  | Cut -> "Brush"

type model = {
  (* the picture and its palette, as an ILBM holds them, every version *)
  history : Ilbm.t Undo.t;
  file : File_menu.t;
  tool : tool;
  brush : Indexed.brush;
  fg : int;
  bg : int;
  symmetry : bool;
  cycling : bool;
  frames : int; (* since the cycling started: the Amiga turned it at each frame *)
  drag : (int * int) option; (* where the drag began *)
  last : int * int; (* the dot the mouse was on at the frame before *)
  before : Ilbm.t; (* the picture when the drag began *)
  editing : (int * int * int) option; (* the palette's editor open: the colour being made, 0 to 15 each *)
  seed : Lehmer.t;
  was : string list;
  was_down : bool;
  was_rdown : bool;
}

let pic_w = 320
let pic_h = 200

(* the Amiga's 12 bits: 0 to 15 a channel, shown as 0 to 255 *)
let amiga (r, g, b) = (r * 17, g * 17, b * 17)

(* the palette of the picture it starts with: black, white, a sky, the
   hills, the rocks, then the two ranges that turn -- water, fire *)
let palette : (int * int * int) array =
  Array.of_list
    (List.map amiga
       ([ (0, 0, 0); (15, 15, 15) ]
       @ List.init 8 (fun i -> (3 + i, 6 + i, 15)) (* the sky, 2 to 9: dark blue down to pale *)
       @ [ (2, 8, 2); (3, 10, 3); (5, 12, 4); (8, 13, 6) ] (* the hills, 10 to 13 *)
       @ [ (6, 5, 4); (9, 8, 7) ] (* the rocks, 14 and 15 *)
       @ List.init 8 (fun i -> let v = if i < 4 then i else 7 - i in (2 + v, 5 + (2 * v), 10 + v)) (* the water, 16 to 23: a wave of blues *)
       @ List.init 8 (fun i -> let v = if i < 4 then i else 7 - i in (15, 3 + (3 * v), v)) (* the fire, 24 to 31 *)))

let ranges : Ilbm.range list =
  [ { low = 16; high = 23; rate = 3277 (* 12 steps a second *); active = true; reverse = false };
    { low = 24; high = 31; rate = 4096 (* 15 *); active = true; reverse = true } ]

(* the picture it starts with, painted by the same means the tools have *)
let scene : Indexed.t =
  Indexed.change (Indexed.create pic_w pic_h 0) (fun p ->
      (* the sky, in bands *)
      for y = 0 to 119 do
        for x = 0 to pic_w - 1 do
          Indexed.dot p x y (2 + min 7 (y / 15))
        done
      done;
      (* the mountains, rocks *)
      for x = 0 to pic_w - 1 do
        let top = 70 + int_of_float (25. *. Float.abs (sin (float_of_int x /. 40.))) in
        for y = top to 125 do
          Indexed.dot p x y (if (x + y) mod 7 = 0 then 15 else 14)
        done
      done;
      (* the hills *)
      for x = 0 to pic_w - 1 do
        let top = 115 + int_of_float (10. *. sin (float_of_int x /. 25.)) in
        for y = top to 160 do
          Indexed.dot p x y (10 + min 3 ((y - top) / 12))
        done
      done;
      (* the waterfall, stripes of the water's colours, falling as they turn *)
      for y = 70 to 150 do
        for x = 210 to 235 do
          Indexed.dot p x y (16 + ((y + (x / 5)) mod 8))
        done
      done;
      (* the sea, waves of them *)
      for y = 150 to pic_h - 1 do
        for x = 0 to pic_w - 1 do
          Indexed.dot p x y (16 + (((x / 6) + (y * 2)) mod 8))
        done
      done;
      (* the fire, on the shore: a triangle of the fire's colours *)
      for y = 125 to 150 do
        let half = (y - 125) / 2 in
        for x = 70 - half to 70 + half do
          Indexed.dot p x y (24 + ((y + (abs (x - 70) / 2)) mod 8))
        done
      done;
      Indexed.fill_rect p 14 (62, 150) (78, 153);
      (* the sun, within one band of the sky: that band's colour as the
         background, a brush cut around it is a disc *)
      Indexed.fill_ellipse p 1 (41, 16) (53, 28))

let initial_doc : Ilbm.t = { width = pic_w; height = pic_h; planes = 5; pixels = scene.pixels; palette; ranges }

let initial =
  {
    history = Undo.start initial_doc;
    file = File_menu.start;
    tool = Freehand;
    brush = Indexed.round 1;
    fg = 1;
    bg = 0;
    symmetry = false;
    cycling = true;
    frames = 0;
    drag = None;
    last = (0, 0);
    before = initial_doc;
    editing = None;
    seed = Lehmer.of_int 1985;
    was = [];
    was_down = false;
    was_rdown = false;
  }

let doc (m : model) : Ilbm.t = Undo.now m.history
let pic_of (d : Ilbm.t) : Indexed.t = { width = d.width; height = d.height; pixels = d.pixels }
let record ~name (d : Ilbm.t) (m : model) = { m with history = Undo.record ~name d m.history }
let amend (d : Ilbm.t) (m : model) = { m with history = Undo.amend d m.history }

(* the picture changed by [f], drawn on a copy *)
let paint (d : Ilbm.t) (f : Indexed.t -> unit) : Ilbm.t = { d with pixels = (Indexed.change (pic_of d) f).pixels }

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

(* two screen units a dot, the picture at the top left *)
let zoom = 2.
let pic_left = -490.
let pic_top = 400.
let dot_at (x, y) = (int_of_float (Float.floor ((x -. pic_left) /. zoom)), int_of_float (Float.floor ((pic_top -. y) /. zoom)))
let on_picture (x, y) = x >= 0 && y >= 0 && x < pic_w && y < pic_h

let tool_box i : Widget.box = { Widget.x = 215. +. (float_of_int (i mod 2) *. 46.); y = 300. -. (float_of_int (i / 2) *. 46.); w = 42.; h = 42. }
let symmetry_box : Widget.box = { Widget.x = 238.; y = 300. -. (5. *. 46.); w = 88.; h = 36. }
let brushes = [ Indexed.Dots [ (0, 0) ]; Indexed.round 1; Indexed.round 2; Indexed.round 4 ]
let brush_box i : Widget.box = { Widget.x = 202. +. (float_of_int i *. 28.); y = 360.; w = 24.; h = 24. }
let swatch_box i : Widget.box = { Widget.x = -470. +. (float_of_int (i mod 16) *. 42.); y = -40. -. (float_of_int (i / 16) *. 42.); w = 38.; h = 38. }
let menu_box : Widget.box = { Widget.x = 400.; y = 475.; w = 150.; h = 30. }
let menu_picture = [ "Picture"; "Undo"; "Redo"; "Palette..."; "Cycling"; "Clear"; "Export ILBM" ]

(* the palette's editor: a slider a channel, 0 to 15 *)
let channel_box i : Widget.box = { Widget.x = 330.; y = 200. -. (float_of_int i *. 60.); w = 200.; h = 24. }
let ok_box : Widget.box = { Widget.x = 280.; y = -40.; w = 80.; h = 30. }
let cancel_box : Widget.box = { Widget.x = 380.; y = -40.; w = 80.; h = 30. }

(*****************************************************************************)
(* The tools *)
(*****************************************************************************)

(* each dot of a stroke, six times when painting in symmetry *)
let spots (m : model) (p : int * int) : (int * int) list = if m.symmetry then Indexed.symmetric ~order:6 ~centre:(pic_w / 2, pic_h / 2) p else [ p ]

(* the shapes that stretch, from [a] to [z] *)
let shape (m : model) (d : Ilbm.t) (a : int * int) (z : int * int) : Ilbm.t =
  paint d (fun p ->
      match m.tool with
      | Line_tool -> List.iter2 (fun a z -> Indexed.line p m.brush m.fg a z) (spots m a) (spots m z)
      | Rect -> Indexed.frame_rect p m.fg a z
      | Filled_rect -> Indexed.fill_rect p m.fg a z
      | Ellipse -> Indexed.frame_ellipse p m.fg a z
      | Filled_ellipse -> Indexed.fill_ellipse p m.fg a z
      | _ -> ())

let press (m : model) (p : int * int) : model =
  let d = doc m in
  match m.tool with
  | Bucket -> record ~name:"Fill" (paint d (fun pic -> List.iter (Indexed.fill pic m.fg) (spots m p))) m
  | Cut -> { m with drag = Some p }
  | Dotted | Freehand ->
      let d' = paint d (fun pic -> List.iter (Indexed.stamp pic m.brush m.fg) (spots m p)) in
      { (record ~name:(tool_name m.tool) d' m) with drag = Some p; last = p }
  | Airbrush -> { (record ~name:"Airbrush" d m) with drag = Some p; last = p }
  | _ -> { (record ~name:(tool_name m.tool) d m) with drag = Some p; before = d }

let drag (m : model) (p : int * int) : model =
  match m.drag with
  | None -> m
  | Some a -> (
      let d = doc m in
      match m.tool with
      | Dotted -> { (amend (paint d (fun pic -> List.iter (Indexed.stamp pic m.brush m.fg) (spots m p))) m) with last = p }
      | Freehand ->
          (* the gaps between two frames' dots filled by a line *)
          let d' = paint d (fun pic -> List.iter2 (fun a z -> Indexed.line pic m.brush m.fg a z) (spots m m.last) (spots m p)) in
          { (amend d' m) with last = p }
      | Airbrush ->
          (* a few dots at random in a disc, at each frame the mouse is down *)
          let seed = ref m.seed in
          let draw () = seed := Lehmer.next !seed; Lehmer.to_unit !seed in
          let d' =
            paint d (fun pic ->
                for _ = 1 to 6 do
                  let a = 2. *. Float.pi *. draw () and r = 10. *. sqrt (draw ()) in
                  let q = (fst p + int_of_float (r *. cos a), snd p + int_of_float (r *. sin a)) in
                  List.iter (fun (x, y) -> Indexed.dot pic x y m.fg) (spots m q)
                done)
          in
          { (amend d' m) with seed = !seed; last = p }
      | Cut -> m
      | _ -> amend (shape m m.before a p) m)

let release (m : model) (p : int * int) : model =
  match (m.tool, m.drag) with
  | Cut, Some a ->
      (* the rectangle becomes the brush, and a freehand tool to paint it *)
      { m with brush = Indexed.Piece (Indexed.cut (pic_of (doc m)) a p, m.bg); tool = Freehand; drag = None }
  | _ -> { m with drag = None }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let kind = { File_menu.magic = "TinyDeluxePaint 1"; extension = ".dpaint" }

let reopened (r : Ilbm.t File_menu.result) (m : model) : model =
  match r with
  | File_menu.Nothing -> m
  | File_menu.New -> { initial with history = Undo.start { initial_doc with pixels = (Indexed.create pic_w pic_h 0).pixels }; file = m.file }
  | File_menu.Opened d -> { initial with history = Undo.start d; file = m.file }

let command (caps : File_menu.caps) (item : string) (m : model) : model =
  match item with
  | "Undo" -> { m with history = Undo.undo m.history }
  | "Redo" -> { m with history = Undo.redo m.history }
  | "Palette..." ->
      let r, g, b = (doc m).palette.(m.fg) in
      { m with editing = Some (r / 17, g / 17, b / 17) }
  | "Cycling" -> { m with cycling = not m.cycling }
  | "Clear" -> record ~name:"Clear" (paint (doc m) (fun p -> Indexed.fill_rect p m.bg (0, 0) (pic_w - 1, pic_h - 1))) m
  | "Export ILBM" ->
      Playground_platform.export caps "picture.iff" (Ilbm.encode (doc m));
      m
  | _ -> m

let editor_update (computer : computer) (m : model) ((r, g, b) : int * int * int) : model =
  let slide i v = int_of_float (Float.round (Gui.slider_in computer (channel_box i) ~from:0. ~to_:15. (float_of_int v))) in
  let colour = (slide 0 r, slide 1 g, slide 2 b) in
  if Gui.button_in computer ok_box "OK" then
    let d = doc m in
    let palette = Array.mapi (fun i c -> if i = m.fg then amiga colour else c) d.palette in
    { (record ~name:"Palette" { d with palette } m) with editing = None }
  else if Gui.button_in computer cancel_box "Cancel" then { m with editing = None }
  else { m with editing = Some colour }

let update (caps : File_menu.caps) (computer : computer) (m : model) : model =
  let current () = doc m in
  let m = { m with frames = (if m.cycling then m.frames + 1 else m.frames) } in
  if File_menu.busy m.file then
    let file, r = File_menu.dialog caps kind computer ~current m.file in
    reopened r { m with file; was_down = computer.mouse.mdown; was = Set_.elements computer.keyboard.keys }
  else
    let mouse = computer.mouse in
    let m = let file, r = File_menu.menu_in caps kind computer { menu_box with x = 240. } ~current m.file in reopened r { m with file } in
    let chosen = Gui.menu_in computer menu_box menu_picture 0 in
    let m = if chosen > 0 then command caps (List.nth menu_picture chosen) m else m in
    let m =
      match m.editing with
      | Some c -> editor_update computer m c
      | None ->
          if Gui.modal () then m
          else
            let p = dot_at (mouse.mx, mouse.my) in
            let pressed = mouse.mdown && not m.was_down in
            let rpressed = mouse.mrdown && not m.was_rdown in
            let hit box n = List.find_opt (fun i -> Widget.contains (box i) mouse.mx mouse.my) (List.init n Fun.id) in
            match (m.drag, pressed) with
            | Some _, _ when mouse.mdown -> drag m p
            | Some _, _ -> release m p
            | None, true when on_picture p -> press m p
            | None, true -> (
                match (hit tool_box (List.length tools), hit brush_box (List.length brushes), hit swatch_box 32) with
                | Some i, _, _ -> { m with tool = List.nth tools i }
                | _, Some i, _ -> { m with brush = List.nth brushes i }
                | _, _, Some i -> { m with fg = i }
                | _ -> if Widget.contains symmetry_box mouse.mx mouse.my then { m with symmetry = not m.symmetry } else m)
            | None, false -> ( match hit swatch_box 32 with Some i when rpressed -> { m with bg = i } | _ -> m)
    in
    let now = Set_.elements computer.keyboard.keys in
    let pressed key = List.mem key now && not (List.mem key m.was) in
    let m = if pressed "Tab" then { m with cycling = not m.cycling } else m in
    let m = if List.mem "Control" now && pressed "z" then command caps "Undo" m else m in
    { m with was = now; was_down = mouse.mdown; was_rdown = mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let colour (r, g, b) = rgb r g b

(* the palette as it shows now: turned by the frames the cycling ran *)
let shown_palette (m : model) : (int * int * int) array =
  let d = doc m in
  Cycling.palette_at d.palette d.ranges (float_of_int m.frames /. 60.)

(* The picture's colours, made again only when its dots or its shown
   palette changed: between two steps of the cycling (a dozen a second),
   frames draw the same image, which the backends keep converted *)
let drawn : (Bytes.t * (int * int * int) array * Rgba_image.t) option ref = ref None

let image (m : model) : Rgba_image.t =
  let d = doc m and pal = shown_palette m in
  match !drawn with
  | Some (px, p, img) when px == d.pixels && p = pal -> img
  | _ ->
      let img = Ilbm.to_rgba { d with palette = pal } in
      drawn := Some (d.pixels, pal, img);
      img

let icon (b : Widget.box) (t : tool) ~ink ~paper : shape list =
  let at dx dy s = s |> move (b.x +. dx) (b.y +. dy) in
  match t with
  | Dotted -> List.init 5 (fun i -> at (-12. +. (float_of_int i *. 6.)) (-8. +. (float_of_int (i mod 2) *. 10.)) (rectangle ink 3. 3.))
  | Freehand -> List.init 12 (fun i -> let x = -13. +. (float_of_int i *. 2.4) in at x (7. *. sin (x /. 4.)) (rectangle ink 3. 3.))
  | Line_tool -> [ at 0. 0. (rectangle ink 30. 2. |> rotate (-35.)) ]
  | Bucket -> [ at 0. 0. (rectangle ink 17. 17. |> rotate 30.); at 0. 0. (rectangle paper 13. 13. |> rotate 30.); at 11. (-8.) (rectangle ink 3. 8.) ]
  | Airbrush -> List.init 9 (fun i -> at (8. *. cos (float_of_int i)) (8. *. sin (float_of_int (i * 2)))  (rectangle ink 2. 2.))
  | Rect -> [ at 0. 0. (rectangle ink 26. 18.); at 0. 0. (rectangle paper 22. 14.) ]
  | Filled_rect -> [ at 0. 0. (rectangle ink 26. 18.) ]
  | Ellipse -> [ at 0. 0. (oval ink 28. 18.); at 0. 0. (oval paper 24. 14.) ]
  | Filled_ellipse -> [ at 0. 0. (oval ink 28. 18.) ]
  | Cut -> List.concat_map (fun i -> let d = -12. +. (float_of_int i *. 6.) in [ at d 9. (rectangle ink 3. 2.); at d (-9.) (rectangle ink 3. 2.) ]) [ 0; 1; 2; 3; 4 ] @ [ at 0. 0. (circle ink 4.) ]

let view (computer : computer) (m : model) : shape list =
  let th = Gui.theme () in
  let d = doc m in
  let pal = shown_palette m in
  let pw = float_of_int pic_w *. zoom and ph = float_of_int pic_h *. zoom in
  let picture = [ rectangle black (pw +. 4.) (ph +. 4.) |> move (pic_left +. (pw /. 2.)) (pic_top -. (ph /. 2.)); bitmap pw ph (image m) |> move (pic_left +. (pw /. 2.)) (pic_top -. (ph /. 2.)) ] in
  let marquee =
    match (m.tool, m.drag) with
    | Cut, Some (x0, y0) ->
        let x1, y1 = dot_at (computer.mouse.mx, computer.mouse.my) in
        let l = float_of_int (min x0 x1) and t = float_of_int (min y0 y1) and w = float_of_int (abs (x1 - x0) + 1) and h = float_of_int (abs (y1 - y0) + 1) in
        let cx = pic_left +. ((l +. (w /. 2.)) *. zoom) and cy = pic_top -. ((t +. (h /. 2.)) *. zoom) in
        [ rectangle white (w *. zoom) 2. |> move cx (cy +. (h *. zoom /. 2.)); rectangle white (w *. zoom) 2. |> move cx (cy -. (h *. zoom /. 2.));
          rectangle white 2. (h *. zoom) |> move (cx -. (w *. zoom /. 2.)) cy; rectangle white 2. (h *. zoom) |> move (cx +. (w *. zoom /. 2.)) cy ]
    | _ -> []
  in
  let toolbox =
    List.concat
      (List.mapi
         (fun i t ->
           let b = tool_box i in
           let ink, paper = if t = m.tool then (white, black) else (black, white) in
           [ rectangle paper b.w b.h |> move b.x b.y ] @ Gui.shapes (Widget.frame black 1. b) @ icon b t ~ink ~paper)
         tools)
  in
  let symmetry =
    let b = symmetry_box in
    let ink, paper = if m.symmetry then (white, black) else (black, white) in
    [ rectangle paper b.w b.h |> move b.x b.y ] @ Gui.shapes (Widget.frame black 1. b) @ [ words ink "Symmetry" |> move b.x b.y ]
  in
  let brush_icons =
    List.concat
      (List.mapi
         (fun i br ->
           let b = brush_box i in
           let on = (match (m.brush, br) with Indexed.Dots a, Indexed.Dots b -> a = b | _ -> false) in
           let size = match br with Indexed.Dots ds -> float_of_int (List.length ds) | _ -> 1. in
           [ rectangle (if on then black else white) b.w b.h |> move b.x b.y; circle (if on then white else black) (1. +. sqrt size) |> move b.x b.y ])
         brushes)
    @ (match m.brush with Indexed.Piece (p, _) -> [ words black (Printf.sprintf "brush: %d by %d" p.width p.height) |> move 245. 400. ] | _ -> [])
  in
  let swatches =
    List.concat
      (List.init 32 (fun i ->
           let b = swatch_box i in
           let frame = if i = m.fg then [ rectangle white (b.w +. 6.) (b.h +. 6.) |> move b.x b.y; rectangle black (b.w +. 2.) (b.h +. 2.) |> move b.x b.y ] else [ rectangle black (b.w +. 2.) (b.h +. 2.) |> move b.x b.y ] in
           frame @ [ rectangle (colour pal.(i)) b.w b.h |> move b.x b.y ] @ if i = m.bg then [ words (if i = 0 then white else black) "bg" |> move b.x b.y ] else []))
  in
  let editor =
    match m.editing with
    | Some (r, g, b) ->
        [ rectangle black 324. 360. |> move 330. 100.; rectangle th.face 320. 356. |> move 330. 100.; words black (Printf.sprintf "Colour %d" m.fg) |> move 330. 260. ]
        @ List.mapi (fun i (label, v) -> words black (Printf.sprintf "%s: %d" label v) |> move 330. ((channel_box i).y +. 24.)) [ ("Red", r); ("Green", g); ("Blue", b) ]
        @ [ rectangle (colour (amiga (r, g, b))) 120. 40. |> move 330. 20. ]
    | None -> []
  in
  let status =
    let p = dot_at (computer.mouse.mx, computer.mouse.my) in
    let r, g, b = d.palette.(m.fg) in
    Printf.sprintf "%s     colour %d (%d, %d, %d)     %s     %s     %s" (tool_name m.tool) m.fg (r / 17) (g / 17) (b / 17)
      (if m.cycling then "cycling (Tab)" else "Tab: cycle")
      (if on_picture p then Printf.sprintf "x %d  y %d  colour %d" (fst p) (snd p) (Indexed.get (pic_of d) (fst p) (snd p)) else "")
      (match Undo.undo_name m.history with Some n -> "Undo " ^ n | None -> "")
  in
  (* the Amiga's Workbench blue behind *)
  [ rectangle (rgb 0 85 170) 1000. 1000.; rectangle th.face 1000. 40. |> move 0. 475.; words black "Deluxe Paint" |> move (-380.) 475. ]
  @ picture @ marquee @ toolbox @ symmetry @ brush_icons @ swatches @ editor
  @ [ words white status |> move 0. (-440.) ]
  @ File_menu.view m.file @ Gui.draw ()

let app caps = game view (update caps) initial

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~rendering:{ antialiasing = true; smooth_images = false } (app (caps :> File_menu.caps)))
