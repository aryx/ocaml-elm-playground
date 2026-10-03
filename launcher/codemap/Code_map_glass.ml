(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_map_glass.mli *)

open Playground
open Code_map_base

(* claude: the magnifying glass (below): round, a reading glass (80
 * columns), or none -- none at first, o going from one to the next, one
 * setting for every map (tinybox's panel and its explorer) *)
type glass = Round | Reading | No_glass

let glass_shape = ref No_glass

(* claude: the menu's panel its own glass, round at first: there the map
 * is a tease, the whole program in miniature, the glass roaming over it *)
let panel_glass_shape = ref Round
let shape_of ~panel = if panel then panel_glass_shape else glass_shape
let cycle_glass ?(panel = false) () = let g = shape_of ~panel in g := match !g with Round -> Reading | Reading -> No_glass | No_glass -> Round
let glass_name ?(panel = false) () = match !(shape_of ~panel) with Round -> "round" | Reading -> "wide" | No_glass -> "none"

(* claude: a glass over the map, the part under the cursor closer. Not a
 * zoom of the map's picture (that would only enlarge its pixels, blurred,
 * the problem pixel_ratio solved): the part under the glass painted
 * again, by the same paint, with a camera [power] times closer, at the
 * window's resolution, anti-aliased. The power is chosen for the file
 * under the cursor, so that its lines come out about 16 units high, the
 * VGA font's own size: readable whatever the file's size. The glass's
 * shape is its picture's pixels made transparent (alpha 0, a soft edge):
 * the playground has no clipping. Painted again only when the cursor
 * moves.
 *
 * Two glasses. A round one, a glance at the code under the mouse. A
 * reading glass, the rectangular
 * kind laid over a page, where one reads: 80 columns of 8 units (640,
 * and a margin) by some 16 lines, whole lines of code rather than a
 * keyhole of them. o goes from one to the other, and to none (the
 * glass always enlarges, even over code big enough to read: none is the
 * way to be rid of it). *)

(* the file under the mouse (its rectangle and geometry), and the power
 * that makes its lines 16 units high; None when the mouse is off the map *)
let under_glass (computer : computer) (t : t) =
  let c = t.cam in
  let a = c.a in
  let mouse = computer.mouse in
  let mpx = px_of a mouse.mx and mpy = py_of a mouse.my in
  if not (on a mpx mpy) then None
  else
    let u = to_u c mpx and v = to_v c mpy in
    let file = match under t u v with Some i -> ( match t.geometry.(i) with Some g -> Some (t.placed.(i).rect, g) | None -> None) | None -> None in
    let power = match file with Some (_, g) -> float_of_int Vga_font.height /. (g.cell_h *. c.z) | None -> 4. in
    Some (u, v, file, power)

(* the part of the map [lc] sees, painted, the pixels of its picture out
 * of the glass's shape transparent: [alpha w h fx fy], w and h the
 * picture's size, fx fy a pixel's centre, gives its alpha *)
let glass_picture (t : t) (lc : camera) (alpha : float -> float -> float -> float -> int option) : Rgba_image.t * float =
  let q = Float.max 0.5 (Float.min 3. (Playground_platform.pixel_ratio ())) in
  let img =
    match t.lens with
    | Some (pc, img) when pc = lc && img.width = (at_ratio lc q).a.pw -> img
    | _ ->
        let img = t.style.paint ~aa:true t (at_ratio lc q) in
        let w = float_of_int img.width and h = float_of_int img.height in
        for y = 0 to img.height - 1 do
          for x = 0 to img.width - 1 do
            match alpha w h (float_of_int x +. 0.5) (float_of_int y +. 0.5) with
            | Some al -> Bigarray.Array1.unsafe_set img.rgba ((4 * ((y * img.width) + x)) + 3) al
            | None -> ()
          done
        done;
        t.lens <- Some (lc, img);
        img
  in
  (img, q)

(* one pixel of soft edge, [dist] from a circle's centre of radius [r] *)
let soft_edge (r : float) (dist : float) : int = if dist <= r -. 1. then 255 else if dist >= r then 0 else int_of_float ((r -. dist) *. 255.)

(* the round glass: centred on the point under the cursor *)
let lens_radius = 185.

let lens (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, _, power) ->
      let power = Float.max 2. (Float.min 10. power) in
      let d = int_of_float (2. *. lens_radius) in
      let lc = { cx = u; cy = v; z = t.cam.z *. power; a = { (t.cam.a) with pw = d; ph = d } } in
      let img, _ =
        glass_picture t lc (fun w _ fx fy ->
            let r = w /. 2. in
            let dx = fx -. r and dy = fy -. r in
            Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))))
      in
      let x = computer.mouse.mx and y = computer.mouse.my in
      let r = lens_radius in
      [
        (* the handle, down and to the right, as a magnifying glass is held *)
        rectangle (rgb 90 60 30) 22. 110. |> move 0. (-.(r +. 50.)) |> rotate 45. |> move x y;
        rectangle (rgb 150 150 160) 26. 18. |> move 0. (-.(r +. 4.)) |> rotate 45. |> move x y;
        (* the rim *)
        circle (rgb 40 40 50) (r +. 9.) |> move x y;
        circle (rgb 190 190 205) (r +. 6.) |> move x y;
        circle (rgb 12 10 28) (r +. 1.) |> move x y;
        bitmap (2. *. r) (2. *. r) img |> move x y;
        (* a glint on the glass *)
        oval white (r *. 0.5) (r *. 0.18) |> rotate 35. |> move (x -. (r *. 0.45)) (y +. (r *. 0.55)) |> fade 0.12;
        words (rgb 190 190 205) (Printf.sprintf "x%.0f" power) |> scale (12. /. words_font_size) |> move (x +. (r *. 0.62)) (y -. (r *. 0.85));
      ]

(* the reading glass. Over a file, it lines up with the start of the
 * column of lines under the mouse (a file is laid out in several), so it
 * shows whole lines from their first character, not the end of one column
 * and the start of the next; up and down, the line under the mouse is
 * drawn where the mouse is. It stays on the screen. *)
let reading_w = 660.
let reading_h = 272.
let reading_corner = 22.

(* a rectangle with rounded corners, from rectangles and circles (the
 * playground has no rounded rectangle) *)
let rounded (color : color) (w : number) (h : number) (r : number) : shape =
  group
    [
      rectangle color w (h -. (2. *. r));
      rectangle color (w -. (2. *. r)) h;
      circle color r |> move ((w /. 2.) -. r) ((h /. 2.) -. r);
      circle color r |> move (-.((w /. 2.) -. r)) ((h /. 2.) -. r);
      circle color r |> move ((w /. 2.) -. r) (-.((h /. 2.) -. r));
      circle color r |> move (-.((w /. 2.) -. r)) (-.((h /. 2.) -. r));
    ]

let reading_glass (computer : computer) (t : t) : shape list =
  match under_glass computer t with
  | None -> []
  | Some (u, v, file, power) ->
      let power = Float.max 1.5 (Float.min 10. power) in
      let c = t.cam in
      let a = c.a in
      let mouse = computer.mouse in
      let z = c.z *. power in
      let margin = 10. in
      let screen = computer.screen in
      let keep lo hi x = Float.max lo (Float.min hi x) in
      let on_screen_x x = keep (screen.left +. (reading_w /. 2.) +. 4.) (screen.right -. (reading_w /. 2.) -. 4.) x in
      let gy = keep (screen.bottom +. (reading_h /. 2.) +. 4.) (screen.top -. (reading_h /. 2.) -. 4.) mouse.my in
      (* across: the column's start at the glass's left margin, the glass
       * over the column on the map; else the point under the mouse where
       * the mouse is *)
      let gx, cx =
        match file with
        | Some (r, g) ->
            let start = r.x +. (Float.floor ((u -. r.x) /. g.colw) *. g.colw) in
            (on_screen_x (sx a (to_px c start) +. (reading_w /. 2.) -. margin), start +. (((reading_w /. 2.) -. margin) /. z))
        | None ->
            let gx = on_screen_x mouse.mx in
            (gx, u +. ((gx -. mouse.mx) /. z))
      in
      (* down: the line under the mouse drawn where the mouse is *)
      let lc = { cx; cy = v -. ((gy -. mouse.my) /. z); z; a = { a with pw = int_of_float reading_w; ph = int_of_float reading_h } } in
      let img, _ =
        glass_picture t lc (fun w h fx fy ->
            (* rounded corners: the distance to the corner's centre *)
            let r = reading_corner *. (w /. reading_w) in
            let dx = Float.max 0. (Float.max (r -. fx) (fx -. (w -. r))) and dy = Float.max 0. (Float.max (r -. fy) (fy -. (h -. r))) in
            if dx > 0. && dy > 0. then Some (soft_edge r (Float.sqrt ((dx *. dx) +. (dy *. dy)))) else None)
      in
      let w = reading_w and h = reading_h and r = reading_corner in
      [
        (* the handle, from the bottom right corner, as a reading glass is held *)
        rectangle (rgb 90 60 30) 24. 120. |> move 0. (-60.) |> rotate 45. |> move (gx +. (w /. 2.) -. 10.) (gy -. (h /. 2.) +. 10.);
        (* the rim *)
        rounded (rgb 40 40 50) (w +. 18.) (h +. 18.) (r +. 9.) |> move gx gy;
        rounded (rgb 190 190 205) (w +. 12.) (h +. 12.) (r +. 6.) |> move gx gy;
        rounded (rgb 12 10 28) (w +. 2.) (h +. 2.) (r +. 1.) |> move gx gy;
        bitmap w h img |> move gx gy;
        (* a glint on the glass *)
        oval white (w *. 0.3) (h *. 0.1) |> rotate 8. |> move (gx -. (w *. 0.28)) (gy +. (h *. 0.36)) |> fade 0.1;
        words (rgb 190 190 205) (Printf.sprintf "x%.1f" power) |> scale (12. /. words_font_size) |> move (gx +. (w /. 2.) -. 24.) (gy +. (h /. 2.) +. 1.);
      ]

(* claude: no glass while the map moves under it: its picture follows
 * the map's camera, so it would be painted anew every frame, as much
 * again as the map's own (on the web, half of a zoom's frame); it comes
 * back the frame the camera stops, as the map's sharp picture does *)
let glass ?(panel = false) (computer : computer) (t : t) : shape list =
  (* claude: none over code read on the map itself, where the names under
   * the mouse are lit (names_lit) *)
  let a = t.cam.a in
  let mpx = px_of a computer.mouse.mx and mpy = py_of a computer.mouse.my in
  (* claude: none in the atlas, whose hover previews and peeks show the code *)
  if t.moving || t.style.units || Code_map_moves.readable_at t (to_u t.cam mpx) (to_v t.cam mpy) then []
  else match !(shape_of ~panel) with Round -> lens computer t | Reading -> reading_glass computer t | No_glass -> []
