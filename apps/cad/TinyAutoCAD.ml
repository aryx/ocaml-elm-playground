(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyAutoCAD: drafting by conversation (AutoCAD, Autodesk, 1982; this
 * is Release 12 for DOS, 1992; plan_cad.md).
 *
 * AutoCAD took Sketchpad's idea (TinySketchpad) from a million-dollar
 * TX-2 to a $5,000 IBM PC, and made CAD an industry: by 1992 it drew
 * most of the world's buildings and machine parts. What it kept and
 * what it changed are both here:
 *
 * - **the command line**: not modes and buttons but a conversation.
 *   Type LINE; it asks "From point:", you answer "0,0" or click; it
 *   asks "To point:", you answer "@100,0" (relative) or "@50<90"
 *   (polar), then C to close. Precision is typed, not drawn: a
 *   draftsman's numbers go straight into the geometry
 *   (appkits/cad/Cad_session.mli, where the whole machine is, without a
 *   screen, and tested);
 * - **object snaps** (F3): the crosshair caught by the drawing's own
 *   points -- endpoint, midpoint, center, quadrant, intersection,
 *   perpendicular -- Sketchpad's aiming with a menu of what to aim at
 *   (Cad_snap); ORTHO (F8) keeps lines square, GRID (F7) and SNAP (F9)
 *   the draftsman's graph paper;
 * - **editing is computing crossings**: TRIM, EXTEND, OFFSET and
 *   FILLET all come down to where two curves cross (Cad_geom, Cad_edit);
 * - **layers**, the draftsman's transparent sheets: the outline white,
 *   the center lines red, the dimensions green, the bolts cyan -- LAYER
 *   OFF DIM and the notes are gone;
 * - **blocks**: Sketchpad's masters with names -- the bolt drawn once,
 *   inserted four times (BLOCK, INSERT);
 * - **dimensions** that measure what they point at (DIMLINEAR);
 * - **DXF**: the drawing as text any other program reads (DXFOUT,
 *   DXFIN; Dxf.mli) -- the file format that outlived its competitors.
 *
 * The screen is Release 12's: the drawing on black, the crosshair
 * across all of it, the status line at the top (the layer, the modes,
 * the coordinates), the screen menu on the right (a click types its
 * command), and the command line's three lines at the bottom. The
 * right button is Enter, as is Space; Escape cancels (Control-C in
 * 1992). The markers of the snaps are Release 14's AutoSnap (1997),
 * shown because they say which rule caught the cursor.
 *
 * What it uses: appkits/cad (Cad_geom, Cad_drawing, Cad_edit,
 * Cad_snap, Dxf, Cad_session), and Playground_platform's store and
 * export for the DXF files, with their capabilities. No Gui: the screen
 * menu is text, as it was.
 *
 * What it deliberately does not do: polylines, hatching, text and
 * linetypes (the center lines are solid red); the paper space of
 * layouts; UCS (the icon is only the world's); ARC by anything but
 * three points, CIRCLE by anything but its center; grips (Release 13);
 * AutoLISP, with which AutoCAD users programmed it (the one the
 * repository has, libs/languages/lisp, is Emacs's).
 *
 * Exercises: POLYLINE and its widths; TEXT with Hershey's strokes
 * (graphics/font, AutoCAD's own SHX fonts were strokes too); OFFSET
 * through a point; linetypes, a dash pattern in drawing units; grips,
 * the selection's own points dragged.
 *)
open Playground
module G = Cad_geom
module D = Cad_drawing
module S = Cad_session

(*****************************************************************************)
(* The drawing we start from *)
(*****************************************************************************)

(* a bracket, in millimetres: a plate with rounded corners, three holes,
   their center lines, four bolts (a block) and three dimensions *)
let bracket =
  let d = D.empty in
  let d = List.fold_left (fun d (l : D.layer) -> D.set_layer l d) d
      [ { name = "CENTER"; color = 1; on = true }; { name = "DIM"; color = 3; on = true }; { name = "BOLTS"; color = 4; on = true } ] in
  let on layer es d = List.fold_left (fun d e -> fst (D.add e { d with current = layer })) d es in
  let d =
    on "0"
      [
        D.Line ((10., 0.), (190., 0.)); D.Arc ((190., 10.), 10., 270., 0.);
        D.Line ((200., 10.), (200., 110.)); D.Arc ((190., 110.), 10., 0., 90.);
        D.Line ((190., 120.), (10., 120.)); D.Arc ((10., 110.), 10., 90., 180.);
        D.Line ((0., 110.), (0., 10.)); D.Arc ((10., 10.), 10., 180., 270.);
        D.Circle ((100., 60.), 20.); D.Circle ((50., 60.), 8.); D.Circle ((150., 60.), 8.);
      ]
      d
  in
  let d =
    on "CENTER"
      [ D.Line ((34., 60.), (166., 60.)); D.Line ((50., 46.), (50., 74.)); D.Line ((100., 34.), (100., 86.)); D.Line ((150., 46.), (150., 74.)) ]
      d
  in
  (* the bolt: a hexagon's head round its shank, drawn on layer 0 so that
     each insertion takes its own layer's colour *)
  let hexagon = List.init 6 (fun i -> D.Line (G.polar (0., 0.) 6. (float_of_int (i * 60)), G.polar (0., 0.) 6. (float_of_int ((i + 1) * 60)))) in
  let bolt = List.map (fun e -> { D.entity = e; layer = "0" }) (D.Circle ((0., 0.), 3.5) :: hexagon) in
  let d = { d with blocks = [ ("BOLT", ((0., 0.), bolt)) ] } in
  let d = on "BOLTS" (List.map (fun (x, y) -> D.Insert ("BOLT", (x, y), 1., 0.)) [ (20., 20.); (180., 20.); (20., 100.); (180., 100.) ]) d in
  let d = on "DIM" [ D.Dimension ((0., 0.), (200., 0.), (100., -18.)); D.Dimension ((200., 0.), (200., 120.), (224., 60.)); D.Dimension ((50., 60.), (150., 60.), (100., 138.)) ] d in
  { d with current = "0" }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  session : S.t;
  typing : string; (* the command line being typed *)
  ortho : bool;
  osnap : bool;
  grid : bool;
  snap : bool;
  started : bool;
  was : string list;
  was_down : bool;
  was_rdown : bool;
}

(* the screen: the drawing area, the screen menu at its right, the
   status line above, the command line below *)
let area_left = -500.
let area_right = 375.
let area_top = 470.
let area_bottom = -380.
let area_w = area_right -. area_left
let area_h = area_top -. area_bottom
let area_cx = (area_left +. area_right) /. 2.
let area_cy = (area_top +. area_bottom) /. 2.
let in_area (x, y) = x >= area_left && x <= area_right && y >= area_bottom && y <= area_top

let initial =
  {
    session = S.start bracket { S.center = (0., 0.); upp = 1.; w = area_w; h = area_h };
    typing = "";
    ortho = false;
    osnap = true;
    grid = false;
    snap = false;
    started = false;
    was = [];
    was_down = false;
    was_rdown = false;
  }

let to_drawing m (x, y) =
  let v = S.view m.session in
  (fst v.center +. ((x -. area_cx) *. v.upp), snd v.center +. ((y -. area_cy) *. v.upp))

let to_screen m (x, y) =
  let v = S.view m.session in
  (area_cx +. ((x -. fst v.center) /. v.upp), area_cy +. ((y -. snd v.center) /. v.upp))

(* the grid's spacing: 10 units, or 100 if they would be too dense *)
let grid_step m =
  let upp = (S.view m.session).upp in
  let rec up s = if s /. upp < 12. then up (s *. 10.) else s in
  up 10.

(*****************************************************************************)
(* The cursor *)
(*****************************************************************************)

(* where a click lands in the drawing: snapped to an object (F3), else
   to the grid (F9), and square with the last point (F8) *)
let cursor m (computer : computer) =
  let raw = to_drawing m (computer.mouse.mx, computer.mouse.my) in
  match S.wants m.session with
  | S.Objects | S.Idle | S.Other -> (raw, None)
  | S.Point -> (
      let upp = (S.view m.session).upp in
      let found = if m.osnap then Cad_snap.find (S.drawing m.session) ~aperture:(8. *. upp) ?from:(S.anchor m.session) raw else None in
      match found with
      | Some (kind, p) -> (p, Some kind)
      | None ->
          let p =
            if m.snap then
              let g = grid_step m in
              (Float.round (fst raw /. g) *. g, Float.round (snd raw /. g) *. g)
            else raw
          in
          let p =
            match (m.ortho, S.anchor m.session) with
            | true, Some (ax, ay) ->
                let x, y = p in
                if Float.abs (x -. ax) >= Float.abs (y -. ay) then (x, ay) else (ax, y)
            | _ -> p
          in
          (p, None))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let menu =
  [ "LINE"; "CIRCLE"; "ARC"; "ERASE"; "MOVE"; "COPY"; "OFFSET"; "TRIM"; "EXTEND"; "FILLET"; "DIMLINEAR"; "BLOCK";
    "INSERT"; "LAYER"; "ZOOM"; "PAN"; "DIST"; "U"; "REDO"; "DXFOUT"; "DXFIN" ]

let menu_top = 400.
let menu_step = 30.
let menu_x = (area_right +. 500.) /. 2.

let menu_at (x, y) =
  if x < area_right || y > menu_top +. (menu_step /. 2.) then None
  else List.nth_opt menu (int_of_float ((menu_top +. (menu_step /. 2.) -. y) /. menu_step))

type caps = < Cap.open_in ; Cap.open_out >

(* what the session asked of the files *)
let perform (caps : caps) m =
  match S.io m.session with
  | Some (S.Save (name, text)) ->
      Playground_platform.store caps name text;
      Playground_platform.export caps name text;
      m
  | Some (S.Load name) -> { m with session = S.loaded name (Playground_platform.fetch caps name) m.session }
  | None -> m

let submit caps m p i = perform caps { m with session = S.input ~cursor:p m.session i }

let update caps (computer : computer) m =
  let m = if m.started then m else { m with session = S.zoom_extents m.session; started = true } in
  let mouse = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.was) in
  let p, _ = cursor m computer in
  let m =
    if pressed "F3" then { m with osnap = not m.osnap }
    else if pressed "F7" then { m with grid = not m.grid }
    else if pressed "F8" then { m with ortho = not m.ortho }
    else if pressed "F9" then { m with snap = not m.snap }
    else m
  in
  let m =
    if pressed "Escape" || (List.mem "Control" now && pressed "c") then submit caps { m with typing = "" } p S.Cancel
    else if pressed "Backspace" && m.typing <> "" then { m with typing = String.sub m.typing 0 (String.length m.typing - 1) }
    else if pressed "Enter" then submit caps { m with typing = "" } p (S.Text m.typing)
    else if List.mem "Control" now then m
    else
      (* Space is Enter, as on AutoCAD's command line *)
      String.fold_left
        (fun m c -> if c = ' ' then submit caps { m with typing = "" } p (S.Text m.typing) else { m with typing = m.typing ^ String.make 1 c })
        m computer.keyboard.typed
  in
  let screen = (mouse.mx, mouse.my) in
  let press = mouse.mdown && not m.was_down in
  let m =
    if press && in_area screen then submit caps m p (S.Pick p)
    else if press then match menu_at screen with Some c -> perform caps { m with session = S.command c m.session; typing = "" } | None -> m
    else if mouse.mrdown && not m.was_rdown then submit caps { m with typing = "" } p (S.Text m.typing)
    else m
  in
  { m with was = now; was_down = mouse.mdown; was_rdown = mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* AutoCAD's colour numbers *)
let aci = function
  | 1 -> rgb 255 0 0
  | 2 -> rgb 255 255 0
  | 3 -> rgb 0 255 0
  | 4 -> rgb 0 255 255
  | 5 -> rgb 0 0 255
  | 6 -> rgb 255 0 255
  | 8 -> rgb 128 128 128
  | _ -> rgb 255 255 255

let segment color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  rectangle color (len +. (width /. 2.)) width
  |> rotate (Float.atan2 (by -. ay) (bx -. ax) *. 180. /. Float.pi)
  |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

(* a stroke, whole or dashed (a selection, a crossing window) *)
let stroke color ~dashed a b =
  if not dashed then [ segment color 1.5 a b ]
  else
    let len = G.dist a b in
    let n = int_of_float (len /. 10.) in
    List.init (n + 1) (fun i ->
        let t0 = float_of_int i *. 10. /. Float.max len 1. and t1 = Float.min 1. ((float_of_int i *. 10. +. 6.) /. Float.max len 1.) in
        let at t = G.add a (G.scale t (G.sub b a)) in
        segment color 1.5 (at t0) (at (Float.min 1. t1)))

(* a curve, in screen coordinates, as strokes *)
let curve m color ~dashed c =
  let upp = (S.view m.session).upp in
  let round center r a0 span =
    let px = r /. upp in
    let n = max 4 (int_of_float (Float.min 120. (Float.max 32. (px /. 3.)) *. span /. 360.)) in
    List.concat
      (List.init n (fun i ->
           let a = a0 +. (span *. float_of_int i /. float_of_int n) and b = a0 +. (span *. float_of_int (i + 1) /. float_of_int n) in
           stroke color ~dashed (to_screen m (G.polar center r a)) (to_screen m (G.polar center r b))))
  in
  match c with
  | G.Segment (a, b) -> stroke color ~dashed (to_screen m a) (to_screen m b)
  | G.Circle (center, r) -> round center r 0. 360.
  | G.Arc (center, r, a0, a1) ->
      let span = G.norm_angle (a1 -. a0) in
      round center r a0 (if span = 0. then 360. else span)

let text color size s (x, y) = words color s |> scale (size /. words_font_size) |> move x y

(* the same, starting at x, a character at a time: DOS's text mode was
   monospaced, and so the command line's columns line up *)
let text_left color size s (x, y) =
  let advance = size *. 0.62 in
  List.init (String.length s) (fun i -> (i, s.[i]))
  |> List.filter (fun (_, c) -> c <> ' ')
  |> List.map (fun (i, c) -> text color size (String.make 1 c) (x +. (advance *. (float_of_int i +. 0.5)), y))

(* an entity's strokes, in its layer's colour, and a dimension's number *)
let entity m ?(color_of = fun c -> c) ~dashed (e : D.ent) =
  let d = S.drawing m.session in
  let pieces = D.pieces d e in
  List.concat_map
    (fun (c, layer) -> if (D.layer d layer).on then curve m (color_of (aci (D.layer d layer).color)) ~dashed c else [])
    pieces
  @
  match e.entity with
  | D.Dimension (p1, p2, l) when (D.layer d e.layer).on ->
      let at, s = D.dimension_text p1 p2 l in
      let size = Float.max 8. (Float.min 22. (3.5 /. (S.view m.session).upp)) in
      [ text (aci (D.layer d e.layer).color) size s (to_screen m at) ]
  | _ -> []

(* the snap's marker, where it caught the cursor (AutoSnap's shapes) *)
let marker kind (x, y) =
  let c = rgb 255 255 0 and s = 7. in
  let lines ps = List.map (fun (a, b) -> segment c 2. a b) ps in
  match kind with
  | Cad_snap.Endpoint -> lines [ ((x -. s, y -. s), (x +. s, y -. s)); ((x +. s, y -. s), (x +. s, y +. s)); ((x +. s, y +. s), (x -. s, y +. s)); ((x -. s, y +. s), (x -. s, y -. s)) ]
  | Cad_snap.Midpoint -> lines [ ((x -. s, y -. s), (x +. s, y -. s)); ((x +. s, y -. s), (x, y +. s)); ((x, y +. s), (x -. s, y -. s)) ]
  | Cad_snap.Center -> List.init 12 (fun i ->
        let a = float_of_int i *. 30. and b = float_of_int (i + 1) *. 30. in
        segment c 2. (G.polar (x, y) s a) (G.polar (x, y) s b))
  | Cad_snap.Quadrant -> lines [ ((x, y -. s), (x +. s, y)); ((x +. s, y), (x, y +. s)); ((x, y +. s), (x -. s, y)); ((x -. s, y), (x, y -. s)) ]
  | Cad_snap.Intersection -> lines [ ((x -. s, y -. s), (x +. s, y +. s)); ((x -. s, y +. s), (x +. s, y -. s)) ]
  | Cad_snap.Perpendicular -> lines [ ((x -. s, y -. s), (x +. s, y -. s)); ((x -. s, y -. s), (x -. s, y +. s)); ((x -. s, y), (x, y)); ((x, y), (x, y -. s)) ]

(* the UCS icon: the world's x and y, and its W *)
let ucs_icon =
  let x0 = area_left +. 30. and y0 = area_bottom +. 30. and c = rgb 255 255 255 in
  [
    segment c 1.5 (x0, y0) (x0 +. 50., y0); segment c 1.5 (x0, y0) (x0, y0 +. 50.);
    segment c 1.5 (x0 +. 50., y0) (x0 +. 42., y0 +. 5.); segment c 1.5 (x0 +. 50., y0) (x0 +. 42., y0 -. 5.);
    segment c 1.5 (x0, y0 +. 50.) (x0 +. 5., y0 +. 42.); segment c 1.5 (x0, y0 +. 50.) (x0 -. 5., y0 +. 42.);
    text c 11. "X" (x0 +. 58., y0); text c 11. "Y" (x0, y0 +. 60.); text c 10. "W" (x0 +. 14., y0 +. 14.);
  ]

let view (computer : computer) m =
  let s = m.session in
  let d = S.drawing s in
  let p, snapped = cursor m computer in
  let screen = (computer.mouse.mx, computer.mouse.my) in
  let inside = in_area screen in
  let selected = S.selected s in
  let grid =
    if not m.grid then []
    else
      let g = grid_step m in
      let (x0, y0) = to_drawing m (area_left, area_bottom) and (x1, y1) = to_drawing m (area_right, area_top) in
      let i0 = int_of_float (Float.ceil (x0 /. g)) and i1 = int_of_float (Float.floor (x1 /. g)) in
      let j0 = int_of_float (Float.ceil (y0 /. g)) and j1 = int_of_float (Float.floor (y1 /. g)) in
      if (i1 - i0 + 1) * (j1 - j0 + 1) > 6000 then []
      else
        List.concat
          (List.init (max 0 (i1 - i0 + 1)) (fun i ->
               List.init (max 0 (j1 - j0 + 1)) (fun j ->
                   let x, y = to_screen m (float_of_int (i0 + i) *. g, float_of_int (j0 + j) *. g) in
                   square (rgb 90 90 90) 2. |> move x y)))
  in
  let ents = List.concat_map (fun (id, e) -> entity m ~dashed:(List.mem id selected) e) d.ents in
  let rubber = if inside then List.concat_map (fun e -> entity m ~color_of:(fun c -> c) ~dashed:false { D.entity = e; layer = d.current }) (S.preview s p) else [] in
  let window =
    match (inside, S.selection_box s p) with
    | true, Some (a, b, crossing) ->
        let (ax, ay), (bx, by) = (to_screen m a, to_screen m b) in
        List.concat_map (fun (u, v) -> stroke (rgb 255 255 255) ~dashed:crossing u v) [ ((ax, ay), (bx, ay)); ((bx, ay), (bx, by)); ((bx, by), (ax, by)); ((ax, by), (ax, ay)) ]
    | _ -> []
  in
  let cross =
    if not inside then []
    else
      let x, y = screen and c = rgb 255 255 255 in
      let box h = [ segment c 1. (x -. h, y -. h) (x +. h, y -. h); segment c 1. (x +. h, y -. h) (x +. h, y +. h); segment c 1. (x +. h, y +. h) (x -. h, y +. h); segment c 1. (x -. h, y +. h) (x -. h, y -. h) ] in
      [ segment c 1. (area_left, y) (area_right, y); segment c 1. (x, area_bottom) (x, area_top) ]
      @ (match S.wants s with S.Objects | S.Idle -> box 5. | S.Point when m.osnap -> box 8. | _ -> [])
      @ match snapped with Some kind -> marker kind (to_screen m p) @ [ text (rgb 255 255 0) 11. (Cad_snap.name kind) (fst (to_screen m p) +. 50., snd (to_screen m p) -. 18.) ] | None -> []
  in
  let grey = rgb 190 190 190 in
  let menu_items =
    List.mapi
      (fun i c ->
        let y = menu_top -. (float_of_int i *. menu_step) in
        let hover = menu_at screen = Some c in
        (if hover then [ rectangle grey (500. -. area_right -. 6.) (menu_step -. 4.) |> move menu_x y ] else [])
        @ [ text (if hover then black else white) 13. c (menu_x, y) ])
      menu
    |> List.concat
  in
  let layer = D.layer d d.current in
  let modes = String.concat "  " (List.filter_map (fun (on, n) -> if on then Some n else None) [ (m.ortho, "ORTHO"); (m.snap, "SNAP"); (m.grid, "GRID"); (m.osnap, "OSNAP") ]) in
  (* the prompt always on the last of the four lines *)
  let history = let l = List.init 3 (fun _ -> "") @ S.log s in List.filteri (fun i _ -> i >= List.length l - 3) l in
  let lines = history @ [ S.prompt s ^ " " ^ m.typing ^ "_" ] in
  let console = rgb 20 20 20 in
  [ rectangle black 1000. 1000. ] @ grid @ ents @ rubber @ window @ ucs_icon @ cross
  @ [
      (* the frames over what overflows the drawing area *)
      rectangle console (500. -. area_right) 1000. |> move menu_x 0.;
      rectangle console 1000. (500. -. area_top) |> move 0. ((area_top +. 500.) /. 2.);
      rectangle console 1000. (area_bottom +. 500.) |> move 0. ((area_bottom -. 500.) /. 2.);
      segment grey 1. (area_right, area_bottom) (area_right, area_top);
      segment grey 1. (-500., area_top) (500., area_top);
      segment grey 1. (-500., area_bottom) (500., area_bottom);
      text white 14. "AutoCAD" (menu_x, 450.);
      text white 14. "* * * *" (menu_x, 428.);
      rectangle (aci layer.color) 16. 16. |> move (-485.) 485.;
    ]
  @ text_left white 14. ("Layer " ^ layer.name) (-470., 485.)
  @ text_left white 14. modes (-200., 485.)
  @ text_left white 14. (Printf.sprintf "%.4f,%.4f" (fst p) (snd p)) (150., 485.)
  @ menu_items
  @ List.concat (List.mapi (fun i l -> text_left (rgb 220 220 220) 15. l (-490., -400. -. (float_of_int i *. 26.))) lines)

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> caps)))
