(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinySketchup: 3D modelling by drawing lines (SketchUp, Brad Schell
 * and Joe Esch, @Last Software, Boulder, 2000; Google SketchUp from
 * 2006, whose look this is, SketchUp 8, 2010).
 *
 * AutoCAD (TinyAutoCAD) is drafting: precise, and learnt over months.
 * SketchUp's inventors wanted 3D for architects who think with a pencil,
 * learnt in an afternoon -- "3D for everyone" -- and got there with
 * three ideas:
 *
 * - **the model is what you draw**: lines, and the faces that appear
 *   by themselves when lines close a flat loop; a line across a face
 *   cuts it in two; everything is sticky, vertices shared, so moving
 *   a ridge line up turns a box into a house (appkits/pushpull,
 *   Skp_model.mli);
 * - **push/pull** (P): a face dragged along its normal, its sides made
 *   as it goes -- the tool that turns a plan into a building, pulling
 *   a wall's window in to make a recess; a circle pulled into a
 *   cylinder whose seams are soft, not drawn;
 * - **the inference engine**: the mouse is on the screen, the point
 *   must be in space; SketchUp guesses from what is near -- an
 *   endpoint (green), a midpoint (cyan), an edge (red), a face (blue),
 *   and, from the last point, the red, green and blue axes -- and says
 *   what it guessed (Skp_infer.mli). No grid, no snap modes, no typing
 *   of coordinates: and yet exact, because a length can always be typed
 *   into the Measurements box (bottom right) after a click: click, move
 *   the mouse in the direction, type 3, Enter.
 *
 * The faces are drawn by the painter's algorithm, but ordered by a BSP
 * tree built from the model (Bsp.mli): the 2D playground has no
 * z-buffer, and a depth per face gets the window's recess wrong. Front
 * faces are white, back faces blue-grey, as in SketchUp, where seeing
 * blue means a face is inside out.
 *
 * Tools, with SketchUp's keys: Select (Space; Shift adds, a double
 * click takes a face with its edges), Line (L), Rectangle (R), Circle
 * (C, 24 sides), Push/Pull (P), Move (M, the selection, or what is under
 * the mouse), Eraser (E, an edge and its faces), Orbit (O), Pan (H).
 * Click, move, click; Escape gives up; a number typed and Enter sets
 * the length, the distance, the radius, or a rectangle's "w,h". The
 * right button dragged orbits, with Shift it pans, the wheel zooms;
 * Shift-Z shows everything; Delete erases the selection; Control-Z
 * undoes, Control-Y redoes. The flag scene=empty starts from nothing.
 *
 * What it uses: appkits/pushpull (Skp_model, Skp_infer, Skp_view,
 * Bsp), Camera and Vec3 (graphics/3d/geometry), appkits/document
 * (Undo). No Gui: the toolbar's buttons are drawn and hit by hand.
 *
 * What it deliberately does not do: groups and components (a
 * component is Sketchpad's master again: TinySketchpad); edges
 * crossing edges (SketchUp splits both where they meet; here only an
 * endpoint on an edge splits it); faces not flat after a move
 * (SketchUp folds them into triangles); materials and the Paint
 * Bucket; the tape measure and guides; Follow Me (a face swept along a
 * path); Rotate and Scale; shadows; the .skp file (binary and
 * undocumented until its SDK).
 *
 * Exercises: lock an inference with Shift, as SketchUp does (the
 * rubber band keeps its axis wherever the mouse goes); edges crossing
 * edges, both split at the crossing; Follow Me, push/pull along a path
 * of edges instead of a normal; export the model as an OBJ file, a
 * vertex line per vertex and a face line per face (holes made into
 * keyholes, as the drawing does); profiles, the silhouette edges drawn
 * thicker, SketchUp's default style.
 *)
open Playground
module M = Skp_model
module I = Skp_infer
module V = Skp_view

(*****************************************************************************)
(* The house we start from *)
(*****************************************************************************)

(* Skp_model.mli's worked example, made with the tools' own operations:
   a rectangle on the ground, pulled up; a line across its top, lifted
   into a ridge; a window in the front wall, pushed in *)
let house =
  let rect x0 x1 z0 z1 y = [ (x0, y, z0); (x1, y, z0); (x1, y, z1); (x0, y, z1) ] in
  let m = M.add_polygon M.empty [ (-3., -2., 0.); (3., -2., 0.); (3., 2., 0.); (-3., 2., 0.) ] in
  let m = M.push_pull m (List.hd m.faces).id (-3.) in
  let m = M.add_edge m (0., -2., 3.) (0., 2., 3.) in
  let ridge = List.filter_map (fun (v, (x, _, z)) -> if x = 0. && z = 3. then Some v else None) m.verts in
  let m = M.move m ridge (0., 0., 2.) in
  let window m pts =
    let m = M.add_polygon m pts in
    let pane = List.find (fun (f : M.face) -> f.holes = [] && M.inside m f (Vec3.centroid pts) && List.length f.outer = 4 && Float.abs (Vec3.dot (M.normal m f) (0., -1., 0.)) > 0.99) m.faces in
    M.push_pull m pane.id (-0.2)
  in
  window (window m (rect 0.8 2.2 1. 2. (-2.))) (rect (-2.2) (-0.8) 1. 2. (-2.))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool = Select | Line | Rectangle | Circle | Push_pull | Move | Eraser | Orbit | Pan

(* a click made, the next awaited *)
type action =
  | Idle
  | Line_from of Vec3.t
  | Corner of Vec3.t * Vec3.t (* a rectangle's first corner, its plane's normal *)
  | Center of Vec3.t * Vec3.t (* a circle's center, its plane's normal *)
  | Pushing of int * Vec3.t * Vec3.t (* the face, where it was clicked, its normal *)
  | Moving of int list * Vec3.t (* the vertices, where they were picked up *)

type sel = Edge of (int * int) | Face of int

type model = {
  history : M.t Undo.t;
  tool : tool;
  action : action;
  view : V.t;
  selection : sel list;
  typed : string; (* the Measurements box *)
  drag : (float * float * V.t * bool) option; (* the mouse and the view when it started, panning *)
  started : bool;
  was : string list;
  was_down : bool;
  was_rdown : bool;
}

let initial =
  {
    history = Undo.start house;
    tool = Push_pull;
    action = Idle;
    view = V.start;
    selection = [];
    typed = "";
    drag = None;
    started = false;
    was = [];
    was_down = false;
    was_rdown = false;
  }

(* the screen: the title and the toolbar above, the status bar below *)
let area : V.area = { cx = 0.; cy = -15.; w = 1000.; h = 880. }
let in_view (_, y) = y > -455. && y < 425.

let tools = [ (Select, " ", "Select"); (Line, "l", "Line"); (Rectangle, "r", "Rectangle"); (Circle, "c", "Circle"); (Push_pull, "p", "Push/Pull"); (Move, "m", "Move"); (Eraser, "e", "Eraser"); (Orbit, "o", "Orbit"); (Pan, "h", "Pan") ]
let button_x i = -470. +. (float_of_int i *. 50.)
let toolbar_y = 450.

(* the tool's button under the mouse, and the Zoom Extents button's *)
let button_at (x, y) =
  if Float.abs (y -. toolbar_y) > 20. then None
  else
    let i = int_of_float (Float.round ((x +. 470.) /. 50.)) in
    if Float.abs (x -. button_x i) > 20. || i < 0 || i > List.length tools then None else Some i

(*****************************************************************************)
(* The point under the mouse *)
(*****************************************************************************)

let now m = Undo.now m.history
let anchor m = match m.action with Line_from a | Corner (a, _) | Center (a, _) | Moving (_, a) -> Some a | Pushing _ | Idle -> None
let mouse (computer : computer) = (computer.mouse.mx, computer.mouse.my)
let ray m computer = V.ray m.view area (mouse computer)
let infer m computer = I.find ~project:(V.project m.view area) ~ray:(ray m computer) ?from:(anchor m) (now m) (mouse computer)

(* two directions in a plane: x and y on a level one, else the level
   one across it and the one up it *)
let plane_axes n =
  let _, _, nz = n in
  if Float.abs nz > 0.999 then ((1., 0., 0.), (0., 1., 0.))
  else
    let u = Vec3.normalize (Vec3.cross (0., 0., 1.) n) in
    (u, Vec3.cross n u)

(* the point in the plane of a rectangle or a circle under way: the
   inferred one if it is in it, else where the ray meets it *)
let in_plane m computer (found : I.found) a n =
  if found.kind <> I.Nowhere && Float.abs (Vec3.dot (Vec3.sub found.point a) n) < M.eps then found.point
  else match I.on_plane (ray m computer) a n with Some p -> p | None -> found.point

let rectangle_corners a n (w, h) =
  let u, v = plane_axes n in
  [ a; Vec3.add a (Vec3.scale w u); Vec3.add a (Vec3.add (Vec3.scale w u) (Vec3.scale h v)); Vec3.add a (Vec3.scale h v) ]

let rectangle_size a n q = let u, v = plane_axes n in (Vec3.dot (Vec3.sub q a) u, Vec3.dot (Vec3.sub q a) v)

let circle_points c n r =
  let u, v = plane_axes n in
  List.init 24 (fun i ->
      let t = float_of_int i *. Float.pi /. 12. in
      Vec3.add c (Vec3.add (Vec3.scale (r *. cos t) u) (Vec3.scale (r *. sin t) v)))

(* the plane a click starts a rectangle or a circle on: the face's
   under the mouse, else level *)
let plane_at m (found : I.found) =
  match Option.bind found.face (M.face (now m)) with
  | Some f when Float.abs (Vec3.dot (Vec3.sub found.point (M.pos (now m) (List.hd f.outer))) (M.normal (now m) f)) < M.eps -> M.normal (now m) f
  | _ -> (0., 0., 1.)

(* how far a push/pull has gone: to the height of a point inferred
   off the face, else along the normal to the mouse's nearest *)
let push_distance m computer (found : I.found) start n =
  match found.kind with
  | (I.Endpoint | I.Midpoint | I.On_edge) when Float.abs (Vec3.dot (Vec3.sub found.point start) n) > M.eps -> Vec3.dot (Vec3.sub found.point start) n
  | _ -> I.along (ray m computer) start n

(* the model as it would be if the click came now, and the Measurements
   box's label and value *)
let preview m computer =
  let found = infer m computer in
  match m.action with
  | Idle ->
      let label = match m.tool with Line -> "Length" | Rectangle -> "Dimensions" | Circle -> "Radius" | Push_pull | Move -> "Distance" | _ -> "Measurements" in
      (now m, label, "")
  | Line_from a -> (now m, "Length", Printf.sprintf "%.2fm" (Vec3.length (Vec3.sub found.point a)))
  | Corner (a, n) ->
      let w, h = rectangle_size a n (in_plane m computer found a n) in
      (now m, "Dimensions", Printf.sprintf "%.2fm, %.2fm" (Float.abs w) (Float.abs h))
  | Center (c, n) -> (now m, "Radius", Printf.sprintf "%.2fm" (Vec3.length (Vec3.sub (in_plane m computer found c n) c)))
  | Pushing (f, start, n) ->
      let d = push_distance m computer found start n in
      (M.push_pull (now m) f d, "Distance", Printf.sprintf "%.2fm" (Float.abs d))
  | Moving (vs, start) ->
      let delta = Vec3.sub found.point start in
      (M.move (now m) vs delta, "Distance", Printf.sprintf "%.2fm" (Vec3.length delta))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let record name model m = { m with history = Undo.record ~name model m.history; action = Idle; typed = "" }

let selected_vertices m =
  M.vertices_of ~edges:(List.filter_map (function Edge (a, b) -> Some (a, b) | Face _ -> None) m.selection) ~faces:(List.filter_map (function Face f -> Some f | Edge _ -> None) m.selection) (now m)

(* a click in the view, with the point it means *)
let click m computer (found : I.found) =
  let shift = computer.keyboard.kshift in
  let p = found.point in
  match (m.tool, m.action) with
  | Select, _ ->
      let under =
        match (found.edge, found.face) with
        | Some e, _ -> [ Edge e ]
        | None, Some f when computer.mouse.mdouble -> (
            Face f :: match M.face (now m) f with Some face -> List.map (fun e -> Edge e) (List.concat_map M.sides (M.loops face)) | None -> [])
        | None, Some f -> [ Face f ]
        | None, None -> []
      in
      if shift then { m with selection = List.filter (fun s -> not (List.mem s under)) m.selection @ List.filter (fun s -> not (List.mem s m.selection)) under }
      else { m with selection = under }
  | Line, Idle -> { m with action = Line_from p }
  | Line, Line_from a ->
      if Vec3.length (Vec3.sub p a) < M.eps then m
      else
        let before = List.length (now m).faces in
        let m' = record "Line" (M.add_edge (now m) a p) m in
        (* SketchUp stops at a face closed; else the next line goes on *)
        if List.length (now m').faces > before then m' else { m' with action = Line_from p }
  | Rectangle, Idle -> { m with action = Corner (p, plane_at m found) }
  | Rectangle, Corner (a, n) ->
      let w, h = rectangle_size a n (in_plane m computer found a n) in
      if Float.abs w < M.eps || Float.abs h < M.eps then m else record "Rectangle" (M.add_polygon (now m) (rectangle_corners a n (w, h))) m
  | Circle, Idle -> { m with action = Center (p, plane_at m found) }
  | Circle, Center (c, n) ->
      let r = Vec3.length (Vec3.sub (in_plane m computer found c n) c) in
      if r < M.eps then m else record "Circle" (M.add_polygon ~curve:true (now m) (circle_points c n r)) m
  | Push_pull, Idle -> (
      match Option.bind found.face (M.face (now m)) with Some f -> { m with action = Pushing (f.id, p, M.normal (now m) f) } | None -> m)
  | Push_pull, Pushing _ | Move, Moving _ -> let model, _, _ = preview m computer in record (if m.tool = Move then "Move" else "Push/Pull") model m
  | Move, Idle ->
      let vs =
        if m.selection <> [] then selected_vertices m
        else
          match (found.vertex, found.edge, found.face) with
          | Some v, _, _ -> [ v ]
          | None, Some (a, b), _ -> [ a; b ]
          | None, None, Some f -> M.vertices_of ~edges:[] ~faces:[ f ] (now m)
          | None, None, None -> []
      in
      if vs = [] then m else { m with action = Moving (vs, p) }
  | Eraser, _ -> ( match found.edge with Some (a, b) -> { (record "Erase" (M.erase_edge (now m) a b) m) with selection = [] } | None -> m)
  | (Orbit | Pan), _ -> { m with drag = Some (computer.mouse.mx, computer.mouse.my, m.view, m.tool = Pan) }
  | _ -> m

(* the number typed, and Enter: the length the mouse's direction gets *)
let enter m computer =
  let found = infer m computer in
  let number s = float_of_string_opt (String.trim s) in
  let sign x = if x < 0. then -1. else 1. in
  let m' =
    match (m.action, String.split_on_char ',' (String.map (fun c -> if c = ';' then ',' else c) m.typed)) with
    | Line_from a, [ l ] -> (
        match number l with
        | Some l ->
            let dir = Vec3.normalize (Vec3.sub found.point a) in
            if Vec3.length dir = 0. then None else Some (click m computer { found with point = Vec3.add a (Vec3.scale l dir) })
        | None -> None)
    | Corner (a, n), [ w; h ] -> (
        match (number w, number h) with
        | Some w, Some h ->
            let w0, h0 = rectangle_size a n (in_plane m computer found a n) in
            Some (record "Rectangle" (M.add_polygon (now m) (rectangle_corners a n (sign w0 *. w, sign h0 *. h))) m)
        | _ -> None)
    | Center (c, n), [ r ] -> ( match number r with Some r when r > 0. -> Some (record "Circle" (M.add_polygon ~curve:true (now m) (circle_points c n r)) m) | _ -> None)
    | Pushing (f, start, n), [ d ] -> (
        match number d with Some d -> Some (record "Push/Pull" (M.push_pull (now m) f (sign (push_distance m computer found start n) *. d)) m) | None -> None)
    | Moving (vs, start), [ d ] -> (
        match number d with
        | Some d ->
            let dir = Vec3.normalize (Vec3.sub found.point start) in
            if Vec3.length dir = 0. then None else Some (record "Move" (M.move (now m) vs (Vec3.scale d dir)) m)
        | None -> None)
    | _ -> None
  in
  match m' with Some m' -> { m' with typed = "" } | None -> { m with typed = "" }

let erase_selection m =
  if m.selection = [] then m
  else
    let model =
      List.fold_left (fun model s -> match s with Edge (a, b) -> M.erase_edge model a b | Face f -> M.erase_face model f) (now m) m.selection
    in
    { (record "Erase" model m) with selection = [] }

let choose tool m = { m with tool; action = Idle; typed = "" }

let update (computer : computer) m =
  let m = if m.started then m else { m with started = true; history = (if List.assoc_opt "scene" computer.flags = Some "empty" then Undo.start M.empty else m.history) } in
  let mouse = computer.mouse in
  let now_keys = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now_keys && not (List.mem k m.was) in
  let control = List.mem "Control" now_keys and shift = computer.keyboard.kshift in
  let m =
    if control && (pressed "z" || pressed "Z") then { m with history = Undo.undo m.history; action = Idle; selection = [] }
    else if control && (pressed "y" || pressed "Y") then { m with history = Undo.redo m.history; action = Idle; selection = [] }
    else if control then m
    else if pressed "Escape" then { m with action = Idle; typed = ""; selection = (if m.action = Idle then [] else m.selection) }
    else if pressed "Enter" then enter m computer
    else if pressed "Backspace" && m.typed <> "" then { m with typed = String.sub m.typed 0 (String.length m.typed - 1) }
    else if pressed "Delete" then erase_selection m
    else if shift && (pressed "z" || pressed "Z") then { m with view = V.extents m.view (List.map snd (now m).verts) }
    else
      match List.find_opt (fun (_, key, _) -> pressed key || pressed (String.uppercase_ascii key)) tools with
      | Some (t, _, _) -> choose t m
      | None ->
          let digits = String.concat "" (List.filter_map (fun c -> if (c >= '0' && c <= '9') || c = '.' || c = ',' || c = ';' || c = '-' then Some (String.make 1 c) else None) (List.of_seq (String.to_seq computer.keyboard.typed))) in
          { m with typed = m.typed ^ digits }
  in
  let at = (mouse.mx, mouse.my) in
  let press = mouse.mdown && not m.was_down in
  let m = if mouse.mwheel <> 0. && in_view at then { m with view = V.zoom m.view mouse.mwheel } else m in
  (* the right button orbits, whatever the tool; with Shift, it pans *)
  let m = if mouse.mrdown && not m.was_rdown && in_view at then { m with drag = Some (mouse.mx, mouse.my, m.view, shift) } else m in
  let m =
    match m.drag with
    | Some (x0, y0, v0, panning) when mouse.mdown || mouse.mrdown ->
        let dx = mouse.mx -. x0 and dy = mouse.my -. y0 in
        { m with view = (if panning then V.pan v0 area dx dy else V.orbit v0 dx dy) }
    | Some _ -> { m with drag = None }
    | None ->
        if press && in_view at then click m computer (infer m computer)
        else if press then
          match button_at at with
          | Some i when i < List.length tools -> let t, _, _ = List.nth tools i in choose t m
          | Some _ -> { m with view = V.extents m.view (List.map snd (now m).verts) }
          | None -> m
        else m
  in
  { m with was = now_keys; was_down = mouse.mdown; was_rdown = mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let red = rgb 220 0 0
let green = rgb 0 150 0
let blue = rgb 0 0 230
let axis_color i = match i with 0 -> red | 1 -> green | _ -> blue

let segment color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  rectangle color (len +. (width /. 2.)) width
  |> rotate (Float.atan2 (by -. ay) (bx -. ax) *. 180. /. Float.pi)
  |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

let dashed color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  let n = int_of_float (Float.min 400. (len /. 8.)) in
  let at t = (ax +. (t *. (bx -. ax)), ay +. (t *. (by -. ay))) in
  List.init (n / 2) (fun i -> segment color width (at (float_of_int (2 * i) /. float_of_int n)) (at (float_of_int ((2 * i) + 1) /. float_of_int n)))

let line3 m color width ?(dash = false) p q =
  match V.segment m.view area p q with Some (a, b) -> if dash then dashed color width a b else [ segment color width a b ] | None -> []

let text color size s (x, y) = words color s |> scale (size /. words_font_size) |> move x y

(* left-aligned, a character at a time (words are centered) *)
let text_left color size s (x, y) =
  let advance = size *. 0.55 in
  List.init (String.length s) (fun i -> (i, s.[i]))
  |> List.filter (fun (_, c) -> c <> ' ')
  |> List.map (fun (i, c) -> text color size (String.make 1 c) (x +. (advance *. (float_of_int i +. 0.5)), y))

(* a colour a fraction t of the way from one to the other *)
let mix (r1, g1, b1) (r2, g2, b2) t = rgb (int_of_float (r1 +. ((r2 -. r1) *. t))) (int_of_float (g1 +. ((g2 -. g1) *. t))) (int_of_float (b1 +. ((b2 -. b1) *. t)))

(* SketchUp's default material: white in front, blue-grey behind, both
   shaded by a light from the front left, above *)
let light = Vec3.normalize (-0.4, -0.7, 0.8)

let face_color model eye (f : M.face) ~selected ~hovered =
  let n = M.normal model f in
  let front = Vec3.dot n (Vec3.sub eye (M.pos model (List.hd f.outer))) > 0. in
  let n = if front then n else Vec3.scale (-1.) n in
  let k = 0.7 +. (0.3 *. Vec3.dot n light) in
  let r, g, b = if front then (255., 255., 255.) else (158., 170., 196.) in
  let c = (r *. k, g *. k, b *. k) in
  mix c (70., 100., 255.) (if selected then 0.45 else if hovered then 0.2 else 0.)

(* a face as one outline for the playground's polygon: each hole
   joined to the outer loop by a cut there and back (a keyhole), the
   hole going round the other way, so that it is not filled; each
   corner with whether the side leaving it is an edge to draw *)
let keyhole (model : M.t) (f : M.face) =
  let drawn x y = List.exists (fun (e : M.edge) -> ((e.a = x && e.b = y) || (e.a = y && e.b = x)) && not e.soft) model.edges in
  let loop l = List.map (fun (x, y) -> (x, drawn x y)) (M.sides l) in
  let bridge outer hole =
    let h = loop hole in
    let h0 = fst (List.hd h) in
    let dist (v, _) = Vec3.length (Vec3.sub (M.pos model v) (M.pos model h0)) in
    let k = fst (List.fold_left (fun (bi, bd) (i, c) -> if dist c < bd then (i, dist c) else (bi, bd)) (0, infinity) (List.mapi (fun i c -> (i, c)) outer)) in
    let ok, flag = List.nth outer k in
    List.filteri (fun i _ -> i < k) outer @ [ (ok, false) ] @ h @ [ (h0, false); (ok, flag) ] @ List.filteri (fun i _ -> i > k) outer
  in
  List.map (fun (v, d) -> (M.pos model v, d)) (List.fold_left bridge (loop f.outer) f.holes)

(* the faces, back to front, each outlined by its edges *)
let faces m (model : M.t) ~hovered =
  let eye = V.eye m.view in
  let polys = List.map (fun (f : M.face) -> { Bsp.corners = keyhole model f; data = f }) model.faces in
  List.concat_map
    (fun (p : M.face Bsp.poly) ->
      let f = p.data in
      match V.polygon m.view area p.corners with
      | [] -> []
      | corners ->
          let color = face_color model eye f ~selected:(List.mem (Face f.id) m.selection) ~hovered:(hovered = Some f.id) in
          let pts = List.map fst corners in
          let sides = List.combine corners (List.tl pts @ [ List.hd pts ]) in
          (* the sides that are no edge (where the tree or the eye cut
             the face) get a hairline of the face's colour: two pieces
             of a face, each antialiased, would leave a seam between *)
          let seams = List.filter_map (fun ((a, drawn), b) -> if drawn then None else Some (segment color 1. a b)) sides in
          let edges = List.filter_map (fun ((a, drawn), b) -> if drawn then Some (segment black 1.6 a b) else None) sides in
          (polygon color pts :: seams) @ edges)
    (Bsp.back_to_front ~eye (Bsp.build polys))

(* the sky, the ground, and the axes on it *)
let world m =
  let _, _, ez = V.eye m.view in
  let far = 2000. in
  let ground = V.polygon m.view area (List.map (fun (x, y) -> ((x, y, 0.), false)) [ (-.far, -.far); (far, -.far); (far, far); (-.far, far) ]) in
  [ rectangle (rgb 222 234 244) 1000. 1000. ]
  @ (if ez > 0. && ground <> [] then [ polygon (rgb 205 204 190) (List.map fst ground) ] else [])
  @ List.concat
      (List.init 3 (fun i ->
           let axis = match i with 0 -> (1., 0., 0.) | 1 -> (0., 1., 0.) | _ -> (0., 0., 1.) in
           line3 m (axis_color i) 1.2 (0., 0., 0.) (Vec3.scale 300. axis) @ line3 m (axis_color i) 1.2 ~dash:true (0., 0., 0.) (Vec3.scale (-300.) axis)))

(* the inference's mark and word *)
let marker (found : I.found) (x, y) =
  let dot c = [ circle (rgb 40 40 40) 7. |> move x y; circle c 5. |> move x y ] in
  let mark =
    match found.kind with
    | I.Endpoint -> dot (rgb 0 200 0)
    | I.Midpoint -> dot (rgb 0 210 230)
    | I.On_edge -> [ square (rgb 40 40 40) 11. |> move x y; square red 8. |> move x y ]
    | I.On_face -> [ square (rgb 40 40 40) 11. |> rotate 45. |> move x y; square blue 8. |> rotate 45. |> move x y ]
    | I.On_axis _ | I.Nowhere -> []
  in
  let word = I.name found.kind in
  mark
  @
  if word = "" then []
  else
    let w = (float_of_int (String.length word) *. 7.5) +. 12. in
    [ rectangle (rgb 255 255 225) w 20. |> move (x +. 18. +. (w /. 2.)) (y -. 24.); text black 13. word (x +. 18. +. (w /. 2.), y -. 24.) ]

(* the tools' pictures, drawn in a 40 x 40 button *)
let icon i (x, y) =
  let at shapes = List.map (move x y) shapes in
  let dark = rgb 50 50 50 in
  let outline pts = List.map (fun (a, b) -> segment dark 2. a b) (List.combine pts (List.tl pts @ [ List.hd pts ])) in
  at
    (match i with
    | 0 -> [ polygon black [ (-6., 12.); (-6., -8.); (-1., -3.); (3., -11.); (6., -9.); (2., -1.); (8., -1.) ] ]
    | 1 -> [ segment (rgb 230 170 40) 5. (-9., -9.) (9., 9.); segment dark 2. (-12., -12.) (-8., -8.) ]
    | 2 -> outline [ (-11., -7.); (11., -7.); (11., 7.); (-11., 7.) ]
    | 3 -> outline (List.init 16 (fun k -> let t = float_of_int k *. Float.pi /. 8. in (10. *. cos t, 10. *. sin t)))
    | 4 -> [ rectangle (rgb 200 200 215) 16. 10. |> move 0. (-6.); rectangle red 3. 10. |> move 0. 4.; polygon red [ (-5., 8.); (5., 8.); (0., 14.) ] ]
    | 5 ->
        let arrow r = [ rectangle red 20. 3. |> rotate r; polygon red [ (10., -4.); (10., 4.); (14., 0.) ] |> rotate r; polygon red [ (-10., -4.); (-10., 4.); (-14., 0.) ] |> rotate r ] in
        arrow 0. @ arrow 90.
    | 6 -> [ rectangle (rgb 240 150 170) 20. 10. |> rotate 30.; rectangle (rgb 90 110 200) 7. 10. |> move 7. 4. |> rotate 30. ]
    | 7 -> outline (List.init 12 (fun k -> let t = float_of_int k *. Float.pi /. 6. in (11. *. cos t, 6. *. sin t))) @ [ circle red 3. |> move 11. 0.; circle green 3. |> move (-11.) 0. ]
    | 8 -> [ oval (rgb 240 200 160) 14. 16. |> move 0. (-3.) ] @ List.init 4 (fun k -> rectangle (rgb 240 200 160) 3.5 10. |> move (-5.25 +. (3.5 *. float_of_int k)) 8.)
    | _ -> outline (List.init 12 (fun k -> let t = float_of_int k *. Float.pi /. 6. in (-2. +. (7. *. cos t), 3. +. (7. *. sin t)))) @ [ segment dark 3. (3., -2.) (10., -10.) ])

let hint m =
  match (m.tool, m.action) with
  | Select, _ -> "Select objects. Shift to extend select. Double-click a face for its edges too."
  | Line, Idle -> "Select start point."
  | Line, _ -> "Select end point or enter value."
  | Rectangle, Idle -> "Select first corner."
  | Rectangle, _ -> "Select opposite corner or enter value (w,h)."
  | Circle, Idle -> "Select center point."
  | Circle, _ -> "Select radius point or enter value."
  | Push_pull, Idle -> "Pick a face to push or pull."
  | Push_pull, _ -> "Pick distance or enter value."
  | Move, Idle -> "Pick two points to move."
  | Move, _ -> "Pick the point to move to or enter value."
  | Eraser, _ -> "Select an edge to erase, with its faces."
  | Orbit, _ -> "Drag in direction to orbit."
  | Pan, _ -> "Drag in direction to pan."

let view (computer : computer) m =
  let at = mouse computer in
  let inside = in_view at && m.drag = None in
  let found = infer m computer in
  let model, label, value = preview m computer in
  let hovered = match (m.tool, m.action) with Push_pull, Idle when inside -> found.face | Push_pull, Pushing (f, _, _) -> Some f | _ -> None in
  let loose = List.filter (fun (e : M.edge) -> M.faces_on model e.a e.b = []) model.edges in
  let selected_edges = List.filter_map (function Edge (a, b) when List.mem_assoc a model.verts && List.mem_assoc b model.verts -> Some (a, b) | _ -> None) m.selection in
  let rubber =
    if not inside then []
    else
      let p = found.point in
      let band_color = match found.kind with I.On_axis i -> axis_color i | _ -> black in
      match m.action with
      | Line_from a -> line3 m band_color 2. a p
      | Moving (_, a) -> line3 m band_color 1.5 ~dash:true a p
      | Corner (a, n) ->
          let pts = rectangle_corners a n (rectangle_size a n (in_plane m computer found a n)) in
          List.concat_map (fun (x, y) -> line3 m black 1.6 x y) (List.combine pts (List.tl pts @ [ List.hd pts ]))
      | Center (c, n) ->
          let pts = circle_points c n (Vec3.length (Vec3.sub (in_plane m computer found c n) c)) in
          line3 m black 1.2 ~dash:true c (in_plane m computer found c n) @ List.concat_map (fun (x, y) -> line3 m black 1.6 x y) (List.combine pts (List.tl pts @ [ List.hd pts ]))
      | Idle | Pushing _ -> []
  in
  let mark =
    if not inside || List.mem m.tool [ Orbit; Pan; Select; Eraser ] then []
    else match V.project m.view area found.point with Some xy -> marker found xy | None -> []
  in
  let cross = if inside then [ segment (rgb 60 60 60) 1. (fst at -. 8., snd at) (fst at +. 8., snd at); segment (rgb 60 60 60) 1. (fst at, snd at -. 8.) (fst at, snd at +. 8.) ] else [] in
  let bar = rgb 236 236 236 and edge = rgb 170 170 170 in
  let buttons =
    List.concat
      (List.init (List.length tools + 1) (fun i ->
           let x = button_x i in
           let active = i < List.length tools && (let t, _, _ = List.nth tools i in t = m.tool) in
           let hover = button_at at = Some i in
           [ rectangle (if active then rgb 190 205 230 else if hover then rgb 220 225 235 else bar) 40. 40. |> move x toolbar_y ]
           @ (if active || hover then [ rectangle edge 40. 1. |> move x (toolbar_y +. 20.); rectangle edge 40. 1. |> move x (toolbar_y -. 20.) ] else [])
           @ icon i (x, toolbar_y)))
  in
  let tip =
    match button_at at with
    | Some i ->
        let name = if i < List.length tools then (let _, key, n = List.nth tools i in n ^ " (" ^ (if key = " " then "Space" else String.uppercase_ascii key) ^ ")") else "Zoom Extents (Shift+Z)" in
        let w = (float_of_int (String.length name) *. 7.5) +. 12. in
        [ rectangle (rgb 255 255 225) w 20. |> move (button_x i +. (w /. 2.)) 412.; text black 13. name (button_x i +. (w /. 2.), 412.) ]
    | None -> []
  in
  let shown = if m.typed <> "" then m.typed else value in
  world m @ faces m model ~hovered
  @ List.concat_map (fun (e : M.edge) -> line3 m black 1.6 (M.pos model e.a) (M.pos model e.b)) loose
  @ List.concat_map (fun (a, b) -> line3 m blue 3. (M.pos model a) (M.pos model b)) selected_edges
  @ rubber @ mark @ cross
  @ [
      rectangle bar 1000. 75. |> move 0. 462.5;
      rectangle (rgb 60 90 150) 1000. 22. |> move 0. 489.;
      text white 14. "House - TinySketchup" (0., 489.);
      rectangle edge 1000. 1. |> move 0. 425.;
      rectangle bar 1000. 45. |> move 0. (-477.5);
      rectangle edge 1000. 1. |> move 0. (-455.);
    ]
  @ buttons @ tip
  @ text_left (rgb 40 40 40) 13. (hint m) (-490., -478.)
  @ [ rectangle white 190. 26. |> move 390. (-478.); rectangle edge 190. 1. |> move 390. (-465.); rectangle edge 190. 1. |> move 390. (-491.) ]
  @ text_left (rgb 40 40 40) 13. label (180., -478.)
  @ text_left black 13. shown (300., -478.)

let app = game view update initial
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
