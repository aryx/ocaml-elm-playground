(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyBlender: a 3D scene built with the mouse and rendered by our
 * own ray tracer (Blender, Ton Roosendaal, NeoGeo, 1994, open source
 * since 2002; this is 2.8's look, 2019; plan_raytracing_remaining.md).
 *
 * Blender is the modeller everyone can have; TinyBlender keeps the
 * ideas that make any modeller one:
 *
 * - **the quad view**: the scene from the top, the front and the
 *   right, drawn as wireframes -- a draftsman's plan and elevations,
 *   where lengths can be read and a drag moves along the two axes
 *   shown -- and through the camera, in perspective. Timothy Johnson's
 *   Sketchpad III (MIT, 1963) had the same four, and every modeller
 *   since (Modeler_view.mli);
 * - **objects are recipes**: a primitive -- the cube, the sphere, the
 *   cylinder, the cone, the torus, the ground -- and three vectors,
 *   location, rotation and scale (Modeler.mli). Nothing is ever
 *   remeshed: G adds to the location, R to the rotation, S multiplies
 *   the scale, each following the mouse in the view it started in,
 *   X, Y or Z locking it to an axis, a number typed setting it (G X 3
 *   Enter: three units along x), a click keeping it, Escape or the
 *   right button giving it up -- Blender's modal transforms;
 * - **the Boolean modifier**: an object minus another, the cutter
 *   (Object > Boolean) -- constructive solid geometry, which the ray
 *   tracer computes exactly along each ray (Csg.mli), no mesh cut;
 * - **the rendered viewport**: the camera's view is not a preview but
 *   the ray tracer itself (the Povray way's), coarse at once and
 *   sharpening as you watch, started again at each change -- Blender
 *   2.8's "Rendered" shading, Cycles in the viewport. F12 renders it
 *   larger, with two rays a pixel; Render > Save Image writes the PNG.
 *   The wires drawn over it fall exactly on the solids, because the
 *   view projects with the ray tracer's own camera;
 * - materials: a colour, a pattern (checker, marble, wood: solid
 *   textures), a mirror, glass.
 *
 * Its opening is Blender's own: the default cube, its light and its
 * camera. The flag scene=demo opens a richer one: a cube with a hole
 * bored through it (a Boolean), a glass ball, a marble ring, a mirror
 * sphere, on a checkered ground.
 *
 * Mouse: a click selects (Shift-click adds), in any view or the
 * outliner; the wheel zooms the three plans. Keys (the mouse in a
 * view): G, R, S; X or Delete deletes; Shift-D duplicates, then moves;
 * A selects all or none; H hides, Alt-H shows all; F12 renders,
 * Escape closes it; Control-Z undoes, Control-Shift-Z redoes; Home
 * zooms back.
 *
 * What it uses: appkits/modeler (Modeler, Modeler_view), the Povray
 * way and the ray tracer (Raytrace) to render, appkits/document (Undo)
 * and the File menu (File_menu: the scene saved as the value it is),
 * the playground's Gui for the menus, the slider and the checkbox. The
 * camera always looks at the origin (Blender's Track To constraint).
 *
 * What it deliberately does not do: meshes you edit (Tab, edit mode:
 * vertices, edges, faces, extrude) -- the objects are the ray tracer's
 * exact solids, which is why a Boolean costs nothing; animation, the
 * timeline and keyframes; the node editor; the camera's own rotation;
 * the 3D cursor; area lights and path tracing in the viewport (the
 * ray tracer has both: Raytrace.algorithms).
 *
 * Exercises: keep a rotation matrix per object instead of Euler
 * angles, so that R about x after a turn about z is exact; the 3D
 * cursor, where objects are added; a keyframe per object and the
 * timeline, rendered frame by frame (Blender's first use, NeoGeo's
 * animations); TinyPovray's scene files as the file format.
 *)
open Playground
module M = Modeler
module V = Modeler_view

(*****************************************************************************)
(* The scenes *)
(*****************************************************************************)

let material ?(look = M.Plain) ?(mirror = 0.) ?(glass = false) color = { M.color; look; mirror; glass }

let demo =
  let o name kind ?(rotation = (0., 0., 0.)) ?(scale = (1., 1., 1.)) ?modifier location =
    { M.name; kind; location; rotation; scale; hidden = false; modifier }
  in
  [
    o "Ground" (M.Mesh (M.Ground, material ~look:M.Checker 0xe8e8e8)) (0., 0., -1.);
    o "Cube" (M.Mesh (M.Cube, material ~mirror:0.15 0xc83c32)) ~modifier:(M.Difference, "Cylinder") (0., 0., 0.);
    o "Cylinder" (M.Mesh (M.Cylinder, M.grey)) ~scale:(0.55, 0.55, 1.5) ~rotation:(90., 0., 0.) (0., 0., 0.);
    o "Sphere" (M.Mesh (M.Sphere, material ~glass:true 0xffffff)) (2.4, -1.6, 0.);
    o "Torus" (M.Mesh (M.Torus 0.3, material ~look:M.Marble 0xf0ece0)) (-2.6, -1., -0.7);
    o "Sphere.001" (M.Mesh (M.Sphere, material ~mirror:0.8 0xd4af37)) ~scale:(0.7, 0.7, 0.7) (0.5, 2.8, -0.3);
    o "Light" (M.Point_light 0xffffff) (4.1, -3., 5.9);
    o "Camera" M.Camera (7.4, -6.9, 5.);
  ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type op = Grab | Rotate | Scale

(* a transform under way: where the mouse started, in which view, the
   axis it is locked to, the number typed, the objects before it *)
type modal = { op : op; view : V.view; start : float * float; axis : int option; number : string; before : M.t }

type model = {
  history : M.t Undo.t;
  file : File_menu.t;
  selected : string list; (* the active one last *)
  modal : modal option;
  live : M.t option; (* the scene while a transform is under way *)
  zoom : float; (* the plans' pixels a unit *)
  rendering : bool; (* the F12 window *)
  said : string;
  started : bool;
  was : string list;
  was_down : bool;
  was_rdown : bool;
}

let initial =
  {
    history = Undo.start M.default;
    file = File_menu.start;
    selected = [ "Cube" ];
    modal = None;
    live = None;
    zoom = 38.;
    rendering = false;
    said = "";
    started = false;
    was = [];
    was_down = false;
    was_rdown = false;
  }

let scene m = match m.live with Some t -> t | None -> Undo.now m.history
let active m = match List.rev m.selected with n :: _ -> M.find (scene m) n | [] -> None

(* an edit of the properties: one undo step for a run of the same one
   (a slider dragged) *)
let edit name t m =
  if t = Undo.now m.history then m
  else if Undo.undo_name m.history = Some name then { m with history = Undo.amend t m.history }
  else { m with history = Undo.record ~name t m.history }

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

let views_left = -500.
let views_right = 270.
let views_top = 468.
let views_bottom = -478.

(* Blender's quad view: the top, the camera; the front, the right *)
let quad = [ (V.Top, 0, 0); (V.Camera_view, 1, 0); (V.Front, 0, 1); (V.Right, 1, 1) ]

(* a view's box: its center and size *)
let view_box view =
  let _, col, row = List.find (fun (v, _, _) -> v = view) quad in
  let w = (views_right -. views_left) /. 2. and h = (views_top -. views_bottom) /. 2. in
  (views_left +. (w *. (float_of_int col +. 0.5)), views_top -. (h *. (float_of_int row +. 0.5)), w, h)

let inside_box (cx, cy, w, h) (x, y) = Float.abs (x -. cx) <= w /. 2. && Float.abs (y -. cy) <= h /. 2.
let view_at p = List.find_map (fun (v, _, _) -> if inside_box (view_box v) p then Some v else None) quad

(* the camera's picture: half the view's pixels, drawn twice as large *)
let picture_size () =
  let _, _, w, h = view_box V.Camera_view in
  (int_of_float (w /. 2.) - 2, int_of_float (h /. 2.) - 2)

let camera t = V.camera t ~target:(0., 0., 0.)

(* where a point of the scene is drawn in a view *)
let to_screen m view p =
  let cx, cy, w, h = view_box view in
  match view with
  | V.Camera_view ->
      Option.map (fun (nx, ny) -> (cx +. (nx *. w /. 2.), cy +. (ny *. h /. 2.))) (V.perspective (camera (scene m)) ~aspect:(w /. h) p)
  | _ ->
      let u, v = V.project view p in
      Some (cx +. (u *. m.zoom), cy +. (v *. m.zoom))

(*****************************************************************************)
(* Rendering, with the Povray way *)
(*****************************************************************************)

let color_of c = rgb ((c lsr 16) land 255) ((c lsr 8) land 255) (c land 255)

let surface (mat : M.material) =
  let c = color_of mat.color in
  let darker = color_of ((mat.color lsr 1) land 0x7f7f7f) in
  let s =
    match mat.look with
    | M.Plain -> Povray.color c
    | M.Checker -> Povray.checker ~size:1. c (rgb 90 90 90)
    | M.Marble -> Povray.marble c (rgb 70 70 90)
    | M.Wood -> Povray.wood c darker
  in
  let s = if mat.mirror > 0. then Povray.shiny mat.mirror s else s in
  if mat.glass then Povray.glassy 1.5 s else s

(* an object's solid: its primitive, scaled, turned, moved -- the axes
   turned from Blender's z up to the ray tracer's y up (Modeler.mli):
   a turn about Blender's y is one about the world's -z *)
let solid (o : M.obj) =
  match o.kind with
  | M.Mesh (M.Ground, mat) ->
      let _, _, h = o.location in
      Some (Povray.move 0. h 0. (Povray.plane (surface mat)))
  | M.Mesh (shape, mat) ->
      let s = surface mat in
      let prim =
        match shape with
        | M.Cube -> Povray.box s
        | M.Sphere -> Povray.sphere s
        | M.Cylinder -> Povray.cylinder s
        | M.Cone -> Povray.cone s
        | M.Torus r -> Povray.torus r s
        | M.Ground -> Povray.plane s
      in
      let sx, sy, sz = o.scale and rx, ry, rz = o.rotation in
      let lx, ly, lz = M.to_world o.location in
      Some
        (prim |> Povray.scale sx sz sy |> Povray.rotate rx 0. 0. |> Povray.rotate 0. 0. (-.ry) |> Povray.rotate 0. rz 0.
       |> Povray.move lx ly lz)
  | M.Point_light _ | M.Sun_light _ | M.Camera -> None

let povray_scene (t : M.t) =
  let visible = List.filter (fun (o : M.obj) -> not o.hidden) t in
  let solids =
    List.filter_map
      (fun (o : M.obj) ->
        if M.is_cutter t o.name then None
        else
          match (solid o, o.modifier) with
          | Some a, Some (op, cutter) -> (
              match Option.bind (M.find t cutter) solid with
              | Some b -> Some (match op with M.Difference -> Povray.diff a b | M.Union -> Povray.union [ a; b ] | M.Intersect -> Povray.inter [ a; b ])
              | None -> Some a)
          | a, _ -> a)
      visible
  in
  let lights =
    List.filter_map
      (fun (o : M.obj) ->
        let x, y, z = M.to_world o.location in
        match o.kind with
        | M.Point_light c -> Some (Povray.lamp (color_of c) x y z)
        | M.Sun_light c -> Some (Povray.sun (color_of c) (-.x) (-.y) (-.z))
        | _ -> None)
      visible
  in
  let cam = camera t in
  Povray.scene ~ambient:0.15 ~sky:(rgb 104 108 116) ~camera:(Povray.camera ~fov:cam.fov ~eye:cam.eye ~target:cam.target ()) lights solids

(* the pictures under way, made a slice a frame in place, as the
   Povray way's are (a Raytrace.progress is mutable): the viewport's,
   and F12's; each started again when its scene changes *)
let viewport : (M.t * Raytrace.progress) option ref = ref None
let render_window : (M.t * Raytrace.progress) option ref = ref None

let progress cache options t ~width ~height =
  match !cache with
  | Some (t', p) when t' = t -> p
  | _ ->
      let p = Raytrace.start ~options (Povray.raytrace_scene (povray_scene t)) ~width ~height in
      cache := Some (t, p);
      p

let viewport_rays = 12_000
let render_size = (480, 368)

(* six bounces: a ray through a glass ball bends in, then out, and
   still has one to show what it met *)
let options = { Raytrace.default_options with depth = 6 }

let viewport_progress m =
  let width, height = picture_size () in
  progress viewport options (scene m) ~width ~height

let render_progress m =
  let width, height = render_size in
  progress render_window { options with samples = 2 } (scene m) ~width ~height

(*****************************************************************************)
(* The transforms, G, R and S *)
(*****************************************************************************)

let axis_name = function 0 -> "X" | 1 -> "Y" | _ -> "Z"
let unit_axis a = match a with 0 -> (1., 0., 0.) | 1 -> (0., 1., 0.) | _ -> (0., 0., 1.)
let coord a (x, y, z) = match a with 0 -> x | 1 -> y | _ -> z

(* the middle of the objects moved, and where it is on the screen *)
let center (t : M.t) names =
  let ps = List.filter_map (fun n -> Option.map (fun (o : M.obj) -> o.location) (M.find t n)) names in
  let k = 1. /. float_of_int (max 1 (List.length ps)) in
  let x, y, z = List.fold_left (fun (a, b, c) (x, y, z) -> (a +. x, b +. y, c +. z)) (0., 0., 0.) ps in
  (k *. x, k *. y, k *. z)

(* a point turned about an axis through [c] *)
let turn_about a angle c p = Vec3.add c (M.rotate_point a angle (Vec3.sub p c))

(* the world's move for a move of the mouse, in the view it is in; in
   the camera's view, along the picture's right and up at the objects'
   depth *)
let world_move m view names (dx, dy) =
  match view with
  | V.Camera_view ->
      let t = scene m in
      let cam = camera t in
      let _, _, _, h = view_box V.Camera_view in
      let right, up, _ = Camera.basis ~up:cam.up ~eye:cam.eye ~target:cam.target () in
      let _, _, depth = Camera.view cam (M.to_world (center t names)) in
      let k = 2. *. depth /. (Camera.focal cam *. h) in
      let wx, wy, wz = Vec3.add (Vec3.scale (dx *. k) right) (Vec3.scale (dy *. k) up) in
      (* the world's y up back to Blender's z up *)
      (wx, -.wz, wy)
  | _ -> V.unproject view (dx /. m.zoom, dy /. m.zoom)

let apply m (md : modal) (mouse : float * float) =
  let names = m.selected in
  let t = md.before in
  let c = center t names in
  let c_screen = Option.value (to_screen m md.view c) ~default:md.start in
  let number = float_of_string_opt md.number in
  let each f = List.fold_left (fun t n -> M.update n f t) t names in
  match md.op with
  | Grab ->
      let d = world_move m md.view names (fst mouse -. fst md.start, snd mouse -. snd md.start) in
      let d =
        match (md.axis, number) with
        | Some a, Some v -> Vec3.scale v (unit_axis a)
        | Some a, None ->
            (* locked to the axis a plan looks along: the mouse's up *)
            let u, v = V.axes md.view in
            let along = if md.view = V.Camera_view || a = u || a = v then coord a d else (snd mouse -. snd md.start) /. m.zoom in
            Vec3.scale along (unit_axis a)
        | None, _ -> d
      in
      each (M.translate d)
  | Rotate ->
      let angle_of (x, y) = Float.atan2 (y -. snd c_screen) (x -. fst c_screen) *. 180. /. Float.pi in
      let a, sign = match md.axis with Some a -> (a, 1.) | None -> V.normal md.view in
      let angle = match number with Some v -> v | None -> sign *. (angle_of mouse -. angle_of md.start) in
      each (fun o -> { (M.turn a angle o) with location = turn_about a angle c o.location })
  | Scale ->
      let dist (x, y) = Float.hypot (x -. fst c_screen) (y -. snd c_screen) in
      let k = match number with Some v -> v | None -> dist mouse /. Float.max 1. (dist md.start) in
      let ks = match md.axis with Some 0 -> (k, 1., 1.) | Some 1 -> (1., k, 1.) | Some _ -> (1., 1., k) | None -> (k, k, k) in
      let kx, ky, kz = ks in
      each (fun o ->
          let (x, y, z) = o.location and (cx, cy, cz) = c in
          { (M.resize ks o) with location = (cx +. (kx *. (x -. cx)), cy +. (ky *. (y -. cy)), cz +. (kz *. (z -. cz))) })

let op_name = function Grab -> "Move" | Rotate -> "Rotate" | Scale -> "Resize"

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let record name t m = { m with history = Undo.record ~name t m.history }

let add kind base m =
  let t, name = M.add kind base (Undo.now m.history) in
  { (record ("Add " ^ base) t m) with selected = [ name ] }

let delete m = if m.selected = [] then m else { (record "Delete" (M.remove m.selected (Undo.now m.history)) m) with selected = [] }

let boolean op m =
  match List.rev m.selected with
  | [ a; b ] -> { (record "Boolean" (M.update a (fun o -> { o with modifier = Some (op, b) }) (Undo.now m.history)) m) with selected = [ a ] }
  | _ -> { m with said = "Boolean: select the cutter, then Shift-click the object (two objects)" }

let start_modal op view (mouse : float * float) m =
  if m.selected = [] then m else { m with modal = Some { op; view; start = mouse; axis = None; number = ""; before = Undo.now m.history } }

let menus =
  [
    File_menu.items;
    [ "Edit"; "Undo"; "Redo"; "Duplicate"; "Delete"; "Select All"; "Hide"; "Unhide All" ];
    [ "Add"; "Cube"; "UV Sphere"; "Cylinder"; "Cone"; "Torus"; "Ground"; "Point Light"; "Sun" ];
    [ "Object"; "Boolean Difference"; "Boolean Union"; "Boolean Intersect"; "Remove Boolean" ];
    [ "Render"; "Render Image"; "Save Image"; "Close Render" ];
  ]

let menu_box i : Widget.box = { Widget.x = -440. +. (float_of_int i *. 108.); y = 484.; w = 104.; h = 28. }

type caps = < Cap.open_in ; Cap.open_out ; Cap.readdir >

let command (caps : caps) c m =
  let t = Undo.now m.history in
  let mesh s = M.Mesh (s, M.grey) in
  match c with
  | "Undo" -> { m with history = Undo.undo m.history }
  | "Redo" -> { m with history = Undo.redo m.history }
  | "Duplicate" ->
      let t, names = M.duplicate m.selected t in
      if names = [] then m else { (record "Duplicate" t m) with selected = names }
  | "Delete" -> delete m
  | "Select All" -> { m with selected = (if List.length m.selected = List.length t then [] else List.map (fun (o : M.obj) -> o.name) t) }
  | "Hide" -> { (record "Hide" (List.map (fun (o : M.obj) -> if List.mem o.name m.selected then { o with hidden = true } else o) t) m) with selected = [] }
  | "Unhide All" -> record "Unhide" (List.map (fun (o : M.obj) -> { o with hidden = false }) t) m
  | "Cube" -> add (mesh M.Cube) "Cube" m
  | "UV Sphere" -> add (mesh M.Sphere) "Sphere" m
  | "Cylinder" -> add (mesh M.Cylinder) "Cylinder" m
  | "Cone" -> add (mesh M.Cone) "Cone" m
  | "Torus" -> add (mesh (M.Torus 0.25)) "Torus" m
  | "Ground" -> add (mesh M.Ground) "Ground" m
  | "Point Light" -> add (M.Point_light 0xffffff) "Light" m
  | "Sun" -> add (M.Sun_light 0xffffff) "Sun" m
  | "Boolean Difference" -> boolean M.Difference m
  | "Boolean Union" -> boolean M.Union m
  | "Boolean Intersect" -> boolean M.Intersect m
  | "Remove Boolean" -> (
      match active m with Some o -> record "Remove Boolean" (M.update o.name (fun o -> { o with modifier = None }) t) m | None -> m)
  | "Render Image" -> { m with rendering = true }
  | "Close Render" -> { m with rendering = false }
  | "Save Image" ->
      let p = render_progress m in
      Playground_platform.export caps "render.png" (Png.encode (Raytrace.picture p));
      { m with rendering = true; said = (if Raytrace.finished p then "Saved render.png" else "Saved render.png, unfinished") }
  | _ -> m

(*****************************************************************************)
(* The properties *)
(*****************************************************************************)

let panel_left = 270.
let panel_x = (panel_left +. 500.) /. 2.
let outliner_top = 440.
let row_h = 22.
let props_top = 150.
let swatches = [ 0xcccccc; 0xffffff; 0x202020; 0xc83c32; 0xe8a33c; 0x4caf50; 0x3c78c8; 0xd4af37 ]
let swatch_box i : Widget.box = { Widget.x = panel_left +. 22. +. (float_of_int i *. 26.); y = props_top -. 150.; w = 22.; h = 22. }
let looks = [ (M.Plain, "Plain"); (M.Checker, "Checker"); (M.Marble, "Marble"); (M.Wood, "Wood") ]
let look_box i : Widget.box = { Widget.x = panel_left +. 30. +. (float_of_int i *. 55.); y = props_top -. 185.; w = 52.; h = 24. }
let mirror_box : Widget.box = { Widget.x = panel_x +. 30.; y = props_top -. 220.; w = 150.; h = 24. }
let glass_box : Widget.box = { Widget.x = panel_x; y = props_top -. 255.; w = 200.; h = 24. }

(* the material of the active object, changed *)
let set_material name f m =
  match active m with
  | Some ({ kind = M.Mesh (s, mat); _ } as o) -> edit name (M.update o.name (fun o -> { o with kind = M.Mesh (s, f mat) }) (Undo.now m.history)) m
  | Some ({ kind = M.Point_light _ | M.Sun_light _; _ } as o) when name = "Color" ->
      let c = (f M.grey).color in
      edit name (M.update o.name (fun o -> { o with kind = (match o.kind with M.Sun_light _ -> M.Sun_light c | _ -> M.Point_light c) }) (Undo.now m.history)) m
  | _ -> m

let properties computer press m =
  let mouse = computer.mouse in
  let clicked (b : Widget.box) = press && Widget.contains b mouse.mx mouse.my in
  match active m with
  | None -> m
  | Some o ->
      let m = List.fold_left (fun m (i, c) -> if clicked (swatch_box i) then set_material "Color" (fun mat -> { mat with color = c }) m else m) m (List.mapi (fun i c -> (i, c)) swatches) in
      match o.kind with
      | M.Mesh (_, mat) ->
          let m = List.fold_left (fun m (i, (look, _)) -> if clicked (look_box i) then set_material "Pattern" (fun mat -> { mat with look }) m else m) m (List.mapi (fun i l -> (i, l)) looks) in
          let mirror = Gui.slider_in computer mirror_box ~from:0. ~to_:1. mat.mirror in
          let m = if mirror <> mat.mirror then set_material "Mirror" (fun mat -> { mat with mirror }) m else m in
          let glass = Gui.checkbox_in computer glass_box "Glass" mat.glass in
          if glass <> mat.glass then set_material "Glass" (fun mat -> { mat with glass }) m else m
      | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let outliner_row i = outliner_top -. 30. -. (float_of_int i *. row_h)

let select shift name m =
  match name with
  | Some n -> { m with selected = (if shift then if List.mem n m.selected then List.filter (( <> ) n) m.selected else m.selected @ [ n ] else [ n ]) }
  | None -> if shift then m else { m with selected = [] }

let pick m view p = V.pick ~to_screen:(to_screen m view) ~tolerance:7. (scene m) p

let modal_update computer (md : modal) m =
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.was) in
  let mouse = (computer.mouse.mx, computer.mouse.my) in
  let axis a = if md.axis = Some a then None else Some a in
  let md =
    if pressed "x" || pressed "X" then { md with axis = axis 0 }
    else if pressed "y" || pressed "Y" then { md with axis = axis 1 }
    else if pressed "z" || pressed "Z" then { md with axis = axis 2 }
    else if pressed "Backspace" && md.number <> "" then { md with number = String.sub md.number 0 (String.length md.number - 1) }
    else { md with number = md.number ^ String.concat "" (List.filter_map (fun c -> if (c >= '0' && c <= '9') || c = '.' || c = '-' then Some (String.make 1 c) else None) (List.of_seq (String.to_seq computer.keyboard.typed))) }
  in
  let live = apply m md mouse in
  if pressed "Escape" || (computer.mouse.mrdown && not m.was_rdown) then { m with modal = None; live = None }
  else if pressed "Enter" || (computer.mouse.mdown && not m.was_down) then { (record (op_name md.op) live m) with modal = None; live = None }
  else { m with modal = Some md; live = Some live }

let update caps computer m =
  let m =
    if m.started then m
    else { m with started = true; history = (if List.assoc_opt "scene" computer.flags = Some "demo" then Undo.start demo else m.history) }
  in
  let mouse = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let pressed k = List.mem k now && not (List.mem k m.was) in
  let press = mouse.mdown && not m.was_down in
  let shift = computer.keyboard.kshift in
  let p = (mouse.mx, mouse.my) in
  let m =
    if File_menu.busy m.file then (
      let file, r = File_menu.dialog caps { File_menu.magic = "TinyBlender 1"; extension = ".blend" } computer ~current:(fun () -> Undo.now m.history) m.file in
      match r with
      | File_menu.Opened t -> { m with file; history = Undo.start t; selected = [] }
      | File_menu.New -> { m with file; history = Undo.start M.default; selected = [] }
      | File_menu.Nothing -> { m with file })
    else
      match m.modal with
      | Some md -> modal_update computer md m
      | None ->
          let kind = { File_menu.magic = "TinyBlender 1"; extension = ".blend" } in
          let m =
            List.fold_left
              (fun m (i, items) ->
                if i = 0 then
                  let file, r = File_menu.menu_in caps kind computer (menu_box i) ~current:(fun () -> Undo.now m.history) m.file in
                  match r with
                  | File_menu.Opened t -> { m with file; history = Undo.start t; selected = [] }
                  | File_menu.New -> { m with file; history = Undo.start M.default; selected = [] }
                  | File_menu.Nothing -> { m with file }
                else
                  match List.nth_opt items (Gui.menu_in computer (menu_box i) items 0) with
                  | Some c when c <> List.hd items -> command caps c { m with said = "" }
                  | _ -> m)
              m
              (List.mapi (fun i items -> (i, items)) menus)
          in
          if Gui.modal () then m
          else
            let m = properties computer press m in
            let control = List.mem "Control" now in
            let alt = List.mem "Alt" now in
            let in_view = if m.rendering then None else view_at p in
            let m =
              if control && (pressed "z" || pressed "Z") then command caps (if shift then "Redo" else "Undo") m
              else if pressed "F12" then { m with rendering = true }
              else if pressed "Escape" then { m with rendering = false }
              else if pressed "Home" then { m with zoom = initial.zoom }
              else
                match in_view with
                | None -> m
                | Some view ->
                    if pressed "g" || pressed "G" then start_modal Grab view p m
                    else if (pressed "r" || pressed "R") && not control then start_modal Rotate view p m
                    else if (pressed "s" || pressed "S") && not control then start_modal Scale view p m
                    else if (pressed "d" || pressed "D") && shift then start_modal Grab view p (command caps "Duplicate" m)
                    else if pressed "x" || pressed "X" || pressed "Delete" then delete m
                    else if pressed "a" || pressed "A" then command caps "Select All" m
                    else if (pressed "h" || pressed "H") && alt then command caps "Unhide All" m
                    else if pressed "h" || pressed "H" then command caps "Hide" m
                    else m
            in
            let m = if mouse.mwheel <> 0. && in_view <> None && in_view <> Some V.Camera_view then { m with zoom = Float.max 5. (Float.min 300. (m.zoom *. (1.1 ** -.mouse.mwheel))) } else m in
            (* a click: in a view, the object under it; in the outliner,
               its row, or its eye *)
            if not press then m
            else
              match in_view with
              | Some view when m.modal = None -> select shift (pick m view p) m
              | _ ->
                  let t = Undo.now m.history in
                  let row = List.find_opt (fun (i, _) -> Float.abs (snd p -. outliner_row i) <= row_h /. 2. && fst p >= panel_left) (List.mapi (fun i o -> (i, o)) t) in
                  (match row with
                  | Some (_, (o : M.obj)) when fst p >= 470. -> record (if o.hidden then "Unhide" else "Hide") (M.update o.name (fun o -> { o with hidden = not o.hidden }) t) m
                  | Some (_, o) -> select shift (Some o.name) m
                  | None -> m)
  in
  (* the pictures, a slice a frame *)
  let vp = viewport_progress m in
  if not (Raytrace.finished vp) then Raytrace.advance vp ~rays:viewport_rays;
  (if m.rendering then
     let rp = render_progress m in
     if not (Raytrace.finished rp) then Raytrace.advance rp ~rays:(3 * viewport_rays));
  { m with was = now; was_down = mouse.mdown; was_rdown = mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let segment color width (ax, ay) (bx, by) =
  let len = Float.hypot (bx -. ax) (by -. ay) in
  rectangle color (len +. (width /. 2.)) width |> rotate (Float.atan2 (by -. ay) (bx -. ax) *. 180. /. Float.pi) |> move ((ax +. bx) /. 2.) ((ay +. by) /. 2.)

(* a segment cut to a box (Liang and Barsky, 1984), if any of it is in *)
let clip (cx, cy, w, h) (ax, ay) (bx, by) =
  let x0 = cx -. (w /. 2.) and x1 = cx +. (w /. 2.) and y0 = cy -. (h /. 2.) and y1 = cy +. (h /. 2.) in
  let dx = bx -. ax and dy = by -. ay in
  let edges = [ (-.dx, ax -. x0); (dx, x1 -. ax); (-.dy, ay -. y0); (dy, y1 -. ay) ] in
  let rec go t0 t1 = function
    | [] -> if t0 <= t1 then Some ((ax +. (t0 *. dx), ay +. (t0 *. dy)), (ax +. (t1 *. dx), ay +. (t1 *. dy))) else None
    | (p, q) :: rest ->
        if p = 0. then if q < 0. then None else go t0 t1 rest
        else
          let r = q /. p in
          if p < 0. then go (Float.max t0 r) t1 rest else go t0 (Float.min t1 r) rest
  in
  go 0. 1. edges

let text color size s (x, y) = words color s |> scale (size /. words_font_size) |> move x y
let text_left color size s (x, y) = text color size s (x +. (Widget.text_width ~size s /. 2.), y)
let background = rgb 57 57 57
let wire_black = rgb 0 0 0
let orange = rgb 241 88 0
let active_orange = rgb 255 170 64
let axis_colors = [| rgb 255 51 82; rgb 139 220 0; rgb 40 144 255 |]

(* an object's wires in a view, in its colour *)
let object_wires m view box color (o : M.obj) =
  List.filter_map
    (fun (a, b) ->
      match (to_screen m view a, to_screen m view b) with
      | Some a, Some b -> Option.map (fun (a, b) -> segment color 1.5 a b) (clip box a b)
      | _ -> None)
    (M.wires o)

let wire_color m (o : M.obj) =
  match List.rev m.selected with
  | n :: _ when n = o.name -> active_orange
  | _ ->
      if List.mem o.name m.selected then orange
      else (* the ground's grid, faint under the rest *)
        match o.kind with M.Mesh (M.Ground, _) -> rgb 48 48 48 | _ -> wire_black

let draw_view m view =
  let ((cx, cy, w, h) as box) = view_box view in
  let t = scene m in
  let frame = [ rectangle background (w -. 2.) (h -. 2.) |> move cx cy ] in
  let content =
    match view with
    | V.Camera_view ->
        let p = viewport_progress m in
        let pw, ph = picture_size () in
        [ bitmap (float_of_int pw *. 2.) (float_of_int ph *. 2.) (Raytrace.picture p) |> move cx cy ]
        (* the selection's wires only, over the picture *)
        @ List.concat_map (fun (o : M.obj) -> if List.mem o.name m.selected && not o.hidden then object_wires m view box (wire_color m o) o else []) t
    | _ ->
        let a, b = V.axes view in
        let n = int_of_float (Float.max w h /. 2. /. m.zoom) + 1 in
        let line u0 v0 u1 v1 color = Option.map (fun (p, q) -> segment color 1. p q) (clip box (cx +. u0, cy +. v0) (cx +. u1, cy +. v1)) in
        let grid =
          List.concat
            (List.init ((2 * n) + 1) (fun i ->
                 let k = float_of_int (i - n) *. m.zoom and far = float_of_int n *. m.zoom in
                 if i = n then []
                 else List.filter_map Fun.id [ line k (-.far) k far (rgb 70 70 70); line (-.far) k far k (rgb 70 70 70) ]))
        in
        let far = Float.max w h in
        grid
        @ List.filter_map Fun.id [ line (-.far) 0. far 0. axis_colors.(a); line 0. (-.far) 0. far axis_colors.(b) ]
        @ List.concat_map (fun (o : M.obj) -> if o.hidden then [] else object_wires m view box (wire_color m o) o) t
        @ List.filter_map
            (fun n ->
              match M.find t n with
              | Some o -> Option.map (fun (x, y) -> circle orange 3.5 |> move x y) (to_screen m view o.location)
              | None -> None)
            m.selected
  in
  let label =
    let heading = V.name view in
    let under = match active m with Some o -> "(1) Collection | " ^ o.name | None -> "(1) Collection" in
    [ text_left (rgb 230 230 230) 13. heading (cx -. (w /. 2.) +. 10., cy +. (h /. 2.) -. 14.); text_left (rgb 230 230 230) 12. under (cx -. (w /. 2.) +. 10., cy +. (h /. 2.) -. 32.) ]
  in
  frame @ content @ label

let kind_letter (o : M.obj) = match o.kind with M.Mesh _ -> "M" | M.Point_light _ | M.Sun_light _ -> "L" | M.Camera -> "C"
let fmt3 (x, y, z) = Printf.sprintf "%.2f  %.2f  %.2f" x y z

let panel m =
  let t = scene m in
  let light = rgb 230 230 230 in
  let outliner =
    [ text_left light 13. "Scene Collection" (panel_left +. 10., outliner_top) ]
    @ List.concat
        (List.mapi
           (fun i (o : M.obj) ->
             let y = outliner_row i in
             let sel = List.mem o.name m.selected in
             (if sel then [ rectangle (rgb 70 90 130) (500. -. panel_left -. 4.) (row_h -. 2.) |> move panel_x y ] else [])
             @ [
                 text_left (rgb 240 150 60) 12. (kind_letter o) (panel_left +. 14., y);
                 text_left (if o.hidden then rgb 120 120 120 else light) 13. o.name (panel_left +. 34., y);
                 text light 13. (if o.hidden then "-" else "o") (484., y);
               ])
           t)
  in
  let props =
    match active m with
    | None -> [ text_left light 13. "Nothing selected" (panel_left +. 10., props_top) ]
    | Some o ->
        [
          text_left light 14. o.name (panel_left +. 10., props_top);
          text_left light 12. ("Location   " ^ fmt3 o.location) (panel_left +. 10., props_top -. 30.);
          text_left light 12. ("Rotation   " ^ fmt3 o.rotation) (panel_left +. 10., props_top -. 52.);
          text_left light 12. ("Scale      " ^ fmt3 o.scale) (panel_left +. 10., props_top -. 74.);
          text_left light 12. (match o.modifier with Some (op, c) -> Printf.sprintf "Boolean: %s %s" (match op with M.Difference -> "Difference" | M.Union -> "Union" | M.Intersect -> "Intersect") c | None -> "") (panel_left +. 10., props_top -. 100.);
        ]
        @ (match o.kind with
          | M.Camera -> []
          | M.Mesh _ | M.Point_light _ | M.Sun_light _ ->
              text_left light 12. "Color" (panel_left +. 10., props_top -. 126.)
              :: List.concat
                   (List.mapi
                      (fun i c ->
                        let b = swatch_box i in
                        [ rectangle (rgb 20 20 20) (b.w +. 2.) (b.h +. 2.) |> move b.x b.y; rectangle (color_of c) b.w b.h |> move b.x b.y ])
                      swatches))
        @
        match o.kind with
        | M.Mesh (_, mat) ->
            List.concat
              (List.mapi
                 (fun i (look, name) ->
                   let b = look_box i in
                   [ rectangle (if mat.look = look then rgb 84 120 190 else rgb 88 88 88) b.w b.h |> move b.x b.y; text light 11. name (b.x, b.y) ])
                 looks)
            @ [ text_left light 12. "Mirror" (panel_left +. 10., mirror_box.y) ]
        | _ -> []
  in
  [ rectangle (rgb 48 48 48) (500. -. panel_left) 1000. |> move panel_x 0.; segment (rgb 30 30 30) 2. (panel_left, props_top +. 22.) (500., props_top +. 22.) ] @ outliner @ props

let render_overlay m =
  if not m.rendering then []
  else
    let p = render_progress m in
    let w, h = render_size in
    let cx = (views_left +. views_right) /. 2. and cy = (views_top +. views_bottom) /. 2. in
    let status = Printf.sprintf "Blender Render   pass %d   %d rays%s" (Raytrace.pass p) (Raytrace.rays_shot p) (if Raytrace.finished p then "   done" else "") in
    [
      rectangle (rgb 30 30 30) (views_right -. views_left) (views_top -. views_bottom) |> move cx cy;
      bitmap (float_of_int w *. 1.5) (float_of_int h *. 1.5) (Raytrace.picture p) |> move cx (cy +. 10.);
      text (rgb 230 230 230) 13. status (cx, views_bottom +. 20.);
    ]

let status m =
  match m.modal with
  | Some md ->
      let axis = match md.axis with Some a -> " along global " ^ axis_name a | None -> "" in
      Printf.sprintf "%s%s%s     click or Enter to confirm, Escape or right click to cancel" (op_name md.op) axis (if md.number <> "" then "  " ^ md.number else "")
  | None ->
      if m.said <> "" then m.said
      else
        Printf.sprintf "%s     %d objects, %d selected%s" (File_menu.title m.file) (List.length (scene m)) (List.length m.selected)
          (match Undo.undo_name m.history with Some n -> "     Undo " ^ n | None -> "")

let view _computer m =
  [ rectangle (rgb 36 36 36) 1000. 1000. ]
  @ List.concat_map (fun (v, _, _) -> draw_view m v) quad
  @ render_overlay m @ panel m
  @ [
      rectangle (rgb 36 36 36) 1000. (500. -. views_top) |> move 0. ((views_top +. 500.) /. 2.);
      rectangle (rgb 36 36 36) 1000. (views_bottom +. 500.) |> move 0. ((views_bottom -. 500.) /. 2.);
      text_left (rgb 200 200 200) 12. (status m) (-490., -489.);
    ]
  @ File_menu.view m.file @ Gui.draw ()

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> caps)))
