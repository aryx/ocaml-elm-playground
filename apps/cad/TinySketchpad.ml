(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinySketchpad: drawing with a light pen, and the drawing keeping its
 * own rules (Sketchpad, Ivan Sutherland, MIT Lincoln Laboratory's TX-2,
 * 1963; plan_cad.md).
 *
 * Sketchpad was Sutherland's PhD thesis, and the first program where a
 * person drew on a screen and the computer understood the drawing.
 * Nearly everything in it was a first, and is still with us:
 *
 * - **the pen aims**: near a point, the light pen *is* that point; near
 *   a line, it is on the line. Draw a line to a corner and it ends on
 *   that very corner; draw one to the middle of another and its end
 *   is put on it, with a constraint saying so (an ancestor of every
 *   CAD's object snaps);
 * - **constraints**: a line made horizontal, two made parallel or of
 *   the same length, stays so as you move things, because the program
 *   relaxes the drawing until they hold (Relax.mli: Sutherland's
 *   method, one point at a time, Gauss-Seidel). The famous demo is
 *   sheet D: a rough hexagon, its corners put on a circle and its
 *   sides made equal, becomes regular before your eyes -- then the
 *   circle can go, which is how sheet B was made;
 * - **masters and instances**: sheet A is a honeycomb of instances of
 *   sheet B's hexagon, some moved, sized and turned by the knobs. Change
 *   the hexagon on B and every cell on A changes -- the ancestor of
 *   AutoCAD's blocks, and of objects (Alan Kay: Sketchpad and Simula);
 * - **the window**: the drawing is on a sheet far bigger than the
 *   screen, and the ZOOM knob and MOVE on nothing move the window over
 *   it;
 * - and a mechanism with no numbers in it, sheet C: a four-bar linkage,
 *   each crank's end on a circle round its pivot, the rod between them
 *   as long as a reference line. MOVE a crank's end round its circle,
 *   and the rest follows.
 *
 * The screen is the TX-2's scope, which plotted dots, not lines: every
 * stroke here is drawn as its dots, one every few pixels -- the trick
 * of this app, and why Sketchpad's pictures have their texture. The
 * push buttons on the right are the ones Sutherland pressed with his
 * left hand (each a mode, lit while it is on), the knobs below his
 * scope's.
 *
 * How to use it: a mode button, then the pen (the mouse). DRAW: click,
 * click, click, a line to each; TERMINATE, Escape or the right button
 * ends the chain (Sutherland flicked the pen away). CIRCLE: its center,
 * then its rim. MOVE: a point (dropped on another, it becomes it), a
 * line, an instance, or nothing -- the window. DELETE, FIX (a fixed
 * point, which relaxation never moves; shown as a square). HORIZONTAL,
 * VERTICAL: a line. PARALLEL, PERPENDICULAR, EQUAL LENGTH: two lines.
 * INSTANCE: places the sheet the INSTANCE OF switch says; SIZE and
 * TURN change the one last placed or moved. SHOW CONSTRAINTS writes
 * each one's symbol by what it holds (bright while it does not hold
 * yet). Flag sheet=a|b|c|d, the sheet shown first.
 *
 * What it uses: appkits/sketch (Sketch, Relax), and the playground's
 * Gui for the knobs and the switch. No kit, no physics: the linkage
 * moves by geometry alone.
 *
 * What it deliberately does not do: arcs (Sketchpad's circles were
 * arcs, with their ends); constraints on instances (Sketchpad's
 * attachers, an instance's points given to the drawing it is in);
 * numbers -- a length typed, a distance -- which Sketchpad could also
 * do; the one-pass method Sutherland tried before relaxation; text;
 * saving (the TX-2 kept drawings on magnetic tape).
 *
 * Exercises: a constraint of your own (an error function in Relax, a
 * button here: nothing to derive, the derivatives are numerical); the
 * midpoint constraint, then a triangle's medians meeting; attachers,
 * so that instances of a resistor can be wired together (Sutherland's
 * circuit diagrams); the one-pass method, and the drawings it cannot
 * order.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode =
  | Draw
  | Draw_circle
  | Move
  | Delete
  | Fix
  | Horizontal
  | Vertical
  | Parallel
  | Perpendicular
  | Equal
  | Instance

let buttons =
  [
    (Draw, "DRAW");
    (Draw_circle, "CIRCLE");
    (Move, "MOVE");
    (Delete, "DELETE");
    (Fix, "FIX");
    (Horizontal, "HORIZONTAL");
    (Vertical, "VERTICAL");
    (Parallel, "PARALLEL");
    (Perpendicular, "PERPENDICULAR");
    (Equal, "EQUAL LENGTH");
    (Instance, "INSTANCE");
  ]

(* what the pen, held down, is moving *)
type drag =
  | Dragging_point of int
  | Dragging_line of int * Sketch.pos (* the line, the pen last here *)
  | Dragging_instance of int * Sketch.pos
  | Panning of Sketch.pos (* the window, the pen last here on screen *)

type model = {
  doc : Sketch.t;
  sheet : int;
  mode : mode;
  (* the point the line or circle being drawn starts from *)
  from : int option;
  (* the first line of a constraint on two *)
  first : int option;
  drag : drag option;
  (* the window over the sheet: its scale, and the sheet's point at its
     middle *)
  zoom : float;
  pan : Sketch.pos;
  (* the sheet INSTANCE places, and the instance the knobs change *)
  master : int;
  current : int option;
  show : bool;
  said : string;
  started : bool;
  was_down : bool;
  was_rdown : bool;
  was : string list;
}

let sheet_names = [ "A"; "B"; "C"; "D" ]

(*****************************************************************************)
(* The four sheets *)
(*****************************************************************************)

let add_points ps s =
  List.fold_left
    (fun (s, ids) p ->
      let s, id = Sketch.add_point p s in
      (s, ids @ [ id ]))
    (s, []) ps

let fix ids s = List.fold_left (fun s id -> Sketch.toggle_fixed id s) s ids
let constrain cs s = List.fold_left (fun s c -> Sketch.constrain c s) s cs

(* D, the construction: six corners, roughly round a circle of radius
   [r]; the circle's center and rim point fixed, the rim point the first
   corner; the corners on the circle and the sides equal *)
let rough_hexagon r =
  let k = r /. 60. in
  let s, ids = add_points [ (0., 0.); (r, 0.) ] Sketch.empty in
  let center = List.nth ids 0 and rim = List.nth ids 1 in
  let s, circle = Sketch.add_item (Sketch.Circle (center, rim)) (fix ids s) in
  let rough = [ (40., 45.); (-15., 75.); (-80., 20.); (-35., -55.); (45., -65.) ] in
  let s, others = add_points (List.map (fun (x, y) -> (k *. x, k *. y)) rough) s in
  let corners = rim :: others in
  let s, sides =
    List.fold_left
      (fun (s, ls) i ->
        let s, l = Sketch.add_item (Sketch.Line (List.nth corners i, List.nth corners ((i + 1) mod 6))) s in
        (s, ls @ [ l ]))
      (s, []) [ 0; 1; 2; 3; 4; 5 ]
  in
  let s = constrain (List.map (fun p -> Sketch.On_circle (p, circle)) corners) s in
  let s = constrain (List.map (fun l -> Sketch.Equal (List.hd sides, l)) (List.tl sides)) s in
  (s, circle)

(* B, the hexagon: the construction relaxed, then its circle erased --
   its constraints on the circle go with it, the equal sides stay *)
let hexagon =
  let s, circle = rough_hexagon 60. in
  let s = Relax.solve ~sweeps:1000 s in
  let center = match Sketch.ends s circle with Some (c, _) -> c | None -> 0 in
  Sketch.delete center (Sketch.delete circle s)

(* A, the honeycomb: instances of B, a flat-topped hexagon of radius 60,
   whose neighbours are 90 across and 52 up (60 * sqrt 3 / 2) *)
let honeycomb =
  let cell (x, y) = { Sketch.master = 1; at = (x, y +. 110.); size = 1.; angle = 0. } in
  let h = 60. *. Float.sqrt 3. /. 2. in
  let cells = [ (0., 0.); (0., 2. *. h); (0., -2. *. h); (90., h); (90., -.h); (-90., h); (-90., -.h) ] in
  let others =
    [
      { Sketch.master = 1; at = (-210., -190.); size = 0.5; angle = 0. };
      { Sketch.master = 1; at = (-80., -190.); size = 0.8; angle = 15. };
      { Sketch.master = 1; at = (110., -190.); size = 1.1; angle = 30. };
    ]
  in
  { Sketch.empty with instances = List.map cell cells @ others }

(* C, the linkage: two pivots, each a fixed circle's center; the cranks'
   ends on those circles; the rod between them as long as the reference
   line above; the ground line between the pivots *)
let linkage =
  let s, ids =
    add_points
      [ (-150., -80.); (-90., -80.); (150., -80.); (150., 40.); (-150., 260.); (100., 260.); (-150., -20.); (95.55, 26.9) ]
      Sketch.empty
  in
  let id i = List.nth ids i in
  let s = fix [ id 0; id 1; id 2; id 3; id 4; id 5 ] s in
  let item it s = Sketch.add_item it s in
  let s, c1 = item (Sketch.Circle (id 0, id 1)) s in
  let s, c2 = item (Sketch.Circle (id 2, id 3)) s in
  let s, reference = item (Sketch.Line (id 4, id 5)) s in
  let s, rod = item (Sketch.Line (id 6, id 7)) s in
  let s, _ = item (Sketch.Line (id 0, id 6)) s in
  let s, _ = item (Sketch.Line (id 2, id 7)) s in
  let s, _ = item (Sketch.Line (id 0, id 2)) s in
  (* the rod's second end where the two hold already (the circles'
     intersection, worked out once): the linkage at rest *)
  constrain [ Sketch.On_circle (id 6, c1); Sketch.On_circle (id 7, c2); Sketch.Equal (rod, reference) ] s

let initial =
  {
    doc = [ honeycomb; hexagon; linkage; fst (rough_hexagon 200.) ];
    sheet = 0;
    mode = Move;
    from = None;
    first = None;
    drag = None;
    zoom = 1.2;
    pan = (0., 0.);
    master = 1;
    current = None;
    show = true;
    said = "";
    started = false;
    was_down = false;
    was_rdown = false;
    was = [];
  }

let current_sheet m : Sketch.sheet = Sketch.sheet m.doc m.sheet
let with_sheet m (s : Sketch.sheet) = { m with doc = Sketch.set_sheet m.sheet s m.doc }

(*****************************************************************************)
(* The scope, the window over the sheet *)
(*****************************************************************************)

let scope_x = -100.
let scope_y = 40.
let scope_half = 390.

let in_scope (x, y) = Float.abs (x -. scope_x) <= scope_half && Float.abs (y -. scope_y) <= scope_half
let to_screen m (x, y) = (scope_x +. ((x -. fst m.pan) *. m.zoom), scope_y +. ((y -. snd m.pan) *. m.zoom))
let to_sheet m (x, y) = (fst m.pan +. ((x -. scope_x) /. m.zoom), snd m.pan +. ((y -. scope_y) /. m.zoom))

(* the pen aims within 10 pixels of the screen, whatever the zoom *)
let aim ?except m p = Sketch.aim m.doc m.sheet ~tolerance:(10. /. m.zoom) ?except p

(* the line nearest the pen, points or not: what a constraint's button
   wants *)
let line_at m p =
  let s = current_sheet m in
  List.fold_left
    (fun best (id, it) ->
      match it with
      | Sketch.Line (a, b) ->
          let d = Sketch.to_segment p (Sketch.pos s a) (Sketch.pos s b) in
          if d <= 10. /. m.zoom && (match best with Some (_, db) -> d < db | None -> true) then Some (id, d) else best
      | Sketch.Circle _ -> best)
    None s.items
  |> Option.map fst

(*****************************************************************************)
(* The pen *)
(*****************************************************************************)

(* the point the pen gives where it is: the one aimed at; a new one on
   the line or circle aimed at, constrained to stay there; or a new one *)
let point_at m p =
  let s = current_sheet m in
  let s, id =
    match aim m p with
    | Sketch.At_point id -> (s, id)
    | Sketch.On_item (it, q) ->
        let s, id = Sketch.add_point q s in
        let c = match List.assoc it s.items with Sketch.Line _ -> Sketch.On_line (id, it) | Sketch.Circle _ -> Sketch.On_circle (id, it) in
        (Sketch.constrain c s, id)
    | Sketch.At_instance _ | Sketch.Nothing -> Sketch.add_point p s
  in
  (with_sheet m s, id)

let terminate m = { m with from = None; first = None }

let set_instance m k f =
  let s = current_sheet m in
  with_sheet m { s with instances = List.mapi (fun i inst -> if i = k then f inst else inst) s.instances }

(* a two-line constraint: the first line waits for the second *)
let two_lines m p make =
  match line_at m p with
  | None -> m
  | Some l -> (
      match m.first with
      | None -> { m with first = Some l }
      | Some f when f = l -> m
      | Some f -> { (with_sheet m (Sketch.constrain (make f l) (current_sheet m))) with first = None })

let pen_down m p screen =
  match m.mode with
  | Draw -> (
      let m, id = point_at m p in
      match m.from with
      | Some f when f <> id -> { (with_sheet m (fst (Sketch.add_item (Sketch.Line (f, id)) (current_sheet m)))) with from = Some id }
      | _ -> { m with from = Some id })
  | Draw_circle -> (
      let m, id = point_at m p in
      match m.from with
      | Some c when c <> id -> { (with_sheet m (fst (Sketch.add_item (Sketch.Circle (c, id)) (current_sheet m)))) with from = None }
      | _ -> { m with from = Some id })
  | Move -> (
      match aim m p with
      | Sketch.At_point id -> { m with drag = Some (Dragging_point id) }
      | Sketch.On_item (l, _) -> (
          match List.assoc l (current_sheet m).items with
          | Sketch.Line _ -> { m with drag = Some (Dragging_line (l, p)) }
          | Sketch.Circle (c, _) -> { m with drag = Some (Dragging_point c) })
      | Sketch.At_instance k -> { m with drag = Some (Dragging_instance (k, p)); current = Some k }
      | Sketch.Nothing -> { m with drag = Some (Panning screen) })
  | Delete -> (
      let s = current_sheet m in
      match aim m p with
      | Sketch.At_point id | Sketch.On_item (id, _) -> with_sheet m (Sketch.delete id s)
      | Sketch.At_instance k ->
          { (with_sheet m { s with instances = List.filteri (fun i _ -> i <> k) s.instances }) with current = None }
      | Sketch.Nothing -> m)
  | Fix -> ( match aim m p with Sketch.At_point id -> with_sheet m (Sketch.toggle_fixed id (current_sheet m)) | _ -> m)
  | Horizontal -> ( match line_at m p with Some l -> with_sheet m (Sketch.constrain (Sketch.Horizontal l) (current_sheet m)) | None -> m)
  | Vertical -> ( match line_at m p with Some l -> with_sheet m (Sketch.constrain (Sketch.Vertical l) (current_sheet m)) | None -> m)
  | Parallel -> two_lines m p (fun a b -> Sketch.Parallel (a, b))
  | Perpendicular -> two_lines m p (fun a b -> Sketch.Perpendicular (a, b))
  | Equal -> two_lines m p (fun a b -> Sketch.Equal (a, b))
  | Instance -> (
      let inst = { Sketch.master = m.master; at = p; size = 1.; angle = 0. } in
      match Sketch.place m.sheet inst m.doc with
      | Some doc -> { m with doc; current = Some (List.length (current_sheet m).instances); said = "" }
      | None -> { m with said = "sheet " ^ List.nth sheet_names m.master ^ " shows this one: a drawing inside itself" })

let dragging m p screen =
  let s = current_sheet m in
  match m.drag with
  | Some (Dragging_point id) -> with_sheet m (Sketch.set_pos id p s)
  | Some (Dragging_line (l, (lx, ly))) -> (
      match Sketch.ends s l with
      | Some (a, b) ->
          let shift id s =
            let x, y = Sketch.pos s id in
            Sketch.set_pos id (x +. fst p -. lx, y +. snd p -. ly) s
          in
          { (with_sheet m (shift b (shift a s))) with drag = Some (Dragging_line (l, p)) }
      | None -> m)
  | Some (Dragging_instance (k, (lx, ly))) ->
      let m = set_instance m k (fun inst -> { inst with at = (fst inst.at +. fst p -. lx, snd inst.at +. snd p -. ly) }) in
      { m with drag = Some (Dragging_instance (k, p)) }
  | Some (Panning (lx, ly)) ->
      let dx = (fst screen -. lx) /. m.zoom and dy = (snd screen -. ly) /. m.zoom in
      { m with pan = (fst m.pan -. dx, snd m.pan -. dy); drag = Some (Panning screen) }
  | None -> m

(* a point dropped on another becomes it *)
let release m p =
  let m =
    match m.drag with
    | Some (Dragging_point id) -> (
        match aim ~except:id m p with
        | Sketch.At_point onto -> with_sheet m (Sketch.merge ~drop:id ~onto (current_sheet m))
        | _ -> m)
    | _ -> m
  in
  { m with drag = None }

(* Sutherland's relaxation, one sweep a frame on the sheet shown: a
   drawing settles as you watch (the TX-2 was not quicker) *)
let sweeps_per_frame = 1

(*****************************************************************************)
(* The panel *)
(*****************************************************************************)

let panel_x = 400.
let button_box i : Widget.box = { Widget.x = panel_x; y = 410. -. (float_of_int i *. 38.); w = 170.; h = 32. }
let terminate_box = button_box (List.length buttons)
let show_box = button_box (List.length buttons + 1)
let sheet_box i : Widget.box = { Widget.x = panel_x -. 63. +. (float_of_int i *. 42.); y = -110.; w = 38.; h = 32. }

let knob_box i : Widget.box =
  let w, h = Gui.knob_size () in
  { Widget.x = -400. +. (float_of_int i *. 150.); y = -425.; w; h }

let switch_box : Widget.box =
  let w, h = Gui.selector_size sheet_names in
  { Widget.x = panel_x; y = -235.; w; h }

let clicked press (b : Widget.box) (mouse : mouse) = press && Widget.contains b mouse.mx mouse.my

let panel computer press m =
  let mouse = computer.mouse in
  let m =
    List.fold_left
      (fun m (i, (mode, _)) -> if clicked press (button_box i) mouse then terminate { m with mode; said = "" } else m)
      m
      (List.mapi (fun i b -> (i, b)) buttons)
  in
  let m = if clicked press terminate_box mouse then terminate m else m in
  let m = if clicked press show_box mouse then { m with show = not m.show } else m in
  let m =
    List.fold_left
      (fun m i ->
        if clicked press (sheet_box i) mouse && i <> m.sheet then terminate { m with sheet = i; current = None; drag = None; said = "" }
        else m)
      m [ 0; 1; 2; 3 ]
  in
  let zoom = Gui.knob_in computer (knob_box 0) ~from:0.25 ~to_:4. m.zoom in
  let m = { m with zoom; master = Gui.selector_in computer switch_box sheet_names m.master } in
  let inst = match m.current with Some k -> List.nth_opt (current_sheet m).instances k | None -> None in
  let size0, angle0 = match inst with Some i -> (i.size, i.angle) | None -> (1., 0.) in
  let size = Gui.knob_in computer (knob_box 1) ~from:0.2 ~to_:3. size0 in
  let angle = Gui.knob_in computer (knob_box 2) ~from:(-180.) ~to_:180. angle0 in
  match m.current with
  | Some k when size <> size0 || angle <> angle0 -> set_instance m k (fun i -> { i with size; angle })
  | _ -> m

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let from_flags (computer : computer) m =
  let sheet =
    match List.assoc_opt "sheet" computer.flags with Some ("b" | "B") -> 1 | Some ("c" | "C") -> 2 | Some ("d" | "D") -> 3 | _ -> 0
  in
  { m with sheet; started = true }

let update computer m =
  let m = if m.started then m else from_flags computer m in
  let mouse = computer.mouse in
  let now = Set_.elements computer.keyboard.keys in
  let press = mouse.mdown && not m.was_down in
  let screen = (mouse.mx, mouse.my) in
  let p = to_sheet m screen in
  let m = panel computer press m in
  let m = if (List.mem "Escape" now && not (List.mem "Escape" m.was)) || (mouse.mrdown && not m.was_rdown) then terminate m else m in
  let m =
    if press && in_scope screen && not (Gui.modal ()) then pen_down m p screen
    else if mouse.mdown && m.drag <> None then dragging m p screen
    else if (not mouse.mdown) && m.drag <> None then release m p
    else m
  in
  (* nothing held: the pen puts a point where it is, then the
     constraints pull it back -- a crank's end dragged slides round its
     circle, and the rest of the linkage follows *)
  let m = with_sheet m (Relax.solve ~sweeps:sweeps_per_frame (current_sheet m)) in
  { m with was = now; was_down = mouse.mdown; was_rdown = mouse.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let phosphor = rgb 175 225 255
let dim = rgb 70 115 140
let bright = rgb 255 235 160
let console = rgb 58 60 56

(* a stroke as the scope drew it: a dot every [gap] pixels *)
let gap = 5.
let dot color size (x, y) = if in_scope (x, y) then [ square color size |> move x y ] else []

let dotted color size (ax, ay) (bx, by) =
  let n = max 1 (int_of_float (Float.hypot (bx -. ax) (by -. ay) /. gap)) in
  List.concat
    (List.init (n + 1) (fun i ->
         let t = float_of_int i /. float_of_int n in
         dot color size (ax +. (t *. (bx -. ax)), ay +. (t *. (by -. ay)))))

let stroke m color size = function
  | Sketch.Seg (a, b) -> dotted color size (to_screen m a) (to_screen m b)
  | Sketch.Round (c, r) ->
      let cx, cy = to_screen m c and r = r *. m.zoom in
      let n = max 12 (int_of_float (2. *. Float.pi *. r /. gap)) in
      List.concat
        (List.init n (fun i ->
             let a = 2. *. Float.pi *. float_of_int i /. float_of_int n in
             dot color size (cx +. (r *. Float.cos a), cy +. (r *. Float.sin a))))

(* a line of the sheet shown, lit *)
let line_dots m color size l =
  let s = current_sheet m in
  match Sketch.ends s l with Some (a, b) -> stroke m color size (Sketch.Seg (Sketch.pos s a, Sketch.pos s b)) | None -> []

let mid s l =
  match Sketch.ends s l with
  | Some (a, b) ->
      let (ax, ay), (bx, by) = (Sketch.pos s a, Sketch.pos s b) in
      ((ax +. bx) /. 2., (ay +. by) /. 2.)
  | None -> (0., 0.)

(* each constraint's symbol by what it holds: a line's beside it, two
   lines' between them, with dotted leaders; bright while it does not
   hold yet *)
let symbols m (s : Sketch.sheet) =
  let at color text (x, y) = if in_scope (x, y) then [ words color text |> scale 1.3 |> move x y ] else [] in
  List.concat_map
    (fun c ->
      let color = if Float.abs (Relax.error_of s c) > 0.5 then bright else dim in
      let beside text l =
        let x, y = to_screen m (mid s l) in
        at color text (x +. 12., y +. 12.)
      in
      let between text l1 l2 =
        let a = to_screen m (mid s l1) and b = to_screen m (mid s l2) in
        let c = ((fst a +. fst b) /. 2., (snd a +. snd b) /. 2.) in
        let leader (x, y) = List.filteri (fun i _ -> i mod 2 = 0) (dotted dim 1.5 c (x, y)) in
        leader a @ leader b @ at color text c
      in
      match c with
      | Sketch.Horizontal l -> beside "H" l
      | Sketch.Vertical l -> beside "V" l
      | Sketch.Parallel (a, b) -> between "//" a b
      | Sketch.Perpendicular (a, b) -> between "90" a b
      | Sketch.Equal (a, b) -> between "=" a b
      | Sketch.On_line _ | Sketch.On_circle _ -> [])
    s.constraints

(* the points: a fixed one as a square, a lone one as a dot *)
let points m (s : Sketch.sheet) =
  List.concat_map
    (fun (id, p) ->
      let x, y = to_screen m p in
      if List.mem id s.fixed then
        List.concat_map (fun (a, b) -> dotted phosphor 2. a b)
          [ ((x -. 5., y -. 5.), (x +. 5., y -. 5.)); ((x +. 5., y -. 5.), (x +. 5., y +. 5.)); ((x +. 5., y +. 5.), (x -. 5., y +. 5.)); ((x -. 5., y +. 5.), (x -. 5., y -. 5.)) ]
      else if List.exists (fun (it, _) -> match Sketch.ends s it with Some (a, b) -> a = id || b = id | None -> false) s.items then []
      else dot phosphor 4. (x, y))
    s.points

(* the tracking cross where the pen is, and what it aims at *)
let pen m (computer : computer) =
  let screen = (computer.mouse.mx, computer.mouse.my) in
  if not (in_scope screen) then []
  else
    let x, y = screen in
    let cross = List.concat_map (fun (dx, dy) -> dot phosphor 2. (x +. dx, y +. dy)) [ (-12., 0.); (-8., 0.); (8., 0.); (12., 0.); (0., -12.); (0., -8.); (0., 8.); (0., 12.) ] in
    let except = match m.drag with Some (Dragging_point id) -> Some id | _ -> None in
    let p = to_sheet m screen in
    let target =
      match m.mode with
      | Horizontal | Vertical | Parallel | Perpendicular | Equal -> (
          match line_at m p with Some l -> line_dots m bright 3. l | None -> [])
      | _ -> (
          match aim ?except m p with
          | Sketch.At_point id -> dot bright 8. (to_screen m (Sketch.pos (current_sheet m) id))
          | Sketch.On_item (_, q) -> dot bright 6. (to_screen m q)
          | Sketch.At_instance k -> (
              match List.nth_opt (current_sheet m).instances k with
              | Some inst -> List.concat_map (stroke m bright 3.) (Sketch.instance_strokes m.doc inst)
              | None -> [])
          | Sketch.Nothing -> [])
    in
    cross @ target

let button (b : Widget.box) label on =
  let face = if on then rgb 255 225 140 else rgb 196 190 172 in
  [ rectangle (rgb 20 20 20) (b.w +. 4.) (b.h +. 4.) |> move b.x b.y; rectangle face b.w b.h |> move b.x b.y; words black label |> scale 0.75 |> move b.x b.y ]

let mode_name m = match List.assoc_opt m.mode buttons with Some n -> n | None -> ""

let view computer m =
  let s = current_sheet m in
  let rubber =
    match (m.from, m.mode) with
    | Some f, Draw -> stroke m phosphor 2.5 (Sketch.Seg (Sketch.pos s f, to_sheet m (computer.mouse.mx, computer.mouse.my)))
    | Some c, Draw_circle ->
        let (cx, cy) as center = Sketch.pos s c and px, py = to_sheet m (computer.mouse.mx, computer.mouse.my) in
        stroke m phosphor 2.5 (Sketch.Round (center, Float.hypot (px -. cx) (py -. cy)))
    | _ -> []
  in
  let first = match m.first with Some l -> line_dots m bright 4. l | None -> [] in
  let status =
    Printf.sprintf "SHEET %s     %s     %d points  %d items  %d constraints  %d instances     error %.2f%s"
      (List.nth sheet_names m.sheet) (mode_name m) (List.length s.points) (List.length s.items) (List.length s.constraints)
      (List.length s.instances) (Relax.error s)
      (if m.said <> "" then "     " ^ m.said else "")
  in
  let label x y text = words (rgb 225 220 200) text |> scale 0.7 |> move x y in
  [
    rectangle console 1000. 1000.;
    rectangle (rgb 20 22 20) ((2. *. scope_half) +. 16.) ((2. *. scope_half) +. 16.) |> move scope_x scope_y;
    rectangle (rgb 4 8 10) (2. *. scope_half) (2. *. scope_half) |> move scope_x scope_y;
  ]
  @ List.concat_map (stroke m phosphor 2.5) (Sketch.strokes m.doc m.sheet)
  @ points m s @ first @ rubber
  @ (if m.show then symbols m s else [])
  @ pen m computer
  @ List.concat (List.mapi (fun i (mode, name) -> button (button_box i) name (mode = m.mode)) buttons)
  @ button terminate_box "TERMINATE" false
  @ button show_box "SHOW CONSTRAINTS" m.show
  @ [ label panel_x (-80.) "SHEET" ]
  @ List.concat (List.mapi (fun i name -> button (sheet_box i) name (i = m.sheet)) sheet_names)
  @ [ label panel_x (-165.) "INSTANCE OF" ]
  @ List.concat
      (List.mapi
         (fun i name ->
           let b = knob_box i in
           [ label b.x (b.y +. (b.h /. 2.) +. 14.) name ])
         [ "ZOOM"; "SIZE"; "TURN" ])
  @ [ label 400. (-420.) "SKETCHPAD  TX-2  1963"; words (rgb 225 220 200) status |> scale 0.65 |> move 0. (-478.) ]
  @ Gui.draw ()

let app = game view update initial
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
