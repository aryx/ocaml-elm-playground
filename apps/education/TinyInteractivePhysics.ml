(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Interactive Physics (Knowledge Revolution, 1989, for
 * the Macintosh; David Baszucki, with Erik Cassel, who went on to make
 * Roblox), the physics lab of the 1990s' classrooms, and of Working
 * Model (1993), its engineering edition: draw bodies, tie them with
 * springs, ropes, rods and pins, press Run, and watch them move while
 * the meters measure -- then rewind the tape and look again.
 *
 *   the tools (left)   Select (drag a body), Disk and Block (drag to
 *                      size one), Spring, Rope and Rod (drag from a
 *                      body, or the background, to another), Pin (click
 *                      on a body: to the background, or to the body
 *                      under it), Erase
 *   space              Run, Stop           r   Reset (back to the drawing)
 *   left, right        the tape, a frame at a time, when stopped
 *   1 2 3 4            the labs: pendulum, spring, ramp, collision
 *   the selected body  f fixed (anchored), [ ] its mass, b its
 *                      bounciness, u its friction, < > its angle
 *   v                  the velocity vectors
 *
 * flags lab=pendulum|spring|ramp|collision, run (running from the start).
 *
 * What it teaches is measurement: the meters (below the world) plot
 * the kinetic and potential energies and their sum frame by frame. On
 * the pendulum and the spring the two trade and the sum stays flat --
 * the conservation of energy, and a check of the integrator (the
 * engine's semi-implicit Euler, Integrate.mli, keeps it bounded
 * instead of letting it grow). On the ramp the sliding block's sum
 * falls (friction turns it into heat) while the rolling disk's does not
 * -- but its motion counts twice, moving and turning (1/2 I w^2: the
 * moment of inertia of a disk is m r^2 / 2, of a block m (w^2 + h^2) /
 * 12, computed here) -- until it drops off the ramp and lands, a
 * collision of clay taking a big piece at once. On the collision the momentum stays, whatever the
 * bounciness, and the energy only if it is 1.
 *
 * And the tape: the Model-View-Update architecture makes it almost
 * free. A frame of the simulation is a value (a Physics.world), so
 * recording is keeping every one in an array, and rewinding is showing
 * an older one; running again from there drops the future, as
 * Interactive Physics did.
 *
 * Units are SI, as Interactive Physics had them: 100 pixels a metre,
 * kilograms, g = 9.81 m/s^2, springs in newtons per metre.
 *
 * Uses: the Playground's Physics (a world: contacts solved together,
 * joints -- Joint2d's pin, rod and rope), springs as forces pushed
 * every tick (Hooke's law with damping, Springs.mli's formula, between
 * the bodies' centres), Scene2d (keys pressed); not the gui toolkit
 * (its buttons are drawn and hit here, a dozen lines).
 *
 * Exercises: the springs and ropes attached anywhere on a body, not at
 * its centre (the force at a point gives a torque too); polygons; an
 * initial velocity set by dragging the arrow; meters of your choosing
 * (a body's position, its speed, a spring's force) as Interactive
 * Physics had them; a motor on a pin (Physics.pin's ~motor); the tape
 * exported as numbers for a spreadsheet (TinyExcel); an integrator to
 * choose (explicit Euler, and the pendulum's energy climbing).
 *)
open Playground

(*****************************************************************************)
(* The drawing: bodies and what ties them *)
(*****************************************************************************)

let px_per_m = 100.
let g = 9.81

type thing = Disk of float (* radius *) | Block of float * float (* width, height *)

type obj = {
  thing : thing;
  x : float;
  y : float;
  angle : float; (* degrees *)
  vx : float; (* pixels a second *)
  vy : float;
  spin : float; (* degrees a second *)
  mass : float; (* kilograms *)
  bounciness : float;
  friction : float;
  fixed : bool;
  color : color;
}

(* an end of a spring, a rope or a rod: a body's centre, or a point of
 * the background *)
type end_ = Center of int | Point of (float * float)

type link =
  | Spring of { a : end_; b : end_; k : float (* N/m *); damping : float (* N s/m *); rest : float (* pixels *) }
  | Rope of end_ * end_
  | Rod of end_ * end_
  | Pin of int * int option * (float * float) (* a body, the other one (None: the background), where *)

type drawing = { objs : obj list; links : link list }

let palette = [| rgb 210 80 60; rgb 60 120 200; rgb 235 175 40; rgb 80 165 90; rgb 150 90 200 |]

let obj ?(angle = 0.) ?(vx = 0.) ?(mass = 1.) ?(bounciness = 0.) ?(friction = 0.3) ?(fixed = false) ?(color = 0) thing x y : obj =
  { thing; x; y; angle; vx; vy = 0.; spin = 0.; mass; bounciness; friction; fixed; color = palette.(color mod Array.length palette) }

(*****************************************************************************)
(* The labs *)
(*****************************************************************************)

let floor_top = -230.
let floor = obj (Block (900., 20.)) 40. (floor_top -. 10.) ~fixed:true ~friction:0.8

let labs : (string * drawing) list =
  [ ( "pendulum",
      (* a rod of 2.5 m, released at 60 degrees: a period of 2 pi
       * sqrt (L / g) = 3.2 s for small swings, a little longer here *)
      { objs = [ floor; obj (Disk 25.) (40. +. (250. *. sin (Float.pi /. 3.))) (380. -. (250. *. cos (Float.pi /. 3.))) ~color:1 ];
        links = [ Rod (Point (40., 380.), Center 1) ] } );
    ( "spring",
      (* 1 kg on 20 N/m: a period of 2 pi sqrt (m / k) = 1.4 s, around
       * where the spring holds the weight, stretched by m g / k = 0.49 m *)
      { objs = [ floor; obj (Block (60., 40.)) 40. 100. ~color:2 ];
        links = [ Spring { a = Point (40., 380.); b = Center 1; k = 20.; damping = 0.; rest = 150. } ] } );
    ( "ramp",
      (* a 20 degree slope: the block slides (losing energy), the disk
       * rolls (keeping it). Two frictions combine as the square root of
       * their product (Physics.rough): the block's 0.1 on the ramp's 0.8
       * is 0.28, under tan 20 = 0.36, so it slides; at 0.2 (0.4) it
       * would stay *)
      (let n = (sin (Float.pi /. 9.), cos (Float.pi /. 9.)) and d = (cos (Float.pi /. 9.), -.sin (Float.pi /. 9.)) in
       let on_ramp s h = (40. +. (s *. fst d) +. (h *. fst n), -60. +. (s *. snd d) +. (h *. snd n)) in
       let bx, by = on_ramp (-300.) 25. and dx, dy = on_ramp (-200.) 40. in
       { objs =
           [ floor;
             obj (Block (700., 20.)) 40. (-60.) ~angle:(-20.) ~fixed:true ~friction:0.8;
             obj (Block (20., 300.)) 480. (-80.) ~fixed:true;
             obj (Block (50., 30.)) bx by ~angle:(-20.) ~friction:0.1 ~color:2;
             obj (Disk 30.) dx dy ~friction:0.8 ~color:1 ];
         links = [] }) );
    ( "collision",
      (* 1 kg at 3 m/s into 2 kg at rest, perfectly elastic and
       * frictionless: 1 m/s back and 2 m/s on (momentum 3 before and
       * after, energy 4.5 J) *)
      { objs =
          [ { floor with friction = 0.; bounciness = 1. };
            obj (Block (20., 300.)) (-400.) (-80.) ~fixed:true ~bounciness:1. ~friction:0.;
            obj (Block (20., 300.)) 480. (-80.) ~fixed:true ~bounciness:1. ~friction:0.;
            obj (Disk 30.) (-250.) (floor_top +. 30.) ~vx:300. ~bounciness:1. ~friction:0. ~color:0;
            obj (Disk 30.) 150. (floor_top +. 30.) ~mass:2. ~bounciness:1. ~friction:0. ~color:1 ];
        links = [] } ) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tool = Select | Disk_tool | Block_tool | Spring_tool | Rope_tool | Rod_tool | Pin_tool | Erase

let tools =
  [ (Select, "Select"); (Disk_tool, "Disk"); (Block_tool, "Block"); (Spring_tool, "Spring"); (Rope_tool, "Rope");
    (Rod_tool, "Rod"); (Pin_tool, "Pin"); (Erase, "Erase") ]

(* a frame of the tape, and what the meters read of it *)
type frame = { world : Physics.world; ke : float; pe : float; momentum : float * float }

type lab = {
  name : string;
  drawing : drawing;
  tool : tool;
  tape : frame array; (* empty: drawing, not running *)
  at : int; (* the frame shown *)
  running : bool;
  selected : int option;
  gesture : (float * float) option; (* where the mouse went down, for a tool's drag *)
  was_down : bool;
  vectors : bool;
  started : bool; (* the flags read *)
}

type model = lab Scene2d.t

let start (name : string) (drawing : drawing) : lab =
  { name; drawing; tool = Select; tape = [||]; at = 0; running = false; selected = None; gesture = None; was_down = false;
    vectors = true; started = true }

let initial_model : model = Scene2d.start { (start "pendulum" (List.assoc "pendulum" labs)) with started = false }

(*****************************************************************************)
(* The simulation *)
(*****************************************************************************)

(* the background: an immovable body far below, what a joint to "the
 * world" is a joint to (Physics.mli); the drawing's bodies follow it *)
let background : Physics.body = Physics.body (circle white 1.) |> Physics.at 0. (-5000.) |> Physics.immovable

let body_of (o : obj) : Physics.body =
  let shape = match o.thing with Disk r -> circle white r | Block (w, h) -> rectangle white w h in
  let b =
    Physics.body shape |> Physics.at o.x o.y |> Physics.pointing o.angle |> Physics.moving o.vx o.vy |> Physics.turn o.spin
    |> Physics.heavy o.mass |> Physics.bouncy o.bounciness |> Physics.rough o.friction
  in
  if o.fixed then Physics.immovable b else b

(* the drawing's bodies where the world has them *)
let objs_of_world (d : drawing) (w : Physics.world) : obj list =
  List.mapi
    (fun i (o : obj) ->
      let b : Physics.body = List.nth w.bodies (i + 1) in
      { o with x = b.x; y = b.y; angle = b.angle; vx = b.vx; vy = b.vy; spin = b.spin })
    d.objs

let end_pos (objs : obj list) (e : end_) : float * float =
  match e with Center i -> let o = List.nth objs i in (o.x, o.y) | Point p -> p

let end_vel (objs : obj list) (e : end_) : float * float =
  match e with Center i -> let o = List.nth objs i in (o.vx, o.vy) | Point _ -> (0., 0.)

let end_body (e : end_) : int = match e with Center i -> i + 1 | Point _ -> 0

(* Hooke's law with damping, in newtons, positive when it pulls the
 * ends together: k times the stretch, plus c times the speed at which
 * the ends move apart *)
let spring_force (objs : obj list) (a : end_) (b : end_) ~(k : float) ~(damping : float) ~(rest : float) : float * (float * float) =
  let (xa, ya), (xb, yb) = (end_pos objs a, end_pos objs b) in
  let len = Float.max 1e-6 (Float.hypot (xb -. xa) (yb -. ya)) in
  let u = ((xb -. xa) /. len, (yb -. ya) /. len) in
  let (vax, vay), (vbx, vby) = (end_vel objs a, end_vel objs b) in
  let apart = (((vbx -. vax) *. fst u) +. ((vby -. vay) *. snd u)) /. px_per_m in
  ((k *. (len -. rest) /. px_per_m) +. (damping *. apart), u)

let build (d : drawing) : Physics.world =
  let w = Physics.world (background :: List.map body_of d.objs) in
  List.fold_left
    (fun w link ->
      match link with
      | Rod (a, b) -> Physics.rod (end_body a) (end_body b) ~at_a:(end_pos d.objs a) ~at_b:(end_pos d.objs b) w
      | Rope (a, b) -> Physics.rope (end_body a) (end_body b) ~at_a:(end_pos d.objs a) ~at_b:(end_pos d.objs b) w
      | Pin (i, other, at) -> Physics.pin (match other with Some j -> j + 1 | None -> 0) (i + 1) ~at w
      | Spring _ -> w)
    w d.links

(* a moment of inertia, kg m^2, from the textbook's formulas *)
let inertia (o : obj) : float =
  match o.thing with
  | Disk r -> o.mass *. (r /. px_per_m) ** 2. /. 2.
  | Block (w, h) -> o.mass *. (((w /. px_per_m) ** 2.) +. ((h /. px_per_m) ** 2.)) /. 12.

(* the meters: the kinetic energy (moving and turning), the potential
 * one (height above the floor, and the springs' stretch), the momentum *)
let measure (d : drawing) (w : Physics.world) : frame =
  let objs = objs_of_world d w in
  let moving = List.filter (fun (o : obj) -> not o.fixed) objs in
  let sum f = List.fold_left (fun acc o -> acc +. f o) 0. moving in
  let ke =
    sum (fun o ->
        let v = Float.hypot o.vx o.vy /. px_per_m and spin = o.spin *. Float.pi /. 180. in
        (o.mass *. v *. v /. 2.) +. (inertia o *. spin *. spin /. 2.))
  in
  let springs =
    List.fold_left
      (fun acc link ->
        match link with
        | Spring { a; b; k; rest; _ } ->
            let (xa, ya), (xb, yb) = (end_pos objs a, end_pos objs b) in
            let x = (Float.hypot (xb -. xa) (yb -. ya) -. rest) /. px_per_m in
            acc +. (k *. x *. x /. 2.)
        | _ -> acc)
      0. d.links
  in
  let pe = sum (fun o -> o.mass *. g *. (o.y -. floor_top) /. px_per_m) +. springs in
  { world = w; ke; pe; momentum = (sum (fun o -> o.mass *. o.vx /. px_per_m), sum (fun o -> o.mass *. o.vy /. px_per_m)) }

(* one tick: the springs push the bodies, then the world moves *)
let tick (d : drawing) (w : Physics.world) : Physics.world =
  let objs = objs_of_world d w in
  let pushes = Array.make (List.length w.bodies) (0., 0.) in
  let add i (fx, fy) = let px, py = pushes.(i) in pushes.(i) <- (px +. fx, py +. fy) in
  List.iter
    (fun link ->
      match link with
      | Spring { a; b; k; damping; rest } ->
          let f, (ux, uy) = spring_force objs a b ~k ~damping ~rest in
          (* newtons on kilograms, in pixels: Physics.push divides by the
           * mass *)
          add (end_body a) (f *. ux *. px_per_m, f *. uy *. px_per_m);
          add (end_body b) (-.f *. ux *. px_per_m, -.f *. uy *. px_per_m)
      | _ -> ())
    d.links;
  let bodies = List.mapi (fun i b -> let fx, fy = pushes.(i) in Physics.push fx fy b) w.bodies in
  Physics.simulate ~gravity:(g *. px_per_m) { w with bodies }

(* a minute of tape at most *)
let tape_length = 3600

(*****************************************************************************)
(* The screen's layout *)
(*****************************************************************************)

(* the tool buttons down the left, the tape's at the top, the meters at
 * the bottom; the world in between *)
let tool_button (i : int) : float * float = (-460., 400. -. (float_of_int i *. 62.))
let button_w = 70.
let button_h = 52.
let tape_buttons = [ ("run", -330.); ("reset", -230.); ("<", -150.); (">", -90.) ]
let tape_y = 470.
let meter_top = -262.

let inside (mx, my) (cx, cy) w h = Float.abs (mx -. cx) < w /. 2. && Float.abs (my -. cy) < h /. 2.

(* the topmost body under a point *)
let hit (objs : obj list) ((px, py) : float * float) : int option =
  let found = ref None in
  List.iteri
    (fun i (o : obj) ->
      let a = -.o.angle *. Float.pi /. 180. in
      let dx = px -. o.x and dy = py -. o.y in
      let lx = (dx *. cos a) -. (dy *. sin a) and ly = (dx *. sin a) +. (dy *. cos a) in
      let under = match o.thing with Disk r -> Float.hypot lx ly <= r | Block (w, h) -> Float.abs lx <= w /. 2. && Float.abs ly <= h /. 2. in
      if under then found := Some i)
    objs;
  !found

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let current (l : lab) : obj list =
  if Array.length l.tape = 0 then l.drawing.objs else objs_of_world l.drawing l.tape.(l.at).world

let run (l : lab) : lab =
  let tape = if Array.length l.tape = 0 then [| measure l.drawing (build l.drawing) |] else Array.sub l.tape 0 (l.at + 1) in
  { l with tape; running = true; gesture = None }

let reset (l : lab) : lab = { l with tape = [||]; at = 0; running = false }

(* the drawing without body [i], and without what was tied to it *)
let erase (d : drawing) (i : int) : drawing =
  let shift e = match e with Center j when j > i -> Center (j - 1) | e -> e in
  let on e = e = Center i in
  let links =
    List.filter_map
      (fun link ->
        match link with
        | Spring s -> if on s.a || on s.b then None else Some (Spring { s with a = shift s.a; b = shift s.b })
        | Rope (a, b) -> if on a || on b then None else Some (Rope (shift a, shift b))
        | Rod (a, b) -> if on a || on b then None else Some (Rod (shift a, shift b))
        | Pin (j, other, at) ->
            if j = i || other = Some i then None
            else
              let s j = if j > i then j - 1 else j in
              Some (Pin (s j, Option.map s other, at)))
      d.links
  in
  { objs = List.filteri (fun j _ -> j <> i) d.objs; links }

let update_obj (d : drawing) (i : int) (f : obj -> obj) : drawing = { d with objs = List.mapi (fun j o -> if j = i then f o else o) d.objs }

(* what a tool does when the mouse goes up, having gone down at [start] *)
let finish (l : lab) (start : float * float) ((mx, my) as p : float * float) : lab =
  let sx, sy = start in
  let d = l.drawing in
  let add o = { l with drawing = { d with objs = d.objs @ [ o ] }; selected = Some (List.length d.objs) } in
  let end_at q = match hit d.objs q with Some i -> Center i | None -> Point q in
  let tie make =
    let a = end_at start and b = end_at p in
    if Float.hypot (mx -. sx) (my -. sy) < 10. || (match (a, b) with Point _, Point _ -> true | _ -> a = b) then l
    else { l with drawing = { d with links = d.links @ [ make a b ] } }
  in
  let color = List.length d.objs in
  match l.tool with
  | Disk_tool ->
      let r = Float.hypot (mx -. sx) (my -. sy) in
      if r < 8. then l else add (obj (Disk r) sx sy ~color)
  | Block_tool ->
      let w = Float.abs (mx -. sx) and h = Float.abs (my -. sy) in
      if w < 8. || h < 8. then l else add (obj (Block (w, h)) ((sx +. mx) /. 2.) ((sy +. my) /. 2.) ~color)
  | Spring_tool ->
      tie (fun a b ->
          let (xa, ya), (xb, yb) = (end_pos d.objs a, end_pos d.objs b) in
          Spring { a; b; k = 20.; damping = 0.; rest = Float.hypot (xb -. xa) (yb -. ya) })
  | Rope_tool -> tie (fun a b -> Rope (a, b))
  | Rod_tool -> tie (fun a b -> Rod (a, b))
  | Select | Pin_tool | Erase -> l

(* what a tool does when the mouse goes down in the world *)
let press (l : lab) (p : float * float) : lab =
  let d = l.drawing in
  match l.tool with
  | Select -> { l with selected = hit (current l) p; gesture = Some p }
  | _ when Array.length l.tape > 0 -> l (* the drawing is changed only before running *)
  | Pin_tool -> (
      (* the bodies under the point, the topmost first *)
      let under = List.filter_map (fun i -> if hit [ List.nth d.objs i ] p = Some 0 then Some i else None) (List.init (List.length d.objs) Fun.id) in
      match List.rev under with
      | [] -> l
      | [ i ] -> { l with drawing = { d with links = d.links @ [ Pin (i, None, p) ] } }
      | i :: j :: _ -> { l with drawing = { d with links = d.links @ [ Pin (i, Some j, p) ] } })
  | Erase -> ( match hit d.objs p with Some i -> { l with drawing = erase d i; selected = None } | None -> l)
  | _ -> { l with gesture = Some p }

let clicked_button (l : lab) (p : float * float) : lab option =
  let tool =
    List.find_map (fun (i, (t, _)) -> if inside p (tool_button i) button_w button_h then Some t else None) (List.mapi (fun i t -> (i, t)) tools)
  in
  match tool with
  | Some t -> Some { l with tool = t; gesture = None }
  | None ->
      List.find_map
        (fun (name, x) ->
          if not (inside p (x, tape_y) (if name = "run" || name = "reset" then 90. else 50.) 40.) then None
          else
            match name with
            | "run" -> Some (if l.running then { l with running = false } else run l)
            | "reset" -> Some (reset l)
            | "<" -> Some { l with running = false; at = max 0 (l.at - 1) }
            | _ -> Some { l with running = false; at = min (Array.length l.tape - 1) (l.at + 1) })
        tape_buttons

let from_flags (flags : flags) (l : lab) : lab =
  let l = match Option.bind (List.assoc_opt "lab" flags) (fun n -> Option.map (fun d -> (n, d)) (List.assoc_opt n labs)) with
    | Some (n, d) -> start n d
    | None -> { l with started = true }
  in
  if List.mem_assoc "run" flags then run l else l

let edit_selected (l : lab) (key : string -> bool) : lab =
  match l.selected with
  | Some i when Array.length l.tape = 0 ->
      let cycle xs x = match List.find_opt (fun y -> y > x +. 1e-9) xs with Some y -> y | None -> List.hd xs in
      let f (o : obj) =
        if key "f" then { o with fixed = not o.fixed }
        else if key "]" then { o with mass = o.mass *. 2. }
        else if key "[" then { o with mass = Float.max 0.125 (o.mass /. 2.) }
        else if key "b" then { o with bounciness = cycle [ 0.; 0.5; 0.9; 1. ] o.bounciness }
        else if key "u" then { o with friction = cycle [ 0.; 0.2; 0.5; 0.8 ] o.friction }
        else if key "," then { o with angle = o.angle +. 5. }
        else if key "." then { o with angle = o.angle -. 5. }
        else o
      in
      { l with drawing = update_obj l.drawing i f }
  | _ -> l

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let key name = Scene2d.pressed (fun k -> Set_.mem name k.keys) m in
  let l = if m.scene.started then m.scene else from_flags computer.flags m.scene in
  let mouse = computer.mouse in
  let p = (mouse.mx, mouse.my) in
  let went_down = mouse.mdown && not l.was_down and went_up = l.was_down && not mouse.mdown in
  (* the keys *)
  let l =
    match List.find_opt (fun (i, _) -> key (string_of_int (i + 1))) (List.mapi (fun i lab -> (i, lab)) labs) with
    | Some (_, (name, d)) -> start name d
    | None -> l
  in
  let l = if Scene2d.pressed (fun k -> k.kspace) m then if l.running then { l with running = false } else run l else l in
  let l = if key "r" then reset l else l in
  let l = if key "v" then { l with vectors = not l.vectors } else l in
  let l = edit_selected l key in
  let l =
    if l.running || Array.length l.tape = 0 then l
    else if Scene2d.pressed (fun k -> k.kleft) m then { l with at = max 0 (l.at - 1) }
    else if Scene2d.pressed (fun k -> k.kright) m then { l with at = min (Array.length l.tape - 1) (l.at + 1) }
    else l
  in
  (* the mouse: a button, or the tool in the world *)
  let l =
    if went_down then match clicked_button l p with Some l -> l | None -> if snd p > meter_top then press l p else l
    else l
  in
  (* dragging a body, before running *)
  let l =
    match (l.tool, l.selected, l.gesture) with
    | Select, Some i, Some _ when mouse.mdown && Array.length l.tape = 0 ->
        { l with drawing = update_obj l.drawing i (fun o -> { o with x = o.x +. mouse.mdx; y = o.y +. mouse.mdy }) }
    | _ -> l
  in
  let l = if went_up then match l.gesture with Some start -> { (finish l start p) with gesture = None } | None -> l else l in
  (* the tape runs *)
  let l =
    if not l.running then l
    else if Array.length l.tape >= tape_length then { l with running = false }
    else
      let next = measure l.drawing (tick l.drawing l.tape.(Array.length l.tape - 1).world) in
      let tape = Array.append l.tape [| next |] in
      { l with tape; at = Array.length tape - 1 }
  in
  { m with scene = { l with was_down = mouse.mdown } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (c : color) (size : number) (s : string) : shape = words c s |> scale size
let ink = rgb 40 40 50
let paper = rgb 245 243 235

let segment (c : color) (width : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle c (Float.hypot dx dy) width |> rotate (atan2 dy dx *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let arrow (c : color) (x, y) (dx, dy) : shape list =
  let len = Float.hypot dx dy in
  if len < 3. then []
  else
    let head = polygon c [ (0., 0.); (-10., 5.); (-10., -5.) ] |> rotate (atan2 dy dx *. 180. /. Float.pi) |> move (x +. dx) (y +. dy) in
    [ segment c 2.5 (x, y) (x +. dx, y +. dy); head ]

(* a spring as a zigzag, its coils closer when compressed *)
let zigzag (c : color) (xa, ya) (xb, yb) : shape list =
  let len = Float.max 1e-6 (Float.hypot (xb -. xa) (yb -. ya)) in
  let ux, uy = ((xb -. xa) /. len, (yb -. ya) /. len) in
  let n = 16 in
  let point i =
    let t = float_of_int i /. float_of_int n in
    let side = if i = 0 || i = n then 0. else if i mod 2 = 0 then 9. else -9. in
    (xa +. ((xb -. xa) *. t) -. (uy *. side), ya +. ((yb -. ya) *. t) +. (ux *. side))
  in
  List.init n (fun i -> segment c 2. (point i) (point (i + 1)))

let draw_obj (selected : bool) (o : obj) : shape =
  let c = if o.fixed then rgb 125 125 135 else o.color in
  let body =
    match o.thing with
    | Disk r ->
        (* a spoke, to see it turn *)
        [ circle c r; rectangle (rgb 255 255 255) r 3. |> move_x (r /. 2.) |> fade 0.7 ]
    | Block (w, h) -> [ rectangle c w h ]
  in
  let outline =
    if not selected then []
    else [ (match o.thing with Disk r -> circle orange (r +. 4.) | Block (w, h) -> rectangle orange (w +. 8.) (h +. 8.)) ]
  in
  group (outline @ body) |> rotate o.angle |> move o.x o.y

let draw_link (drawn : obj list) (objs : obj list) (link : link) : shape list =
  let anchor e = match e with Point (x, y) -> [ square ink 10. |> move x y ] | Center _ -> [] in
  match link with
  | Spring { a; b; _ } -> zigzag (rgb 60 60 60) (end_pos objs a) (end_pos objs b) @ anchor a @ anchor b
  | Rope (a, b) -> (segment (rgb 150 110 60) 2. (end_pos objs a) (end_pos objs b) :: anchor a) @ anchor b
  | Rod (a, b) -> (segment ink 5. (end_pos objs a) (end_pos objs b) :: anchor a) @ anchor b
  | Pin (i, _, (px, py)) ->
      (* the pin moves and turns with its body: where it was drawn, in
       * the body's frame then, put back in its frame now *)
      let o0 = List.nth drawn i and o = List.nth objs i in
      let a = (o.angle -. o0.angle) *. Float.pi /. 180. in
      let dx = px -. o0.x and dy = py -. o0.y in
      let x = o.x +. (dx *. cos a) -. (dy *. sin a) and y = o.y +. (dx *. sin a) +. (dy *. cos a) in
      [ circle ink 6. |> move x y; circle (rgb 255 255 255) 3. |> move x y ]

(* the meters: the energies over the tape, and the momentum *)
let meters (l : lab) (w : number) : shape list =
  let n = Array.length l.tape in
  let gx = -470. and gw = 540. and gy = -470. and gh = 170. in
  let frame = [ rectangle (rgb 255 255 255) gw gh |> move (gx +. (gw /. 2.)) (gy +. (gh /. 2.)); segment ink 1. (gx, gy) (gx +. gw, gy) ] in
  let ty = gy +. gh +. 12. in
  let title =
    group [ text ink 1.3 "ENERGY (J):" |> move (gx +. 60.) ty; text (rgb 210 60 50) 1.3 "kinetic" |> move (gx +. 180.) ty;
            text (rgb 50 90 200) 1.3 "potential" |> move (gx +. 280.) ty; text ink 1.3 "total" |> move (gx +. 370.) ty ]
  in
  if n = 0 then frame @ [ title; text (rgb 120 120 120) 1.3 "press Run (space)" |> move (gx +. (gw /. 2.)) (gy +. (gh /. 2.)) ]
  else
    let top = Array.fold_left (fun a f -> Float.max a (Float.max (Float.abs f.ke) (Float.abs (f.ke +. f.pe)))) 0.1 l.tape in
    let top = Array.fold_left (fun a f -> Float.max a (Float.abs f.pe)) top l.tape in
    (* ten seconds across, or the whole tape if longer *)
    let span = max 600 n in
    let x i = gx +. (gw *. float_of_int i /. float_of_int span) in
    let y v = gy +. (gh *. 0.95 *. v /. top) in
    let stride = max 1 (n / 280) in
    let curve c f =
      List.init ((n - 1) / stride) (fun k -> let i = k * stride and j = min (n - 1) ((k + 1) * stride) in segment c 2. (x i, y (f l.tape.(i))) (x j, y (f l.tape.(j))))
    in
    let now = l.tape.(l.at) in
    let px, py = now.momentum in
    frame
    @ curve (rgb 210 60 50) (fun f -> f.ke) @ curve (rgb 50 90 200) (fun f -> f.pe) @ curve ink (fun f -> f.ke +. f.pe)
    @ [ segment (rgb 235 150 40) 1.5 (x l.at, gy) (x l.at, gy +. gh); title;
        text ink 1.3 (Printf.sprintf "t %.2f s   kinetic %.2f   potential %.2f   total %.2f" (float_of_int l.at /. 60.) now.ke now.pe (now.ke +. now.pe))
        |> move ((w /. 2.) -. 210.) (gy +. gh -. 20.);
        text ink 1.3 (Printf.sprintf "momentum  %.2f, %.2f kg m/s" px py) |> move ((w /. 2.) -. 210.) (gy +. gh -. 50.) ]

(* what the selected body is, and is doing: two lines *)
let readout (l : lab) (objs : obj list) : string * string =
  match l.selected with
  | Some i when i < List.length objs ->
      let o = List.nth objs i in
      let what = match o.thing with Disk r -> Printf.sprintf "disk r %.2f m" (r /. px_per_m) | Block (w, h) -> Printf.sprintf "block %.2f x %.2f m" (w /. px_per_m) (h /. px_per_m) in
      let mass = if o.fixed then "fixed" else Printf.sprintf "%.3g kg" o.mass in
      ( Printf.sprintf "%s  %s  bounce %.1f  friction %.1f" what mass o.bounciness o.friction,
        Printf.sprintf "x %.2f  y %.2f m  v %.2f m/s" (o.x /. px_per_m) ((o.y -. floor_top) /. px_per_m) (Float.hypot o.vx o.vy /. px_per_m) )
  | _ -> ("select a body to read it", "")

let view (computer : computer) (m : model) : shape list =
  let l = m.scene in
  let w = computer.screen.width and h = computer.screen.height in
  let objs = current l in
  let world =
    List.concat_map (draw_link l.drawing.objs objs) l.drawing.links
    @ List.mapi (fun i o -> draw_obj (l.selected = Some i) o) objs
    @ (if l.vectors then List.concat_map (fun (o : obj) -> if o.fixed then [] else arrow (rgb 30 140 60) (o.x, o.y) (o.vx /. 4., o.vy /. 4.)) objs else [])
  in
  (* what the mouse is drawing *)
  let preview =
    match l.gesture with
    | Some (sx, sy) when computer.mouse.mdown -> (
        let mx, my = (computer.mouse.mx, computer.mouse.my) in
        match l.tool with
        | Disk_tool -> [ circle (rgb 120 120 120) (Float.hypot (mx -. sx) (my -. sy)) |> fade 0.4 |> move sx sy ]
        | Block_tool -> [ rectangle (rgb 120 120 120) (Float.abs (mx -. sx)) (Float.abs (my -. sy)) |> fade 0.4 |> move ((sx +. mx) /. 2.) ((sy +. my) /. 2.) ]
        | Spring_tool | Rope_tool | Rod_tool -> [ segment (rgb 120 120 120) 2. (sx, sy) (mx, my) ]
        | _ -> [])
    | _ -> []
  in
  let tool_buttons =
    List.mapi
      (fun i (t, name) ->
        let x, y = tool_button i in
        group [ rectangle (if t = l.tool then rgb 60 90 160 else rgb 215 215 205) button_w button_h;
                text (if t = l.tool then white else ink) 1.2 name ]
        |> move x y)
      tools
  in
  let tape =
    List.map
      (fun (name, x) ->
        let label = if name = "run" then (if l.running then "Stop" else "Run") else if name = "reset" then "Reset" else name in
        group [ rectangle (rgb 215 215 205) (if name = "run" || name = "reset" then 90. else 50.) 36.; text ink 1.4 label ] |> move x tape_y)
      tape_buttons
  in
  let status =
    if Array.length l.tape = 0 then Printf.sprintf "%s: drawing" (String.uppercase_ascii l.name)
    else Printf.sprintf "%s: frame %d / %d, t = %.2f s%s" (String.uppercase_ascii l.name) l.at (Array.length l.tape - 1) (float_of_int l.at /. 60.) (if l.running then "" else ", stopped")
  in
  [ rectangle paper w h ]
  @ world @ preview
  @ [ rectangle (rgb 230 228 218) w (h /. 2. +. meter_top) |> move_y ((-.h /. 4.) +. (meter_top /. 2.)) ]
  @ meters l w
  @ [ rectangle (rgb 230 228 218) 90. ((h /. 2.) -. meter_top) |> move ((-.w /. 2.) +. 45.) (((h /. 2.) +. meter_top) /. 2.) ]
  @ tool_buttons @ tape
  @ [ text ink 1.6 "TINY INTERACTIVE PHYSICS" |> move 180. (tape_y +. 5.);
      text ink 1.3 status |> move 180. (tape_y -. 22.);
      text ink 1.3 (fst (readout l objs)) |> move ((w /. 2.) -. 210.) (-.h /. 2. +. 90.);
      text ink 1.3 (snd (readout l objs)) |> move ((w /. 2.) -. 210.) (-.h /. 2. +. 62.);
      text (rgb 110 110 110) 1.2 "space run/stop  r reset  <- -> tape  1-4 labs  f fixed  [ ] mass  b bounce  u friction  , . angle  v vectors"
      |> move 40. ((-.h /. 2.) +. 12.) ]

let app = game view update initial_model

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
