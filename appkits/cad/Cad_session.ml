(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cad_session.mli *)

module G = Cad_geom
module D = Cad_drawing

type pt = G.pt
type view = { center : pt; upp : float; w : float; h : float }
type input = Text of string | Pick of pt | Cancel
type want = Idle | Point | Objects | Other
type io = Save of string * string | Load of string

(*****************************************************************************)
(* The states *)
(*****************************************************************************)

(* what a selection of objects is gathered for *)
type after = For_erase | For_move of bool (* a copy? *) | For_block of string * pt | For_edges of bool (* EXTEND? *)

(* each command's steps: where it is, and what it has been told *)
type state =
  | Line_from
  | Line_to of { first : pt; last : pt; segs : (int * pt) list (* the lines drawn, newest first, and their starts *) }
  | Circle_center
  | Circle_radius of pt * bool (* the center; a diameter asked? *)
  | Arc_points of pt list
  | Select of { after : after; picked : int list; corner : pt option }
  | Base of bool * int list
  | Second of bool * int list * pt
  | Offset_distance
  | Offset_pick
  | Offset_side of int
  | Trim_pick of bool * int list (* EXTEND?, the edges *)
  | Fillet_first
  | Fillet_radius
  | Fillet_second of int * pt
  | Dim_points of pt list
  | Block_name
  | Block_base of string
  | Insert_name
  | Insert_point of string
  | Insert_scale of string * pt
  | Insert_rotation of string * pt * float
  | Layer_option
  | Layer_name of string (* the option: M, S, ON, OFF *)
  | Layer_color
  | Layer_color_name of int
  | Zoom_option
  | Zoom_corner of pt option
  | Pan_points of pt option
  | Id_point
  | Dist_points of pt option
  | File_name of bool (* writing? *)

(* the drawing before the command: what U gives back *)
type run = { name : string; state : state; before : D.t }

type t = {
  drawing : D.t;
  view : view;
  previous_view : view option;
  run : run option;
  last_command : string option;
  last_point : pt option;
  undo : (string * D.t) list;
  redo : (string * D.t) list;
  log : string list;
  fillet_radius : float;
  offset_distance : float;
  file : string;
  io : io option;
}

let start drawing view =
  {
    drawing;
    view;
    previous_view = None;
    run = None;
    last_command = None;
    last_point = None;
    undo = [];
    redo = [];
    log = [];
    fillet_radius = 0.;
    offset_distance = 1.;
    file = "drawing";
    io = None;
  }

let drawing t = t.drawing
let view t = t.view
let log t = t.log
let io t = t.io
let running t = Option.map (fun r -> r.name) t.run
let resize w h t = { t with view = { t.view with w; h } }

(* AutoCAD's four decimals *)
let fmt x = Printf.sprintf "%.4f" x

let say line t =
  let log = t.log @ [ line ] in
  let n = List.length log in
  { t with log = (if n > 200 then List.filteri (fun i _ -> i >= n - 200) log else log) }

(* the pick box: a click this near an entity picks it *)
let pickbox t = 6. *. t.view.upp

(*****************************************************************************)
(* Prompts *)
(*****************************************************************************)

let prompt t =
  match t.run with
  | None -> "Command:"
  | Some r -> (
      match r.state with
      | Line_from -> "From point:"
      | Line_to _ -> "To point:"
      | Circle_center -> "<Center point>:"
      | Circle_radius (_, false) -> "Diameter/<Radius>:"
      | Circle_radius (_, true) -> "Diameter:"
      | Arc_points [] -> "<Start point>:"
      | Arc_points [ _ ] -> "<Second point>:"
      | Arc_points _ -> "End point:"
      | Select { corner = None; _ } -> "Select objects:"
      | Select { corner = Some _; _ } -> "Other corner:"
      | Base _ -> "Base point or displacement:"
      | Second _ -> "Second point of displacement:"
      | Offset_distance -> Printf.sprintf "Offset distance <%s>:" (fmt t.offset_distance)
      | Offset_pick -> "Select object to offset:"
      | Offset_side _ -> "Side to offset?"
      | Trim_pick (false, _) -> "<Select object to trim>:"
      | Trim_pick (true, _) -> "<Select object to extend>:"
      | Fillet_first -> "Radius/<Select first object>:"
      | Fillet_radius -> Printf.sprintf "Enter fillet radius <%s>:" (fmt t.fillet_radius)
      | Fillet_second _ -> "Select second object:"
      | Dim_points [] -> "First extension line origin:"
      | Dim_points [ _ ] -> "Second extension line origin:"
      | Dim_points _ -> "Dimension line location:"
      | Block_name | Insert_name -> "Block name (or ?):"
      | Block_base _ -> "Insertion base point:"
      | Insert_point _ -> "Insertion point:"
      | Insert_scale _ -> "X scale factor <1>:"
      | Insert_rotation _ -> "Rotation angle <0>:"
      | Layer_option -> "?/Make/Set/ON/OFF/Color:"
      | Layer_name ("M" | "S") -> Printf.sprintf "New current layer <%s>:" t.drawing.current
      | Layer_name "ON" -> "Layer name(s) to turn On:"
      | Layer_name _ -> "Layer name(s) to turn Off:"
      | Layer_color -> "Color:"
      | Layer_color_name c -> Printf.sprintf "Layer name(s) for color %d <%s>:" c t.drawing.current
      | Zoom_option -> "All/Extents/Previous/Window/<Scale(X)>:"
      | Zoom_corner None -> "First corner:"
      | Zoom_corner (Some _) -> "Other corner:"
      | Pan_points None -> "Displacement:"
      | Pan_points (Some _) -> "Second point:"
      | Id_point -> "Point:"
      | Dist_points None -> "First point:"
      | Dist_points (Some _) -> "Second point:"
      | File_name _ -> Printf.sprintf "File name <%s>:" t.file)

let wants t =
  match t.run with
  | None -> Idle
  | Some r -> (
      match r.state with
      | Select _ | Offset_pick | Trim_pick _ | Fillet_first | Fillet_second _ -> Objects
      | Block_name | Insert_name | Layer_option | Layer_name _ | Layer_color | Layer_color_name _ | File_name _
      | Offset_distance | Fillet_radius | Insert_scale _ | Zoom_option ->
          Other
      | _ -> Point)

(*****************************************************************************)
(* Points typed *)
(*****************************************************************************)

(* "x,y", "@dx,dy", "@d<a", "d<a"; a distance alone, towards the cursor
   from the last point *)
let parse_point t ?cursor s =
  let s = String.trim s in
  let rel = String.length s > 0 && s.[0] = '@' in
  let s = if rel then String.sub s 1 (String.length s - 1) else s in
  let base = if rel then Option.value t.last_point ~default:(0., 0.) else (0., 0.) in
  let num x = float_of_string_opt (String.trim x) in
  match String.index_opt s '<' with
  | Some i -> (
      match (num (String.sub s 0 i), num (String.sub s (i + 1) (String.length s - i - 1))) with
      | Some d, Some a -> Some (G.polar base d a)
      | _ -> None)
  | None -> (
      match String.split_on_char ',' s with
      | [ x; y ] | [ x; y; _ ] -> (
          match (num x, num y) with Some x, Some y -> Some (G.add base (x, y)) | _ -> None)
      | [ d ] when not rel -> (
          match (num d, t.last_point, cursor) with
          | Some d, Some last, Some c when G.dist last c > 0. -> Some (G.polar last d (G.angle last c))
          | _ -> None)
      | _ -> None)

let point_of t ?cursor = function Pick p -> Some p | Text s -> parse_point t ?cursor s | Cancel -> None

(* a number typed; or two points (the last and one picked): their
   distance *)
let number_of t = function
  | Text s -> float_of_string_opt (String.trim s)
  | Pick p -> Option.map (fun l -> G.dist l p) t.last_point
  | Cancel -> None

(*****************************************************************************)
(* The view *)
(*****************************************************************************)

let fit t (ax, ay) (bx, by) =
  let w = Float.abs (bx -. ax) and h = Float.abs (by -. ay) in
  let upp = Float.max (w /. t.view.w) (h /. t.view.h) *. 1.1 in
  let upp = if upp > 0. then upp else t.view.upp in
  { t with previous_view = Some t.view; view = { t.view with center = ((ax +. bx) /. 2., (ay +. by) /. 2.); upp } }

let zoom_extents t = match D.extents t.drawing with Some (a, b) -> fit t a b | None -> t

(*****************************************************************************)
(* Editing helpers *)
(*****************************************************************************)

(* an entity added on a given layer *)
let add_on layer e (d : D.t) =
  let current = d.current in
  let d, id = D.add e { d with current = layer } in
  ({ d with current }, id)

let entity t id = Option.map (fun (e : D.ent) -> e.entity) (D.get t.drawing id)
let layer_of t id = match D.get t.drawing id with Some e -> e.layer | None -> t.drawing.current

let circumcenter (ax, ay) (bx, by) (cx, cy) =
  let d = 2. *. ((ax *. (by -. cy)) +. (bx *. (cy -. ay)) +. (cx *. (ay -. by))) in
  if Float.abs d < 1e-12 then None
  else
    let a2 = (ax *. ax) +. (ay *. ay) and b2 = (bx *. bx) +. (by *. by) and c2 = (cx *. cx) +. (cy *. cy) in
    Some
      ( ((a2 *. (by -. cy)) +. (b2 *. (cy -. ay)) +. (c2 *. (ay -. by))) /. d,
        ((a2 *. (cx -. bx)) +. (b2 *. (ax -. cx)) +. (c2 *. (bx -. ax))) /. d )

(* the arc from a through b to c *)
let arc3 a b c =
  match circumcenter a b c with
  | None -> None
  | Some o ->
      let s = G.angle o a and e = G.angle o c in
      if G.within s e (G.angle o b) then Some (D.Arc (o, G.dist o a, s, e)) else Some (D.Arc (o, G.dist o a, e, s))

let color_of s =
  match String.uppercase_ascii (String.trim s) with
  | "RED" -> Some 1
  | "YELLOW" -> Some 2
  | "GREEN" -> Some 3
  | "CYAN" -> Some 4
  | "BLUE" -> Some 5
  | "MAGENTA" -> Some 6
  | "WHITE" -> Some 7
  | s -> ( match int_of_string_opt s with Some c when c >= 1 && c <= 255 -> Some c | _ -> None)

(*****************************************************************************)
(* Starting and ending commands *)
(*****************************************************************************)

let aliases =
  [
    ("L", "LINE"); ("C", "CIRCLE"); ("A", "ARC"); ("E", "ERASE"); ("M", "MOVE"); ("CO", "COPY"); ("CP", "COPY");
    ("O", "OFFSET"); ("TR", "TRIM"); ("EX", "EXTEND"); ("F", "FILLET"); ("DLI", "DIMLINEAR"); ("B", "BLOCK");
    ("I", "INSERT"); ("LA", "LAYER"); ("Z", "ZOOM"); ("P", "PAN"); ("DI", "DIST"); ("UNDO", "U"); ("SAVE", "DXFOUT");
    ("OPEN", "DXFIN");
  ]

let set t state = match t.run with Some r -> { t with run = Some { r with state } } | None -> t

(* the command over: an undo step if it changed the drawing *)
let finish t =
  match t.run with
  | None -> t
  | Some r ->
      let t = { t with run = None } in
      (* by value, but for the ids' counter: a line drawn then taken
         back by U changed nothing *)
      if { t.drawing with next = 0 } <> { r.before with next = 0 } then { t with undo = (r.name, r.before) :: t.undo; redo = [] } else t

let begin_ name t =
  let name = String.uppercase_ascii (String.trim name) in
  let name = Option.value (List.assoc_opt name aliases) ~default:name in
  let run state = { t with run = Some { name; state; before = t.drawing }; last_command = Some name } in
  match name with
  | "LINE" -> run Line_from
  | "CIRCLE" -> run Circle_center
  | "ARC" -> run (Arc_points [])
  | "ERASE" -> run (Select { after = For_erase; picked = []; corner = None })
  | "MOVE" -> run (Select { after = For_move false; picked = []; corner = None })
  | "COPY" -> run (Select { after = For_move true; picked = []; corner = None })
  | "OFFSET" -> run Offset_distance
  | "TRIM" -> say "Select cutting edges:" (run (Select { after = For_edges false; picked = []; corner = None }))
  | "EXTEND" -> say "Select boundary edges:" (run (Select { after = For_edges true; picked = []; corner = None }))
  | "FILLET" -> run Fillet_first
  | "DIMLINEAR" -> run (Dim_points [])
  | "BLOCK" -> run Block_name
  | "INSERT" -> run Insert_name
  | "LAYER" -> run Layer_option
  | "ZOOM" -> run Zoom_option
  | "PAN" -> run (Pan_points None)
  | "ID" -> run Id_point
  | "DIST" -> run (Dist_points None)
  | "DXFOUT" -> run (File_name true)
  | "DXFIN" -> run (File_name false)
  | "U" -> (
      match t.undo with
      | (n, d) :: rest -> say n { t with drawing = d; undo = rest; redo = (n, t.drawing) :: t.redo; last_command = Some "U" }
      | [] -> say "Everything has been undone" t)
  | "REDO" -> (
      match t.redo with
      | (n, d) :: rest -> say n { t with drawing = d; redo = rest; undo = (n, t.drawing) :: t.undo }
      | [] -> say "Previous command did not undo things" t)
  | _ -> say (Printf.sprintf "Unknown command \"%s\".  Type ? for list of commands." name) t

let commands =
  "LINE CIRCLE ARC ERASE MOVE COPY OFFSET TRIM EXTEND FILLET DIMLINEAR BLOCK INSERT LAYER ZOOM PAN ID DIST U REDO DXFOUT DXFIN"

let command name t =
  let t = if t.run <> None then finish (say "*Cancel*" t) else t in
  begin_ name (say ("Command: " ^ String.uppercase_ascii name) t)

(*****************************************************************************)
(* The steps *)
(*****************************************************************************)

let selection_step t after picked corner i =
  let select picked = set t (Select { after; picked; corner = None }) in
  let found ids =
    let fresh = List.filter (fun id -> not (List.mem id picked)) ids in
    say (Printf.sprintf "%d found" (List.length ids)) (select (picked @ fresh))
  in
  match (i, corner) with
  | Pick p, None -> (
      match D.pick t.drawing (pickbox t) p with
      | Some id -> found [ id ]
      | None -> set t (Select { after; picked; corner = Some p }))
  | Pick q, Some p -> found (D.window t.drawing p q ~crossing:(fst q < fst p))
  | Text s, None when String.uppercase_ascii (String.trim s) = "ALL" -> found (D.ids t.drawing)
  | Text s, None when String.uppercase_ascii (String.trim s) = "L" -> (
      match List.rev (D.ids t.drawing) with id :: _ -> found [ id ] | [] -> t)
  | Text "", _ -> (
      (* Enter: the selection is made *)
      match after with
      | For_erase -> finish { t with drawing = D.remove picked t.drawing }
      | For_move copy -> if picked = [] then finish t else set t (Base (copy, picked))
      | For_block (name, base) ->
          if picked = [] then finish t
          else
            let es = List.filter_map (D.get t.drawing) picked in
            let blocks = List.remove_assoc name t.drawing.blocks @ [ (name, (base, es)) ] in
            finish { t with drawing = { (D.remove picked t.drawing) with blocks } }
      | For_edges extend -> set t (Trim_pick (extend, picked)))
  | Text _, _ -> say "*Invalid selection*" t
  | Cancel, _ -> t

let move_objects t copy objs d =
  List.fold_left
    (fun (dr : D.t) id ->
      match D.get dr id with
      | Some e ->
          let moved = D.move_entity d e.entity in
          if copy then fst (add_on e.layer moved dr) else D.replace id [ moved ] dr
      | None -> dr)
    t.drawing objs

let step ?cursor t r i =
  let point () = point_of t ?cursor i in
  let with_point f = match point () with Some p -> f p | None -> say "Invalid point." t in
  let remember p t = { t with last_point = Some p } in
  match r.state with
  | Line_from -> with_point (fun p -> remember p (set t (Line_to { first = p; last = p; segs = [] })))
  | Line_to { first; last; segs } -> (
      match i with
      | Text "" -> finish t
      | Text s when String.uppercase_ascii (String.trim s) = "C" ->
          if List.length segs < 2 then say "Invalid point." t
          else finish (remember first { t with drawing = fst (D.add (D.Line (last, first)) t.drawing) })
      | Text s when String.uppercase_ascii (String.trim s) = "U" -> (
          match segs with
          | (id, from) :: rest -> remember from (set { t with drawing = D.remove [ id ] t.drawing } (Line_to { first; last = from; segs = rest }))
          | [] -> say "All segments already undone." t)
      | _ ->
          with_point (fun p ->
              let drawing, id = D.add (D.Line (last, p)) t.drawing in
              remember p (set { t with drawing } (Line_to { first; last = p; segs = (id, last) :: segs }))))
  | Circle_center -> with_point (fun c -> remember c (set t (Circle_radius (c, false))))
  | Circle_radius (c, diameter) -> (
      match i with
      | Text s when String.uppercase_ascii (String.trim s) = "D" -> set t (Circle_radius (c, true))
      | _ -> (
          match (match i with Pick p -> Some (G.dist c p) | _ -> number_of t i) with
          | Some v when v > 0. -> finish { t with drawing = fst (D.add (D.Circle (c, if diameter then v /. 2. else v)) t.drawing) }
          | _ -> say "Requires numeric distance or second point." t))
  | Arc_points ps ->
      with_point (fun p ->
          match ps @ [ p ] with
          | [ a; b; c ] -> (
              match arc3 a b c with
              | Some arc -> finish (remember p { t with drawing = fst (D.add arc t.drawing) })
              | None -> say "Points are collinear." t)
          | ps -> remember p (set t (Arc_points ps)))
  | Select { after; picked; corner } -> selection_step t after picked corner i
  | Base (copy, objs) -> with_point (fun p -> remember p (set t (Second (copy, objs, p))))
  | Second (copy, objs, base) -> (
      match i with
      | Text "" -> finish { t with drawing = move_objects t copy objs base } (* the base as the displacement *)
      | _ -> with_point (fun p -> finish (remember p { t with drawing = move_objects t copy objs (G.sub p base) })))
  | Offset_distance -> (
      match i with
      | Text "" -> set t Offset_pick
      | _ -> (
          match number_of t i with
          | Some d when d > 0. -> set { t with offset_distance = d } Offset_pick
          | _ -> say "Requires a positive distance." t))
  | Offset_pick -> (
      match i with
      | Text "" -> finish t
      | Pick p -> (
          match D.pick t.drawing (pickbox t) p with Some id -> set t (Offset_side id) | None -> say "No object found." t)
      | _ -> t)
  | Offset_side id ->
      with_point (fun side ->
          match entity t id with
          | Some e -> (
              match Cad_edit.offset t.offset_distance e ~side with
              | Ok e' -> set { t with drawing = fst (add_on (layer_of t id) e' t.drawing) } Offset_pick
              | Error msg -> say msg (set t Offset_pick))
          | None -> set t Offset_pick)
  | Trim_pick (extend, edges) -> (
      match i with
      | Text "" -> finish t
      | Pick p -> (
          match D.pick t.drawing (pickbox t) p with
          | None -> say "No object found." t
          | Some id -> (
              (* no edges chosen: every other entity is one *)
              let edges = List.filter (( <> ) id) (if edges = [] then D.ids t.drawing else edges) in
              let curves = List.concat_map (fun e -> List.map fst (D.pieces t.drawing e)) (List.filter_map (D.get t.drawing) edges) in
              match entity t id with
              | None -> t
              | Some e -> (
                  let result = if extend then Result.map (fun e -> [ e ]) (Cad_edit.extend curves e ~at:p) else Cad_edit.trim curves e ~at:p in
                  match result with
                  | Ok es -> { t with drawing = D.replace id es t.drawing }
                  | Error msg -> say msg t)))
      | _ -> t)
  | Fillet_first -> (
      match i with
      | Text s when String.uppercase_ascii (String.trim s) = "R" -> set t Fillet_radius
      | Text "" -> finish t
      | Pick p -> (
          match D.pick t.drawing (pickbox t) p with
          | Some id when (match entity t id with Some (D.Line _) -> true | _ -> false) -> set t (Fillet_second (id, p))
          | Some _ -> say "Can only fillet lines." t
          | None -> say "No object found." t)
      | _ -> t)
  | Fillet_radius -> (
      match i with
      | Text "" -> finish t
      | _ -> ( match number_of t i with Some r when r >= 0. -> finish { t with fillet_radius = r } | _ -> say "Requires a distance." t))
  | Fillet_second (id1, q1) -> (
      match i with
      | Pick q2 -> (
          match D.pick t.drawing (pickbox t) q2 with
          | Some id2 when id2 <> id1 -> (
              match (entity t id1, entity t id2) with
              | Some e1, Some e2 -> (
                  match Cad_edit.fillet t.fillet_radius (e1, q1) (e2, q2) with
                  | Ok (l1, l2, arc) ->
                      let d = D.replace id2 [ l2 ] (D.replace id1 [ l1 ] t.drawing) in
                      let d = match arc with Some a -> fst (add_on (layer_of t id1) a d) | None -> d in
                      finish { t with drawing = d }
                  | Error msg -> finish (say msg t))
              | _ -> finish t)
          | _ -> say "No object found." t)
      | Text "" -> finish t
      | _ -> t)
  | Dim_points ps ->
      with_point (fun p ->
          match ps @ [ p ] with
          | [ a; b; l ] -> finish { t with drawing = fst (D.add (D.Dimension (a, b, l)) t.drawing) }
          | ps -> remember p (set t (Dim_points ps)))
  | Block_name | Insert_name -> (
      match i with
      | Text "" -> finish t
      | Text "?" -> say ("Defined blocks: " ^ String.concat " " (List.map fst t.drawing.blocks)) t
      | Text s ->
          let name = String.uppercase_ascii (String.trim s) in
          if r.state = Block_name then set t (Block_base name)
          else if List.mem_assoc name t.drawing.blocks then set t (Insert_point name)
          else finish (say (Printf.sprintf "Block %s not found." name) t)
      | _ -> t)
  | Block_base name -> with_point (fun p -> set t (Select { after = For_block (name, p); picked = []; corner = None }))
  | Insert_point name -> with_point (fun p -> remember p (set t (Insert_scale (name, p))))
  | Insert_scale (name, p) -> (
      match i with
      | Text "" -> set t (Insert_rotation (name, p, 1.))
      | _ -> ( match number_of t i with Some s when s <> 0. -> set t (Insert_rotation (name, p, s)) | _ -> say "Requires a number." t))
  | Insert_rotation (name, p, s) -> (
      let place rot = finish { t with drawing = fst (D.add (D.Insert (name, p, s, rot)) t.drawing) } in
      match i with
      | Text "" -> place 0.
      | Pick q -> place (G.angle p q)
      | Text x -> ( match float_of_string_opt (String.trim x) with Some a -> place a | None -> say "Requires an angle." t)
      | Cancel -> t)
  | Layer_option -> (
      match i with
      | Text "" -> finish t
      | Text s -> (
          match String.uppercase_ascii (String.trim s) with
          | "?" ->
              List.fold_left
                (fun t (l : D.layer) ->
                  say (Printf.sprintf "%-12s %-4s %d%s" l.name (if l.on then "On" else "Off") l.color (if l.name = t.drawing.current then "  (current)" else "")) t)
                t t.drawing.layers
          | "M" | "MAKE" -> set t (Layer_name "M")
          | "S" | "SET" -> set t (Layer_name "S")
          | "ON" -> set t (Layer_name "ON")
          | "OFF" -> set t (Layer_name "OFF")
          | "C" | "COLOR" -> set t Layer_color
          | _ -> say "Invalid option keyword." t)
      | _ -> t)
  | Layer_name option -> (
      match i with
      | Text s ->
          let name = String.uppercase_ascii (String.trim s) in
          let d = t.drawing in
          let t =
            if name = "" then t
            else
              match option with
              | "M" -> { t with drawing = { (D.ensure_layer name d) with current = name } }
              | "S" ->
                  if List.exists (fun (l : D.layer) -> l.name = name) d.layers then { t with drawing = { d with current = name } }
                  else say (Printf.sprintf "Cannot find layer %s." name) t
              | _ -> { t with drawing = D.set_layer { (D.layer d name) with on = option = "ON" } d }
          in
          set t Layer_option
      | _ -> t)
  | Layer_color -> (
      match i with
      | Text s -> ( match color_of s with Some c -> set t (Layer_color_name c) | None -> say "Invalid color." t)
      | _ -> t)
  | Layer_color_name c -> (
      match i with
      | Text s ->
          let name = if String.trim s = "" then t.drawing.current else String.uppercase_ascii (String.trim s) in
          set { t with drawing = D.set_layer { (D.layer t.drawing name) with color = c } t.drawing } Layer_option
      | _ -> t)
  | Zoom_option -> (
      match i with
      | Pick p -> set t (Zoom_corner (Some p))
      | Text s -> (
          match String.uppercase_ascii (String.trim s) with
          | "A" | "ALL" | "E" | "EXTENTS" -> finish (zoom_extents t)
          | "W" | "WINDOW" -> set t (Zoom_corner None)
          | "P" | "PREVIOUS" -> finish (match t.previous_view with Some v -> { t with view = v; previous_view = Some t.view } | None -> t)
          | s -> (
              let s = if String.ends_with ~suffix:"X" s then String.sub s 0 (String.length s - 1) else s in
              match float_of_string_opt s with
              | Some k when k > 0. -> finish { t with previous_view = Some t.view; view = { t.view with upp = t.view.upp /. k } }
              | _ -> say "Invalid option keyword." t))
      | Cancel -> t)
  | Zoom_corner None -> with_point (fun p -> set t (Zoom_corner (Some p)))
  | Zoom_corner (Some a) -> with_point (fun b -> finish (fit t a b))
  | Pan_points None -> with_point (fun p -> set t (Pan_points (Some p)))
  | Pan_points (Some a) ->
      with_point (fun b -> finish { t with previous_view = Some t.view; view = { t.view with center = G.sub t.view.center (G.sub b a) } })
  | Id_point -> with_point (fun (x, y) -> finish (remember (x, y) (say (Printf.sprintf "X = %s     Y = %s     Z = %s" (fmt x) (fmt y) (fmt 0.)) t)))
  | Dist_points None -> with_point (fun p -> remember p (set t (Dist_points (Some p))))
  | Dist_points (Some a) ->
      with_point (fun b ->
          let dx, dy = G.sub b a in
          finish
            (say
               (Printf.sprintf "Distance = %s,  Angle in XY Plane = %s,  Delta X = %s,  Delta Y = %s" (fmt (G.dist a b))
                  (Printf.sprintf "%.0f" (G.angle a b)) (fmt dx) (fmt dy))
               t))
  | File_name writing -> (
      match i with
      | Text s ->
          let base = if String.trim s = "" then t.file else String.trim s in
          let base = if Filename.check_suffix base ".dxf" then Filename.chop_suffix base ".dxf" else base in
          let name = base ^ ".dxf" in
          let t = { t with file = base } in
          if writing then finish (say ("Written " ^ name) { t with io = Some (Save (name, Dxf.to_string t.drawing)) })
          else finish { t with io = Some (Load name) }
      | _ -> t)

let input ?cursor t i =
  let t = { t with io = None } in
  match (t.run, i) with
  | None, Text "" -> (
      match t.last_command with Some c -> begin_ c (say ("Command: " ^ c) t) | None -> say "Command:" t)
  | None, Text "?" -> say commands (say "Command: ?" t)
  | None, Text s -> begin_ s (say ("Command: " ^ String.uppercase_ascii (String.trim s)) t)
  | None, (Pick _ | Cancel) -> t
  | Some _, Cancel -> finish (say "*Cancel*" t)
  | Some r, _ ->
      let echo = match i with Text s -> " " ^ s | _ -> "" in
      step ?cursor (say (prompt t ^ echo) t) r i

let loaded name text t =
  match text with
  | None -> say (Printf.sprintf "Can't open file %s" name) t
  | Some s -> (
      match Dxf.of_string s with
      | Ok d -> say ("Read " ^ name) (zoom_extents { t with drawing = d; undo = ("DXFIN", t.drawing) :: t.undo; redo = [] })
      | Error e -> say (Printf.sprintf "Error in %s: %s" name e) t)

(*****************************************************************************)
(* What the application draws *)
(*****************************************************************************)

let anchor t =
  match t.run with
  | None -> None
  | Some r -> (
      match r.state with
      | Line_to { last; _ } -> Some last
      | Circle_radius (c, _) -> Some c
      | Arc_points (_ :: _ as ps) -> Some (List.nth ps (List.length ps - 1))
      | Second (_, _, b) -> Some b
      | Dim_points (_ :: _ as ps) -> Some (List.nth ps (List.length ps - 1))
      | Insert_rotation (_, p, _) -> Some p
      | Pan_points (Some p) | Dist_points (Some p) -> Some p
      | _ -> None)

let box (ax, ay) (bx, by) = [ D.Line ((ax, ay), (bx, ay)); D.Line ((bx, ay), (bx, by)); D.Line ((bx, by), (ax, by)); D.Line ((ax, by), (ax, ay)) ]

let preview t c =
  match t.run with
  | None -> []
  | Some r -> (
      match r.state with
      | Line_to { last; _ } -> [ D.Line (last, c) ]
      | Circle_radius (ctr, d) -> [ D.Circle (ctr, G.dist ctr c /. if d then 2. else 1.) ]
      | Arc_points [ a ] -> [ D.Line (a, c) ]
      | Arc_points [ a; b ] -> Option.to_list (arc3 a b c)
      | Second (_, objs, base) -> List.filter_map (fun id -> Option.map (D.move_entity (G.sub c base)) (entity t id)) objs
      | Offset_side id -> (
          match entity t id with Some e -> Result.to_list (Cad_edit.offset t.offset_distance e ~side:c) | None -> [])
      | Dim_points [ a ] -> [ D.Line (a, c) ]
      | Dim_points [ a; b ] -> [ D.Dimension (a, b, c) ]
      | Insert_point name -> [ D.Insert (name, c, 1., 0.) ]
      | Insert_scale (name, p) -> [ D.Insert (name, p, 1., 0.) ]
      | Insert_rotation (name, p, s) -> [ D.Insert (name, p, s, G.angle p c) ]
      | Pan_points (Some a) | Dist_points (Some a) -> [ D.Line (a, c) ]
      | Zoom_corner (Some a) -> box a c
      | _ -> [])

let selected t =
  match t.run with
  | Some { state = Select { picked; _ }; _ } -> picked
  | Some { state = Trim_pick (_, edges); _ } -> edges
  | Some { state = Base (_, objs) | Second (_, objs, _); _ } -> objs
  | Some { state = Offset_side id | Fillet_second (id, _); _ } -> [ id ]
  | _ -> []

let selection_box t c =
  match t.run with Some { state = Select { corner = Some a; _ }; _ } -> Some (a, c, fst c < fst a) | _ -> None
