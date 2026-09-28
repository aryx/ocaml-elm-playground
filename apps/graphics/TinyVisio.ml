(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyVisio: shapes that are spreadsheets
 * (Jeremy Jaech, Dave Walter and Ted Johnson, Shapeware, 1992 -- three
 * of Aldus's founders; Visio Corporation, bought by Microsoft in 2000).
 *
 * Sold as drawing for people who cannot draw -- drag a shape from a
 * stencil, drag a connector between two, done -- and that was true,
 * but the idea underneath is the one worth seeing, and it is in the
 * panel at the bottom: **the ShapeSheet**. Each shape is a small
 * spreadsheet, its cells named (PinX, Width, Geometry1.X2...), each a
 * formula over the others, and the shape on the page is whatever they
 * compute (Diagram.mli). Click a row, type a formula, Enter: the
 * shape changes. The block arrow, selected at the start, is the
 * classic: its head is User.Head = MIN(0.5, Width*0.5), so stretching
 * it (the handle at its bottom-right) lengthens the shaft and not the
 * head; type Width*0.5 into User.Head instead and it stretches whole.
 *
 * The second idea follows from the first: **glue is a formula**.
 * Drag from a shape's connection point (the small crosses) to
 * another's, and the connector's ends hold formulas naming those
 * shapes' cells, Sheet.2!PinX...; move a shape and the spreadsheet
 * engine recomputes the connector -- the same dependency graph that
 * recalculates TinyExcel's totals. Select a connector to see its
 * BeginX's formula; drag an end away and it is unglued, a number
 * again. Delete (the Delete key) a shape and the connectors glued to
 * it keep their place, their formulas replaced by their values.
 *
 * And the third, an algorithm: the connectors run in right angles
 * around the shapes, never through them, rerouted as they move
 * (Ortho_route.mli: the orthogonal visibility graph and a shortest
 * path paying for each bend).
 *
 *   drag from the stencil     a shape on the page
 *   drag a shape              moved; its connectors follow
 *   drag the corner handle    resized, the formulas deciding the rest
 *   drag from a cross         a connector, glued where you let go
 *   type                      the selected shape's text (Backspace)
 *   click a ShapeSheet row    its formula to edit; Enter, Escape
 *
 * What it uses: appkits/diagram (Diagram, Ortho_route) over
 * appkits/sheet's engine and its Formula -- the whole page one sheet,
 * a shape a column. What it does not use: gui/ (its panels are its
 * own drawing, as TinyExcel's grid is), and appkits/draw, whose
 * figures are numbers where these are formulas.
 *
 * What it deliberately does not do: rotation (Visio's Angle cell, and
 * PAR(PNT()) in the glue's formula to follow it), a page's own
 * ShapeSheet, masters shared by their instances (an instance here
 * copies its master's formulas: editing a master changes nothing
 * drawn), groups, layers, several pages, curves in a geometry, IF and
 * the ShapeSheet's other functions, and saving.
 *
 * Exercises: Angle, in the outline and in glue; a master edited and
 * its instances following, cells inherited until overridden (Visio's
 * local and inherited formulas); GUARD(), a cell the mouse cannot
 * change; a connector's text at its middle; save and open the page
 * (Saved, as TinyExcel's sheet).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type drag =
  | Idle
  | From_stencil of int
  | Moving of int * (float * float) (* the pin's offset from the mouse *)
  | Resizing of int
  | Dragging_end of int * Diagram.ends

(* a ShapeSheet row being edited: its formula, selected until typed
   over as a spreadsheet's cell is ([fresh]) *)
type edit = { cell : string; text : string; fresh : bool }

type model = {
  page : Diagram.t;
  selected : int option;
  drag : drag;
  editing : edit option;
  message : string;
  was_down : bool;
  was : string list;
}

let master name = List.find (fun (m : Diagram.master) -> m.name = name) Diagram.masters

(* an order that is in stock or not: a flowchart, glued, and a block
   arrow to stretch *)
let initial =
  let d = Diagram.empty in
  let add d name (x, y) ?(size = (1.5, 0.75)) text =
    let d, id = Diagram.drop d (master name) (x, y) in
    let d = Diagram.resize d id size in
    let d = Diagram.move d id (x, y) in
    (Diagram.set_text d id text, id)
  in
  let d, order = add d "Data" (1.6, 6.9) "Order in" in
  let d, stock = add d "Decision" (4.7, 6.9) ~size:(1.7, 1.1) "In stock?" in
  let d, ship = add d "Process" (8.4, 6.9) "Ship it" in
  let d, more = add d "Process" (4.7, 4.3) "Order more" in
  let d, arrow = add d "Block arrow" (2.4, 1.8) ~size:(2.6, 1.0) "smart" in
  let connect d (a, pa) (b, pb) text =
    let d, c = Diagram.drop d (master "Dynamic connector") (0., 0.) in
    let d = Diagram.glue d c Diagram.Begin ~target:a ~point:pa in
    let d = Diagram.glue d c Diagram.End ~target:b ~point:pb in
    Diagram.set_text d c text
  in
  (* the points: 0 top, 1 right, 2 bottom, 3 left *)
  let d = connect d (order, 1) (stock, 3) "" in
  let d = connect d (stock, 1) (ship, 3) "yes" in
  let d = connect d (stock, 2) (more, 0) "no" in
  let d = connect d (more, 3) (order, 2) "" in
  { page = d; selected = Some arrow; drag = Idle; editing = None; message = ""; was_down = false; was = [] }

(*****************************************************************************)
(* Where things are *)
(*****************************************************************************)

(* the page, 11 by 8.5 inches, landscape *)
let inch = 68.
let page_w = 11.
let page_h = 8.5
let page_left = -355.
let page_bottom = -108.
let to_screen (x, y) = (page_left +. (x *. inch), page_bottom +. (y *. inch))
let to_page (x, y) = ((x -. page_left) /. inch, (y -. page_bottom) /. inch)
let on_page (x, y) = x >= 0. && x <= page_w && y >= 0. && y <= page_h

(* the stencil's items, down the left *)
let stencil_item i = (-430., 400. -. (float_of_int i *. 90.))
let in_stencil (mx, my) = List.find_opt (fun i -> let x, y = stencil_item i in Float.abs (mx -. x) < 65. && Float.abs (my -. y) < 40.) (List.init (List.length Diagram.masters) Fun.id)

(* the ShapeSheet: two columns of rows under the page *)
let row_h = 18.
let rows_per_column = 18
let row_place k = ((if k < rows_per_column then -490. else 5.), -150. -. (float_of_int (k mod rows_per_column) *. row_h))

let row_at (mx, my) (s : Diagram.shape) =
  List.find_opt
    (fun k -> let x, y = row_place k in mx >= x && mx <= x +. 485. && Float.abs (my -. y) <= row_h /. 2.)
    (List.init (List.length s.rows) Fun.id)

let distance (x1, y1) (x2, y2) = Float.sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))

(* the connection point nearest the mouse, close enough to glue to *)
let point_near m p ~except =
  List.fold_left
    (fun best (s : Diagram.shape) ->
      if s.kind <> Diagram.Box || Some s.id = except then best
      else
        List.fold_left
          (fun best (i, (q, _)) -> if distance p q < 0.18 && (match best with None -> true | Some (_, _, d) -> distance p q < d) then Some (s.id, i, distance p q) else best)
          best
          (List.mapi (fun i x -> (i, x)) (Diagram.connection_points m.page s)))
    None (Diagram.shapes m.page)

(* the topmost box under the mouse, or a connector passing close *)
let shape_at m p =
  let near_route (s : Diagram.shape) =
    let rec pieces = function a :: (b :: _ as rest) -> (a, b) :: pieces rest | _ -> [] in
    List.exists
      (fun ((x1, y1), (x2, y2)) ->
        let px, py = p in
        px >= Float.min x1 x2 -. 0.1 && px <= Float.max x1 x2 +. 0.1 && py >= Float.min y1 y2 -. 0.1 && py <= Float.max y1 y2 +. 0.1)
      (pieces (Diagram.route m.page s))
  in
  List.fold_left
    (fun found (s : Diagram.shape) ->
      match s.kind with
      | Diagram.Box ->
          let l, b, r, t = Diagram.bounds m.page s in
          if fst p >= l && fst p <= r && snd p >= b && snd p <= t then Some s.id else found
      | Diagram.Connector -> if found = None && near_route s then Some s.id else found)
    None (Diagram.shapes m.page)

let selected_shape m = Option.bind m.selected (Diagram.shape m.page)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let press m (mx, my) =
  let p = to_page (mx, my) in
  match (in_stencil (mx, my), selected_shape m) with
  | Some i, _ -> { m with drag = From_stencil i; editing = None }
  | None, Some s when my < -130. -> (
      match row_at (mx, my) s with
      | Some k -> let cell, text = List.nth s.rows k in { m with editing = Some { cell; text; fresh = true }; message = "" }
      | None -> { m with editing = None })
  | None, sel when on_page p -> (
      let m = { m with editing = None } in
      let end_hit =
        match sel with
        | Some ({ kind = Diagram.Connector; _ } as s) ->
            let b, e = Diagram.ends_of m.page s in
            if distance p b < 0.15 then Some (s.id, Diagram.Begin) else if distance p e < 0.15 then Some (s.id, Diagram.End) else None
        | _ -> None
      in
      let handle_hit =
        match sel with
        | Some ({ kind = Diagram.Box; _ } as s) -> let _, b, r, _ = Diagram.bounds m.page s in distance p (r, b) < 0.15
        | _ -> false
      in
      match end_hit with
      | Some (id, e) -> { m with drag = Dragging_end (id, e) }
      | None when handle_hit -> { m with drag = Resizing (Option.get m.selected) }
      | None -> (
          match point_near m p ~except:None with
          | Some (target, point, _) ->
              (* a connector from the point, its end following the mouse *)
              let page, c = Diagram.drop m.page (master "Dynamic connector") p in
              let page = Diagram.glue page c Diagram.Begin ~target ~point in
              let page = Diagram.place_end page c Diagram.End p in
              { m with page; selected = Some c; drag = Dragging_end (c, Diagram.End) }
          | None -> (
              match shape_at m p with
              | Some id ->
                  let s = Option.get (Diagram.shape m.page id) in
                  let drag =
                    if s.kind = Diagram.Box then
                      let px = Option.value (Diagram.value m.page id "PinX") ~default:0. and py = Option.value (Diagram.value m.page id "PinY") ~default:0. in
                      Moving (id, (px -. fst p, py -. snd p))
                    else Idle
                  in
                  { m with selected = Some id; drag }
              | None -> { m with selected = None })))
  | _ -> m

let dragging m (mx, my) =
  let p = to_page (mx, my) in
  match m.drag with
  | Moving (id, (dx, dy)) -> { m with page = Diagram.move m.page id (fst p +. dx, snd p +. dy) }
  | Resizing id -> (
      match Diagram.shape m.page id with
      | Some s -> let l, _, _, t = Diagram.bounds m.page s in { m with page = Diagram.resize m.page id (fst p -. l, t -. snd p) }
      | None -> m)
  | Dragging_end (id, e) -> { m with page = Diagram.place_end m.page id e p }
  | Idle | From_stencil _ -> m

let release m (mx, my) =
  let p = to_page (mx, my) in
  let m =
    match m.drag with
    | From_stencil i when on_page p ->
        let master = List.nth Diagram.masters i in
        let page, id = Diagram.drop m.page master p in
        { m with page; selected = Some id; message = Printf.sprintf "Sheet.%d, a %s: its ShapeSheet is below" id master.name }
    | Dragging_end (id, e) -> (
        match point_near m p ~except:None with
        | Some (target, point, _) ->
            let page = Diagram.glue m.page id e ~target ~point in
            let cell = match e with Diagram.Begin -> "BeginX" | Diagram.End -> "EndX" in
            let formula = Option.bind (Diagram.shape page id) (fun s -> List.assoc_opt cell s.rows) in
            { m with page; message = Printf.sprintf "glued: %s = %s" cell (Option.value formula ~default:"") }
        | None -> { m with message = "not glued: its ends are numbers" })
    | _ -> m
  in
  { m with drag = Idle }

let typing m k pressed =
  match (m.editing, m.selected) with
  | Some e, Some id ->
      if pressed "Enter" then
        match Diagram.set_formula m.page id e.cell e.text with
        | Ok page -> { m with page; editing = None; message = e.cell ^ " = " ^ e.text }
        | Error why -> { m with message = "#" ^ why }
      else if pressed "Escape" then { m with editing = None }
      else if pressed "Backspace" then
        let text = if e.fresh || e.text = "" then "" else String.sub e.text 0 (String.length e.text - 1) in
        { m with editing = Some { e with text; fresh = false } }
      else if k.typed <> "" then { m with editing = Some { e with text = (if e.fresh then k.typed else e.text ^ k.typed); fresh = false } }
      else m
  | None, Some id -> (
      match Diagram.shape m.page id with
      | Some s ->
          if pressed "Delete" then { m with page = Diagram.delete m.page id; selected = None; message = Printf.sprintf "Sheet.%d deleted" id }
          else if pressed "Backspace" && s.text <> "" then { m with page = Diagram.set_text m.page id (String.sub s.text 0 (String.length s.text - 1)) }
          else if k.typed <> "" then { m with page = Diagram.set_text m.page id (s.text ^ k.typed) }
          else m
      | None -> m)
  | _ -> m

let update computer m =
  let mouse = computer.mouse in
  let at = (mouse.mx, mouse.my) in
  let m = if mouse.mdown && not m.was_down then press m at else if mouse.mdown then dragging m at else if m.was_down then release m at else m in
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key m.was) in
  let m = typing m k pressed in
  { m with was_down = mouse.mdown; was = now }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let ink = rgb 30 60 120
let fill_blue = rgb 207 226 243

let segment color w (x1, y1) (x2, y2) =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.sqrt ((dx *. dx) +. (dy *. dy)) +. w) w |> rotate (Float.atan2 dy dx *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let rec polyline color w = function a :: (b :: _ as rest) -> segment color w a b :: polyline color w rest | _ -> []

let left_text ?(size = 14.) color x y s = words color s |> scale (size /. words_font_size) |> move (x +. (Widget.text_width ~size s /. 2.)) y

(* a size at most [size] at which the text fits [width]: a glue
   formula is long *)
let fitting ~size ~width s = let w = Widget.text_width ~size s in if w <= width then size else size *. width /. w

(* a shape as the page shows it: a box filled and outlined, its text in
   the middle; a connector's route and its arrowhead *)
let draw_shape page (s : Diagram.shape) =
  match s.kind with
  | Diagram.Box ->
      let points = List.map to_screen (Diagram.outline page s) in
      let l, b, r, t = Diagram.bounds page s in
      let cx, cy = to_screen ((l +. r) /. 2., (b +. t) /. 2.) in
      (if List.length points >= 3 then [ polygon fill_blue points ] else [])
      @ polyline ink 2. (points @ match points with p :: _ -> [ p ] | [] -> [])
      @ [ words ink s.text |> scale (15. /. words_font_size) |> move cx cy ]
  | Diagram.Connector -> (
      let route = List.map to_screen (Diagram.route page s) in
      let line = polyline black 2. route in
      match List.rev route with
      | (x2, y2) :: (x1, y1) :: _ ->
          let a = Float.atan2 (y2 -. y1) (x2 -. x1) in
          let head = polygon black [ (0., 0.); (-12., 5.); (-12., -5.) ] |> rotate (a *. 180. /. Float.pi) |> move x2 y2 in
          (* the text beside the connector's first piece *)
          let label =
            match route with
            | (ax, ay) :: (bx, by) :: _ when s.text <> "" -> [ words (rgb 120 20 20) s.text |> scale (14. /. words_font_size) |> move (((ax +. bx) /. 2.) +. 14.) (((ay +. by) /. 2.) +. 10.) ]
            | _ -> []
          in
          line @ [ head ] @ label
      | _ -> line)

let handles page (s : Diagram.shape) =
  let square color (x, y) = rectangle color 9. 9. |> move x y in
  match s.kind with
  | Diagram.Box ->
      let l, b, r, t = Diagram.bounds page s in
      List.map (fun p -> square (rgb 40 160 60) (to_screen p)) [ (l, b); (l, t); (r, t) ] @ [ rectangle (rgb 20 110 40) 13. 13. |> move (fst (to_screen (r, b))) (snd (to_screen (r, b))) ]
  | Diagram.Connector ->
      (* a glued end red, an end on its own green, as Visio showed them *)
      let b, e = Diagram.ends_of page s in
      let glued which = List.exists (fun (e', _, _) -> e' = which) s.glue in
      let color which = if glued which then rgb 200 30 30 else rgb 40 160 60 in
      [ square (color Diagram.Begin) (to_screen b); square (color Diagram.End) (to_screen e) ]

(* the stencil's pictures: each master dropped on a scratch page *)
let icons =
  lazy
    (List.map
       (fun (master : Diagram.master) ->
         let d, id = Diagram.drop Diagram.empty master (0., 0.) in
         let d = if master.kind = Diagram.Box then Diagram.resize d id (1.4, 0.7) else d in
         let d = if master.kind = Diagram.Box then Diagram.move d id (0., 0.) else d in
         match Diagram.shape d id with
         | Some s when s.kind = Diagram.Box -> `Outline (Diagram.outline d s)
         | _ -> `Line)
       Diagram.masters)

let stencil m =
  List.concat
    (List.mapi
       (fun i ((master : Diagram.master), icon) ->
         let x, y = stencil_item i in
         let lit = match m.drag with From_stencil j -> i = j | _ -> false in
         let picture =
           match icon with
           | `Outline pts -> let pts = List.map (fun (px, py) -> (x +. (px *. 60.), y +. 10. +. (py *. 60.))) pts in polygon fill_blue pts :: polyline ink 1.5 (pts @ [ List.hd pts ])
           | `Line -> [ segment black 2. (x -. 40., y +. 10.) (x +. 30., y +. 10.); polygon black [ (x +. 40., y +. 10.); (x +. 28., y +. 15.); (x +. 28., y +. 5.) ] ]
         in
         (rectangle (if lit then rgb 255 240 200 else rgb 245 245 248) 130. 80. |> move x y) :: picture @ [ words (rgb 50 50 50) master.name |> scale (fitting ~size:13. ~width:120. master.name /. words_font_size) |> move x (y -. 28.) ])
       (List.combine Diagram.masters (Lazy.force icons)))

let shapesheet m =
  match selected_shape m with
  | None -> [ left_text (rgb 90 90 90) (-490.) (-130.) "ShapeSheet: select a shape" ]
  | Some s ->
      left_text ~size:15. (rgb 20 20 20) (-490.) (-128.) (Printf.sprintf "ShapeSheet  Sheet.%d  (%s)" s.id s.master)
      :: List.concat
           (List.mapi
              (fun k (name, formula) ->
                let x, y = row_place k in
                let editing = match m.editing with Some e -> e.cell = name | None -> false in
                let shown_formula = match m.editing with Some e when e.cell = name -> if e.fresh then "[" ^ e.text ^ "]" else e.text ^ "_" | _ -> formula in
                let value = Diagram.shown m.page s.id name in
                let value = match float_of_string_opt value with Some f -> Printf.sprintf "%.3g" f | None -> value in
                [ rectangle (if editing then rgb 255 250 205 else if k mod 2 = 0 then rgb 250 250 252 else rgb 238 240 246) 485. row_h |> move (x +. 242.) y;
                  left_text ~size:13. (rgb 40 40 90) (x +. 4.) y name;
                  left_text ~size:(fitting ~size:13. ~width:262. shown_formula) (rgb 20 20 20) (x +. 150.) y shown_formula;
                  left_text ~size:13. (rgb 20 100 40) (x +. 420.) y value ])
              s.rows)

let view computer m =
  let page = m.page in
  let mouse = to_page (computer.mouse.mx, computer.mouse.my) in
  let pw = page_w *. inch and ph = page_h *. inch in
  let px, py = (page_left +. (pw /. 2.), page_bottom +. (ph /. 2.)) in
  (* the inch grid, faint *)
  let grid =
    List.init 10 (fun i -> segment (rgb 232 236 242) 1. (to_screen (float_of_int (i + 1), 0.)) (to_screen (float_of_int (i + 1), page_h)))
    @ List.init 8 (fun i -> segment (rgb 232 236 242) 1. (to_screen (0., float_of_int (i + 1))) (to_screen (page_w, float_of_int (i + 1))))
  in
  let boxes, connectors = List.partition (fun (s : Diagram.shape) -> s.kind = Diagram.Box) (Diagram.shapes page) in
  (* the crosses: on the box under the mouse, or on all of them while a
     connector's end is being dragged *)
  let crosses =
    let show (s : Diagram.shape) =
      (match m.drag with Dragging_end _ -> true | _ -> false)
      || (let l, b, r, t = Diagram.bounds page s in fst mouse >= l -. 0.2 && fst mouse <= r +. 0.2 && snd mouse >= b -. 0.2 && snd mouse <= t +. 0.2)
    in
    List.concat_map
      (fun s ->
        if show s then
          List.concat_map
            (fun (p, _) -> let x, y = to_screen p in [ segment (rgb 30 90 220) 2. (x -. 5., y -. 5.) (x +. 5., y +. 5.); segment (rgb 30 90 220) 2. (x -. 5., y +. 5.) (x +. 5., y -. 5.) ])
            (Diagram.connection_points page s)
        else [])
      boxes
  in
  let ghost = match m.drag with From_stencil i when on_page mouse -> [ words (rgb 120 120 120) (List.nth Diagram.masters i).name |> move computer.mouse.mx computer.mouse.my ] | _ -> [] in
  [ rectangle (rgb 205 210 218) computer.screen.width computer.screen.height;
    rectangle (rgb 160 165 175) pw ph |> move (px +. 5.) (py -. 5.);
    rectangle white pw ph |> move px py ]
  @ grid
  @ List.concat_map (draw_shape page) boxes
  @ List.concat_map (draw_shape page) connectors
  @ crosses
  @ (match selected_shape m with Some s -> handles page s | None -> [])
  @ stencil m
  @ [ left_text ~size:15. (rgb 20 20 20) (-495.) 470. "Stencil";
      left_text ~size:15. (if String.length m.message > 0 && m.message.[0] = '#' then rgb 190 30 30 else rgb 30 30 30) (-355.) 485. m.message ]
  @ shapesheet m @ ghost

let app = game view update initial
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
