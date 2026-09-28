(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slot = Undo | Redo | Canvas | Dialog_label | Diameter | Close

let canvas_size = (220., 170.)

let layout th =
  Layout.(
    center
      (column ~gap:10.
         [
           row ~gap:10. [ leaf Undo (Immediate.button_size th "Undo"); leaf Redo (Immediate.button_size th "Redo") ];
           leaf Canvas canvas_size;
           leaf Dialog_label (Gui4.label_size th "Diameter at (-000, -000)");
           leaf Diameter (Immediate.slider_size th);
           leaf Close (Immediate.button_size th "Close");
         ]))

(*****************************************************************************)
(* The rules, the same in all four *)
(*****************************************************************************)

type circle = { x : float; y : float; d : float }

type model = {
  circles : circle list;
  (* the versions before and after, whole lists: undo is going back to
     one, which is all a list of circles needs *)
  past : circle list list;
  future : circle list list;
  (* the circle under the mouse, filled grey *)
  selected : int option;
  (* the context menu, open for a circle, at the point of the right
     click *)
  menu : (int * (float * float)) option;
  (* the dialog, open for a circle -- and the circles as they were when
     it opened, which is what Close compares with and Undo goes back
     to: the one step of a whole adjustment *)
  adjusting : (int * circle list) option;
}

let initial = { circles = []; past = []; future = []; selected = None; menu = None; adjusting = None }
let menu_items = [ "Adjust diameter..." ]
let from = 10. and to_ = 100.

(* the menu or the dialog is up: the canvas is left alone *)
let busy m = m.menu <> None || m.adjusting <> None

(* the circle a point is in, the nearest one's center if several *)
let under circles (px, py) =
  let dist c = Float.hypot (px -. c.x) (py -. c.y) in
  List.mapi (fun i c -> (i, c)) circles
  |> List.filter (fun (_, c) -> dist c < c.d /. 2.)
  |> List.sort (fun (_, a) (_, b) -> compare (dist a) (dist b))
  |> function
  | (i, _) :: _ -> Some i
  | [] -> None

(* the same model when nothing changed: immediate mode's runner below
   draws a frame twice only when a frame changed something *)
let hover p m =
  let selected = under m.circles p in
  if busy m || selected = m.selected then m else { m with selected }

(* a click on nothing makes a circle there, one step of history *)
let add ((x, y) as p) m =
  if busy m || under m.circles p <> None then m
  else { m with circles = m.circles @ [ { x; y; d = 30. } ]; past = m.circles :: m.past; future = []; selected = Some (List.length m.circles) }

let open_menu p m = if busy m then m else match under m.circles p with Some i -> { m with selected = Some i; menu = Some (i, p) } | None -> m

let on_canvas m = function Widget.Hover p -> hover p m | Widget.Press p -> add p m | Widget.Right_press p -> open_menu p m

(* the menu closes: on its one item, the dialog opens *)
let menu_closed choice m =
  match (m.menu, choice) with Some (i, _), Some 0 -> { m with menu = None; adjusting = Some (i, m.circles) } | _ -> { m with menu = None }

(* the slider: the circle changes as it moves, and the history does not *)
let set_diameter d m =
  match m.adjusting with Some (i, _) -> { m with circles = List.mapi (fun j c -> if j = i then { c with d } else c) m.circles } | None -> m

(* Close: the whole adjustment becomes one step -- if it changed
   anything *)
let close m =
  match m.adjusting with
  | Some (_, before) when before <> m.circles -> { m with adjusting = None; past = before :: m.past; future = [] }
  | _ -> { m with adjusting = None }

let can_undo m = m.past <> [] && not (busy m)
let can_redo m = m.future <> [] && not (busy m)

let undo m =
  match m.past with p :: rest when not (busy m) -> { m with circles = p; past = rest; future = m.circles :: m.future; selected = None } | _ -> m

let redo m =
  match m.future with f :: rest when not (busy m) -> { m with circles = f; future = rest; past = m.circles :: m.past; selected = None } | _ -> m

(* where things are said: from the canvas's center, y up *)
let relative (b : Widget.box) (c : circle) = (c.x -. b.x, c.y -. b.y)

let diameter m = match m.adjusting with Some (i, _) -> (List.nth m.circles i).d | None -> 30.

let dialog_text b m =
  match m.adjusting with
  | Some (i, _) ->
      let x, y = relative b (List.nth m.circles i) in
      Printf.sprintf "Diameter at (%.0f, %.0f)" x y
  | None -> ""

let summary b m =
  Printf.sprintf "%s; undo %d, redo %d%s"
    (String.concat " " (List.map (fun c -> let x, y = relative b c in Printf.sprintf "(%.0f,%.0f,%.0f)" x y c.d) m.circles))
    (List.length m.past) (List.length m.future)
    (if m.adjusting <> None then ", adjusting" else "")

(* The canvas's picture: circles are not a thing Widget.paint has, so a
   disc is its rows, two units apart -- a scanline fill, as
   graphics/2d/Circle does with pixels -- each reaching one unit into
   the next, so that rows landing between pixels leave no seam, and
   none past the disc's own edge. A circle is a dark disc with a lighter
   one inside it, grey when selected. *)
let disc color (cx, cy) r =
  let rows = max 1 (int_of_float (Float.ceil r)) in
  List.init rows (fun k ->
      let lo = -.r +. (2. *. float_of_int k) in
      let hi = Float.min r (lo +. 3.) in
      (* the chord at the middle of the row's own two units *)
      let mid = Float.min r (lo +. 1.) in
      let half = sqrt (Float.max 0. ((r *. r) -. (mid *. mid))) in
      Widget.Fill (color, { Widget.x = cx; y = cy +. ((lo +. hi) /. 2.); w = 2. *. half; h = hi -. lo }))

let drawing (th : Theme.t) (b : Widget.box) m =
  (Widget.Fill (Color.white, b) :: Widget.frame th.edge 1. b)
  @ List.concat
      (List.mapi
         (fun i c ->
           let r = c.d /. 2. in
           disc th.edge (c.x, c.y) r @ disc (if m.selected = Some i then th.face_hot else Color.white) (c.x, c.y) (r -. 1.5))
         m.circles)

(*****************************************************************************)
(* The four ways *)
(*****************************************************************************)

let make th panel (arch : Gui4.architecture) : Gui4.runner =
  let box = Gui4.places panel (layout th) in
  let canvas_box = box Canvas in
  match arch with
  (* ---- immediate: the menu first, since it has the mouse; the canvas
     asked what happened, then drawn as it now is ---- *)
  | Immediate ->
      let m = ref initial in
      let frame u =
        let u =
          match !m.menu with
          | Some (_, at) ->
              let u, answer = Immediate.context_menu u at menu_items in
              (match answer with `Open -> () | `Chosen k -> m := menu_closed (Some k) !m | `Dismissed -> m := menu_closed None !m);
              u
          | None -> u
        in
        (* the dialog before the canvas, so that the circle is drawn at
           the diameter the slider gives it this frame *)
        let u =
          match !m.adjusting with
          | None -> u
          | Some _ ->
              let u = Immediate.label u (box Dialog_label) (dialog_text canvas_box !m) in
              let u, d = Immediate.slider u (box Diameter) ~from ~to_ (diameter !m) in
              if d <> diameter !m then m := set_diameter d !m;
              let u, closed = Immediate.button u (box Close) "Close" in
              if closed then m := close !m;
              u
        in
        let u, undone = Immediate.button ~enabled:(can_undo !m) u (box Undo) "Undo" in
        if undone then m := undo !m;
        let u, redone = Immediate.button ~enabled:(can_redo !m) u (box Redo) "Redo" in
        if redone then m := redo !m;
        let u, events = Immediate.canvas u canvas_box in
        m := List.fold_left on_canvas !m events;
        Immediate.draw u (drawing th canvas_box !m)
      in
      (* A widget is drawn when it is asked for, so a click that takes
         a widget away -- Close closing the dialog -- leaves it drawn
         for that frame: immediate mode's frame of lag. When a frame
         changed the model, it is run once more with the edges taken
         out of the input (nothing is clicked twice), which is what
         MVU's step does with its second view *)
      let ui = ref (Immediate.set_theme th Immediate.empty) in
      let step (i : Widget.input) =
        let before = !m in
        let u = frame (Immediate.frame i !ui) in
        let u = if !m != before then frame (Immediate.frame { i with typed = ""; mclick = false } u) else u in
        ui := u;
        Immediate.paint u
      in
      { step; summary = (fun () -> summary canvas_box !m) }
  (* ---- callbacks: the model is in a ref, and every callback that
     changes it must bring every widget into line -- the canvas's
     picture, the two buttons, the dialog, the slider's starting
     value -- by calling [redraw] ---- *)
  | Callbacks ->
      let m = ref initial in
      (* the callbacks need the widgets, and the widgets their
         callbacks: a forward reference, set once both exist -- the
         knot every callback program ties somewhere *)
      let redraw = ref (fun () -> ()) in
      let undo_b = Retained.button (box Undo) "Undo" (fun () -> m := undo !m; !redraw ()) in
      let redo_b = Retained.button (box Redo) "Redo" (fun () -> m := redo !m; !redraw ()) in
      let said = Retained.label (box Dialog_label) "" in
      let slider = Retained.slider (box Diameter) ~from ~to_ 30. (fun d -> m := set_diameter d !m; !redraw ()) in
      let close_b = Retained.button (box Close) "Close" (fun () -> m := close !m; !redraw ()) in
      let dialog = Retained.group [ said; slider; close_b ] in
      let menu =
        Retained.context_menu menu_items (fun choice ->
            m := menu_closed choice !m;
            (* the dialog's slider starts at the circle's diameter: one
               more thing to keep in line, by hand *)
            Retained.set_value slider (diameter !m);
            !redraw ())
      in
      let canvas =
        Retained.canvas canvas_box (fun ev ->
            m := on_canvas !m ev;
            (match (ev, !m.menu) with Widget.Right_press _, Some (_, at) -> Retained.popup menu at | _ -> ());
            !redraw ())
      in
      (redraw :=
         fun () ->
           Retained.set_drawing canvas (drawing th canvas_box !m);
           Retained.set_enabled undo_b (can_undo !m);
           Retained.set_enabled redo_b (can_redo !m);
           Retained.set_shown dialog (!m.adjusting <> None);
           Retained.set_text said (dialog_text canvas_box !m));
      !redraw ();
      let ui = Retained.window (Retained.group [ undo_b; redo_b; canvas; dialog; menu ]) in
      { step = Gui4.retained th ui; summary = (fun () -> summary canvas_box !m) }
  (* ---- MVC: the controllers change the model, and one view function
     brings every widget into line after every change -- which must
     work out for itself what changed, since it is told only that
     something did: whether the menu has just opened, to pop it up
     ---- *)
  | Mvc ->
      let model = Mvc.create initial in
      let undo_b = Retained.button (box Undo) "Undo" (fun () -> Mvc.change model undo) in
      let redo_b = Retained.button (box Redo) "Redo" (fun () -> Mvc.change model redo) in
      let said = Retained.label (box Dialog_label) "" in
      let slider = Retained.slider (box Diameter) ~from ~to_ 30. (fun d -> Mvc.change model (set_diameter d)) in
      let close_b = Retained.button (box Close) "Close" (fun () -> Mvc.change model close) in
      let dialog = Retained.group [ said; slider; close_b ] in
      let menu = Retained.context_menu menu_items (fun choice -> Mvc.change model (menu_closed choice)) in
      let canvas = Retained.canvas canvas_box (fun ev -> Mvc.change model (fun m -> on_canvas m ev)) in
      let was = ref initial in
      let sync () =
        let m = Mvc.get model in
        Retained.set_drawing canvas (drawing th canvas_box m);
        Retained.set_enabled undo_b (can_undo m);
        Retained.set_enabled redo_b (can_redo m);
        Retained.set_shown dialog (m.adjusting <> None);
        Retained.set_text said (dialog_text canvas_box m);
        Retained.set_value slider (diameter m);
        (match (!was.menu, m.menu) with None, Some (_, at) -> Retained.popup menu at | _ -> ());
        was := m
      in
      Mvc.on_change model sync;
      sync ();
      let ui = Retained.window (Retained.group [ undo_b; redo_b; canvas; dialog; menu ]) in
      { step = Gui4.retained th ui; summary = (fun () -> summary canvas_box (Mvc.get model)) }
  (* ---- MVU: the canvas's picture, the dialog and the menu are all in
     the view, drawn from the model or left out of it -- nothing to
     keep in line ---- *)
  | Mvu ->
      let update msg m =
        match msg with
        | `Canvas ev -> on_canvas m ev
        | `Undo -> undo m
        | `Redo -> redo m
        | `Menu choice -> menu_closed choice m
        | `Diameter d -> set_diameter d m
        | `Close -> close m
      in
      let view m =
        Mvu.group
          ([
             Mvu.button ~enabled:(can_undo m) (box Undo) "Undo" `Undo;
             Mvu.button ~enabled:(can_redo m) (box Redo) "Redo" `Redo;
             Mvu.canvas canvas_box (drawing th canvas_box m) (fun ev -> Some (`Canvas ev));
           ]
          @ (match m.adjusting with
            | Some _ ->
                [
                  Mvu.label (box Dialog_label) (dialog_text canvas_box m);
                  Mvu.slider (box Diameter) ~from ~to_ (diameter m) (fun d -> `Diameter d);
                  Mvu.button (box Close) "Close" `Close;
                ]
            | None -> [])
          @ match m.menu with Some (_, at) -> [ Mvu.context_menu at menu_items (fun c -> `Menu c) ] | None -> [])
      in
      let model = ref initial and state = ref Mvu.empty in
      let step i =
        let st, m, paint = Mvu.step th i !state ~view ~update !model in
        state := st;
        model := m;
        paint
      in
      { step; summary = (fun () -> summary canvas_box !model) }
