(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 6: Circle Drawer
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 6).
 *
 * Click to put a circle down. The one nearest the mouse is the
 * selected one, and shown darker. Right-click it and a dialog lets
 * you change its size, live. Undo and redo take back whole edits.
 *
 * This is the task 7GUIs uses to ask about **undo**, and the question
 * it really asks is not "can you undo" but *what is one edit*:
 *
 *   adding a circle          one edit
 *   dragging the slider      one edit -- the whole adjustment, from
 *                            opening the dialog to closing it, and
 *                            not the fifty values the slider passed
 *                            through on the way
 *
 * Which is why the dialog's value is kept aside while it is open
 * ([live] below) and recorded once, when it closes. Get that wrong
 * and undo walks back through every pixel the slider moved -- the
 * most common undo bug there is, and the reason the task exists.
 *
 * The undo itself is appkits/document/Undo: the state is a value, so
 * a version is the old list of circles, kept. There is no inverse of
 * "add a circle" to write, and none of "make it bigger" -- which is
 * the whole of what the command pattern does elsewhere, and where its
 * bugs live (see Undo.mli).
 *
 * Deliberately not faithful in one place: 7GUIs puts a one-item
 * context menu ("Adjust diameter..") between the right click and the
 * dialog. Here the right click opens the dialog. The menu would be a
 * popup at the mouse, which gui/Immediate's dropdown is not -- an
 * exercise, and the reason it is not one line.
 *
 * Exercises: the context menu between the right click and the dialog;
 * dragging a circle, which is a third kind of edit and wants its own
 * name in the history; a list of the past edits by name, which is
 * what Undo.undo_name is for; a circle taken away again.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type circle = { cx : number; cy : number; d : number }

type model = {
  (* the circles, and every earlier version of them *)
  circles : circle list Undo.t;
  (* while the dialog is open: which circle, and the circles as they
     look right now -- not yet recorded, because the whole adjustment
     is one edit *)
  adjusting : int option;
  live : circle list option;
  (* the right button at the previous frame, to see it go down *)
  was_rdown : bool;
}

let initial =
  { circles = Undo.start []; adjusting = None; live = None; was_rdown = false }

let shown model = match model.live with Some cs -> cs | None -> Undo.now model.circles

type slot = Title | Canvas | Undo_ | Redo | Status | Dialog_label | Dialog_slider | Dialog_done

let canvas_size = (640., 340.)

let panel =
  Layout.(
    center
      (column ~gap:12.
         [
           leaf Title (Gui.label_size "7GUIs 6: Circle Drawer");
           leaf Canvas canvas_size;
           row ~gap:12. [ leaf Undo_ (Gui.button_size "undo"); leaf Redo (Gui.button_size "redo") ];
           leaf Status (Gui.label_size "circles 00   back 00   forward 00   undo: Adjust Diameter");
           space 10.;
           leaf Dialog_label (Gui.label_size "diameter of the circle at (000, 000)");
           leaf Dialog_slider (Gui.slider_size ());
           leaf Dialog_done (Gui.button_size "done");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

(* the circle the mouse is nearest, among those it is inside *)
let under (mx, my) circles =
  let inside i c =
    let dx = mx -. c.cx and dy = my -. c.cy in
    if (dx *. dx) +. (dy *. dy) <= (c.d /. 2.) ** 2. then Some (i, (dx *. dx) +. (dy *. dy)) else None
  in
  List.filteri (fun _ _ -> true) circles
  |> List.mapi inside
  |> List.filter_map (fun x -> x)
  |> List.fold_left
       (fun best (i, d2) ->
         match best with Some (_, best_d2) when best_d2 <= d2 -> best | _ -> Some (i, d2))
       None
  |> Option.map fst

let diameter_of model i =
  match List.nth_opt (shown model) i with Some c -> c.d | None -> 60.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  let m = computer.mouse in
  Gui.label_in computer (box Title) "7GUIs 6: Circle Drawer";
  let canvas : Widget.box = box Canvas in
  let circles = shown model in
  let selected = under (m.mx, m.my) circles in
  (* the dialog, when one is open: the slider changes the circle now,
     and nothing is recorded until "done" *)
  let model =
    match model.adjusting with
    | None -> model
    | Some i ->
        let c = List.nth_opt circles i in
        Gui.label_in computer (box Dialog_label)
          (match c with
          | Some c -> Printf.sprintf "diameter of the circle at (%.0f, %.0f)" c.cx c.cy
          | None -> "diameter");
        let d =
          Gui.slider_in computer (box Dialog_slider) ~from:10. ~to_:200. (diameter_of model i)
        in
        let live = List.mapi (fun j c -> if j = i then { c with d } else c) circles in
        if Gui.button_in computer (box Dialog_done) "done" then
          (* one edit for the whole adjustment, however far the slider
             travelled while it was open *)
          { model with circles = Undo.record ~name:"Adjust Diameter" live model.circles;
            adjusting = None; live = None }
        else { model with live = Some live }
  in
  (* the canvas: a left click puts a circle down, a right click on one
     opens the dialog. Both are ignored while the dialog is open,
     which a real toolkit would do with a modal grab *)
  let rdown_edge = m.mrdown && not model.was_rdown in
  let model =
    if model.adjusting <> None then model
    else if rdown_edge then
      match selected with Some i -> { model with adjusting = Some i } | None -> model
      (* a circle is put down whole: the click has to be far enough
         inside the canvas for it to fit, since nothing here clips *)
    else if m.mclick && Widget.contains (Widget.inset 30. canvas) m.mx m.my then
      { model with
        circles =
          Undo.record ~name:"Add Circle"
            (Undo.now model.circles @ [ { cx = m.mx; cy = m.my; d = 60. } ])
            model.circles }
    else model
  in
  (* the buttons say "undo" and keep their size; which edit they would
     take back is in the line below them, where a menu would put it *)
  let model =
    if Gui.button_in ~enabled:(Undo.can_undo model.circles) computer (box Undo_) "undo" then
      { model with circles = Undo.undo model.circles; adjusting = None; live = None }
    else model
  in
  let model =
    if Gui.button_in ~enabled:(Undo.can_redo model.circles) computer (box Redo) "redo" then
      { model with circles = Undo.redo model.circles; adjusting = None; live = None }
    else model
  in
  Gui.label_in computer (box Status)
    (Printf.sprintf "circles %d   back %d   forward %d   undo: %s"
       (List.length (shown model)) (Undo.undos model.circles) (Undo.redos model.circles)
       (match Undo.undo_name model.circles with Some n -> n | None -> "-"));
  { model with was_rdown = m.mrdown }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  let at = places computer in
  let canvas : Widget.box = List.assoc Canvas at in
  let th = Gui.theme () in
  let m = computer.mouse in
  let circles = shown model in
  let selected = under (m.mx, m.my) circles in
  [ rectangle th.background s.width s.height; rectangle white canvas.w canvas.h |> move canvas.x canvas.y ]
  (* an outlined circle is a filled one with a smaller one on top: the
     playground draws filled shapes, and that is enough *)
  @ List.concat
      (List.mapi
         (fun i c ->
           [
             circle th.edge c.d |> move c.cx c.cy;
             circle (if selected = Some i then th.face_hot else white) (c.d -. 4.) |> move c.cx c.cy;
           ])
         circles)
  @ Gui.draw ()
  @ [
      words (rgb 120 120 120) "click to add a circle, right-click one to change its size"
      |> move_y (-350.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
