(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slot = Bar_label | Bar | Figure | Duration_label | Duration | Reset

let layout th =
  Layout.(
    center
      (column ~gap:10.
         [
           leaf Bar_label (Gui4.label_size th "Elapsed Time");
           leaf Bar (Immediate.progress_size th);
           leaf Figure (Gui4.label_size th "10.0s");
           leaf Duration_label (Gui4.label_size th "Duration");
           leaf Duration (Immediate.slider_size th);
           leaf Reset (Immediate.button_size th "Reset");
         ]))

(* the rules, the same in all four *)
type model = { frames : int; duration : float (* seconds *) }

let initial = { frames = 0; duration = 10. }
let elapsed m = float_of_int m.frames /. 60.

(* full once the elapsed time reaches the duration; a duration of zero
   is all of it, not a division by zero *)
let fraction m = if m.duration <= 0. then 1. else min 1. (elapsed m /. m.duration)
let figure m = Printf.sprintf "%.1fs" (elapsed m)
let tick m = { m with frames = m.frames + 1 }
let summary m = Printf.sprintf "%s of %.1fs" (figure m) m.duration

let make th panel (arch : Gui4.architecture) : Gui4.runner =
  let box = Gui4.places panel (layout th) in
  let from = 0. and to_ = 30. in
  match arch with
  (* ---- immediate: the clock is a variable, advanced every frame ---- *)
  | Immediate ->
      let m = ref initial in
      let frame u =
        m := tick !m;
        (* the slider and Reset asked first, so that the bar and the
           figure show this frame's changes *)
        let u, duration = Immediate.slider u (box Duration) ~from ~to_ !m.duration in
        let u, reset = Immediate.button u (box Reset) "Reset" in
        m := { frames = (if reset then 0 else !m.frames); duration };
        let u = Immediate.label u (box Bar_label) "Elapsed Time" in
        let u = Immediate.progress u (box Bar) (fraction !m) in
        let u = Immediate.label u (box Figure) (figure !m) in
        Immediate.label u (box Duration_label) "Duration"
      in
      { step = Gui4.immediate th frame; summary = (fun () -> summary !m) }
  (* ---- callbacks: the clock is one more callback, and every one of
     them must redraw what it changed ---- *)
  | Callbacks ->
      let frames = ref 0 and duration = ref initial.duration in
      let bar = Retained.progress (box Bar) 0. and shown = Retained.label (box Figure) "0.0s" in
      let redraw () =
        let m = { frames = !frames; duration = !duration } in
        Retained.set_value bar (fraction m);
        Retained.set_text shown (figure m)
      in
      let slider = Retained.slider (box Duration) ~from ~to_ initial.duration (fun v -> duration := v; redraw ()) in
      let reset = Retained.button (box Reset) "Reset" (fun () -> frames := 0; redraw ()) in
      let on_tick () = incr frames; redraw () in
      let ui =
        Retained.window
          (Retained.group
             [ Retained.label (box Bar_label) "Elapsed Time"; bar; shown; Retained.label (box Duration_label) "Duration"; slider; reset ])
      in
      { step = Gui4.retained ~before:on_tick th ui; summary = (fun () -> summary { frames = !frames; duration = !duration }) }
  (* ---- MVC: the tick is a change to the model like any other ---- *)
  | Mvc ->
      let model = Mvc.create initial in
      let bar = Retained.progress (box Bar) 0. and shown = Retained.label (box Figure) "0.0s" in
      let slider = Retained.slider (box Duration) ~from ~to_ initial.duration (fun v -> Mvc.change model (fun m -> { m with duration = v })) in
      let reset = Retained.button (box Reset) "Reset" (fun () -> Mvc.change model (fun m -> { m with frames = 0 })) in
      Mvc.on_change model (fun () ->
          let m = Mvc.get model in
          Retained.set_value bar (fraction m);
          Retained.set_text shown (figure m);
          Retained.set_value slider m.duration);
      let ui =
        Retained.window
          (Retained.group
             [ Retained.label (box Bar_label) "Elapsed Time"; bar; shown; Retained.label (box Duration_label) "Duration"; slider; reset ])
      in
      { step = Gui4.retained ~before:(fun () -> Mvc.change model tick) th ui; summary = (fun () -> summary (Mvc.get model)) }
  (* ---- MVU: the tick is a message, from the clock rather than from a
     widget -- Elm's subscriptions ---- *)
  | Mvu ->
      let update msg m = match msg with `Tick -> tick m | `Duration v -> { m with duration = v } | `Reset -> { m with frames = 0 } in
      let view m =
        Mvu.group
          [
            Mvu.label (box Bar_label) "Elapsed Time";
            Mvu.progress (box Bar) (fraction m);
            Mvu.label (box Figure) (figure m);
            Mvu.label (box Duration_label) "Duration";
            Mvu.slider (box Duration) ~from ~to_ m.duration (fun v -> `Duration v);
            Mvu.button (box Reset) "Reset" `Reset;
          ]
      in
      let model = ref initial and state = ref Mvu.empty in
      let step i =
        model := update `Tick !model;
        let st, m, paint = Mvu.step th i !state ~view ~update !model in
        state := st;
        model := m;
        paint
      in
      { step; summary = (fun () -> summary !model) }
