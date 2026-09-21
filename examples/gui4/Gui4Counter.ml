(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slot = Count | Bump

let layout th =
  Layout.(center (column ~gap:10. [ leaf Count (Gui4.label_size th "000"); leaf Bump (Immediate.button_size th "count") ]))

let make th panel (arch : Gui4.architecture) : Gui4.runner =
  let box = Gui4.places panel (layout th) in
  match arch with
  (* ---- immediate mode: the count is ours, the button a question ---- *)
  | Immediate ->
      let count = ref 0 in
      let frame u =
        (* the button asked first, so that the label shows this frame's
           click *)
        let u, clicked = Immediate.button u (box Bump) "count" in
        if clicked then incr count;
        Immediate.label u (box Count) (string_of_int !count)
      in
      { step = Gui4.immediate th frame; summary = (fun () -> string_of_int !count) }
  (* ---- callbacks: the count in a ref, and the label told by hand ---- *)
  | Callbacks ->
      let count = ref 0 in
      let shown = Retained.label (box Count) "0" in
      let bump = Retained.button (box Bump) "count" (fun () -> incr count; Retained.set_text shown (string_of_int !count)) in
      let ui = Retained.window (Retained.group [ shown; bump ]) in
      { step = Gui4.retained th ui; summary = (fun () -> string_of_int !count) }
  (* ---- MVC: the model the truth, the view told when it changes ---- *)
  | Mvc ->
      let model = Mvc.create 0 in
      let shown = Retained.label (box Count) "0" in
      let bump = Retained.button (box Bump) "count" (fun () -> Mvc.change model (fun n -> n + 1)) in
      Mvc.on_change model (fun () -> Retained.set_text shown (string_of_int (Mvc.get model)));
      let ui = Retained.window (Retained.group [ shown; bump ]) in
      { step = Gui4.retained th ui; summary = (fun () -> string_of_int (Mvc.get model)) }
  (* ---- MVU: a message, an update, and a view of the model ---- *)
  | Mvu ->
      let update `Bumped n = n + 1 in
      let view n = Mvu.group [ Mvu.label (box Count) (string_of_int n); Mvu.button (box Bump) "count" `Bumped ] in
      let model = ref 0 and state = ref Mvu.empty in
      let step i =
        let st, m, paint = Mvu.step th i !state ~view ~update !model in
        state := st;
        model := m;
        paint
      in
      { step; summary = (fun () -> string_of_int !model) }
