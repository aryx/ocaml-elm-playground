(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slot = C_label | C | F_label | F

let layout th =
  Layout.(
    center
      (column ~gap:12.
         [
           leaf C_label (Gui4.label_size th "Celsius");
           leaf C (Immediate.field_size th);
           leaf F_label (Gui4.label_size th "Fahrenheit");
           leaf F (Immediate.field_size th);
         ]))

(* the rule, the same in all four: a number typed into one converts
   into the other; anything else is kept as typed, the other left *)
type model = { c : string; f : string }

let initial = { c = "20"; f = "68" }
let show v = Printf.sprintf "%.1f" v

let typed_c c m = match float_of_string_opt c with Some v -> { c; f = show ((v *. 9. /. 5.) +. 32.) } | None -> { m with c }
let typed_f f m = match float_of_string_opt f with Some v -> { f; c = show ((v -. 32.) *. 5. /. 9.) } | None -> { m with f }
let summary m = Printf.sprintf "%s C = %s F" m.c m.f

let make th panel (arch : Gui4.architecture) : Gui4.runner =
  let box = Gui4.places panel (layout th) in
  match arch with
  (* ---- immediate: the model is ours, each field a question ---- *)
  | Immediate ->
      let m = ref initial in
      let frame u =
        (* each field's change put into the model at once, so that the
           field asked for after it shows the conversion this frame *)
        let u = Immediate.label u (box C_label) "Celsius" in
        let u, c = Immediate.field u (box C) !m.c in
        if c <> !m.c then m := typed_c c !m;
        let u = Immediate.label u (box F_label) "Fahrenheit" in
        let u, f = Immediate.field u (box F) !m.f in
        if f <> !m.f then m := typed_f f !m;
        u
      in
      { step = Gui4.immediate th frame; summary = (fun () -> summary !m) }
  (* ---- callbacks: the fields hold the temperatures, and each one's
     handler writes into the other ---- *)
  | Callbacks ->
      let other = ref None and one = ref None in
      let c_field =
        Retained.field (box C) initial.c (fun c ->
            match (float_of_string_opt c, !other) with Some v, Some f -> Retained.set_text f (show ((v *. 9. /. 5.) +. 32.)) | _ -> ())
      in
      let f_field =
        Retained.field (box F) initial.f (fun f ->
            match (float_of_string_opt f, !one) with Some v, Some c -> Retained.set_text c (show ((v -. 32.) *. 5. /. 9.)) | _ -> ())
      in
      other := Some f_field;
      one := Some c_field;
      let ui =
        Retained.window
          (Retained.group [ Retained.label (box C_label) "Celsius"; c_field; Retained.label (box F_label) "Fahrenheit"; f_field ])
      in
      { step = Gui4.retained th ui; summary = (fun () -> summary { c = Retained.text c_field; f = Retained.text f_field }) }
  (* ---- MVC: one model, the fields its controllers and its views ---- *)
  | Mvc ->
      let model = Mvc.create initial in
      let c_field = Retained.field (box C) initial.c (fun c -> Mvc.change model (typed_c c)) in
      let f_field = Retained.field (box F) initial.f (fun f -> Mvc.change model (typed_f f)) in
      Mvc.on_change model (fun () ->
          let m = Mvc.get model in
          Retained.set_text c_field m.c;
          Retained.set_text f_field m.f);
      let ui =
        Retained.window
          (Retained.group [ Retained.label (box C_label) "Celsius"; c_field; Retained.label (box F_label) "Fahrenheit"; f_field ])
      in
      { step = Gui4.retained th ui; summary = (fun () -> summary (Mvc.get model)) }
  (* ---- MVU: two messages, one update, one view ---- *)
  | Mvu ->
      let update msg m = match msg with `C c -> typed_c c m | `F f -> typed_f f m in
      let view m =
        Mvu.group
          [
            Mvu.label (box C_label) "Celsius";
            Mvu.field (box C) m.c (fun c -> `C c);
            Mvu.label (box F_label) "Fahrenheit";
            Mvu.field (box F) m.f (fun f -> `F f);
          ]
      in
      let model = ref initial and state = ref Mvu.empty in
      let step i =
        let st, m, paint = Mvu.step th i !state ~view ~update !model in
        state := st;
        model := m;
        paint
      in
      { step; summary = (fun () -> summary !model) }
