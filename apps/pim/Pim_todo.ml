(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pim_todo.mli *)

type t = { selected : string option; show_completed : bool }

let start : t = { selected = None; show_completed = false }

let y0 = 15.
let rows = 12

let due_day (td : Ics.todo) : int option = Option.map (fun (m : Ics.moment) -> Civil.days_from_civil m.date) td.due

(* the Palm's order: priority, then due date, the undated last *)
let shown (d : Palm.data) (t : t) : Ics.todo list =
  List.filter (fun (td : Ics.todo) -> t.show_completed || not td.completed) d.todos
  |> List.stable_sort (fun (a : Ics.todo) (b : Ics.todo) ->
         let p (td : Ics.todo) = if td.priority = 0 then 10 else td.priority in
         let due td = Option.value ~default:max_int (due_day td) in
         compare (p a, due a) (p b, due b))

let buttons (t : t) = Palm.buttons [ "New"; "Due"; (if t.show_completed then "Hide done" else "Show all") ]

let change (d : Palm.data) (uid : string) (f : Ics.todo -> Ics.todo) : Palm.data =
  { d with todos = List.map (fun (td : Ics.todo) -> if td.uid = uid then f td else td) d.todos }

let update (i : Palm.input) (d : Palm.data) (t : t) : Palm.data * t =
  let d, t =
    match Palm.tapped i (buttons t) with
    | Some "New" ->
        let uid, d = Palm.uid d in
        ({ d with todos = d.todos @ [ { uid; summary = ""; due = None; priority = 1; completed = false } ] }, { t with selected = Some uid })
    | Some "Due" -> (
        match t.selected with
        | Some uid ->
            ( change d uid (fun td ->
                  { td with due = (if td.due = None then Some { date = Civil.civil_from_days i.today; time = None; utc = false } else None) }),
              t )
        | None -> (d, t))
    | Some ("Show all" | "Hide done") -> (d, { t with show_completed = not t.show_completed })
    | _ -> (
        match (Palm.row_at ~y0 ~rows i, i.tap) with
        | Some r, Some (x, _) -> (
            match List.nth_opt (shown d t) r with
            | Some td when x < 10. -> (change d td.uid (fun td -> { td with completed = not td.completed }), t)
            | Some td when x < 18. -> (change d td.uid (fun td -> { td with priority = (td.priority mod 5) + 1 }), t)
            | Some td -> (d, { t with selected = Some td.uid })
            | None -> (d, { t with selected = None }))
        | _ -> (d, t))
  in
  (* what is written goes into the selected item; Enter ends it, and an
   * item left with no text is gone *)
  match t.selected with
  | None -> (d, t)
  | Some uid ->
      let d =
        change d uid (fun td ->
            let s = td.summary ^ i.typed in
            { td with summary = (if i.backspace && s <> "" then String.sub s 0 (String.length s - 1) else s) })
      in
      if i.enter then
        ({ d with todos = List.filter (fun (td : Ics.todo) -> td.uid <> uid || td.summary <> "") d.todos }, { t with selected = None })
      else (d, t)

let view ~(time : float) ~(today : int) (d : Palm.data) (t : t) : Playground.shape list =
  let row r (td : Ics.todo) =
    let y = y0 +. (float_of_int r *. 11.) in
    let due =
      match due_day td with
      | Some n ->
          let date = Civil.civil_from_days n in
          Printf.sprintf "%d/%d%s" date.month date.day (if n < today && not td.completed then "!" else "")
      | None -> "-"
    in
    (if t.selected = Some td.uid then [ Palm.rect Palm.light (18., y, Palm.size -. 18., 11.) ] else [])
    @ Palm.checkbox ~x:1. ~y td.completed
    @ [ Palm.text ~x:11. ~y (if td.priority = 0 then "" else string_of_int td.priority);
        Palm.text ~color:(if td.completed then Palm.mid else Palm.ink) ~x:20. ~y (Palm.fit 108. td.summary);
        Palm.text_right ~x:158. ~y due ]
    @ if t.selected = Some td.uid then Palm.caret ~time ~x:20. ~y td.summary else []
  in
  Palm.title "To Do List" @ List.concat (List.mapi row (List.filteri (fun k _ -> k < rows) (shown d t))) @ Palm.draw_buttons (buttons t)
