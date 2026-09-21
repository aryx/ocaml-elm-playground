(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Retained.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type kind =
  | Button of (unit -> unit)
  | Label
  | Field of (string -> unit)
  | Slider of float * float * (float -> unit) (* from, to, on change *)
  | Progress
  | Menu of string list * (int -> unit)
  | Group of t list

and t = {
  box : Widget.box;
  kind : kind;
  (* everything below is the widget's own, which is what "retained"
     means: the toolkit does not recompute it, it keeps it *)
  mutable text : string;
  mutable enabled : bool;
  mutable hot : bool;
  mutable held : bool;
  mutable focused : bool;
  mutable caret : int;
  (* a slider's value, a progress bar's fraction *)
  mutable value : float;
  (* a menu's chosen item, and whether its items are showing *)
  mutable chosen : int;
  mutable opened : bool;
  mutable under : int option;
}

type ui = { root : t; mutable was_down : bool; mutable keys_before : string list }

(*****************************************************************************)
(* Building the tree *)
(*****************************************************************************)

let make box kind text =
  { box; kind; text; enabled = true; hot = false; held = false; focused = false; caret = 0; value = 0.; chosen = 0; opened = false; under = None }

let button box s f = make box (Button f) s
let label box s = make box Label s
let field box s f = make box (Field f) s
let slider box ~from ~to_ v f = { (make box (Slider (from, to_, f)) "") with value = v }
let progress box fraction = { (make box Progress "") with value = fraction }
let menu box items chosen f = { (make box (Menu (items, f)) "") with chosen }

(* a group has no rectangle of its own: it is its children *)
let group kids = make { Widget.x = 0.; y = 0.; w = 0.; h = 0. } (Group kids) ""

let text t = t.text
let set_text t s = t.text <- s
let set_enabled t b = t.enabled <- b
let value t = t.value
let set_value t v = t.value <- v
let chosen t = t.chosen
let window root = { root; was_down = false; keys_before = [] }

let rec leaves t =
  match t.kind with Group kids -> List.concat_map leaves kids | _ -> [ t ]

let takes_keys t = match t.kind with Field _ -> t.enabled | _ -> false

(*****************************************************************************)
(* One frame: the callbacks fire *)
(*****************************************************************************)

let handle (i : Widget.input) (ui : ui) =
  let pressed k = List.mem k i.keys && not (List.mem k ui.keys_before) in
  let press = i.mdown && not ui.was_down in
  let widgets = leaves ui.root in
  (* Tab walks the tree, which here *is* the tab order: the widgets
     are objects in an order, and the order they were built in is the
     one the walk finds *)
  (if pressed "Tab" then
     let fields = List.filter takes_keys widgets in
     let order = if List.mem "Shift" i.keys then List.rev fields else fields in
     let rec next = function
       | [] -> ()
       | [ last ] -> if last.focused then (last.focused <- false; match order with f :: _ -> f.focused <- true; f.caret <- String.length f.text | [] -> ())
       | a :: (b :: _ as rest) ->
           if a.focused then (
             a.focused <- false;
             b.focused <- true;
             b.caret <- String.length b.text)
           else next rest
     in
     if List.exists (fun w -> w.focused) order then next order
     else match order with f :: _ -> f.focused <- true; f.caret <- String.length f.text | [] -> ());
  (* a menu showing its items has the mouse, wherever it goes *)
  let open_menu = List.find_opt (fun w -> w.opened) widgets in
  (* the mouse: each widget keeps whether it is under it and whether
     the press that is going on began inside it *)
  let claimed = ref false in
  List.iter
    (fun w ->
      w.hot <-
        w.enabled && Widget.contains w.box i.mx i.my
        && (match open_menu with Some m -> m == w | None -> true);
      if press && w.hot && not !claimed then (
        claimed := true;
        w.held <- true);
      if not i.mdown && not i.mclick then w.held <- false)
    widgets;
  (* a click on a field gives it the keys; one on a widget that does
     not take them (a button) leaves them where they were, the Mac's
     rule and the immediate toolkit's; one on nothing at all takes
     them away *)
  (if i.mclick then
     let hit = List.find_opt (fun w -> w.hot && w.held) widgets in
     let on_nothing = not (List.exists (fun w -> w.held) widgets) in
     List.iter
       (fun w ->
         let now =
           match hit with
           | Some h when takes_keys h -> h == w
           | Some _ -> w.focused
           | None -> if on_nothing then false else w.focused
         in
         (* claude: and the caret where the click landed, as the other
          * three put it (found by examples/gui4/tests/Unit_gui4) *)
         if now then w.caret <- Text.byte_of_column w.text (Look.field_column_at Theme.default w.box w.text ~caret:w.caret i.mx);
         w.focused <- now)
       widgets);
  (* and then the callbacks, which is the whole of this architecture *)
  (* the default theme's knob, for where a slider's value is: [handle]
     has no theme, the one thing it would need one for *)
  let th = Theme.default in
  List.iter
    (fun w ->
      (match w.kind with
      (* a slider follows the mouse while the press that began on it
         lasts *)
      | Slider (from, to_, f) when w.held && i.mdown ->
          let travel = max 0. (w.box.w -. th.knob) in
          if travel > 0. then (
            let x0 = Widget.left w.box +. (th.knob /. 2.) in
            let v = from +. (max 0. (min 1. ((i.mx -. x0) /. travel)) *. (to_ -. from)) in
            w.value <- v;
            f v)
      (* a menu: a click on it opens or closes it; while it is open, a
         click on an item chooses it, and a click anywhere closes it *)
      | Menu (items, f) ->
          let was_open = w.opened in
          let under =
            if not was_open then None
            else List.find_opt (fun k -> Widget.contains (Look.menu_item th w.box k) i.mx i.my) (List.init (List.length items) Fun.id)
          in
          w.under <- under;
          (match under with
          | Some k when i.mclick ->
              w.chosen <- k;
              f k
          | _ -> ());
          if i.mclick && w.hot && w.held then w.opened <- not was_open
          else if was_open && i.mclick then w.opened <- false
      | _ -> ());
      if i.mclick && w.hot && w.held then (
        w.held <- false;
        match w.kind with Button f -> f () | _ -> ());
      if w.focused then
        match w.kind with
        | Field on_change ->
            let text, caret = Text.edit ~typed:i.typed ~pressed w.text w.caret in
            w.caret <- caret;
            if text <> w.text then (
              w.text <- text;
              on_change text)
        | _ -> ())
    widgets;
  (* a press that is over is over, wherever the mouse was let go *)
  if not i.mdown then List.iter (fun w -> w.held <- false) widgets;
  ui.was_down <- i.mdown;
  ui.keys_before <- i.keys

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let paint (th : Theme.t) (ui : ui) =
  leaves ui.root
  |> List.concat_map (fun w ->
         match w.kind with
         | Button _ -> Look.button th w.box w.text ~hot:w.hot ~held:w.held ~enabled:w.enabled
         | Label -> Look.label th w.box w.text
         | Field _ ->
             Look.field th w.box w.text
               ~caret:(if w.focused then Some w.caret else None)
               ~enabled:w.enabled
         | Slider (from, to_, _) ->
             let fraction = if to_ = from then 0. else max 0. (min 1. ((w.value -. from) /. (to_ -. from))) in
             Look.slider th w.box ~fraction ~hot:w.hot ~held:w.held
         | Progress -> Look.progress th w.box w.value
         | Menu (items, _) ->
             let label = match List.nth_opt items w.chosen with Some s -> s | None -> "" in
             Look.menu_closed th w.box label ~hot:w.hot ~held:w.held
             @ if w.opened then Look.menu_items th w.box items ~under:w.under else []
         | Group _ -> [])
