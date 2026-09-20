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

type kind = Button of (unit -> unit) | Label | Field of (string -> unit) | Group of t list

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
}

type ui = { root : t; mutable was_down : bool; mutable keys_before : string list }

let make box kind text =
  { box; kind; text; enabled = true; hot = false; held = false; focused = false; caret = 0 }

let button box s f = make box (Button f) s
let label box s = make box Label s
let field box s f = make box (Field f) s

(* a group has no rectangle of its own: it is its children *)
let group kids = make { Widget.x = 0.; y = 0.; w = 0.; h = 0. } (Group kids) ""

let text t = t.text
let set_text t s = t.text <- s
let set_enabled t b = t.enabled <- b
let window root = { root; was_down = false; keys_before = [] }

let rec leaves t =
  match t.kind with Group kids -> List.concat_map leaves kids | _ -> [ t ]

let takes_keys t = match t.kind with Field _ -> t.enabled | _ -> false

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
  (* the mouse: each widget keeps whether it is under it and whether
     the press that is going on began inside it *)
  let claimed = ref false in
  List.iter
    (fun w ->
      w.hot <- w.enabled && Widget.contains w.box i.mx i.my;
      if press && w.hot && not !claimed then (
        claimed := true;
        w.held <- true);
      if not i.mdown && not i.mclick then w.held <- false)
    widgets;
  (* a click on a field gives it the keys and puts the caret where it
     landed; a click that reached no widget takes them away *)
  (if i.mclick then
     let hit = List.find_opt (fun w -> w.hot && w.held) widgets in
     List.iter (fun w -> w.focused <- (match hit with Some h -> h == w && takes_keys w | None -> false)) widgets);
  (* and then the callbacks, which is the whole of this architecture *)
  List.iter
    (fun w ->
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
  ui.was_down <- i.mdown;
  ui.keys_before <- i.keys

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
         | Group _ -> [])
