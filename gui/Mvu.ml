(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mvu.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'msg element =
  | Button of Widget.box * string * 'msg * bool (* enabled *)
  | Label of Widget.box * string
  | Field of Widget.box * string * (string -> 'msg) * bool
  | Slider of Widget.box * float * float * float * (float -> 'msg) (* from, to, value *)
  | Progress of Widget.box * float
  | Menu of Widget.box * string list * int * (int -> 'msg)
  | Group of 'msg element list

let button ?(enabled = true) box s msg = Button (box, s, msg, enabled)
let label box s = Label (box, s)
let field ?(enabled = true) box s f = Field (box, s, f, enabled)
let slider box ~from ~to_ v f = Slider (box, from, to_, v, f)
let progress box fraction = Progress (box, fraction)
let menu box items chosen f = Menu (box, items, chosen, f)
let group kids = Group kids

(* The view is rebuilt every frame, so nothing about *how* it is being
 * used can live in it: this is what is underneath -- in Elm, the
 * browser. *)
type t = {
  focus : Widget.id option;
  caret : int;
  was_down : bool;
  keys_before : string list;
  (* the press that is going on began in this widget *)
  held : Widget.id option;
  (* the menu showing its items, which has the mouse wherever it goes *)
  open_menu : Widget.id option;
}

let empty = { focus = None; caret = 0; was_down = false; keys_before = []; held = None; open_menu = None }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec leaves = function Group kids -> List.concat_map leaves kids | e -> [ e ]
let box_of = function
  | Button (b, _, _, _) | Label (b, _) | Field (b, _, _, _) | Slider (b, _, _, _, _) | Progress (b, _) | Menu (b, _, _, _) -> b
  | Group _ -> assert false

let takes_keys = function Field (_, _, _, enabled) -> enabled | _ -> false
let enabled = function Button (_, _, _, e) | Field (_, _, _, e) -> e | Label _ | Progress _ -> false | _ -> true

(*****************************************************************************)
(* The events of one frame *)
(*****************************************************************************)

let events (th : Theme.t) (i : Widget.input) (t : t) view =
  let pressed k = List.mem k i.keys && not (List.mem k t.keys_before) in
  let press = i.mdown && not t.was_down in
  let widgets = leaves view in
  let id e = Widget.id (box_of e) in
  let hot e =
    enabled e && Widget.contains (box_of e) i.mx i.my
    && (match t.open_menu with Some m -> m = id e | None -> true)
  in
  (* Tab walks the view's own order, which is the order the view
     function wrote the widgets in *)
  let fields = List.filter takes_keys widgets in
  let focus =
    if pressed "Tab" then (
      let order = if List.mem "Shift" i.keys then List.rev fields else fields in
      let rec after = function
        | [] -> ( match order with e :: _ -> Some (id e) | [] -> None)
        | [ last ] -> if Some (id last) = t.focus then (match order with e :: _ -> Some (id e) | [] -> None) else after []
        | a :: (b :: _ as rest) -> if Some (id a) = t.focus then Some (id b) else after rest
      in
      after order)
    else t.focus
  in
  let held =
    if press then List.find_opt hot widgets |> Option.map id
    else if i.mdown || i.mclick then t.held
    else None
  in
  let clicked = if i.mclick then List.find_opt (fun e -> hot e && Some (id e) = held) widgets else None in
  (* a click on a field gives it the keys, and takes them from
     whatever had them; a click on nothing takes them away *)
  let focus =
    match clicked with
    | Some e when takes_keys e -> Some (id e)
    (* a button leaves the keys where they were; a click on nothing
       takes them away *)
    | Some _ -> focus
    | None -> if i.mclick && held = None then None else focus
  in
  let caret =
    match (clicked, focus) with
    | Some (Field (b, text, _, _) as e), _ when Some (id e) = focus ->
        Text.byte_of_column text (Look.field_column_at th b text ~caret:t.caret i.mx)
    | _ ->
        if focus <> t.focus then
          match List.find_opt (fun e -> Some (id e) = focus) widgets with
          | Some (Field (_, text, _, _)) -> String.length text
          | _ -> t.caret
        else t.caret
  in
  (* the menu showing its items, and the one under the mouse *)
  let menu_under e =
    match e with
    | Menu (b, items, _, _) when t.open_menu = Some (id e) ->
        List.find_opt (fun k -> Widget.contains (Look.menu_item th b k) i.mx i.my) (List.init (List.length items) Fun.id)
    | _ -> None
  in
  let open_menu =
    match clicked with
    | Some (Menu _ as e) -> if t.open_menu = Some (id e) then None else Some (id e)
    | _ -> if i.mclick then None else t.open_menu
  in
  (* and now the only thing this architecture does with all of it:
     turn it into messages *)
  let msgs =
    List.concat_map
      (fun e ->
        match e with
        | Button (_, _, msg, _) when clicked = Some e -> [ msg ]
        | Field (_, text, to_msg, _) when Some (id e) = focus ->
            let after, _ = Text.edit ~typed:i.typed ~pressed text caret in
            if after <> text then [ to_msg after ] else []
        | Slider (b, from, to_, _, to_msg) when held = Some (id e) && i.mdown ->
            let travel = max 0. (b.w -. th.knob) in
            if travel <= 0. then []
            else
              let x0 = Widget.left b +. (th.knob /. 2.) in
              [ to_msg (from +. (max 0. (min 1. ((i.mx -. x0) /. travel)) *. (to_ -. from))) ]
        | Menu (_, _, _, to_msg) when i.mclick -> ( match menu_under e with Some k -> [ to_msg k ] | None -> [])
        | _ -> [])
      widgets
  in
  let caret =
    match List.find_opt (fun e -> Some (id e) = focus) widgets with
    | Some (Field (_, text, _, _)) -> snd (Text.edit ~typed:i.typed ~pressed text caret)
    | _ -> caret
  in
  let t' = { focus; caret; was_down = i.mdown; keys_before = i.keys; held = (if i.mdown then held else None); open_menu } in
  let paint =
    List.concat_map
      (fun e ->
        match e with
        | Button (b, s, _, enabled) -> Look.button th b s ~hot:(hot e) ~held:(Some (id e) = held && i.mdown) ~enabled
        | Label (b, s) -> Look.label th b s
        | Field (b, text, _, enabled) ->
            Look.field th b text ~caret:(if Some (id e) = focus && enabled then Some caret else None) ~enabled
        | Slider (b, from, to_, v, _) ->
            let fraction = if to_ = from then 0. else max 0. (min 1. ((v -. from) /. (to_ -. from))) in
            Look.slider th b ~fraction ~hot:(hot e) ~held:(Some (id e) = held && i.mdown)
        | Progress (b, fraction) -> Look.progress th b fraction
        | Menu (b, items, chosen, _) ->
            let label = match List.nth_opt items chosen with Some s -> s | None -> "" in
            Look.menu_closed th b label ~hot:(hot e) ~held:(Some (id e) = held && i.mdown)
            @ if open_menu = Some (id e) then Look.menu_items th b items ~under:(menu_under e) else []
        | Group _ -> [])
      widgets
  in
  (t', msgs, paint)

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

let step (th : Theme.t) (i : Widget.input) (t : t) ~view ~update model =
  (* the loop, in three lines: what the person did to this model, what
   * the model becomes, and the picture of *that* *)
  let t, msgs, _ = events th i t (view model) in
  let model = List.fold_left (fun m msg -> update msg m) model msgs in
  let _, _, paint = events th { i with typed = ""; mclick = false; keys = t.keys_before } t (view model) in
  (t, model, paint)
