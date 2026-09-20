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

type 'msg element =
  | Button of Widget.box * string * 'msg
  | Label of Widget.box * string
  | Field of Widget.box * string * (string -> 'msg)
  | Group of 'msg element list

let button box s msg = Button (box, s, msg)
let label box s = Label (box, s)
let field box s f = Field (box, s, f)
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
}

let empty = { focus = None; caret = 0; was_down = false; keys_before = []; held = None }

let rec leaves = function Group kids -> List.concat_map leaves kids | e -> [ e ]
let box_of = function Button (b, _, _) | Label (b, _) | Field (b, _, _) -> b | Group _ -> assert false
let takes_keys = function Field _ -> true | _ -> false

let events (th : Theme.t) (i : Widget.input) (t : t) view =
  let pressed k = List.mem k i.keys && not (List.mem k t.keys_before) in
  let press = i.mdown && not t.was_down in
  let widgets = leaves view in
  let id e = Widget.id (box_of e) in
  let hot e = Widget.contains (box_of e) i.mx i.my in
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
    | Some _ -> None
    | None -> if i.mclick then None else focus
  in
  let caret =
    match (clicked, focus) with
    | Some (Field (b, text, _) as e), _ when Some (id e) = focus ->
        Text.byte_of_column text (Look.field_column_at th b text ~caret:t.caret i.mx)
    | _ ->
        if focus <> t.focus then
          match List.find_opt (fun e -> Some (id e) = focus) widgets with
          | Some (Field (_, text, _)) -> String.length text
          | _ -> t.caret
        else t.caret
  in
  (* and now the only thing this architecture does with all of it:
     turn it into messages *)
  let msgs =
    List.concat_map
      (fun e ->
        match e with
        | Button (_, _, msg) when clicked = Some e -> [ msg ]
        | Field (_, text, to_msg) when Some (id e) = focus ->
            let after, _ = Text.edit ~typed:i.typed ~pressed text caret in
            if after <> text then [ to_msg after ] else []
        | _ -> [])
      widgets
  in
  let caret =
    match List.find_opt (fun e -> Some (id e) = focus) widgets with
    | Some (Field (_, text, _)) -> snd (Text.edit ~typed:i.typed ~pressed text caret)
    | _ -> caret
  in
  let paint =
    List.concat_map
      (fun e ->
        match e with
        | Button (b, s, _) -> Look.button th b s ~hot:(hot e) ~held:(Some (id e) = held) ~enabled:true
        | Label (b, s) -> Look.label th b s
        | Field (b, text, _) ->
            Look.field th b text ~caret:(if Some (id e) = focus then Some caret else None) ~enabled:true
        | Group _ -> [])
      widgets
  in
  ({ focus; caret; was_down = i.mdown; keys_before = i.keys; held }, msgs, paint)

let step (th : Theme.t) (i : Widget.input) (t : t) ~view ~update model =
  (* the loop, in three lines: what the person did to this model, what
   * the model becomes, and the picture of *that* *)
  let t, msgs, _ = events th i t (view model) in
  let model = List.fold_left (fun m msg -> update msg m) model msgs in
  let _, _, paint = events th { i with typed = ""; mclick = false; keys = t.keys_before } t (view model) in
  (t, model, paint)
