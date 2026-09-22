(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See File_menu.mli *)

type caps = < Cap.open_in ; Cap.open_out ; Cap.readdir >
type kind = { magic : string; extension : string }

type dialog =
  | Closed
  (* the name being typed *)
  | Saving_as of string
  (* the documents of this kind, and the one selected *)
  | Opening of string list * int option

type t = { name : string option; dialog : dialog; said : string; was : string list }

let start = { name = None; dialog = Closed; said = ""; was = [] }
let items = [ "File"; "New"; "Open..."; "Save"; "Save As..."; "Export" ]

type 'd result = Nothing | New | Opened of 'd

let title t = Option.value t.name ~default:"untitled"
let said t = t.said
let busy t = t.dialog <> Closed

let with_extension kind name =
  let e = kind.extension in
  if Filename.check_suffix name e then name else name ^ e

let save (caps : caps) kind ~current name t =
  let bytes = Saved.to_string ~magic:kind.magic (current ()) in
  Playground_platform.store caps name bytes;
  { t with name = Some name; dialog = Closed; said = Printf.sprintf "saved %s, %d bytes" name (String.length bytes) }

let open_ (caps : caps) kind name t =
  match Option.bind (Playground_platform.fetch caps name) (Saved.of_string ~magic:kind.magic) with
  | Some d -> ({ t with name = Some name; dialog = Closed; said = "opened " ^ name }, Opened d)
  | None -> ({ t with dialog = Closed; said = Printf.sprintf "%s: not a %s document" name (List.hd (String.split_on_char ' ' kind.magic)) }, Nothing)

let command (caps : caps) kind ~current item t =
  match item with
  | "New" -> ({ start with said = "" }, New)
  | "Open..." ->
      let names = List.filter (fun n -> Filename.check_suffix n kind.extension) (Playground_platform.stored caps) in
      ({ t with dialog = Opening (names, None) }, Nothing)
  | "Save" -> (match t.name with Some name -> (save caps kind ~current name t, Nothing) | None -> ({ t with dialog = Saving_as "" }, Nothing))
  | "Save As..." -> ({ t with dialog = Saving_as (match t.name with Some n -> Filename.remove_extension n | None -> "") }, Nothing)
  | "Export" ->
      let name = with_extension kind (title t) in
      let bytes = Saved.to_string ~magic:kind.magic (current ()) in
      Playground_platform.export caps name bytes;
      ({ t with said = Printf.sprintf "exported %s, %d bytes" name (String.length bytes) }, Nothing)
  | _ -> (t, Nothing)

let menu_in ?(items = items) (caps : caps) kind computer box ~current t =
  let picked = Gui.menu_in computer box items 0 in
  if picked > 0 then command caps kind ~current (List.nth items picked) t else (t, Nothing)

let autosave (caps : caps) kind ~current t =
  match t.name with
  | Some name ->
      let bytes = Saved.to_string ~magic:kind.magic (current ()) in
      Playground_platform.store caps name bytes;
      { t with said = Printf.sprintf "%s, saved as it changes" name }
  | None -> t

(* the panel, in the middle of the screen *)
let panel : Widget.box = { Widget.x = 0.; y = 60.; w = 460.; h = 330. }
let row y : Widget.box = { Widget.x = 0.; y = panel.y +. (panel.h /. 2.) -. y; w = panel.w -. 40.; h = 30. }
let button i label : Widget.box = { Widget.x = (if i = 0 then 90. else 190.); y = Widget.bottom panel +. 30.; w = 80.; h = snd (Gui.button_size label) }

let dialog (caps : caps) kind computer ~current t =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key t.was) in
  let t, result =
    match t.dialog with
    | Closed -> (t, Nothing)
    | Saving_as typed ->
        (* the name field takes the keys at once: there is nothing else
           in the dialog to type into *)
        let typed =
          if k.typed <> "" then typed ^ k.typed
          else if pressed "Backspace" && typed <> "" then String.sub typed 0 (String.length typed - 1)
          else typed
        in
        let ok = Gui.button_in computer (button 0 "Save") "Save" || pressed "Enter" in
        let cancel = Gui.button_in computer (button 1 "Cancel") "Cancel" || pressed "Escape" in
        if cancel then ({ t with dialog = Closed; said = "" }, Nothing)
        else if ok && String.trim typed <> "" then (save caps kind ~current (with_extension kind (String.trim typed)) t, Nothing)
        else ({ t with dialog = Saving_as typed }, Nothing)
    | Opening (names, chosen) ->
        let list_box = { (row 60.) with h = 200.; y = (row 60.).y -. 85. } in
        let chosen = if names = [] then None else Gui.list_in computer list_box names chosen in
        let ok = Gui.button_in ~enabled:(chosen <> None) computer (button 0 "Open") "Open" || (pressed "Enter" && chosen <> None) in
        let cancel = Gui.button_in computer (button 1 "Cancel") "Cancel" || pressed "Escape" in
        if cancel then ({ t with dialog = Closed; said = "" }, Nothing)
        else
          match (ok, chosen) with
          | true, Some i -> open_ caps kind (List.nth names i) t
          | _ -> ({ t with dialog = Opening (names, chosen) }, Nothing)
  in
  ({ t with was = now }, result)

let view t =
  let th = Gui.theme () in
  let frame =
    [
      rectangle (rgb 90 90 95) (panel.w +. 6.) (panel.h +. 6.) |> move (panel.x +. 4.) (panel.y -. 4.);
      rectangle th.face panel.w panel.h |> move panel.x panel.y;
    ]
  in
  let heading s = words th.text s |> move 0. (row 20.).y in
  match t.dialog with
  | Closed -> []
  | Saving_as typed ->
      let f = row 70. in
      let w = Widget.text_width ~size:th.text_size typed in
      frame
      @ [
          heading "Save the document as:";
          rectangle white f.w f.h |> move f.x f.y;
          (* from the field's left, the caret after the name *)
          words black typed |> move (Widget.left f +. 8. +. (w /. 2.)) f.y;
          rectangle black 2. 18. |> move (Widget.left f +. 10. +. w) f.y;
          words th.text "Enter to save, Escape to cancel" |> scale 0.8 |> move 0. (f.y -. 50.);
        ]
  | Opening (names, _) -> frame @ [ heading "Open a document:" ] @ if names = [] then [ words th.text "nothing saved yet" |> move 0. 60. ] else []
