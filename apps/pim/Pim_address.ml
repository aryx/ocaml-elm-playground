(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pim_address.mli *)

(* a card being written: which (None, a new one), its fields, and the
 * one being written into *)
type edit = { uid : string option; fields : string list; field : int }

type mode = List | Card of string | Edit of edit
type t = { mode : mode; lookup : string; top : int }

let start : t = { mode = List; lookup = ""; top = 0 }

let y0 = 15.
let rows = 12
let labels = [ "Last name"; "First name"; "Company"; "Work"; "E-mail" ]

let sorted (d : Palm.data) : Vcard.card list = List.sort Vcard.compare_by_name d.cards
let find (d : Palm.data) (uid : string) : Vcard.card option = List.find_opt (fun (c : Vcard.card) -> c.uid = uid) d.cards

(* the first card whose last name starts with what was written *)
let looked_up (d : Palm.data) (lookup : string) : int option =
  let l = String.lowercase_ascii lookup in
  let rec go i = function
    | [] -> None
    | (c : Vcard.card) :: rest ->
        let name = String.lowercase_ascii (if c.name.family <> "" then c.name.family else Vcard.display_name c) in
        if String.starts_with ~prefix:l name then Some i else go (i + 1) rest
  in
  if lookup = "" then None else go 0 (sorted d)

let fields_of (c : Vcard.card) : string list =
  [ c.name.family; c.name.given; c.org;
    (match c.phones with p :: _ -> p.number | [] -> "");
    (match c.emails with e :: _ -> e.address | [] -> "") ]

(* the fields written back into the card (the first phone and e-mail
 * replaced, the others kept) *)
let card_of (c : Vcard.card) (fields : string list) : Vcard.card =
  let f i = List.nth fields i in
  let phones : Vcard.phone list =
    match (f 3, c.phones) with
    | "", _ :: rest -> rest
    | "", [] -> []
    | number, p :: rest -> { p with number } :: rest
    | number, [] -> [ { number; kinds = [ "work" ] } ]
  in
  let emails : Vcard.email list =
    match (f 4, c.emails) with
    | "", _ :: rest -> rest
    | "", [] -> []
    | address, e :: rest -> { e with address } :: rest
    | address, [] -> [ { address; kinds = [ "internet" ] } ]
  in
  { c with
    name = { c.name with family = f 0; given = f 1 };
    full_name = String.trim (f 1 ^ " " ^ f 0);
    org = f 2;
    phones;
    emails }

let list_buttons = [ ("New", (128., 147., Palm.width "New" +. 10., 12.)) ]
let card_buttons = Palm.buttons [ "Done"; "Edit" ]
let edit_buttons = Palm.buttons [ "Done"; "Delete" ]

let type_into (i : Palm.input) (s : string) : string =
  let s = s ^ i.typed in
  if i.backspace && s <> "" then String.sub s 0 (String.length s - 1) else s

let update (i : Palm.input) (d : Palm.data) (t : t) : Palm.data * t =
  match t.mode with
  | List -> (
      let lookup = type_into i t.lookup in
      let top = match looked_up d lookup with Some n -> max 0 (min n (List.length d.cards - rows)) | None -> t.top in
      let top = if i.up then max 0 (top - 1) else if i.down then min (max 0 (List.length d.cards - rows)) (top + 1) else top in
      let t = { t with lookup; top } in
      match Palm.tapped i list_buttons with
      | Some "New" -> (d, { t with mode = Edit { uid = None; fields = List.map (fun _ -> "") labels; field = 0 } })
      | _ -> (
          let cards = sorted d in
          let chosen =
            match Palm.row_at ~y0 ~rows i with
            | Some r -> List.nth_opt cards (t.top + r)
            | None -> if i.enter then Option.bind (looked_up d lookup) (List.nth_opt cards) else None
          in
          match chosen with Some c -> (d, { t with mode = Card c.uid; lookup = "" }) | None -> (d, t)))
  | Card uid -> (
      match (Palm.tapped i card_buttons, find d uid) with
      | Some "Done", _ | _, None -> (d, { t with mode = List })
      | Some "Edit", Some c -> (d, { t with mode = Edit { uid = Some uid; fields = fields_of c; field = 0 } })
      | _ -> (d, t))
  | Edit e -> (
      let n = List.length labels in
      let field =
        match Palm.row_at ~y0 ~rows:n i with
        | Some r -> r
        | None -> if i.tab || i.enter || i.down then (e.field + 1) mod n else if i.up then (e.field + n - 1) mod n else e.field
      in
      let fields = List.mapi (fun k f -> if k = e.field then type_into i f else f) e.fields in
      let e = { e with fields; field } in
      match Palm.tapped i edit_buttons with
      | Some "Done" ->
          if List.for_all (fun f -> String.trim f = "") fields then (d, { t with mode = List })
          else (
            match e.uid with
            | Some uid ->
                ( { d with cards = List.map (fun (c : Vcard.card) -> if c.uid = uid then card_of c fields else c) d.cards },
                  { t with mode = Card uid } )
            | None ->
                let uid, d = Palm.uid d in
                ({ d with cards = d.cards @ [ card_of { (Vcard.make "") with uid } fields ] }, { t with mode = Card uid }))
      | Some "Delete" ->
          ( { d with cards = List.filter (fun (c : Vcard.card) -> Some c.uid <> e.uid) d.cards },
            { t with mode = List } )
      | _ -> (d, { t with mode = Edit e }))

let view ~(time : float) (d : Palm.data) (t : t) : Playground.shape list =
  match t.mode with
  | List ->
      let cards = sorted d in
      let found = looked_up d t.lookup in
      let row r (c : Vcard.card) =
        let y = y0 +. (float_of_int r *. 11.) in
        let name = if c.name.family <> "" then c.name.family ^ ", " ^ c.name.given else Vcard.display_name c in
        let phone = match c.phones with p :: _ -> p.number | [] -> "" in
        let chosen = found = Some (t.top + r) in
        let color = if chosen then Palm.paper else Palm.ink in
        (if chosen then [ Palm.rect Palm.ink (0., y, Palm.size, 11.) ] else [])
        @ [ Palm.text ~color ~x:2. ~y (Palm.fit 100. name); Palm.text_right ~color ~x:158. ~y phone ]
      in
      Palm.title "Address"
      @ List.concat (List.mapi row (List.filteri (fun k _ -> k >= t.top && k < t.top + rows) cards))
      @ (let x = 5. +. Palm.width "Look Up:" in
         [ Palm.text ~bold:true ~x:1. ~y:148. "Look Up:"; Palm.text ~x ~y:148. t.lookup; Palm.rect Palm.ink (x, 158., 60., 0.5) ]
         @ Palm.caret ~time ~x ~y:148. t.lookup)
      @ Palm.draw_buttons list_buttons
  | Card uid -> (
      match find d uid with
      | None -> []
      | Some c ->
          let lines =
            [ (true, Vcard.display_name c); (false, c.org); (false, c.title) ]
            @ List.map (fun (p : Vcard.phone) -> (false, Printf.sprintf "%s: %s" (String.capitalize_ascii (match p.kinds with k :: _ -> k | [] -> "tel")) p.number)) c.phones
            @ List.map (fun (e : Vcard.email) -> (false, "E-mail: " ^ e.address)) c.emails
            @ List.concat_map
                (fun (a : Vcard.address) ->
                  [ (false, a.street); (false, String.concat " " (List.filter (( <> ) "") [ a.locality; a.region; a.code ])); (false, a.country) ])
                c.addresses
            @ (match c.birthday with Some b -> [ (false, "Birthday: " ^ Civil.to_string b) ] | None -> [])
            @ [ (false, c.note) ]
            |> List.filter (fun (_, s) -> s <> "")
          in
          Palm.title "Address View"
          @ List.mapi (fun r (bold, s) -> Palm.text ~bold ~x:4. ~y:(y0 +. 2. +. (float_of_int r *. 11.)) (Palm.fit 150. s)) lines
          @ Palm.draw_buttons card_buttons)
  | Edit e ->
      Palm.title "Address Edit"
      @ List.concat
          (List.mapi
             (fun r (label, value) ->
               let y = y0 +. (float_of_int r *. 11.) in
               [ Palm.text ~color:Palm.mid ~x:2. ~y (label ^ ":"); Palm.text ~x:54. ~y (Palm.fit 100. value);
                 Palm.rect Palm.light (54., y +. 10., 104., 0.5) ]
               @ if r = e.field then Palm.caret ~time ~x:54. ~y value else [])
             (List.combine labels e.fields))
      @ Palm.draw_buttons (if e.uid = None then [ List.hd edit_buttons ] else edit_buttons)
