(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Browser_forms.mli *)

type effect =
  | Nothing
  | Focus of Dom.element
  | Unfocus
  | Changed of Browser_page.t
  | Submit of { url : string; post : (string * string) option; page : Browser_page.t }

let value_of = Browser_page.value_of
let with_value = Browser_page.with_value

let submission (p : Browser_page.t) (form : Forms.form) ~(submitter : Dom.element option) : string * (string * string) option =
  let fields = Urlencoded.encode (Forms.submission form ~value:(value_of p) ~submitter) in
  let action = Browser_url.resolve p.url (if form.action = "" then p.url else form.action) in
  if form.post then (action, Some ("application/x-www-form-urlencoded", fields))
  else (fst (Browser_url.split_query (fst (Browser_url.split_fragment action))) ^ "?" ^ fields, None)

let submit (p : Browser_page.t) (form : Forms.form) ~(submitter : Dom.element option) : effect =
  let url, post = submission p form ~submitter in
  Submit { url; post; page = p }

let click (p : Browser_page.t) (e : Dom.element) : effect =
  match Forms.control e with
  | None -> Nothing
  | Some control -> (
      let v = value_of p e in
      let form = Forms.form_of p.forms e in
      match control.kind with
      | Text | Password | Textarea -> Focus e
      | Checkbox -> Changed (with_value p e { v with checked = not v.checked })
      | Radio ->
          (* the others of its name, in its form, unchecked *)
          let others =
            match form with
            | Some f -> List.filter (fun (c : Forms.control) -> c.kind = Radio && c.name = control.name && c.element != e) f.controls
            | None -> []
          in
          let p =
            List.fold_left (fun p (c : Forms.control) -> with_value p c.element { (value_of p c.element) with checked = false }) p others
          in
          Changed (with_value p e { v with checked = true })
      | Select opts -> Changed (with_value p e { v with selected = (v.selected + 1) mod max 1 (List.length opts) })
      | Submit -> ( match form with Some f -> submit p f ~submitter:(Some e) | None -> Nothing)
      | Reset -> (
          match form with
          | Some f ->
              Changed
                {
                  p with
                  values = List.filter (fun (e', _) -> not (List.exists (fun (c : Forms.control) -> c.element == e') f.controls)) p.values;
                }
          | None -> Nothing)
      (* a script's business: Browser_script.click *)
      | Button | Hidden -> Nothing)

let key (p : Browser_page.t) (e : Dom.element) (key : string) : effect =
  let v = value_of p e in
  let is_textarea = match Forms.control e with Some { kind = Textarea; _ } -> true | _ -> false in
  match key with
  | "backspace" ->
      let cs = Browser_text.characters v.text in
      Changed (with_value p e { v with text = String.concat "" (List.filteri (fun i _ -> i < List.length cs - 1) cs) })
  | "enter" | "return" when is_textarea -> Changed (with_value p e { v with text = v.text ^ "\n" })
  | "enter" | "return" -> ( match Forms.form_of p.forms e with Some f -> submit p f ~submitter:None | None -> Nothing)
  | "escape" -> Unfocus
  | _ -> Nothing

let typed (p : Browser_page.t) (e : Dom.element) (s : string) : Browser_page.t =
  let v = value_of p e in
  with_value p e { v with text = v.text ^ s }
