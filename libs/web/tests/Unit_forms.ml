(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_forms.mli *)

let fields = Alcotest.(list (pair string string))

(* the page's first form, and a value for each control: its initial
 * one, unless [changed] says otherwise (by the control's name) *)
let form_of (html : string) : Forms.form = List.hd (Forms.forms (Html_tree.of_string html))

let initial (f : Forms.form) (e : Dom.element) : Forms.value =
  (List.find (fun (c : Forms.control) -> c.element == e) f.controls).initial

let by_name (f : Forms.form) (name : string) : Dom.element =
  (List.find (fun (c : Forms.control) -> c.name = Some name) f.controls).element

let tests =
  Testo.categorize "Forms"
    [
      Testo.create "the worked example" (fun () ->
          let f =
            form_of
              "<form action=/search><input name=q value=\"caf&eacute; au lait\"><select name=lang><option>en<option selected>fr</select><input type=submit name=go value=Search></form>"
          in
          Alcotest.(check (pair string bool)) "GET /search" ("/search", false) (f.action, f.post);
          Alcotest.check fields "submitted by the button"
            [ ("q", "caf\xC3\xA9 au lait"); ("lang", "fr"); ("go", "Search") ]
            (Forms.submission f ~value:(initial f) ~submitter:(Some (by_name f "go")));
          Alcotest.check fields "by Return in the field: no button"
            [ ("q", "caf\xC3\xA9 au lait"); ("lang", "fr") ]
            (Forms.submission f ~value:(initial f) ~submitter:None));
      Testo.create "checkboxes and radios: only the checked" (fun () ->
          let f =
            form_of
              "<form method=post><input type=checkbox name=a checked><input type=checkbox name=b value=yes><input type=radio name=r value=1><input type=radio name=r value=2 checked></form>"
          in
          Alcotest.(check bool) "POST" true f.post;
          Alcotest.check fields "a on, r 2" [ ("a", "on"); ("r", "2") ] (Forms.submission f ~value:(initial f) ~submitter:None);
          let value e = if e == by_name f "b" then { (initial f e) with checked = true } else initial f e in
          Alcotest.check fields "b checked" [ ("a", "on"); ("b", "yes"); ("r", "2") ] (Forms.submission f ~value ~submitter:None));
      Testo.create "what is never sent" (fun () ->
          let f = form_of "<form><input value=x><input type=reset name=r><input type=submit name=s value=S><input type=hidden name=h value=v></form>" in
          Alcotest.check fields "no name, reset, a button not clicked: only the hidden" [ ("h", "v") ]
            (Forms.submission f ~value:(initial f) ~submitter:None));
      Testo.create "textarea and select's values" (fun () ->
          let f = form_of "<form><textarea name=t>line 1\nline 2</textarea><select name=s><option value=a>A<option>B</select></form>" in
          Alcotest.check fields "the text, the first option's value" [ ("t", "line 1\nline 2"); ("s", "a") ]
            (Forms.submission f ~value:(initial f) ~submitter:None));
      Testo.create "which form a control is in" (fun () ->
          let root = Html_tree.of_string "<form><input name=a></form><input name=b><form><input name=c></form>" in
          let forms = Forms.forms root in
          let input name = List.find (fun e -> Dom.attribute "name" e = Some name) (Dom.find_all "input" root) in
          Alcotest.(check (list bool)) "a in the first, b in none, c in the second"
            [ true; false; true ]
            (List.map (fun n -> Forms.form_of forms (input n) <> None) [ "a"; "b"; "c" ]);
          Alcotest.(check int) "two forms" 2 (List.length forms));
    ]
