(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type slot = Kind | Out | Back | Book | Said

let kinds = [ "one-way flight"; "return flight" ]

let layout th =
  Layout.(
    center
      (column ~gap:10.
         [
           leaf Kind (Immediate.menu_size th kinds);
           leaf Out (Immediate.field_size th);
           leaf Back (Immediate.field_size th);
           leaf Book (Immediate.button_size th "Book");
           leaf Said (Gui4.label_size th "booked: return, 27.3.2014");
         ]))

(* the rules, the same in all four *)
type model = { kind : int; out : string; back : string; booked : string }

let initial = { kind = 0; out = "27.3.2014"; back = "27.3.2014"; booked = "" }

(* a date as (year, month, day), so that comparing them is comparing
   tuples; None if it is not one *)
let date s =
  match String.split_on_char '.' s with
  | [ d; m; y ] -> (
      match (int_of_string_opt d, int_of_string_opt m, int_of_string_opt y) with
      | Some d, Some m, Some y when d >= 1 && d <= 31 && m >= 1 && m <= 12 -> Some (y, m, d)
      | _ -> None)
  | _ -> None

let return m = m.kind = 1

let bookable m =
  match (date m.out, date m.back) with Some _, _ when not (return m) -> true | Some a, Some b -> b >= a | _ -> false

let book m = { m with booked = Printf.sprintf "booked: %s, %s" (if return m then "return" else "one-way") (if return m then m.back else m.out) }
let summary m = Printf.sprintf "%s %s %s [%s]" (List.nth kinds m.kind) m.out m.back m.booked

let make th panel (arch : Gui4.architecture) : Gui4.runner =
  let box = Gui4.places panel (layout th) in
  match arch with
  (* ---- immediate: the rules are three lines where the widgets are
     asked for ---- *)
  | Immediate ->
      let m = ref initial in
      let frame u =
        (* the menu first, since what it says decides whether the return
           date is on; its items go over everything anyway, on the
           toolkit's overlay *)
        let u, kind = Immediate.menu u (box Kind) kinds !m.kind in
        m := { !m with kind };
        let u, out = Immediate.field u (box Out) !m.out in
        let u, back = Immediate.field ~enabled:(return !m) u (box Back) !m.back in
        m := { !m with out; back };
        let u, booking = Immediate.button ~enabled:(bookable !m) u (box Book) "Book" in
        if booking then m := book !m;
        Immediate.label u (box Said) !m.booked
      in
      { step = Gui4.immediate th frame; summary = (fun () -> summary !m) }
  (* ---- callbacks: the state is in the widgets, and every handler
     must call [check] -- forget it in one, and Book is wrong ---- *)
  | Callbacks ->
      (* the handlers need [check], and [check] needs the widgets: a
         forward reference, the usual knot of callback code *)
      let check = ref (fun () -> ()) in
      let out = Retained.field (box Out) initial.out (fun _ -> !check ()) in
      let back = Retained.field (box Back) initial.back (fun _ -> !check ()) in
      let kind = Retained.menu (box Kind) kinds 0 (fun _ -> !check ()) in
      let said = Retained.label (box Said) "" in
      let now () = { kind = Retained.chosen kind; out = Retained.text out; back = Retained.text back; booked = Retained.text said } in
      let book_button = Retained.button (box Book) "Book" (fun () -> Retained.set_text said (book (now ())).booked) in
      (check :=
         fun () ->
           Retained.set_enabled back (return (now ()));
           Retained.set_enabled book_button (bookable (now ())));
      !check ();
      let ui = Retained.window (Retained.group [ out; back; book_button; kind; said ]) in
      { step = Gui4.retained th ui; summary = (fun () -> summary (now ())) }
  (* ---- MVC: one model; the controllers change it, one view re-reads
     all of it on every change ---- *)
  | Mvc ->
      let model = Mvc.create initial in
      let out = Retained.field (box Out) initial.out (fun s -> Mvc.change model (fun m -> { m with out = s })) in
      let back = Retained.field (box Back) initial.back (fun s -> Mvc.change model (fun m -> { m with back = s })) in
      let book_button = Retained.button (box Book) "Book" (fun () -> Mvc.change model book) in
      let kind = Retained.menu (box Kind) kinds 0 (fun k -> Mvc.change model (fun m -> { m with kind = k })) in
      let said = Retained.label (box Said) "" in
      let view () =
        let m = Mvc.get model in
        Retained.set_enabled back (return m);
        Retained.set_enabled book_button (bookable m);
        Retained.set_text said m.booked
      in
      Mvc.on_change model view;
      view ();
      let ui = Retained.window (Retained.group [ out; back; book_button; kind; said ]) in
      { step = Gui4.retained th ui; summary = (fun () -> summary (Mvc.get model)) }
  (* ---- MVU: the rules are where the view is made ---- *)
  | Mvu ->
      let update msg m =
        match msg with `Kind k -> { m with kind = k } | `Out s -> { m with out = s } | `Back s -> { m with back = s } | `Book -> book m
      in
      let view m =
        Mvu.group
          [
            Mvu.field (box Out) m.out (fun s -> `Out s);
            Mvu.field ~enabled:(return m) (box Back) m.back (fun s -> `Back s);
            Mvu.button ~enabled:(bookable m) (box Book) "Book" `Book;
            Mvu.menu (box Kind) kinds m.kind (fun k -> `Kind k);
            Mvu.label (box Said) m.booked;
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
