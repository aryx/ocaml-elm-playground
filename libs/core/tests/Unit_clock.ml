(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* core: Clock *)

let t = Testo.create

let show ~offset (t : float) : string =
  let d, tod = Clock.local ~offset t in
  Civil.to_string d ^ " " ^ Clock.to_string tod

(* the worked example of Clock.mli, and the same instant elsewhere *)
let test_known () =
  List.iter
    (fun (offset, t, expected) ->
      Alcotest.(check string) (Printf.sprintf "%.0f at %s" t (Clock.offset_to_string offset)) expected (show ~offset t))
    [ (120, 1790253735., "2026-09-24 14:42:15");
      (0, 1790253735., "2026-09-24 12:42:15");
      (-240, 1790253735., "2026-09-24 08:42:15");
      (* India, and a day later than New York on the same instant *)
      (330, 1790253735., "2026-09-24 18:12:15");
      (-240, 1790208000., "2026-09-23 20:00:00");
      (0, 0., "1970-01-01 00:00:00");
      (* before the epoch: floor, not truncation *)
      (0, -1., "1969-12-31 23:59:59");
      (0, -86400.5, "1969-12-30 23:59:59") ]

(* the fraction of a second, for a sweeping second hand *)
let test_fraction () =
  let _, tod = Clock.split ~offset:0 1790253735.25 in
  Alcotest.(check (float 1e-6)) "a quarter second" 15.25 tod.second;
  let _, tod = Clock.split ~offset:0 (-0.75) in
  Alcotest.(check (float 1e-6)) "before the epoch" 59.25 tod.second

(* back from a date and a time, whatever the offset *)
let test_round_trip () =
  List.iter
    (fun offset ->
      List.iter
        (fun t ->
          let d, tod = Clock.local ~offset t in
          Alcotest.(check (float 1e-6)) (Printf.sprintf "%.1f at %d" t offset) t (Clock.of_local ~offset d tod))
        [ 0.; -1.; 1790253735.; 1790253735.5; 951782400.; -2208988800. ])
    [ 0; 60; 120; -240; 330; 345; -720; 840 ]

let test_strings () =
  let tod : Clock.time_of_day = { hour = 9; minute = 5; second = 7.9 } in
  Alcotest.(check string) "seconds truncated" "09:05:07" (Clock.to_string tod);
  Alcotest.(check string) "without" "09:05" (Clock.to_string ~seconds:false tod);
  Alcotest.(check string) "Paris" "+02:00" (Clock.offset_to_string 120);
  Alcotest.(check string) "New York" "-04:00" (Clock.offset_to_string (-240));
  Alcotest.(check string) "India" "+05:30" (Clock.offset_to_string 330);
  Alcotest.(check string) "UTC" "+00:00" (Clock.offset_to_string 0)

let tests =
  Testo.categorize "Clock"
    [ t "the known instants" test_known;
      t "fractions of a second" test_fraction;
      t "there and back" test_round_trip;
      t "as strings" test_strings ]
