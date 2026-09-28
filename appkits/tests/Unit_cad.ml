(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/cad: AutoCAD's drawing -- the .mli's worked examples (a
 * segment crossing a circle, a fillet), TRIM, EXTEND and OFFSET, the
 * snaps, the command line's conversation (typed points, Close, Enter
 * repeating, U), the two windows, and a drawing through DXF and back. *)

module D = Cad_drawing
module G = Cad_geom
module S = Cad_session

let t = Testo.create
let pt = Alcotest.(pair (float 1e-6) (float 1e-6))
let pts = Alcotest.list pt

let test_crossings () =
  Alcotest.check pts "a segment through a circle" [ (2., 0.); (8., 0.) ]
    (G.intersections (G.Segment ((0., 0.), (10., 0.))) (G.Circle ((5., 0.), 3.)));
  Alcotest.check pts "two circles" [ (3., -4.); (3., 4.) ]
    (List.sort compare (G.intersections (G.Circle ((0., 0.), 5.)) (G.Circle ((6., 0.), 5.))));
  Alcotest.check pts "parallel lines: none" [] (G.carrier_intersections (G.Segment ((0., 0.), (1., 0.))) (G.Segment ((0., 1.), (1., 1.))));
  Alcotest.check pts "carriers cross, pieces do not" [] (G.intersections (G.Segment ((0., 0.), (1., 0.))) (G.Segment ((5., -1.), (5., 1.))));
  Alcotest.(check bool) "on the arc from 350 to 10" true (G.within 350. 10. 0.)

let test_fillet () =
  match Cad_edit.fillet 10. (D.Line ((0., 0.), (100., 0.)), (50., 0.)) (D.Line ((100., 0.), (100., 100.)), (100., 50.)) with
  | Ok (D.Line (a1, b1), D.Line (a2, b2), Some (D.Arc (c, r, s, e))) ->
      Alcotest.check pt "the first line kept from its far end" (0., 0.) a1;
      Alcotest.check pt "to the tangent point" (90., 0.) b1;
      Alcotest.check pt "the second, to its tangent point" (100., 10.) b2;
      Alcotest.check pt "the second's far end" (100., 100.) a2;
      Alcotest.check pt "the arc's center" (90., 10.) c;
      Alcotest.check (Alcotest.float 1e-6) "its radius" 10. r;
      Alcotest.check (Alcotest.list (Alcotest.float 1e-6)) "from 270 to 0" [ 270.; 0. ] [ s; e ]
  | _ -> Alcotest.fail "two lines and an arc"

let test_trim_extend_offset () =
  let edges = [ G.Segment ((30., -10.), (30., 10.)); G.Segment ((70., -10.), (70., 10.)) ] in
  (match Cad_edit.trim edges (D.Line ((0., 0.), (100., 0.))) ~at:(50., 0.) with
  | Ok [ D.Line (a, b); D.Line (c, d) ] ->
      Alcotest.check pts "the middle cut out" [ (0., 0.); (30., 0.); (70., 0.); (100., 0.) ] [ a; b; c; d ]
  | _ -> Alcotest.fail "two lines left");
  (match Cad_edit.trim edges (D.Line ((0., 0.), (100., 0.))) ~at:(10., 0.) with
  | Ok [ D.Line (a, b) ] -> Alcotest.check pts "an end cut off" [ (30., 0.); (100., 0.) ] [ a; b ]
  | _ -> Alcotest.fail "one line left");
  (match Cad_edit.trim [ G.Segment ((0., -20.), (0., 20.)) ] (D.Circle ((0., 0.), 10.)) ~at:(10., 0.) with
  | Ok [ D.Arc (_, _, s, e) ] -> Alcotest.check (Alcotest.list (Alcotest.float 1e-6)) "a circle cut: the left half stays" [ 90.; 270. ] [ s; e ]
  | _ -> Alcotest.fail "an arc");
  (match Cad_edit.extend [ G.Circle ((100., 0.), 20.) ] (D.Line ((0., 0.), (50., 0.))) ~at:(45., 0.) with
  | Ok (D.Line (a, b)) -> Alcotest.check pts "extended to the circle's near side" [ (0., 0.); (80., 0.) ] [ a; b ]
  | _ -> Alcotest.fail "a longer line");
  (match Cad_edit.offset 5. (D.Line ((0., 0.), (100., 0.))) ~side:(50., -30.) with
  | Ok (D.Line (a, b)) -> Alcotest.check pts "a line offset below" [ (0., -5.); (100., -5.) ] [ a; b ]
  | _ -> Alcotest.fail "a line");
  match Cad_edit.offset 5. (D.Circle ((0., 0.), 20.)) ~side:(1., 1.) with
  | Ok (D.Circle (_, r)) -> Alcotest.check (Alcotest.float 1e-9) "a circle offset inside" 15. r
  | _ -> Alcotest.fail "a circle"

let square () =
  let d = D.empty in
  let d, _ = D.add (D.Line ((0., 0.), (100., 0.))) d in
  let d, _ = D.add (D.Line ((100., 0.), (100., 100.))) d in
  let d, _ = D.add (D.Circle ((200., 0.), 20.)) d in
  d

let test_snaps () =
  let d = square () in
  let snap ?from p = Cad_snap.find d ~aperture:5. ?from p in
  Alcotest.(check bool) "the corner: an endpoint" true (snap (98., 2.) = Some (Cad_snap.Endpoint, (100., 0.)));
  Alcotest.(check bool) "the middle" true (snap (51., 1.) = Some (Cad_snap.Midpoint, (50., 0.)));
  Alcotest.(check bool) "a circle's rim: its center" true (snap (214.2, 14.2) = Some (Cad_snap.Center, (200., 0.)));
  Alcotest.(check bool) "perpendicular from a point" true (snap ~from:(30., 50.) (30., 1.) = Some (Cad_snap.Perpendicular, (30., 0.)))

let session () = S.start D.empty { S.center = (0., 0.); upp = 1.; w = 800.; h = 600. }
let feed s lines = List.fold_left (fun s l -> S.input s (S.Text l)) s lines

let lines s =
  List.filter_map (fun id -> match D.get (S.drawing s) id with Some { entity = D.Line (a, b); _ } -> Some (a, b) | _ -> None) (D.ids (S.drawing s))

let test_line_command () =
  let s = feed (session ()) [ "LINE"; "0,0"; "@100,0"; "@50<90" ] in
  Alcotest.(check string) "still asking" "To point:" (S.prompt s);
  let s = feed s [ "C" ] in
  Alcotest.(check (list (pair pt pt))) "three lines, closed" [ ((0., 0.), (100., 0.)); ((100., 0.), (100., 50.)); ((100., 50.), (0., 0.)) ] (lines s);
  Alcotest.(check string) "and done" "Command:" (S.prompt s);
  Alcotest.(check (list string)) "the conversation" [ "Command: LINE"; "From point: 0,0"; "To point: @100,0"; "To point: @50<90"; "To point: C" ] (S.log s);
  let s = feed s [ "" ] in
  Alcotest.(check string) "Enter: LINE again" "From point:" (S.prompt s);
  let s = feed s [ "10,10"; "20,10"; "U"; "" ] in
  Alcotest.(check int) "U inside LINE takes its segment back" 3 (List.length (lines s));
  let s = feed s [ "U" ] in
  Alcotest.(check int) "U at the prompt: the whole first LINE" 0 (List.length (lines s));
  let s = feed s [ "REDO" ] in
  Alcotest.(check int) "and REDO" 3 (List.length (lines s));
  let s = feed s [ "FROB" ] in
  Alcotest.(check bool) "an unknown command said so" true
    (List.exists (fun l -> l = "Unknown command \"FROB\".  Type ? for list of commands.") (S.log s))

let test_windows () =
  let s = S.start (square ()) { S.center = (0., 0.); upp = 1.; w = 800.; h = 600. } in
  let s = feed s [ "ERASE" ] in
  (* dragged to the right, from (-10,-10) to (110,50): nothing is wholly inside *)
  let right = S.input (S.input s (S.Pick (-10., -10.))) (S.Pick (110., 50.)) in
  Alcotest.(check int) "window: only what is wholly inside" 1 (List.length (S.selected right));
  (* the same box dragged to the left: what it crosses too *)
  let left = S.input (S.input s (S.Pick (110., 50.))) (S.Pick (-10., -10.)) in
  Alcotest.(check int) "crossing: what it touches" 2 (List.length (S.selected left));
  let s = feed left [ "" ] in
  Alcotest.(check int) "erased" 1 (List.length (D.ids (S.drawing s)))

let test_move_and_trim () =
  let s = S.start (square ()) { S.center = (0., 0.); upp = 1.; w = 800.; h = 600. } in
  let s = S.input (feed s [ "MOVE" ]) (S.Pick (50., 0.)) in
  let s = feed s [ ""; "0,0"; "@0,10" ] in
  Alcotest.(check bool) "moved up 10" true (List.mem ((0., 10.), (100., 10.)) (lines s));
  let s = S.input (feed s [ "TRIM"; "" ]) (S.Pick (100., 5.)) in
  let s = feed s [ "" ] in
  Alcotest.(check bool) "trimmed at the moved line, no edges chosen: all of them" true (List.mem ((100., 10.), (100., 100.)) (lines s))

let test_fillet_command () =
  let s = S.start (square ()) { S.center = (0., 0.); upp = 1.; w = 800.; h = 600. } in
  let s = feed s [ "F"; "R"; "10"; "" ] in
  Alcotest.(check string) "the radius set, FILLET again" "Radius/<Select first object>:" (S.prompt s);
  let s = S.input (S.input s (S.Pick (50., 0.))) (S.Pick (100., 50.)) in
  Alcotest.(check (list (pair pt pt))) "the lines cut back" [ ((0., 0.), (90., 0.)); ((100., 100.), (100., 10.)) ] (lines s);
  Alcotest.(check bool) "and the arc between" true
    (List.exists (fun id -> match D.get (S.drawing s) id with Some { entity = D.Arc _; _ } -> true | _ -> false) (D.ids (S.drawing s)))

let test_dxf () =
  let d = square () in
  let d = D.ensure_layer "CENTER" d in
  let d = D.set_layer { name = "CENTER"; color = 1; on = false } d in
  let d, _ = D.add (D.Arc ((5., 5.), 2.5, 0., 90.)) { d with current = "CENTER" } in
  let d = { d with blocks = [ ("BOLT", ((0., 0.), [ { D.entity = D.Circle ((0., 0.), 3.); layer = "0" } ])) ] } in
  let d, _ = D.add (D.Insert ("BOLT", (10., 20.), 2., 45.)) d in
  match Dxf.of_string (Dxf.to_string d) with
  | Error e -> Alcotest.fail e
  | Ok d' ->
      let ents (d : D.t) = List.map snd d.ents in
      Alcotest.(check bool) "the same entities, on the same layers" true (ents d = ents d');
      Alcotest.(check bool) "the same layers, one off" true (d.layers = d'.layers);
      Alcotest.(check bool) "the same blocks" true (d.blocks = d'.blocks);
      Alcotest.(check string) "the current layer" "CENTER" d'.current

let tests =
  [
    t "cad: crossings (Cad_geom.mli's worked example)" test_crossings;
    t "cad: a fillet (Cad_edit.mli's worked example)" test_fillet;
    t "cad: trim, extend, offset" test_trim_extend_offset;
    t "cad: object snaps" test_snaps;
    t "cad: LINE, Close, Enter, U, REDO" test_line_command;
    t "cad: window and crossing" test_windows;
    t "cad: MOVE, then TRIM" test_move_and_trim;
    t "cad: FILLET, its radius, then two lines" test_fillet_command;
    t "cad: a drawing through DXF and back" test_dxf;
  ]
