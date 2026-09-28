(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/diagram: Diagram, Ortho_route *)

let t = Testo.create
let master name = List.find (fun (m : Diagram.master) -> m.name = name) Diagram.masters
let close name expected actual = if Float.abs (expected -. actual) > 1e-6 then Alcotest.failf "%s: %g, not %g" name actual expected
let get d id name = Option.value (Diagram.value d id name) ~default:nan

(* Diagram.mli's arrow: the head keeps its length *)
let test_shapesheet () =
  let d, arrow = Diagram.drop Diagram.empty (master "Block arrow") (3., 3.) in
  let d = Result.get_ok (Diagram.set_formula d arrow "Width" "2") in
  close "the head's base, 2 wide" 1.5 (get d arrow "Geometry1.X2");
  let d = Result.get_ok (Diagram.set_formula d arrow "Width" "4") in
  close "stretched to 4" 3.5 (get d arrow "Geometry1.X2");
  close "the head still 0.5" 0.5 (get d arrow "User.Head");
  let d = Result.get_ok (Diagram.set_formula d arrow "User.Head" "Width*0.5") in
  close "a formula of one's own: half of it head" 2. (get d arrow "Geometry1.X2");
  Alcotest.(check bool) "a name it does not have" true (Result.is_error (Diagram.set_formula d arrow "Width" "Depth*2"));
  Alcotest.(check bool) "a cycle is an error in the cell" true
    (let d = Result.get_ok (Diagram.set_formula d arrow "Width" "Height+1") in
     let d = Result.get_ok (Diagram.set_formula d arrow "Height" "Width+1") in
     Diagram.value d arrow "Width" = None)

(* glue is a formula: the connector follows *)
let test_glue () =
  let d, box = Diagram.drop Diagram.empty (master "Process") (2., 4.) in
  let d, diamond = Diagram.drop d (master "Decision") (6., 4.) in
  let d, c = Diagram.drop d (master "Dynamic connector") (4., 4.) in
  (* the box's right side (point 1), the diamond's left (point 3) *)
  let d = Diagram.glue d c Diagram.Begin ~target:box ~point:1 in
  let d = Diagram.glue d c Diagram.End ~target:diamond ~point:3 in
  close "begins at the box's right" 2.75 (get d c "BeginX");
  close "ends at the diamond's left" 5.25 (get d c "EndX");
  let d = Diagram.move d diamond (6., 6.) in
  close "the diamond moved up 2, the end follows" 6. (get d c "EndY");
  close "and the other end stays" 4. (get d c "BeginY");
  let d = Diagram.delete d diamond in
  close "deleted: the end left where it was" 6. (get d c "EndY");
  Alcotest.(check int) "and unglued" 1 (List.length (Option.get (Diagram.shape d c)).glue);
  let d = Diagram.move d box (2., 1.) in
  close "the other end still glued" 1. (get d c "BeginY")

(* Ortho_route.mli's two routes *)
let test_route () =
  let a = (0., 0., 1., 1.) and b = (4., 0., 5., 1.) in
  let straight = Ortho_route.route ~boxes:[ a; b ] ~margin:0.1 ((1., 0.5), Some Ortho_route.Right) ((4., 0.5), Some Ortho_route.Left) in
  Alcotest.(check int) "across: no bend" 0 (Ortho_route.bends straight);
  let wall = (2., -1., 3., 2.) in
  let around = Ortho_route.route ~boxes:[ a; b; wall ] ~margin:0.1 ((1., 0.5), Some Ortho_route.Right) ((4., 0.5), Some Ortho_route.Left) in
  Alcotest.(check int) "around a box in the way: four bends" 4 (Ortho_route.bends around);
  let rec pieces = function p :: (q :: _ as rest) -> (p, q) :: pieces rest | _ -> [] in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
      if x1 <> x2 && y1 <> y2 then Alcotest.fail "a piece not at right angles";
      let mx = (x1 +. x2) /. 2. and my = (y1 +. y2) /. 2. in
      if mx > 2. && mx < 3. && my > -1. && my < 2. then Alcotest.fail "through the box")
    (pieces around)

let tests = Testo.categorize "diagram" [ t "ShapeSheet" test_shapesheet; t "glue" test_glue; t "routes" test_route ]
