(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/sheet: the formula language, and the graph underneath a
 * spreadsheet -- what gets recalculated when a cell changes, in what
 * order, and what happens when the graph has a loop in it. *)

let t = Testo.create

(* --- Formula ---------------------------------------------------------- *)

let test_cell_names () =
  let names = [ ("A1", (0, 0)); ("B1", (1, 0)); ("A2", (0, 1)); ("Z9", (25, 8)); ("AA1", (26, 0)) ] in
  names
  |> List.iter (fun (name, cell) ->
         Alcotest.(check (option (pair int int))) (name ^ " reads as") (Some cell) (Formula.cell_of_name name);
         Alcotest.(check string) (name ^ " writes as") name (Formula.name_of_cell cell));
  (* base 26 with no zero: the column after Z is AA, not BA *)
  Alcotest.(check (list string)) "the 26th column and the 27th" [ "Z1"; "AA1" ]
    [ Formula.name_of_cell (25, 0); Formula.name_of_cell (26, 0) ];
  Alcotest.(check (option (pair int int))) "not a cell" None (Formula.cell_of_name "hello")

(* the .mli's worked example: multiplication binds tighter, which is
   the shape of the grammar rather than a table of precedences *)
let test_precedence () =
  match Formula.parse "2+3*4" with
  | Error msg -> Alcotest.fail msg
  | Ok e ->
      let expected =
        Formula.Binop ('+', Formula.Number 2., Formula.Binop ('*', Formula.Number 3., Formula.Number 4.))
      in
      Alcotest.(check bool) "2+(3*4)" true (e = expected)

let test_parens_and_unary () =
  let ok s = match Formula.parse s with Ok e -> e | Error msg -> Alcotest.fail msg in
  Alcotest.(check bool) "(2+3)*4 is not 2+3*4" true (ok "(2+3)*4" <> ok "2+3*4");
  Alcotest.(check bool) "a leading minus" true
    (ok "-A1" = Formula.Unary ('-', Formula.Ref (0, 0)))

let test_what_a_cell_holds () =
  let content = Formula.content_of in
  Alcotest.(check bool) "a number" true (content "12" = Formula.Value 12.);
  Alcotest.(check bool) "some text" true (content "hello" = Formula.Text "hello");
  Alcotest.(check bool) "nothing" true (content "   " = Formula.Blank);
  Alcotest.(check bool) "a formula" true (content "=A1" = Formula.Formula (Formula.Ref (0, 0)));
  (* a formula that does not parse is something a cell can hold: the
     text stays, to be corrected *)
  Alcotest.(check bool) "a broken formula" true
    (match content "=A1+" with Formula.Invalid _ -> true | _ -> false)

let test_refs_of_a_range () =
  match Formula.parse "SUM(A1:B2)+C3" with
  | Error msg -> Alcotest.fail msg
  | Ok e ->
      Alcotest.(check (list (pair int int)))
        "every cell of the range, and the other one"
        [ (0, 0); (0, 1); (1, 0); (1, 1); (2, 2) ]
        (Formula.refs e)

(* --- Sheet ------------------------------------------------------------- *)

let sheet cells = List.fold_left (fun s (name, text) ->
    match Formula.cell_of_name name with
    | Some c -> Sheet.set c text s
    | None -> Alcotest.failf "%s is not a cell" name) Sheet.empty cells

let shown s name =
  match Formula.cell_of_name name with
  | Some c -> Sheet.show (Sheet.value s c)
  | None -> Alcotest.failf "%s is not a cell" name

let test_a_sheet_computes () =
  let s = sheet [ ("A1", "12"); ("B1", "=A1*2"); ("C1", "=B1+A2"); ("A2", "3"); ("D1", "hello") ] in
  Alcotest.(check string) "a number" "12" (shown s "A1");
  Alcotest.(check string) "a formula" "24" (shown s "B1");
  Alcotest.(check string) "a formula over two cells" "27" (shown s "C1");
  Alcotest.(check string) "text is text" "hello" (shown s "D1");
  Alcotest.(check string) "an empty cell" "" (shown s "Z9")

(* the .mli's diagram: a change walks forwards, and nothing else is
   touched. This is the number that makes it a spreadsheet *)
let test_only_what_changed_is_recalculated () =
  let s = sheet [ ("A1", "12"); ("A2", "3"); ("B1", "=A1*2"); ("C1", "=B1+A2"); ("E1", "=99") ] in
  let after = Sheet.set (0, 0) "20" s in
  Alcotest.(check string) "B1 followed A1" "40" (shown after "B1");
  Alcotest.(check string) "and C1 followed B1" "43" (shown after "C1");
  Alcotest.(check int) "three cells: A1, B1, C1 -- and not E1" 3 (Sheet.recalculated after);
  let after = Sheet.set (0, 1) "5" s in
  Alcotest.(check int) "changing A2 reaches only A2 and C1" 2 (Sheet.recalculated after)

(* what is computed before what: C1 reads B1, so B1 has to be done
   first, whatever order the cells were typed in *)
let test_order_respects_the_graph () =
  (* typed backwards on purpose: C1 before B1 before A1 *)
  let s = sheet [ ("C1", "=B1+1"); ("B1", "=A1+1"); ("A1", "1") ] in
  Alcotest.(check string) "B1" "2" (shown s "B1");
  Alcotest.(check string) "C1, which needed B1 first" "3" (shown s "C1")

let test_a_cycle_is_found_and_said () =
  let s = sheet [ ("A1", "=B1+1"); ("B1", "=A1+1") ] in
  Alcotest.(check string) "A1" "#cycle" (shown s "A1");
  Alcotest.(check string) "B1" "#cycle" (shown s "B1");
  (* and it recovers: breaking the loop recomputes both *)
  let s = Sheet.set (0, 0) "1" s in
  Alcotest.(check string) "A1 again" "1" (shown s "A1");
  Alcotest.(check string) "and B1 with it" "2" (shown s "B1")

let test_functions_over_a_range () =
  let s =
    sheet
      [ ("A1", "hours"); ("A2", "3"); ("A3", "4"); ("A4", "5");
        ("B1", "=SUM(A1:A4)"); ("B2", "=AVERAGE(A2:A4)"); ("B3", "=MAX(A2:A4)");
        ("B4", "=COUNT(A1:A4)") ]
  in
  (* the heading is skipped rather than being an error: that is what
     makes SUM over a column do what you meant *)
  Alcotest.(check string) "SUM skips the text" "12" (shown s "B1");
  Alcotest.(check string) "AVERAGE" "4" (shown s "B2");
  Alcotest.(check string) "MAX" "5" (shown s "B3");
  Alcotest.(check string) "COUNT counts the numbers" "3" (shown s "B4")

let test_errors_are_values_and_spread () =
  let s = sheet [ ("A1", "hello"); ("B1", "=A1*2"); ("C1", "=B1+1"); ("D1", "=1/0"); ("E1", "=A1+") ] in
  Alcotest.(check string) "arithmetic on text" "#text" (shown s "B1");
  Alcotest.(check string) "and it spreads" "#text" (shown s "C1");
  Alcotest.(check string) "dividing by zero" "#div0" (shown s "D1");
  Alcotest.(check bool) "a broken formula is an error, and its text is kept" true
    (String.length (shown s "E1") > 0 && Sheet.raw s (4, 0) = "=A1+")

(* 1979's answer, kept beside the real one. Typing into a cell stored
   it ([store]); the sheet was recalculated afterwards in one pass,
   row by row -- so a formula reading a cell *below or to the right*
   of it was a pass behind, and people pressed the key twice. *)
let test_the_way_it_was_done_in_1979 () =
  let s = sheet [ ("A1", "=B1+1"); ("B1", "1") ] in
  Alcotest.(check string) "A1, to start with" "2" (shown s "A1");
  (* B1 typed, and nothing recalculated yet *)
  let typed = Sheet.store (1, 0) "10" s in
  let once = Sheet.recalculate Sheet.Rows typed in
  Alcotest.(check string)
    "one pass in row order: A1 was computed before B1, so it is behind" "2" (shown once "A1");
  Alcotest.(check string) "B1 itself is right" "10" (shown once "B1");
  let twice = Sheet.recalculate Sheet.Rows once in
  Alcotest.(check string) "the second pass catches up -- the habit of 1979" "11" (shown twice "A1");
  (* and what the graph does with the same change *)
  let modern = Sheet.set (1, 0) "10" s in
  Alcotest.(check string) "the graph needs one" "11" (shown modern "A1");
  Alcotest.(check int) "and touched two cells, not the sheet" 2 (Sheet.recalculated modern)

let test_saving_and_loading () =
  let s = sheet [ ("A1", "12"); ("B1", "=A1*2"); ("C1", "hello") ] in
  let text = Sheet.to_string s in
  let back = Sheet.of_string text in
  Alcotest.(check string) "what was typed comes back" "=A1*2" (Sheet.raw back (1, 0));
  Alcotest.(check string) "and it computes again" "24" (shown back "B1");
  Alcotest.(check string) "text too" "hello" (shown back "C1")

let tests =
  [
    t "how a cell is named" test_cell_names;
    t "multiplication binds tighter" test_precedence;
    t "parentheses and a leading minus" test_parens_and_unary;
    t "what typing into a cell means" test_what_a_cell_holds;
    t "a range is every cell in it" test_refs_of_a_range;
    t "a sheet computes" test_a_sheet_computes;
    t "only what depends on a change is recalculated" test_only_what_changed_is_recalculated;
    t "what is computed before what" test_order_respects_the_graph;
    t "a cycle is found, said, and recovered from" test_a_cycle_is_found_and_said;
    t "functions over a range" test_functions_over_a_range;
    t "errors are values, and they spread" test_errors_are_values_and_spread;
    t "the way it was done in 1979, and why twice" test_the_way_it_was_done_in_1979;
    t "saving and loading" test_saving_and_loading;
  ]
