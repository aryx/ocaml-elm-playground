(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/nls: Nls_doc *)

let t = Testo.create

(* Nls_doc.mli's list *)
let doc () =
  Nls_doc.of_outline
    [ (0, "(shop) Shopping list"); (1, "produce"); (2, "apples"); (2, "bananas"); (1, "dairy"); (0, "Things to do") ]

let sid_of d text = (List.find (fun ((s : Nls_doc.statement), _) -> s.text = text) (Nls_doc.visible d ~levels:None) |> fst).sid
let numbered d = List.map (fun ((s : Nls_doc.statement), _) -> (Option.get (Nls_doc.number d s.sid), s.text)) (Nls_doc.visible d ~levels:None)

let test_numbers () =
  let d = doc () in
  Alcotest.(check (list (pair string string))) "levels alternate digits and letters"
    [ ("1", "(shop) Shopping list"); ("1a", "produce"); ("1a1", "apples"); ("1a2", "bananas"); ("1b", "dairy"); ("2", "Things to do") ]
    (numbered d);
  Alcotest.(check (option int)) "by name" (Some (sid_of d "(shop) Shopping list")) (Nls_doc.find d "SHOP");
  Alcotest.(check (option int)) "by number" (Some (sid_of d "bananas")) (Nls_doc.find d "1a2");
  Alcotest.(check (option int)) "neither" None (Nls_doc.find d "3");
  let long = Nls_doc.of_outline ((0, "list") :: List.init 27 (fun i -> (1, string_of_int i))) in
  Alcotest.(check (option string)) "letters past z, as columns" (Some "1aa") (Nls_doc.number long (sid_of long "26"))

(* the worked example: produce moved after dairy, its SIDs kept *)
let test_move () =
  let d = doc () in
  let produce = sid_of d "produce" and apples = sid_of d "apples" in
  match Nls_doc.move d produce ~target:(sid_of d "dairy") Nls_doc.After with
  | None -> Alcotest.fail "moved"
  | Some d ->
      Alcotest.(check (list (pair string string))) "renumbered"
        [ ("1", "(shop) Shopping list"); ("1a", "dairy"); ("1b", "produce"); ("1b1", "apples"); ("1b2", "bananas"); ("2", "Things to do") ]
        (numbered d);
      Alcotest.(check (option string)) "the same SID, a new number" (Some "1b1") (Nls_doc.number d apples);
      Alcotest.(check bool) "not into itself" true (Nls_doc.move d produce ~target:apples Nls_doc.After = None)

let test_insert_copy () =
  let d = doc () in
  let d, _ = Nls_doc.insert d ~target:(sid_of d "bananas") Nls_doc.After "cherries" in
  let d, _ = Nls_doc.insert d ~target:(sid_of d "dairy") Nls_doc.Down "milk" in
  let d, _ = Nls_doc.insert d ~target:(sid_of d "milk") Nls_doc.Up "bakery" in
  Alcotest.(check (list string)) "after, down, up"
    [ "1"; "1a"; "1a1"; "1a2"; "1a3"; "1b"; "1b1"; "1c"; "2" ]
    (List.map fst (numbered d));
  Alcotest.(check (option string)) "cherries after bananas" (Some "1a3") (Nls_doc.number d (sid_of d "cherries"));
  Alcotest.(check (option string)) "bakery after dairy" (Some "1c") (Nls_doc.number d (sid_of d "bakery"));
  (match Nls_doc.copy d (sid_of d "produce") ~target:(sid_of d "Things to do") Nls_doc.Down with
  | Some c -> Alcotest.(check int) "a branch copied: four more statements" (List.length (numbered d) + 4) (List.length (numbered c))
  | None -> Alcotest.fail "copied");
  Alcotest.(check (list string)) "deleted with its branch" [ "1"; "1a"; "1a1"; "1b"; "2" ]
    (List.map fst (numbered (Nls_doc.delete d (sid_of d "produce"))))

let test_view_text () =
  let d = doc () in
  Alcotest.(check int) "the first level" 2 (List.length (Nls_doc.visible d ~levels:(Some 1)));
  Alcotest.(check int) "two levels" 4 (List.length (Nls_doc.visible d ~levels:(Some 2)));
  Alcotest.(check (list (triple int int string))) "links" [ (4, 10, "shop"); (14, 18, "1a") ] (Nls_doc.links "see <shop> or <1a>");
  Alcotest.(check (option (pair int int))) "the word under the bug" (Some (4, 8)) (Nls_doc.word_at "big blue sea" 6);
  Alcotest.(check (option (pair int int))) "on a space, the next word" (Some (10, 13)) (Nls_doc.word_at "big blue  sea" 8);
  Alcotest.(check (option string)) "a name" (Some "shop") (Nls_doc.name_of "(shop) Shopping list")

let tests = Testo.categorize "nls" [ t "numbers" test_numbers; t "move" test_move; t "insert, copy" test_insert_copy; t "views and text" test_view_text ]
