(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gui/Text_edit: the piece table's worked examples, its word wrap,
 * its undo -- and the test that matters most, which is not a worked
 * example at all: a few thousand random edits against the obvious
 * implementation (a string, copied every time), checking they say the
 * same thing at every step.
 *
 * A clever structure is only worth having if it is indistinguishable
 * from the slow one, and "indistinguishable" is a claim about inputs
 * nobody thought of. *)

let t = Testo.create

(* --- the .mli's worked example, piece by piece --------------------- *)

let test_pieces_of_the_worked_example () =
  let e = Text_edit.of_string "Hello world" in
  Alcotest.(check int) "one piece to start with" 1 (Text_edit.pieces e);
  let e = Text_edit.insert "there " (Text_edit.at 6 e) in
  Alcotest.(check string) "the text reads as it should" "Hello there world" (Text_edit.to_string e);
  Alcotest.(check int) "split, then the new one: three" 3 (Text_edit.pieces e);
  (* five keystrokes at the end, one piece: the append buffer grows
     and the last piece grows with it *)
  let e = Text_edit.at (String.length (Text_edit.to_string e)) e in
  let e = List.fold_left (fun e _ -> Text_edit.insert "x" e) e [ 1; 2; 3; 4; 5 ] in
  Alcotest.(check string) "typed at the end" "Hello there worldxxxxx" (Text_edit.to_string e);
  (* one new piece for the five keystrokes, not five: the second x
     extended the piece the first one made *)
  Alcotest.(check int) "four pieces, not eight" 4 (Text_edit.pieces e);
  let e = Text_edit.delete ~from:6 ~len:5 e in
  Alcotest.(check string) "a hole in the middle" "Hello  worldxxxxx" (Text_edit.to_string e);
  Alcotest.(check int) "still four: a delete shortened one" 4 (Text_edit.pieces e)

(* the original is never touched, which is the whole structure in one
   assertion: an edited text still contains its first version *)
let test_the_original_is_never_touched () =
  let e = Text_edit.of_string "Hello world" in
  let edited = Text_edit.insert "there " (Text_edit.at 6 e) in
  Alcotest.(check string) "the first version is still there" "Hello world" (Text_edit.to_string e);
  Alcotest.(check string) "and the new one is new" "Hello there world" (Text_edit.to_string edited)

(* --- the caret and the selection ------------------------------------ *)

let test_selection_is_a_caret_with_two_ends () =
  let e = Text_edit.of_string "Hello world" in
  let e = Text_edit.select ~anchor:6 ~caret:11 e in
  Alcotest.(check string) "what is selected" "world" (Text_edit.selected e);
  (* dragged backwards: the caret is before the anchor, and the range
     is still the same *)
  let back = Text_edit.select ~anchor:11 ~caret:6 e in
  Alcotest.(check string) "whichever way round" "world" (Text_edit.selected back);
  (* typing over a selection replaces it *)
  let typed = Text_edit.insert "there" e in
  Alcotest.(check string) "replaced" "Hello there" (Text_edit.to_string typed);
  Alcotest.(check int) "and the caret is after what was typed" 11 (Text_edit.caret typed);
  (* backspace with a selection deletes the selection, not a letter *)
  let deleted = Text_edit.delete_backward e in
  Alcotest.(check string) "the selection went" "Hello " (Text_edit.to_string deleted)

(* a character is not a byte, here as everywhere else *)
let test_backspace_over_a_two_byte_character () =
  let e = Text_edit.of_string "caf\xc3\xa9" in
  let e = Text_edit.at (String.length "caf\xc3\xa9") e in
  Alcotest.(check string) "both bytes of the e acute" "caf" (Text_edit.to_string (Text_edit.delete_backward e))

(* --- undo, which is the reason for the structure -------------------- *)

let test_undo_is_the_old_list_of_pieces () =
  let e = Text_edit.of_string "Hello" in
  let e = Text_edit.insert " world" (Text_edit.at 5 e) in
  let e = Text_edit.insert "!" (Text_edit.at 11 e) in
  Alcotest.(check string) "two edits" "Hello world!" (Text_edit.to_string e);
  Alcotest.(check int) "two versions to go back to" 2 (Text_edit.undos e);
  let e = Text_edit.undo e in
  Alcotest.(check string) "one back" "Hello world" (Text_edit.to_string e);
  let e = Text_edit.undo e in
  Alcotest.(check string) "and back to the start" "Hello" (Text_edit.to_string e);
  Alcotest.(check int) "with two to redo" 2 (Text_edit.redos e);
  let e = Text_edit.redo (Text_edit.redo e) in
  Alcotest.(check string) "forward again" "Hello world!" (Text_edit.to_string e);
  (* an edit after an undo makes the future unreachable, as it does in
     every editor *)
  let e = Text_edit.insert "?" (Text_edit.undo e) in
  Alcotest.(check int) "no future left" 0 (Text_edit.redos e);
  Alcotest.(check string) "the new branch" "Hello world?" (Text_edit.to_string e)

let test_undo_at_the_start_does_nothing () =
  let e = Text_edit.of_string "Hello" in
  Alcotest.(check string) "nothing to undo" "Hello" (Text_edit.to_string (Text_edit.undo e));
  Alcotest.(check string) "nothing to redo" "Hello" (Text_edit.to_string (Text_edit.redo e))

(* --- word wrap ------------------------------------------------------ *)

let texts_of = List.map snd

let test_wrap_breaks_at_spaces () =
  let e = Text_edit.of_string "the quick brown fox jumps over the lazy dog" in
  Alcotest.(check (list string))
    "greedy: as many words as fit"
    [ "the quick"; "brown fox"; "jumps over"; "the lazy"; "dog" ]
    (texts_of (Text_edit.lines ~width:10 e))

let test_wrap_breaks_inside_a_long_word () =
  let e = Text_edit.of_string "a nonrepresentational word" in
  Alcotest.(check (list string))
    "a word longer than the line has to be cut"
    [ "a"; "nonrepr"; "esentat"; "ional"; "word" ]
    (texts_of (Text_edit.lines ~width:7 e))

let test_wrap_keeps_the_texts_own_breaks () =
  let e = Text_edit.of_string "one\ntwo\n\nfour" in
  Alcotest.(check (list string))
    "newlines are breaks the text asked for"
    [ "one"; "two"; ""; "four" ]
    (texts_of (Text_edit.lines ~width:80 e))

let test_place_and_offset_are_inverses () =
  let e = Text_edit.of_string "the quick brown fox jumps over the lazy dog" in
  let width = 10 in
  (* the f of fox: line 1 ("brown fox"), column 6 *)
  let pos = 16 in
  let line, column = Text_edit.place ~width e pos in
  Alcotest.(check (pair int int)) "which line, how far along" (1, 6) (line, column);
  Alcotest.(check int) "and back again" pos (Text_edit.offset ~width e ~line ~column)

(* --- and the one that checks the inputs nobody thought of ----------- *)

(* the obvious implementation: a string, copied at every edit *)
module Naive = struct
  let insert s pos text = String.sub text 0 pos ^ s ^ String.sub text pos (String.length text - pos)

  let delete ~from ~len text =
    String.sub text 0 from ^ String.sub text (from + len) (String.length text - from - len)
end

let test_against_a_string () =
  (* deterministic, like everything else here: the same seed, the same
     five thousand edits, every run *)
  let rng = Random.State.make [| 5 |] in
  let word () = String.make (1 + Random.State.int rng 4) "abcdefg".[Random.State.int rng 7] in
  let table = ref (Text_edit.of_string "Hello world") in
  (* the naive side keeps its versions the way the piece table does:
     the past, the present, and the future an undo made reachable *)
  let now = ref "Hello world" and past = ref [] and future = ref [] in
  let edited text =
    past := !now :: !past;
    future := [];
    now := text
  in
  for step = 1 to 5000 do
    let n = String.length !now in
    (match Random.State.int rng 10 with
    | 0 | 1 | 2 | 3 ->
        (* type something somewhere *)
        let pos = Random.State.int rng (n + 1) in
        let s = word () in
        table := Text_edit.insert s (Text_edit.at pos !table);
        edited (Naive.insert s pos !now)
    | 4 | 5 | 6 ->
        (* delete a stretch *)
        if n > 0 then (
          let from = Random.State.int rng n in
          let len = 1 + Random.State.int rng (min 5 (n - from)) in
          table := Text_edit.delete ~from ~len !table;
          edited (Naive.delete ~from ~len !now))
    | 7 ->
        (* type over a selection *)
        if n > 1 then (
          let a = Random.State.int rng n in
          let b = a + 1 + Random.State.int rng (n - a) in
          let s = word () in
          table := Text_edit.insert s (Text_edit.select ~anchor:a ~caret:b !table);
          edited (Naive.insert s a (Naive.delete ~from:a ~len:(b - a) !now)))
    | 8 ->
        (* go back a version *)
        table := Text_edit.undo !table;
        (match !past with
        | [] -> ()
        | text :: rest ->
            future := !now :: !future;
            now := text;
            past := rest)
    | _ ->
        (* and forward again *)
        table := Text_edit.redo !table;
        (match !future with
        | [] -> ()
        | text :: rest ->
            past := !now :: !past;
            now := text;
            future := rest));
    if Text_edit.to_string !table <> !now then
      Alcotest.failf "step %d: the table says %S and a string says %S" step
        (Text_edit.to_string !table) !now;
    Alcotest.(check int)
      (Printf.sprintf "step %d: as many versions behind" step)
      (List.length !past) (Text_edit.undos !table)
  done;
  (* and the structure stayed a structure: far fewer pieces than edits *)
  Alcotest.(check bool)
    (Printf.sprintf "%d pieces after 5000 edits" (Text_edit.pieces !table))
    true
    (Text_edit.pieces !table < 5000)

let tests =
  [
    t "the piece counts of the worked example" test_pieces_of_the_worked_example;
    t "the original is never touched" test_the_original_is_never_touched;
    t "a selection is a caret with two ends" test_selection_is_a_caret_with_two_ends;
    t "backspace takes a whole character" test_backspace_over_a_two_byte_character;
    t "undo is the old list of pieces" test_undo_is_the_old_list_of_pieces;
    t "undo and redo at the ends do nothing" test_undo_at_the_start_does_nothing;
    t "word wrap breaks at spaces" test_wrap_breaks_at_spaces;
    t "and inside a word that is too long" test_wrap_breaks_inside_a_long_word;
    t "and keeps the breaks the text asked for" test_wrap_keeps_the_texts_own_breaks;
    t "place and offset are inverses" test_place_and_offset_are_inverses;
    t "5000 random edits agree with a string" test_against_a_string;
  ]
