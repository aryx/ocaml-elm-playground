(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/document: what every application shares -- a document as a
 * value, the versions of it, and what was cut. *)

let t = Testo.create

(* --- Document -------------------------------------------------------- *)

let test_dirty_is_a_pointer_comparison () =
  let doc = Document.create "hello" in
  Alcotest.(check bool) "nothing to save yet" false (Document.dirty doc);
  let doc = Document.edit (fun s -> s ^ "!") doc in
  Alcotest.(check bool) "edited" true (Document.dirty doc);
  Alcotest.(check string) "and it says so" "untitled *" (Document.title doc);
  let doc = Document.mark_saved doc in
  Alcotest.(check bool) "saved" false (Document.dirty doc);
  Alcotest.(check string) "the star goes out" "untitled" (Document.title doc)

(* the property the pointer comparison buys, and the one most editors
   get wrong: go back to the version you saved and there is nothing to
   save again *)
let test_undoing_back_to_the_saved_version_is_clean () =
  let saved = "hello" in
  let doc = Document.create saved in
  let edited = Document.put (saved ^ "!") doc in
  Alcotest.(check bool) "changed" true (Document.dirty edited);
  (* an undo puts *the old value itself* back -- the same one, not a
     copy, which is what makes this work *)
  let back = Document.put saved edited in
  Alcotest.(check bool) "and back where we started" false (Document.dirty back)

(* and the catch: a structure that rebuilds a version rather than
   keeping it needs a real comparison, which is what ?equal is for *)
let test_equal_for_rebuilt_versions () =
  let rebuilt () = String.concat "" [ "hel"; "lo" ] in
  let doc = Document.create (rebuilt ()) in
  let back = Document.put (rebuilt ()) doc in
  Alcotest.(check bool) "equal, but not the same value" true (Document.dirty back);
  let doc = Document.create ~equal:String.equal (rebuilt ()) in
  let back = Document.put (rebuilt ()) doc in
  Alcotest.(check bool) "with an equal, it is clean" false (Document.dirty back)

let test_title_is_the_file_name () =
  let doc = Document.create ~path:"/home/pad/notes/todo.txt" "hi" in
  Alcotest.(check string) "the name, not the path" "todo.txt" (Document.title doc);
  Alcotest.(check string) "with a star once edited" "todo.txt *"
    (Document.title (Document.put "hi there" doc))

(* --- Undo ------------------------------------------------------------ *)

let test_the_worked_example () =
  let h = Undo.start "a" in
  let h = Undo.record "b" h in
  let h = Undo.record "c" h in
  Alcotest.(check string) "now" "c" (Undo.now h);
  let h = Undo.undo h in
  Alcotest.(check string) "one back" "b" (Undo.now h);
  let h = Undo.undo h in
  Alcotest.(check string) "and back to the start" "a" (Undo.now h);
  Alcotest.(check bool) "nothing behind it" false (Undo.can_undo h);
  Alcotest.(check int) "two ahead" 2 (Undo.redos h);
  (* a new edit makes the branch you did not take unreachable *)
  let h = Undo.record "d" h in
  Alcotest.(check int) "no future left" 0 (Undo.redos h);
  Alcotest.(check string) "now" "d" (Undo.now h);
  Alcotest.(check string) "and one version behind" "a" (Undo.now (Undo.undo h))

let test_names_are_for_the_menu () =
  let h = Undo.start [] in
  let h = Undo.record ~name:"Add Circle" [ 1 ] h in
  let h = Undo.record ~name:"Adjust Diameter" [ 2 ] h in
  Alcotest.(check (option string)) "what undo would take back" (Some "Adjust Diameter") (Undo.undo_name h);
  let h = Undo.undo h in
  Alcotest.(check (option string)) "and now the one before" (Some "Add Circle") (Undo.undo_name h);
  Alcotest.(check (option string)) "with the other to put back" (Some "Adjust Diameter") (Undo.redo_name h)

let test_the_oldest_versions_are_forgotten () =
  let h = ref (Undo.start ~limit:3 0) in
  for i = 1 to 10 do
    h := Undo.record i !h
  done;
  Alcotest.(check int) "only three versions kept" 3 (Undo.undos !h);
  let back = Undo.undo (Undo.undo (Undo.undo !h)) in
  Alcotest.(check int) "as far back as it goes" 7 (Undo.now back);
  Alcotest.(check bool) "and no further" false (Undo.can_undo back)

let test_undo_at_the_ends_does_nothing () =
  let h = Undo.start "only" in
  Alcotest.(check string) "nothing to undo" "only" (Undo.now (Undo.undo h));
  Alcotest.(check string) "nothing to redo" "only" (Undo.now (Undo.redo h))

(* --- Clipboard -------------------------------------------------------- *)

let test_clipboard () =
  let c = Clipboard.empty in
  Alcotest.(check bool) "nothing cut yet" false (Clipboard.has c);
  Alcotest.(check (option string)) "so paste does nothing" None (Clipboard.get c);
  let c = Clipboard.put "world" (Clipboard.put "hello" c) in
  Alcotest.(check (option string)) "the last thing copied" (Some "world") (Clipboard.get c)

let tests =
  [
    t "dirty is a comparison, not a flag" test_dirty_is_a_pointer_comparison;
    t "undoing back to the saved version is clean" test_undoing_back_to_the_saved_version_is_clean;
    t "a rebuilt version needs an equal" test_equal_for_rebuilt_versions;
    t "the title is the file's name" test_title_is_the_file_name;
    t "the worked example of a history" test_the_worked_example;
    t "an edit's name is for the menu" test_names_are_for_the_menu;
    t "the oldest versions are forgotten" test_the_oldest_versions_are_forgotten;
    t "undo and redo at the ends do nothing" test_undo_at_the_ends_does_nothing;
    t "the clipboard holds the last thing copied" test_clipboard;
  ]
