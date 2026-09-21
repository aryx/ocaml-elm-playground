(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/embed: a document of parts it does not know, tested with
 * two kinds of part made up here -- a note (some text) and a counter
 * (a number, as tall as a quarter of its width). The .mli's saved
 * example byte for byte, a layout computed by hand, the paths, and
 * the promise a compound document makes: a kind this program cannot
 * read passes through it unchanged. *)

let t = Testo.create

let rec note s : Component.part =
  {
    kind = "note";
    height = (fun _ -> 20.);
    draw = (fun _ ~active:_ -> []);
    input = (fun _ _ -> note s);
    menu = [];
    command = (fun _ -> note s);
    save = (fun () -> s);
  }

let rec counter n : Component.part =
  {
    kind = "counter";
    height = (fun w -> w /. 4.);
    draw = (fun _ ~active:_ -> []);
    input = (fun _ _ -> counter n);
    menu = [ "Counter"; "Add" ];
    command = (fun c -> if c = "Add" then counter (n + 1) else counter n);
    save = (fun () -> string_of_int n);
  }

let registry : Component.registry = [ ("note", note); ("counter", fun s -> counter (int_of_string s)) ]
let saved doc = Compound.save doc
let example = Compound.Column [ Part (note "Hello"); Row [ Part (counter 42) ] ]

let test_the_saved_example () =
  Alcotest.(check string) "as the .mli shows it" "column 2\npart note 5\nHello\nrow 1\npart counter 2\n42\n" (saved example)

let test_saved_and_read_back () =
  let doc =
    Compound.Column
      [ Part (note "two\nlines, and a part 3\n"); Row [ Part (counter 1); Column [ Part (counter 2); Part (note "") ] ] ]
  in
  let back = Compound.load registry (saved doc) in
  Alcotest.(check string) "the same document" (saved doc) (saved back);
  Alcotest.(check (option string)) "a part found by its path" (Some "2")
    (Option.map (fun (p : Component.part) -> p.save ()) (Compound.get back [ 1; 1; 0 ]))

(* the rule that makes compound documents work between programs *)
let test_an_unknown_kind_is_kept () =
  let text = saved (Compound.Column [ Part (counter 7); Part (note "a note\nwith part 99\nin it") ]) in
  let only_counters = [ ("counter", fun s -> counter (int_of_string s)) ] in
  let doc = Compound.load only_counters text in
  Alcotest.(check (option string)) "shown as a placeholder" (Some "note")
    (Option.map (fun (p : Component.part) -> p.kind) (Compound.get doc [ 1 ]));
  Alcotest.(check string) "and saved back byte for byte" text (saved doc)

let box = Alcotest.(list (float 1e-9))
let as_list (b : Widget.box) = [ b.x; b.y; b.w; b.h ]

(* by hand: a counter 200 wide is 50 tall; the row under it, 14 lower,
   gives each counter (200 - 14) / 2 = 93, so 23.25 tall *)
let test_layout_by_hand () =
  let doc = Compound.Column [ Part (counter 0); Row [ Part (counter 1); Part (counter 2) ] ] in
  let boxes, height = Compound.layout doc ~left:0. ~top:0. ~width:200. in
  Alcotest.(check (list (list int))) "the paths, in order" [ [ 0 ]; [ 1; 0 ]; [ 1; 1 ] ] (List.map fst boxes);
  Alcotest.(check (list box)) "the rectangles"
    [ [ 100.; -25.; 200.; 50. ]; [ 46.5; -75.625; 93.; 23.25 ]; [ 153.5; -75.625; 93.; 23.25 ] ]
    (List.map (fun (_, b) -> as_list b) boxes);
  Alcotest.(check (float 1e-9)) "and the whole" 87.25 height;
  Alcotest.(check (option (list int))) "a click in the second counter" (Some [ 1; 1 ]) (Compound.at_point boxes (150., -70.))

let test_paths () =
  let doc = Compound.Column [ Part (counter 0); Row [ Part (counter 1); Part (counter 2) ] ] in
  let doc = Compound.insert_after doc [ 1; 0 ] (Part (note "new")) in
  Alcotest.(check string) "inserted inside the row"
    "column 2\npart counter 1\n0\nrow 3\npart counter 1\n1\npart note 3\nnew\npart counter 1\n2\n" (saved doc);
  let doc = Compound.remove (Compound.remove doc [ 1; 0 ]) [ 1; 0 ] in
  Alcotest.(check string) "a row left with one part is that part" "column 2\npart counter 1\n0\npart counter 1\n2\n"
    (saved doc);
  let doc = Compound.remove (Compound.remove doc [ 0 ]) [ 0 ] in
  Alcotest.(check string) "and the root stays, empty" "column 0\n" (saved doc)

(* a part is a value: a command gives a new one, and the document's
   undo is keeping the old document *)
let test_parts_are_values () =
  let doc = Compound.Column [ Part (counter 1) ] in
  let h = Undo.start doc in
  let p = Option.get (Compound.get doc [ 0 ]) in
  let h = Undo.record ~name:"Add" (Compound.set doc [ 0 ] (p.command "Add")) h in
  Alcotest.(check string) "after" "column 1\npart counter 1\n2\n" (saved (Undo.now h));
  Alcotest.(check string) "and undone" "column 1\npart counter 1\n1\n" (saved (Undo.now (Undo.undo h)))

(* a row's width shared 3 to 1, by dragging the gap between its
   children: each counter as tall as a quarter of its width *)
let test_a_row_shared_out () =
  let doc = Compound.Column [ Row [ Part (counter 1); Part (counter 2) ] ] in
  let doc = Compound.resize_row doc [ 0 ] 0 0.75 in
  let boxes, height = Compound.layout doc ~left:0. ~top:0. ~width:200. in
  Alcotest.(check (list (float 1e-9))) "the room, 186, three to one" [ 139.5; 46.5 ] (List.map (fun (_, (b : Widget.box)) -> b.w) boxes);
  Alcotest.(check (float 1e-9)) "as tall as its tallest" 34.875 height;
  Alcotest.(check (list int)) "the paths go through the wrappers" [ 1; 2 ]
    (List.map (fun p -> int_of_string ((Option.get (Compound.get doc p)).save ())) [ [ 0; 0 ]; [ 0; 1 ] ])

(* frame negotiation: a height given is kept when it is more than the
   part asks for, and the part's own when it is less *)
let test_a_height_given () =
  let doc = Compound.Column [ Part (counter 1) ] in
  let h doc = (snd (List.hd (fst (Compound.layout doc ~left:0. ~top:0. ~width:200.)))).Widget.h in
  Alcotest.(check (float 1e-9)) "its own: a quarter of 200" 50. (h doc);
  Alcotest.(check (float 1e-9)) "given more" 120. (h (Compound.set_height doc [ 0 ] (Some 120.)));
  Alcotest.(check (float 1e-9)) "given less: it keeps what it needs" 50. (h (Compound.set_height doc [ 0 ] (Some 10.)));
  Alcotest.(check (float 1e-9)) "given back its own" 50. (h (Compound.set_height (Compound.set_height doc [ 0 ] (Some 120.)) [ 0 ] None))

let test_splitters () =
  let doc = Compound.Column [ Part (counter 0); Row [ Part (counter 1); Part (counter 2) ] ] in
  match Compound.splitters doc ~left:0. ~top:0. ~width:200. with
  | [ s ] ->
      Alcotest.(check (list int)) "in the row" [ 1 ] s.row;
      Alcotest.(check (float 1e-9)) "in the gap between the two" 100. s.grip.x;
      Alcotest.(check (pair (float 1e-9) (float 1e-9))) "the two together" (0., 200.) s.span
  | l -> Alcotest.failf "%d splitters, not one" (List.length l)

let test_sizes_are_saved () =
  let doc = Compound.Column [ Part (counter 0); Row [ Part (counter 1); Part (counter 2) ] ] in
  let doc = Compound.set_height (Compound.resize_row doc [ 1 ] 0 0.25) [ 0 ] (Some 80.) in
  let text = saved doc in
  Alcotest.(check string) "read back, the same" text (saved (Compound.load registry text));
  Alcotest.(check bool) "and it says so" true (String.length text > 0 && String.sub text 0 21 = "column 2\nsized 80 1\np")

let tests =
  [
    t "a row shared out by its children's shares" test_a_row_shared_out;
    t "a height given, and frame negotiation" test_a_height_given;
    t "where a row's children meet" test_splitters;
    t "the sizes are saved with the document" test_sizes_are_saved;
    t "the saved example" test_the_saved_example;
    t "a document saved and read back" test_saved_and_read_back;
    t "a kind nobody knows is kept whole" test_an_unknown_kind_is_kept;
    t "layout by hand" test_layout_by_hand;
    t "inserting and removing by path" test_paths;
    t "parts are values, and undo is free" test_parts_are_values;
  ]
