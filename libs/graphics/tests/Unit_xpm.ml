(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/images/xpm: Xpm.mli's worked example *)

let t = Testo.create

let hero =
  {|/* XPM */
static char *hero[] = {
"10 3 3 1",          /* width, height, colors, characters per pixel */
". c None",
"R c #dc281e",
"K c #643214",
"...RRRR...",
"..RRRRRRR.",
".KKK...KKK"
};
|}

let expected : Xpm.t =
  { name = "hero";
    colors = [ ('.', None); ('R', Some (220, 40, 30)); ('K', Some (100, 50, 20)) ];
    rows = [ "...RRRR..."; "..RRRRRRR."; ".KKK...KKK" ] }

let check_xpm msg (a : Xpm.t) (b : Xpm.t) =
  Alcotest.(check string) (msg ^ ": name") a.name b.name;
  Alcotest.(check (list (pair char (option (triple int int int))))) (msg ^ ": colors") a.colors b.colors;
  Alcotest.(check (list string)) (msg ^ ": rows") a.rows b.rows

let test_parse () =
  check_xpm "the hero" expected (Xpm.parse hero);
  check_xpm "printed and read back" expected (Xpm.parse (Xpm.print expected))

(* what other tools write: GIMP's 16 bits a channel and other keys,
 * names, a hotspot in the header *)
let test_others () =
  let x = Xpm.parse {|static char * a_xpm[] = {
"2 1 2 1 0 0",
"  c #FFFF00000000 m white",
"# s border c black",
" #"};|} in
  Alcotest.(check (list (pair char (option (triple int int int)))))
    "colors" [ (' ', Some (255, 0, 0)); ('#', Some (0, 0, 0)) ] x.colors;
  Alcotest.(check string) "name" "a_xpm" x.name

let test_errors () =
  let fails msg text =
    match Xpm.parse text with
    | exception Failure _ -> ()
    | _ -> Alcotest.failf "%s: parsed" msg
  in
  fails "2 characters per pixel" {|"1 1 1 2", "ab c None", "ab"|};
  fails "a row too short" {|"2 1 1 1", ". c None", "."|};
  fails "not in the palette" {|"1 1 1 1", ". c None", "x"|};
  fails "a named color" {|"1 1 1 1", ". c goldenrod", "."|}

let tests =
  Testo.categorize "Xpm" [ t "parse and print" test_parse; t "other tools' files" test_others; t "errors" test_errors ]
