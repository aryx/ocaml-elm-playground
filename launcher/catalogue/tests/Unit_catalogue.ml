(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* launcher/catalogue: Catalogue *)

let t = Testo.create

let test_plain () =
  Alcotest.(check string) "the .mli's example" "games/fps/: TinyDoom's BSP" (Catalogue.plain "`games/fps/`: [TinyDoom](x.ml)'s **BSP**");
  Alcotest.(check string) "a bracket that is no link" "[1] a" (Catalogue.plain "[1] a")

let sample =
  {|# Catalogue

Introduction, not a section.

# Games

## Platform

`games/platform/`: run and jump,
over **platforms**.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMario](games/platform/TinyMario.ml) | 2D | Super Mario Bros. (Nintendo, 1985) | Run, jump. | The `side-scroller`. |

## Empty

No table: left out.

# Apps

## Graphics

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMacPaint](apps/graphics/TinyMacPaint.ml) | app | MacPaint (Bill Atkinson, 1984) | Paint. | QuickDraw. |
|}

let test_sample () =
  match Catalogue.parse sample with
  | [ platform; graphics ] ->
      Alcotest.(check string) "a section's title" "Platform" platform.title;
      Alcotest.(check bool) "under # Games" true platform.games;
      Alcotest.(check bool) "under # Apps" false graphics.games;
      Alcotest.(check string) "its intro, its lines joined, plain" "games/platform/: run and jump, over platforms." platform.intro;
      let mario = List.hd platform.programs in
      Alcotest.(check (list string)) "a row"
        [ "TinyMario"; "games/platform/TinyMario.ml"; "2D"; "Super Mario Bros. (Nintendo, 1985)"; "Run, jump."; "The side-scroller." ]
        [ mario.name; mario.source; mario.look; mario.after; mario.one_line; mario.brought ];
      Alcotest.(check string) "a 2D frame" "tests/2d/golden/TinyMario.png" (Catalogue.golden_frame mario)
  | l -> Alcotest.failf "%d sections, not 2 (the empty one left out)" (List.length l)

(* the test runs in _build/default/launcher/catalogue/tests *)
let test_real () =
  let text = In_channel.with_open_bin "../../../CATALOG.md" In_channel.input_all in
  let sections = Catalogue.parse text in
  let programs = List.concat_map (fun (s : Catalogue.section) -> s.programs) sections in
  let rows = List.length (List.filter (String.starts_with ~prefix:"| [") (String.split_on_char '\n' text)) in
  Alcotest.(check int) "every row, a program" rows (List.length programs);
  Alcotest.(check bool) "games and apps" true (List.exists (fun (s : Catalogue.section) -> s.games) sections && List.exists (fun (s : Catalogue.section) -> not s.games) sections);
  List.iter (fun (p : Catalogue.program) -> if p.after = "" || p.one_line = "" then Alcotest.failf "%s: a cell empty" p.name) programs

let tests =
  Testo.categorize "Catalogue" [ t "plain" test_plain; t "a small catalogue" test_sample; t "CATALOG.md" test_real ]
