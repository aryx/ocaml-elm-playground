(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/typeset/Linebreak: the .mli's worked example, line by line,
 * and the law the optimal breaker has to obey on paragraphs nobody
 * wrote by hand -- that it is never worse than greedy, since greedy's
 * breaks are one of the choices it had. *)

let t = Testo.create

(* every letter one unit wide, as in the .mli *)
let words_of s =
  String.split_on_char ' ' s
  |> List.filter (fun w -> w <> "")
  |> List.map (fun w -> { Linebreak.text = w; width = float_of_int (String.length w) })
  |> Array.of_list

let params = { Linebreak.measure = 10.; space = 1.; stretch = 1.; shrink = 0.5 }
let example = words_of "aaa bb cc ddddd ee ff gggg"

let texts words (lines : Linebreak.line list) =
  List.map
    (fun (l : Linebreak.line) ->
      String.concat " " (Array.to_list (Array.map (fun (w : Linebreak.word) -> w.text) (Array.sub words l.first (l.last - l.first + 1)))))
    lines

let test_greedy_on_the_worked_example () =
  let lines = Linebreak.greedy params example in
  Alcotest.(check (list string)) "the lines" [ "aaa bb cc"; "ddddd ee"; "ff gggg" ] (texts example lines);
  Alcotest.(check (list (float 1e-9))) "the ratios" [ 0.5; 2.; 0. ]
    (List.map (fun (l : Linebreak.line) -> l.ratio) lines);
  Alcotest.(check (float 1e-6)) "the score, one river and all" 656706.25 (Linebreak.total lines)

let test_optimal_on_the_worked_example () =
  let lines = Linebreak.optimal params example in
  Alcotest.(check (list string)) "ff taken up a line" [ "aaa bb cc"; "ddddd ee ff"; "gggg" ]
    (texts example lines);
  Alcotest.(check (list (float 1e-9))) "the middle line shrunk as far as it may" [ 0.5; -1.; 0. ]
    (List.map (fun (l : Linebreak.line) -> l.ratio) lines);
  Alcotest.(check (float 1e-6)) "fifty times better" 12706.25 (Linebreak.total lines)

(* justified, the words of a line are set this far apart *)
let test_spacing () =
  match Linebreak.optimal params example with
  | [ first; middle; last ] ->
      Alcotest.(check (float 1e-9)) "stretched by half a unit" 1.5 (Linebreak.spacing params first);
      Alcotest.(check (float 1e-9)) "shrunk to half" 0.5 (Linebreak.spacing params middle);
      Alcotest.(check (float 1e-9)) "the last line at its natural width" 1. (Linebreak.spacing params last)
  | _ -> Alcotest.fail "expected three lines"

(* a word wider than the line cannot be broken without hyphenation,
   which this does not do: it goes on a line of its own, overfull, and
   the paragraph around it is still set *)
let test_a_word_too_wide () =
  let words = words_of "a supercalifragilistic word" in
  let lines = Linebreak.optimal params words in
  Alcotest.(check (list string)) "alone on its line" [ "a"; "supercalifragilistic"; "word" ] (texts words lines)

(* the law, on paragraphs nobody wrote by hand *)
let test_never_worse_than_greedy () =
  let rng = Random.State.make [| 9 |] in
  for trial = 1 to 300 do
    let n = 1 + Random.State.int rng 40 in
    let words =
      Array.init n (fun _ ->
          let len = 1 + Random.State.int rng 8 in
          { Linebreak.text = String.make len 'x'; width = float_of_int len })
    in
    let p = { params with measure = float_of_int (10 + Random.State.int rng 30) } in
    let g = Linebreak.total (Linebreak.greedy p words) in
    let o = Linebreak.total (Linebreak.optimal p words) in
    if o > g +. 1e-6 then
      Alcotest.failf "trial %d: optimal %.2f is worse than greedy %.2f" trial o g;
    (* and every word is set, once, in order *)
    let covered =
      List.concat_map (fun (l : Linebreak.line) -> List.init (l.last - l.first + 1) (fun k -> l.first + k))
        (Linebreak.optimal p words)
    in
    Alcotest.(check (list int)) (Printf.sprintf "trial %d: every word once" trial) (List.init n Fun.id) covered
  done

let tests =
  [
    t "greedy, on the worked example" test_greedy_on_the_worked_example;
    t "optimal, on the worked example" test_optimal_on_the_worked_example;
    t "how far apart the words are set" test_spacing;
    t "a word wider than the line" test_a_word_too_wide;
    t "never worse than greedy, on 300 paragraphs" test_never_worse_than_greedy;
  ]
