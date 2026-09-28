(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/richtext/Rich: the .mli's worked example run by run, the two
 * rules every word processor has about what typing looks like, and a
 * few thousand random edits against the naive way -- one look stored
 * per character -- which the run table has to be indistinguishable
 * from. *)

let t = Testo.create
let plain = Style.plain
let bold = Style.toggle_bold plain
let italic = Style.toggle_italic plain
let both = Style.toggle_italic bold

(* the runs as (text, look) pairs, which is how a person reads them *)
let shown r =
  let s = Rich.to_string r in
  List.map (fun (start, len, st) -> (String.sub s start len, st)) (Rich.runs r)

let check_runs name expected r =
  let names = List.map fst in
  Alcotest.(check (list string)) (name ^ ": the runs") (names expected) (names (shown r));
  Alcotest.(check bool) (name ^ ": their looks") true (List.map snd expected = List.map snd (shown r))

(* the .mli's worked example *)
let test_the_worked_example () =
  let r = Rich.of_string "The quick brown fox" in
  let r = Rich.restyle Style.toggle_bold (Rich.select ~anchor:4 ~caret:15 r) in
  check_runs "quick brown made bold" [ ("The ", plain); ("quick brown", bold); (" fox", plain) ] r;
  let r = Rich.restyle Style.toggle_italic (Rich.select ~anchor:10 ~caret:19 r) in
  check_runs "brown fox made italic, overlapping"
    [ ("The ", plain); ("quick ", bold); ("brown", both); (" fox", italic) ]
    r

(* what you type looks like what is before it *)
let test_typing_takes_the_look_before_it () =
  let r = Rich.restyle Style.toggle_bold (Rich.select ~anchor:4 ~caret:9 (Rich.of_string "The quick fox")) in
  (* inside the bold word *)
  let r = Rich.insert "est" (Rich.at 9 r) in
  check_runs "typed at the end of the bold word" [ ("The ", plain); ("quickest", bold); (" fox", plain) ] r;
  (* at the very start, the look of what comes after *)
  let r = Rich.insert ">" (Rich.at 0 r) in
  check_runs "typed at the very start" [ (">The ", plain); ("quickest", bold); (" fox", plain) ] r

(* a look set with nothing selected is for what you type next *)
let test_the_typing_style () =
  let r = Rich.at 3 (Rich.of_string "abc") in
  let r = Rich.restyle Style.toggle_bold r in
  check_runs "nothing on the screen changed" [ ("abc", plain) ] r;
  let r = Rich.insert "X" r in
  let r = Rich.insert "Y" r in
  check_runs "but what is typed next is bold, and stays so" [ ("abc", plain); ("XY", bold) ] r;
  (* and moving the caret forgets a look that was waiting *)
  let r = Rich.restyle Style.toggle_italic (Rich.at 0 r) in
  let r = Rich.insert "z" (Rich.at 1 r) in
  check_runs "the pending italic went with the move" [ ("azbc", plain); ("XY", bold) ] r

(* typing over a selection takes the look of its first character *)
let test_typing_over_a_selection () =
  let r = Rich.restyle Style.toggle_bold (Rich.select ~anchor:0 ~caret:3 (Rich.of_string "boldplain")) in
  let r = Rich.insert "BOLD" (Rich.select ~anchor:1 ~caret:6 r) in
  check_runs "replaced, in the first character's look" [ ("bBOLD", bold); ("ain", plain) ] r

(* deleting across a boundary, and the neighbours merging again *)
let test_deleting_merges_what_came_to_look_alike () =
  let r = Rich.restyle Style.toggle_bold (Rich.select ~anchor:3 ~caret:6 (Rich.of_string "abcdefghi")) in
  check_runs "three runs" [ ("abc", plain); ("def", bold); ("ghi", plain) ] r;
  let r = Rich.delete_backward (Rich.select ~anchor:3 ~caret:6 r) in
  check_runs "the bold gone, and the two plains one run" [ ("abcghi", plain) ] r

(* a text with looks is a value: undo is keeping the old one *)
let test_undo_is_the_old_value () =
  let before = Rich.of_string "hello" in
  let h = Undo.start before in
  let after = Rich.restyle Style.toggle_bold (Rich.select ~anchor:0 ~caret:5 before) in
  let h = Undo.record ~name:"Bold" after h in
  check_runs "bold now" [ ("hello", bold) ] (Undo.now h);
  check_runs "and back" [ ("hello", plain) ] (Undo.now (Undo.undo h))

(* --- and the inputs nobody thought of ------------------------------- *)

(* the naive way: a look stored for every character, copied every time *)
module Naive = struct
  type t = { chars : string; looks : Style.t list }

  let insert s at st t =
    {
      chars = String.sub t.chars 0 at ^ s ^ String.sub t.chars at (String.length t.chars - at);
      looks =
        List.filteri (fun i _ -> i < at) t.looks
        @ List.init (String.length s) (fun _ -> st)
        @ List.filteri (fun i _ -> i >= at) t.looks;
    }

  let delete a b t =
    {
      chars = String.sub t.chars 0 a ^ String.sub t.chars b (String.length t.chars - b);
      looks = List.filteri (fun i _ -> i < a || i >= b) t.looks;
    }

  let restyle f a b t = { t with looks = List.mapi (fun i st -> if i >= a && i < b then f st else st) t.looks }
end

let test_against_one_look_per_character () =
  let rng = Random.State.make [| 7 |] in
  let looks = [| Style.toggle_bold; Style.toggle_italic; Style.toggle_underline; Style.toggle_strike |] in
  let r = ref (Rich.of_string "the quick brown fox") in
  let n = ref { Naive.chars = "the quick brown fox"; looks = List.init 19 (fun _ -> plain) } in
  for step = 1 to 3000 do
    let len = String.length !n.chars in
    let a = Random.State.int rng (len + 1) in
    let b = a + Random.State.int rng (len - a + 1) in
    (match Random.State.int rng 3 with
    | 0 ->
        (* type a letter at a caret, with no look pending: it takes the
           look before it (or after it, at the very start) *)
        let st = if len = 0 then plain else if a > 0 then List.nth !n.looks (a - 1) else List.hd !n.looks in
        r := Rich.insert "x" (Rich.at a !r);
        n := Naive.insert "x" a st !n
    | 1 ->
        if b > a then (
          r := Rich.delete_backward (Rich.select ~anchor:a ~caret:b !r);
          n := Naive.delete a b !n)
    | _ ->
        if b > a then (
          let f = looks.(Random.State.int rng 4) in
          r := Rich.restyle f (Rich.select ~anchor:a ~caret:b !r);
          n := Naive.restyle f a b !n));
    if Rich.to_string !r <> !n.chars then
      Alcotest.failf "step %d: the text says %S and the naive one %S" step (Rich.to_string !r) !n.chars;
    List.iteri
      (fun i st ->
        if Rich.style_at !r i <> st then Alcotest.failf "step %d: character %d has the wrong look" step i)
      !n.looks;
    (* and the table stays a table: covering the text, no two
       neighbours alike, nothing empty *)
    let rs = Rich.runs !r in
    Alcotest.(check int) (Printf.sprintf "step %d: the runs cover the text" step)
      (String.length !n.chars) (List.fold_left (fun acc (_, l, _) -> acc + l) 0 rs);
    let rec no_twins = function
      | (_, _, a) :: ((_, _, b) :: _ as rest) -> a <> b && no_twins rest
      | _ -> true
    in
    if not (no_twins rs) then Alcotest.failf "step %d: two neighbouring runs look alike" step
  done

let tests =
  [
    t "the worked example, run by run" test_the_worked_example;
    t "typing takes the look before it" test_typing_takes_the_look_before_it;
    t "a look with nothing selected is for what comes next" test_the_typing_style;
    t "typing over a selection takes its first look" test_typing_over_a_selection;
    t "deleting merges what came to look alike" test_deleting_merges_what_came_to_look_alike;
    t "undo is the old value" test_undo_is_the_old_value;
    t "3000 random edits agree with one look per character" test_against_one_look_per_character;
  ]
