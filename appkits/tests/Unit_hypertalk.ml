(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* appkits/hypertalk: the .mli's example, the values that are all
 * strings, the control structures, and the message path -- run against
 * a world that is three fields, a card number and a log. *)

let t = Testo.create

type w = { fields : (string * string) list; card : int; log : string list }

let world : w Hypertalk.world =
  {
    get_field =
      (fun w n -> match List.assoc_opt (String.lowercase_ascii n) w.fields with Some s -> s | None -> raise (Hypertalk.Error ("no field " ^ n)));
    set_field = (fun w n v -> { w with fields = (String.lowercase_ascii n, v) :: List.remove_assoc (String.lowercase_ascii n) w.fields });
    go =
      (fun w r ->
        let card = match r with Hypertalk.Next -> w.card + 1 | Prev -> w.card - 1 | First -> 1 | Last -> 3 | Numbered n -> n | Named _ -> 2 in
        { w with card });
    answer = (fun w s -> { w with log = ("answer " ^ s) :: w.log });
    beep = (fun w -> { w with log = "beep" :: w.log });
    number_of_cards = (fun _ -> 3);
    card_number = (fun w -> w.card);
    card_name = (fun _ -> "Home");
  }

let start = { fields = [ ("count", "0"); ("a", ""); ("log", "") ]; card = 1; log = [] }
let field w n = List.assoc n w.fields

(* one script, one message *)
let run ?(w = start) text msg = Hypertalk.send world [ Hypertalk.parse text ] msg w

let test_the_worked_example () =
  let script = "on mouseUp\n  add 1 to field \"Count\"\n  if field \"Count\" > 9 then answer \"Ten clicks!\"\nend mouseUp" in
  let w = ref start in
  for _ = 1 to 10 do
    w := run ~w:!w script "mouseUp"
  done;
  Alcotest.(check string) "ten clicks counted" "10" (field !w "count");
  Alcotest.(check (list string)) "and answered once, at the tenth" [ "answer Ten clicks!" ] !w.log

let test_everything_is_a_string () =
  let put e = field (run (Printf.sprintf "on go\n  put %s into field \"A\"\nend go" e) "go") "a" in
  Alcotest.(check string) "a string and a number add" "7" (put "\"3\" + 4");
  Alcotest.(check string) "and join" "7up" (put "3 + 4 & \"up\"");
  Alcotest.(check string) "&& joins with a space" "hello world" (put "\"hello\" && \"world\"");
  Alcotest.(check string) "whole numbers without .0" "2" (put "6 / 3");
  Alcotest.(check string) "the others as they are" "2.5" (put "5 / 2");
  Alcotest.(check string) "precedence" "7" (put "1 + 2 * 3");
  Alcotest.(check string) "an unknown word stands for itself" "hello" (put "hello");
  Alcotest.(check string) "numbers compared as numbers" "true" (put "10 > 9");
  Alcotest.(check string) "text compared as text, whatever its case" "true" (put "\"Apple\" is \"apple\"");
  Alcotest.(check string) "contains" "true" (put "\"HyperCard\" contains \"card\"");
  Alcotest.(check string) "the card" "1 of 3: Home" (put "the number of this card & \" of \" & the number of cards & \": \" & the name of this card")

let test_control () =
  let w = run "on go\n  repeat with i = 1 to 10\n    add i to field \"Count\"\n  end repeat\nend go" "go" in
  Alcotest.(check string) "repeat with: 1 + ... + 10" "55" (field w "count");
  let w = run "on go\n  repeat 3 times\n    put \"x\" after field \"A\"\n  end repeat\nend go" "go" in
  Alcotest.(check string) "repeat n times, put after" "xxx" (field w "a");
  let w = run "on go\n  if 1 > 2 then\n    put \"yes\" into field \"A\"\n  else\n    put \"no\" into field \"A\"\n  end if\nend go" "go" in
  Alcotest.(check string) "if, as a block" "no" (field w "a");
  let w = run "on go\n  if 1 < 2 then put \"yes\" into field \"A\"\n  else put \"no\" into field \"A\"\nend go" "go" in
  Alcotest.(check string) "if on a line, else on the next" "yes" (field w "a");
  let w = run "on go\n  go to next card\n  go next\n  beep\nend go" "go" in
  Alcotest.(check int) "go, twice" 3 w.card;
  Alcotest.(check (list string)) "and beep" [ "beep" ] w.log

(* the path: button, card, background, stack *)
let test_the_message_path () =
  let button = Hypertalk.parse "on mouseUp\n  put \"button \" after field \"Log\"\n  pass mouseUp\nend mouseUp" in
  let card = Hypertalk.parse "on mouseUp\n  put \"card \" after field \"Log\"\nend mouseUp" in
  let background = Hypertalk.parse "on mouseUp\n  put \"background\" after field \"Log\"\nend mouseUp" in
  let w = Hypertalk.send world [ button; card; background ] "mouseUp" start in
  Alcotest.(check string) "passed once, then answered by the card" "button card " (field w "log");
  let silent = Hypertalk.parse "-- nothing here\n" in
  let w = Hypertalk.send world [ silent; silent; background ] "mouseUp" start in
  Alcotest.(check string) "a script with no handler lets it through" "background" (field w "log");
  let w = Hypertalk.send world [ silent; card ] "openStack" start in
  Alcotest.(check string) "and nobody answering is not an error" "" (field w "log")

(* a word on a line of its own is a message, sent from here upwards:
   how you make up a command *)
let test_your_own_commands () =
  let button = Hypertalk.parse "on mouseUp\n  greet\n  greet\nend mouseUp" in
  let stack = Hypertalk.parse "on greet\n  put \"hi \" after field \"Log\"\nend greet" in
  let w = Hypertalk.send world [ button; stack ] "mouseUp" start in
  Alcotest.(check string) "found up the path, twice" "hi hi " (field w "log")

let test_errors () =
  let fails name f = match f () with exception Hypertalk.Error _ -> () | _ -> Alcotest.failf "%s: no error" name in
  fails "a word where a number goes" (fun () -> run "on go\n  add \"cat\" to field \"Count\"\nend go" "go");
  fails "no such field" (fun () -> run "on go\n  put field \"Nope\" into field \"A\"\nend go" "go");
  fails "a handler with no end" (fun () -> Hypertalk.parse "on go\n  beep\n");
  fails "ended with another name" (fun () -> Hypertalk.parse "on go\n  beep\nend stop");
  fails "calling itself forever" (fun () -> run "on go\n  go\nend go" "go");
  (match Hypertalk.parse "on a\n  beep\nend a\n\non b\n  put\nend b" with
  | exception Hypertalk.Error m -> Alcotest.(check bool) ("the line is said: " ^ m) true (String.length m > 6 && String.sub m 0 6 = "line 6")
  | _ -> Alcotest.fail "no error for a put with nothing")

let tests =
  [
    t "HyperTalk: the worked example" test_the_worked_example;
    t "every value is a string" test_everything_is_a_string;
    t "repeat, if, go" test_control;
    t "the message path, and pass" test_the_message_path;
    t "a word is a message: your own commands" test_your_own_commands;
    t "mistakes, and where they are" test_errors;
  ]
