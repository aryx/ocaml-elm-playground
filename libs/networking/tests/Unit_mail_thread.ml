(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_mail_thread.mli *)

(* a message: its id, what it names, its subject, when *)
type m = { id : string; refs : string list; subject : string; date : float }

let msg ?(refs = []) id subject date = { id; refs; subject; date }

let threads (l : m list) : m Mail_thread.tree list =
  Mail_thread.threads ~id:(fun m -> Some m.id) ~references:(fun m -> m.refs) ~subject:(fun m -> m.subject) ~date:(fun m -> m.date) l

(* a tree as text: "1(2(3 5) 4)", "_" for a container never received *)
let rec show (ts : m Mail_thread.tree list) : string =
  String.concat " " (List.map (fun (Mail_thread.Node (m, kids)) -> (match m with Some m -> m.id | None -> "_") ^ if kids = [] then "" else "(" ^ show kids ^ ")") ts)

let str = Alcotest.(check string)

let tests =
  Testo.categorize "Mail_thread"
    [
      Testo.create "the thread of five, by References" (fun () ->
          (* Our_mail's: 2 answers 1; 3 answers 2; 4 answers 2; 5 answers 3 *)
          let l =
            [ msg "1" "the plan" 1.; msg ~refs:[ "1" ] "2" "Re: the plan" 2.; msg ~refs:[ "1"; "2" ] "3" "Re: the plan" 3.; msg ~refs:[ "1"; "2" ] "4" "Re: the plan" 4.;
              msg ~refs:[ "1"; "2"; "3" ] "5" "Re: the plan" 5. ]
          in
          str "one tree" "1(2(3(5) 4))" (show (threads l));
          (* in any order they arrive *)
          str "the same, backwards" "1(2(3(5) 4))" (show (threads (List.rev l))));
      Testo.create "a parent never received: an empty container" (fun () ->
          str "two replies hold on to it" "_(b c)" (show (threads [ msg ~refs:[ "x" ] "b" "Re: q" 1.; msg ~refs:[ "x" ] "c" "Re: q" 2. ]));
          str "one reply: promoted" "b" (show (threads [ msg ~refs:[ "x" ] "b" "Re: q" 1. ]));
          str "a missing middle: skipped" "a(c)" (show (threads [ msg "a" "q" 1.; msg ~refs:[ "a"; "b" ] "c" "Re: q" 2. ])));
      Testo.create "by subject, when no reference says" (fun () ->
          str "Re: under the original" "l(r)" (show (threads [ msg "l" "lunch" 1.; msg "r" "Re: lunch" 2. ]));
          str "two of a kind: together" "_(a b)" (show (threads [ msg "a" "lunch" 1.; msg "b" "Lunch" 2. ]));
          str "different subjects apart" "a b" (show (threads [ msg "a" "lunch" 1.; msg "b" "dinner" 2. ])));
      Testo.create "no loops" (fun () ->
          str "itself in its References" "a" (show (threads [ msg ~refs:[ "a" ] "a" "x" 1. ]));
          (* a names b, then b names a: the second link would close a loop *)
          str "each other's" "b(a)" (show (threads [ msg ~refs:[ "b" ] "a" "x" 1.; msg ~refs:[ "a" ] "b" "x" 2. ]));
          (* the same id twice is two messages, the essay says, kept apart *)
          str "an id twice" "a(b(a))" (show (threads [ msg "a" "x" 1.; msg ~refs:[ "a" ] "b" "Re: x" 2.; msg ~refs:[ "b" ] "a" "x" 3. ])));
      Testo.create "base_subject" (fun () ->
          str "Re:s" "lunch" (Mail_thread.base_subject "Re: RE: Re[2]: Fwd: Lunch ");
          Alcotest.(check bool) "a reply" true (Mail_thread.is_reply "re: x");
          Alcotest.(check bool) "not: Rex" false (Mail_thread.is_reply "Rex: x"));
    ]
