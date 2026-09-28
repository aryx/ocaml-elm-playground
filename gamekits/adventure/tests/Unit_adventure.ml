(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let t = Testo.create

(* Adventure.mli's worked example: a lamp and a closed box in the hall,
 * a key in the box *)
let test_worked_example () =
  let open Adventure in
  let rules =
    [ { verb = "take"; obj = any; with_ = None; test = always;
        act = (fun s w -> (put (Option.get s.obj) Carried w, "Taken.")) };
      { verb = "open"; obj = Some "box"; with_ = None; test = always; act = (fun _ w -> (set "box open" w, "Opened.")) } ]
  in
  let say s w = run rules s w in
  let take o = { verb = "take"; obj = Some o; with_ = None } in
  let w = start "hall" [ ("lamp", Room "hall"); ("box", Room "hall"); ("key", Inside "box") ] in
  let w, r = say (take "key") w in
  Alcotest.(check string) "in the closed box" "You see no key here." r;
  let w, r = say { verb = "open"; obj = Some "box"; with_ = None } w in
  Alcotest.(check string) "opened" "Opened." r;
  let w, r = say (take "key") w in
  Alcotest.(check string) "taken" "Taken." r;
  Alcotest.(check bool) "carried" true (where w "key" = Carried);
  let w, r = say (take "lamp") (go "garden" w) in
  Alcotest.(check string) "not in the garden" "You see no lamp here." r;
  let w, r = say { verb = "sing"; obj = None; with_ = None } w in
  Alcotest.(check string) "no rule" "You can't do that." r;
  Alcotest.(check int) "five turns" 5 w.turns;
  Alcotest.(check (list string)) "the key carried, into the garden" [ "key" ] (carried w);
  Alcotest.(check (list string)) "nothing lying in the garden" [] (in_room w)

let tests = [ t "the worked example" test_worked_example ]
