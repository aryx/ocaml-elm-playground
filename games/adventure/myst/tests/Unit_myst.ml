(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* games/adventure/myst: Myst_island and Myst_stills *)

let t = Testo.create

(*****************************************************************************)
(* The scripts *)
(*****************************************************************************)

(* the island as a script sees it, without the game's screen: the card
 * shown and the fields *)
type state = { card : string; fields : Myst_island.fields }

let world : state Hypertalk.world =
  { get_field = (fun s f -> match List.assoc_opt f s.fields with Some v -> v | None -> raise (Hypertalk.Error f));
    set_field = (fun s f v -> { s with fields = (f, v) :: List.remove_assoc f s.fields });
    go =
      (fun s r ->
        match r with
        | Named c when Myst_island.find_card c <> None -> { s with card = c }
        | _ -> raise (Hypertalk.Error "no such card"));
    answer = (fun s _ -> s);
    beep = (fun s -> s);
    number_of_cards = (fun _ -> List.length Myst_island.cards);
    card_number = (fun _ -> 1);
    card_name = (fun s -> s.card) }

(* a click on a button of the card, or on the card itself *)
let click ?button (s : state) : state =
  let card = Option.get (Myst_island.find_card s.card) in
  let from = match button with Some b -> [ List.assoc b Myst_island.button_scripts ] | None -> [] in
  Hypertalk.send world (List.map Hypertalk.parse (from @ [ card.script; Myst_island.stack_script ])) "mouseUp" s

let test_scripts_parse () =
  List.iter
    (fun (c : Myst_island.card) -> ignore (Hypertalk.parse c.script))
    Myst_island.cards;
  List.iter (fun (_, s) -> ignore (Hypertalk.parse s)) Myst_island.button_scripts;
  ignore (Hypertalk.parse Myst_island.stack_script)

(* every card a script goes to exists: each edge and middle of each card
 * followed *)
let test_cards_lead_somewhere () =
  List.iter
    (fun (c : Myst_island.card) ->
      let script = Hypertalk.parse c.script in
      List.iter
        (fun msg ->
          let s = Hypertalk.send world [ script ] msg { card = c.name; fields = Myst_island.initial_fields } in
          Alcotest.(check bool) (c.name ^ " " ^ msg) true (Myst_island.find_card s.card <> None))
        (Hypertalk.handlers script))
    Myst_island.cards

let test_puzzle () =
  let start = { card = "library"; fields = Myst_island.initial_fields } in
  let flip_all s = List.fold_left (fun s b -> click ~button:b s) s Myst_island.switches in
  let dial n s = List.fold_left (fun s () -> click ~button:"dial" s) s (List.init n (fun _ -> ())) in
  let shelf s = Myst_island.field s.fields "shelf" in
  Alcotest.(check string) "the dial alone" "closed" (shelf (dial 4 start));
  Alcotest.(check string) "three switches, 3" "closed" (shelf (dial 3 (click ~button:"dock switch" (flip_all start))));
  let solved = dial 4 (flip_all start) in
  Alcotest.(check string) "four switches, 4" "open" (shelf solved);
  Alcotest.(check string) "the dial goes round" "closed" (shelf (dial 1 solved));
  Alcotest.(check string) "the passage" "secret" (click ~button:"passage" solved).card;
  Alcotest.(check string) "the passage, closed" "library" (click ~button:"passage" start).card

(*****************************************************************************)
(* The buttons *)
(*****************************************************************************)

(* a click on the dock's switch is a ray that meets it: somewhere on a
 * grid over the picture, and never on the planks at the bottom *)
let test_pick () =
  let dock = Option.get (Myst_island.find_card "dock") in
  let found = ref false in
  for gy = 0 to Myst_island.height / 4 do
    for gx = 0 to Myst_island.width / 4 do
      let x = float_of_int (gx * 4) and y = float_of_int (gy * 4) in
      if Myst_island.pick dock Myst_island.initial_fields ~x ~y = Some "dock switch" then found := true
    done
  done;
  Alcotest.(check bool) "the switch is somewhere" true !found;
  Alcotest.(check (option string)) "the planks" None (Myst_island.pick dock Myst_island.initial_fields ~x:256. ~y:330.);
  (* the library's door is in the middle of the dock's picture, but a
   * button of another card *)
  Alcotest.(check (option string)) "the door, from afar" None (Myst_island.pick dock Myst_island.initial_fields ~x:256. ~y:171.);
  let outside = Option.get (Myst_island.find_card "library outside") in
  Alcotest.(check (option string)) "the door, before it" (Some "library door")
    (Myst_island.pick outside Myst_island.initial_fields ~x:256. ~y:200.)

(*****************************************************************************)
(* The stills *)
(*****************************************************************************)

let test_all_embedded () =
  let wanted = List.sort compare (List.map (fun (c, f) -> Myst_island.still_name c f) Myst_island.stills) in
  let have = List.sort compare (List.map fst Myst_stills.all) in
  Alcotest.(check (list string)) "make_stills.exe's, and only them" wanted have

(* each still is its scene's: a few 16 x 16 blocks ray traced again,
 * their average colour within JPEG's error of the still's. Changing the
 * island without running make_stills.exe again fails here. *)
let test_stills_are_the_scene () =
  let blocks = [ (64, 64); (248, 160); (400, 272); (128, 300) ] in
  List.iter
    (fun ((card : Myst_island.card), fields) ->
      let name = Myst_island.still_name card fields in
      let still = Jpeg.decode (List.assoc name Myst_stills.all) in
      let world = Raytrace.world ~options:Myst_island.options (Povray.raytrace_scene (Myst_island.scene card fields)) in
      List.iter
        (fun (bx, by) ->
          let sum = Array.make 6 0 in
          for y = by to by + 15 do
            for x = bx to bx + 15 do
              let rgb = Raytrace.pixel world ~width:Myst_island.width ~height:Myst_island.height ~x ~y in
              let i = 4 * ((y * still.width) + x) in
              sum.(0) <- sum.(0) + ((rgb lsr 16) land 0xFF);
              sum.(1) <- sum.(1) + ((rgb lsr 8) land 0xFF);
              sum.(2) <- sum.(2) + (rgb land 0xFF);
              for k = 0 to 2 do
                sum.(3 + k) <- sum.(3 + k) + Bigarray.Array1.get still.rgba (i + k)
              done
            done
          done;
          for k = 0 to 2 do
            let d = abs (sum.(k) - sum.(3 + k)) / 256 in
            if d > 6 then Alcotest.failf "%s, the block at (%d, %d): off by %d (make_stills.exe again?)" name bx by d
          done)
        blocks)
    Myst_island.stills

let tests =
  [ t "the scripts parse" test_scripts_parse;
    t "every card leads to a card" test_cards_lead_somewhere;
    t "the puzzle" test_puzzle;
    t "a click is a ray" test_pick;
    t "every still is embedded" test_all_embedded;
    t "the stills are the scene's" test_stills_are_the_scene ]
