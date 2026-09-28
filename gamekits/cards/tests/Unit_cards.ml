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

(* the numbering: rank first, then clubs, diamonds, hearts, spades *)
let test_numbering () =
  let open Cards in
  Alcotest.(check string) "0" "AC" (name (of_index 0));
  Alcotest.(check string) "3" "AS" (name (of_index 3));
  Alcotest.(check string) "37" "TD" (name (of_index 37));
  Alcotest.(check string) "51" "KS" (name (of_index 51));
  Alcotest.(check int) "a deck" 52 (List.length (List.sort_uniq compare deck));
  Alcotest.(check bool) "hearts are red" true (red (of_index 2));
  Alcotest.(check bool) "clubs are black" false (red (of_index 4))

(* Cards.mli's worked example, deal 1, as players have printed it since
 * Windows' FreeCell (and Rosetta Code's "Deal cards for FreeCell") *)
let test_deal_1 () =
  let rows =
    [ "JD 2D 9H JC 5D 7H 7C 5H"; "KD KC 9S 5S AD QC KH 3H"; "2S KS 9D QD JS AS AH 3C"; "4C 5C TS QH 4H AC 4D 7S";
      "3S TD 4S TH 8H 2C JH 7D"; "6D 8S 8D QS 6C 3D 8C TC"; "6S 9C 2H 6H" ]
  in
  let dealt = List.map Cards.name (Cards.deal 1) in
  Alcotest.(check (list string)) "deal 1" (String.concat " " rows |> String.split_on_char ' ') dealt;
  Alcotest.(check int) "every card once" 52 (List.length (List.sort_uniq compare (Cards.deal 617)))

let tests = [ t "the cards' numbering" test_numbering; t "Microsoft's deal 1" test_deal_1 ]
