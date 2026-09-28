(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_lehmer.mli *)

let after (n : int) (s : Lehmer.t) : Lehmer.t =
  let rec go n s = if n = 0 then s else go (n - 1) (Lehmer.next s) in
  go n s

let tests =
  Testo.categorize "Lehmer"
    [
      Testo.create "the worked example: the sequence from seed 1" (fun () ->
          let s1 = Lehmer.next (Lehmer.of_int 1) in
          let firsts = List.map (fun n -> (after n s1 :> int)) [ 0; 1; 2; 3; 4 ] in
          Alcotest.(check (list int)) "five" [ 16807; 282475249; 1622650073; 984943658; 1144108930 ] firsts);
      Testo.create "Park and Miller's check: the 10,000th is 1043618065" (fun () ->
          Alcotest.(check int) "10,000th" 1043618065 (after 10000 (Lehmer.of_int 1) :> int));
      Testo.create "Schrage's trick is the plain product, mod 2^31 - 1" (fun () ->
          (* native ints have 63 bits: the product itself fits here *)
          let s = ref (Lehmer.of_int 42) in
          for _ = 1 to 1000 do
            let plain = 16807 * (!s :> int) mod 2147483647 in
            s := Lehmer.next !s;
            Alcotest.(check int) "same" plain (!s :> int)
          done);
      Testo.create "every int is a seed, never 0" (fun () ->
          List.iter
            (fun n -> Alcotest.(check bool) (string_of_int n) true ((Lehmer.of_int n :> int) >= 1))
            [ 0; 1; -1; 2147483647; -2147483647; max_int; min_int ]);
      Testo.create "scrambled: neighbour seeds, unrelated games" (fun () ->
          let first n = Lehmer.to_unit (Lehmer.next (Lehmer.scramble n)) in
          (* as is, seed 2's first draw is twice seed 1's; scrambled, no *)
          let u1 = Lehmer.to_unit (Lehmer.next (Lehmer.of_int 1)) and u2 = Lehmer.to_unit (Lehmer.next (Lehmer.of_int 2)) in
          Alcotest.(check bool) "as is: 0.000008 and 0.000016" true (u1 < 0.00001 && u2 < 0.00002);
          let firsts = List.map first [ 1; 2; 3; 4; 5 ] in
          Alcotest.(check bool) "scrambled: spread over [0, 1)" true
            (List.exists (fun u -> u > 0.5) firsts && List.exists (fun u -> u < 0.5) firsts);
          Alcotest.(check bool) "scrambled: not twice" true (Float.abs (first 2 -. (2. *. first 1)) > 0.01));
      Testo.create "to_unit in [0, 1)" (fun () ->
          let s = ref (Lehmer.of_int 7) in
          for _ = 1 to 1000 do
            s := Lehmer.next !s;
            let u = Lehmer.to_unit !s in
            Alcotest.(check bool) "in range" true (u >= 0. && u < 1.)
          done);
    ]
