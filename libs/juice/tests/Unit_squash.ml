(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_squash.mli *)

let near = Alcotest.float 1e-3
let landing = Squash.landing ~amount:0.4

let tests =
  Testo.categorize "Squash"
    [
      Testo.create "the worked example: a 40-pixel ball landing" (fun () ->
          let sx, sy = Squash.keep_area (landing 0.) in
          Alcotest.check near "66.7 wide" 66.667 (40. *. sx);
          Alcotest.check near "24 tall" 24. (40. *. sy);
          Alcotest.check near "at 5%" 0.859 (landing 0.05);
          Alcotest.check near "at 10%, stretched" 1.1 (landing 0.1);
          Alcotest.check near "at half the time, nearly round" 1.006 (landing 0.5);
          Alcotest.check (Alcotest.float 1e-9) "at the end, round" 1. (landing 1.));
      Testo.create "the most it stretches: 1.149, at 13% of the time" (fun () ->
          let best = ref (0., 0.) in
          for i = 0 to 100_000 do
            let p = float_of_int i /. 100_000. in
            if landing p > fst !best then best := (landing p, p)
          done;
          Alcotest.check near "how much" 1.149 (fst !best);
          Alcotest.check (Alcotest.float 1e-2) "when" 0.13 (snd !best));
      Testo.create "the area is kept at every moment" (fun () ->
          for i = 0 to 100 do
            let sx, sy = Squash.keep_area (landing (float_of_int i /. 100.)) in
            Alcotest.check (Alcotest.float 1e-9) (Printf.sprintf "at %d%%" i) 1. (sx *. sy)
          done);
    ]
