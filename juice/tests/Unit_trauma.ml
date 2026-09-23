(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_trauma.mli *)

let near = Alcotest.float 1e-4

(* the mean of [f i] for i from 0 to 99_999 *)
let mean (f : int -> float) : float =
  let s = ref 0. in
  for i = 0 to 99_999 do
    s := !s +. f i
  done;
  !s /. 100_000.

let tests =
  Testo.categorize "Trauma"
    [
      Testo.create "the worked example: the hash and the noise at seed 1" (fun () ->
          Alcotest.check near "hash at 0" (-0.1084) (Trauma.hash ~seed:1 0);
          Alcotest.check near "hash at 1" 0.5113 (Trauma.hash ~seed:1 1);
          Alcotest.check near "noise at 0: the hash" (-0.1084) (Trauma.noise ~seed:1 0.);
          Alcotest.check near "noise at 0.5: the mean" 0.2014 (Trauma.noise ~seed:1 0.5);
          Alcotest.check near "noise at 0.25: smoothstep's 0.156 of the way" (-0.0116) (Trauma.noise ~seed:1 0.25));
      Testo.create "the worked example: trauma decays, shake is its square" (fun () ->
          Alcotest.check near "0.5 to 0.25 in a quarter second" 0.25 (Trauma.decay ~dt:0.25 0.5);
          Alcotest.check near "never below 0" 0. (Trauma.decay ~dt:10. 0.5);
          Alcotest.check near "never above 1" 1. (Trauma.add 0.8 0.5);
          Alcotest.check near "half the trauma, a quarter of the shake" 0.25 (Trauma.shake 0.5));
      Testo.create "no trauma, no offset" (fun () ->
          let o = Trauma.offset ~seed:3 ~trauma:0. 12.34 in
          Alcotest.check near "across" 0. o.dx;
          Alcotest.check near "up" 0. o.dy;
          Alcotest.check near "turned" 0. o.angle);
      Testo.create "the offset stays within its bounds" (fun () ->
          for i = 0 to 1000 do
            let o = Trauma.offset ~seed:5 ~trauma:1. (float_of_int i /. 60.) in
            if Float.abs o.dx > 40. || Float.abs o.dy > 40. || Float.abs o.angle > 5. then Alcotest.failf "out of bounds at frame %d" i
          done);
      Testo.create "the hash: mean 0, neighbours and seeds unrelated" (fun () ->
          (* a correlation estimated from 100000 samples: 3 E[xy], the
           * values being uniform in [-1, 1] (variance 1/3) *)
          Alcotest.check (Alcotest.float 0.01) "mean" 0. (mean (Trauma.hash ~seed:7));
          Alcotest.check (Alcotest.float 0.02) "neighbours" 0. (3. *. mean (fun i -> Trauma.hash ~seed:7 i *. Trauma.hash ~seed:7 (i + 1)));
          Alcotest.check (Alcotest.float 0.02) "seeds" 0. (3. *. mean (fun i -> Trauma.hash ~seed:7 i *. Trauma.hash ~seed:8 i)));
    ]
