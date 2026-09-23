(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_hash.mli *)

let near = Alcotest.float 1e-4

(* the mean of [f i] for i from 0 to 99_999 *)
let mean (f : int -> float) : float =
  let s = ref 0. in
  for i = 0 to 99_999 do
    s := !s +. f i
  done;
  !s /. 100_000.

let tests =
  Testo.categorize "Hash"
    [
      Testo.create "the worked example: seed 1" (fun () ->
          Alcotest.check near "at 0" (-0.1084) (Hash.hash ~seed:1 0);
          Alcotest.check near "at 1" 0.5113 (Hash.hash ~seed:1 1);
          Alcotest.check near "at 2" 0.5300 (Hash.hash ~seed:1 2);
          Alcotest.check near "in [0, 1]" 0.4458 (Hash.unit ~seed:1 0));
      Testo.create "mean 0, neighbours and seeds unrelated" (fun () ->
          (* a correlation estimated from 100000 samples: 3 E[xy], the
           * values being uniform in [-1, 1] (variance 1/3) *)
          Alcotest.check (Alcotest.float 0.01) "mean" 0. (mean (Hash.hash ~seed:7));
          Alcotest.check (Alcotest.float 0.02) "neighbours" 0. (3. *. mean (fun i -> Hash.hash ~seed:7 i *. Hash.hash ~seed:7 (i + 1)));
          Alcotest.check (Alcotest.float 0.02) "seeds" 0. (3. *. mean (fun i -> Hash.hash ~seed:7 i *. Hash.hash ~seed:8 i)));
    ]
