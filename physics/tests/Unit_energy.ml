(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Energy *)

let t = Testo.create

(* Energy.mli's example *)
let test_example () =
  let b = Body.make ~mass:2. ~vel:(3., 0.) (0., 0.) in
  Alcotest.(check (float 1e-9)) "kinetic" 9. (Energy.kinetic b);
  Alcotest.(check (float 1e-9)) "momentum" 6. (fst (Energy.momentum b))

(* a projectile: kinetic + potential constant under Verlet, exact for
 * a constant acceleration *)
let test_conserved () =
  let g = 10. in
  let total (b : Body.t) = Energy.kinetic b +. Energy.gravity ~g b in
  let b0 = Body.make ~mass:3. ~vel:(4., 10.) (0., 0.) in
  let rec go i b = if i = 0 then b else go (i - 1) (Integrate.verlet ~force:(Force.uniform (0., -.g)) ~dt:0.1 b) in
  Alcotest.(check (float 1e-9)) "the same after 20 steps" (total b0) (total (go 20 b0))

let tests = Testo.categorize "Energy" [ t "the worked example" test_example; t "a projectile's energy" test_conserved ]
