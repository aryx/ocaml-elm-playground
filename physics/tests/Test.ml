(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"physics" (fun _env -> List.concat [ Unit_integrate.tests; Unit_energy.tests; Unit_physics_api.tests; Unit_collide.tests; Unit_resolve.tests; Unit_broadphase.tests; Unit_solver.tests; Unit_springs.tests; Unit_kepler.tests; Unit_quat3d.tests; Unit_body3d.tests; Unit_integrate3d.tests; Unit_force3d.tests; Unit_physics3d_api.tests; Unit_collide3d.tests; Unit_resolve3d.tests; Unit_broadphase3d.tests; Unit_rolling3d.tests; Unit_solver3d.tests; Unit_character3d.tests; Unit_sweep3d.tests ])
