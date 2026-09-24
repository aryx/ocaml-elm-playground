(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"core" (fun _env ->
    Unit_base64.tests @ Unit_civil.tests @ Unit_clock.tests @ Unit_julian.tests @ Unit_recur.tests
    @ Unit_ics.tests)
