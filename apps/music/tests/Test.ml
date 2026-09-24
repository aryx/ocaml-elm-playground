(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* the voices against Voice.S: one drifting from it is a compile error *)
module _ : Voice.S = Voice_minimoog
module _ : Voice.S = Voice_hammond
module _ : Voice.S = Voice_tb303
module _ : Voice.S = Voice_dx7
module _ : Voice.S = Voice_rhodes
module _ : Voice.S = Voice_cs80
module _ : Voice.S = Voice_juno
module _ : Voice.S = Voice_tr808

let () =
  Testo.interpret_argv ~project_name:"music" (fun _env ->
      Unit_minimoog.tests @ Unit_hammond.tests @ Unit_tb303.tests @ Unit_dx7.tests @ Unit_rhodes.tests @ Unit_cs80.tests
      @ Unit_juno.tests @ Unit_tr808.tests @ Unit_rebirth.tests)
