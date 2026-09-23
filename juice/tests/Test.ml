(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"juice" (fun _env -> Unit_ease.tests @ Unit_tween.tests @ Unit_squash.tests @ Unit_hash.tests @ Unit_trauma.tests @ Unit_emitter.tests)
