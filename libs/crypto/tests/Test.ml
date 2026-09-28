(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () = Testo.interpret_argv ~project_name:"crypto" (fun _env -> Unit_sha1.tests @ Unit_sha2.tests @ Unit_hmac.tests @ Unit_ciphers.tests @ Unit_public_key.tests)
