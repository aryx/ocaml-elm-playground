(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let () =
  Testo.interpret_argv ~project_name:"elm_playground_raster" (fun _env ->
      List.concat
        [ Unit_affine.tests; Unit_framebuffer.tests; Unit_fill.tests; Unit_line.tests; Unit_circle.tests; Unit_blit.tests; Unit_text.tests ])
