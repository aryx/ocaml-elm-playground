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
  Testo.interpret_argv ~project_name:"playground" (fun _env ->
      List.concat [ Unit_camera2d.tests; Unit_tilemap.tests; Unit_sprite.tests; Unit_scene2d.tests; Unit_input_script.tests; Unit_pixel_bounds.tests; Unit_logo.tests; Unit_logo3d.tests; Unit_karel.tests; Unit_audio3d.tests; Unit_audio.tests; Unit_bigbang.tests; Unit_puzzlescript.tests; Unit_input.tests; Unit_ai.tests; Unit_juice.tests ])
