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
  Testo.interpret_argv ~project_name:"graphics" (fun _env ->
      List.concat
        [
          Unit_affine.tests;
          Unit_framebuffer.tests;
          Unit_fill.tests;
          Unit_line.tests;
          Unit_circle.tests;
          Unit_blit.tests;
          Unit_text.tests;
          Unit_antialiasing.tests;
          Unit_vec3.tests;
          Unit_camera.tests;
          Unit_lighting.tests;
          Unit_project.tests;
          Unit_cull.tests;
          Unit_zbuffer.tests;
          Unit_interpolate.tests;
          Unit_shading.tests;
          Unit_texture.tests;
          Unit_triangle.tests;
          Unit_painter.tests;
          Unit_render.tests;
          Unit_rgba.tests;
        ])
