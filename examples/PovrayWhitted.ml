(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The picture that made ray tracing: Turner Whitted's, in "An Improved
 * Illumination Model for Shaded Display" (CACM, 1980) -- a glass
 * sphere and a mirror sphere over a red and yellow checkerboard,
 * rendered on a VAX-11/780 at 512 x 512 in 74 minutes. What no
 * rasterizer could draw then, nor draws now without tricks: the
 * checkerboard seen in the mirror, the mirror seen through the glass,
 * the board bent and turned upside down inside it (a ball lens), and
 * every shadow.
 *
 * The same scene on the Povray way, by each of the ray tracer's
 * algorithms (the arrows; the flag evolution puts them side by side):
 * the last one, Whitted's, is the only one that knows what a mirror or
 * glass is -- to the three before it both spheres are white balls.
 * Under the picture, the rays Whitted's algorithm shoots from the
 * points the eye's rays meet, reflected and refracted, and those the
 * attenuation cutoff did not shoot (Raytrace.mli).
 *
 * Whitted's own picture has its spheres hanging in the air and a
 * hollow glass sphere (a thin shell); here they are two solid balls,
 * the glass one a lens, the index of glass, 1.5.
 *
 * What it uses: the Playground, and the Povray way (checker, shiny,
 * glassy, orbit).
 *
 * Exercises: set the glass's index to 1. (air: no bending at all) and
 * to 2.42 (diamond); make the mirror a perfect one (shiny 1.) and a
 * dull one (0.3); drag the camera until the mirror shows the glass.
 *)
open Playground
open Povray

let board = checker (rgb 210 40 30) (rgb 240 210 50)

let app (caps : < Cap.open_out >) =
  orbit ~export:caps ~file:"whitted.png" ~size:(400, 300)
    (scene ~ambient:0.3 ~sky:(rgb 110 160 230)
       ~camera:(camera ~fov:45. ~eye:(0., 1.8, 4.) ~target:(0., 0.9, -2.) ())
       [ area_lamp 0.6 (lamp (rgb 230 230 230) 4. 8. 4.) ]
       [ move 0. 0. 0. (plane board);
         move 0.9 1.1 (-3.5) (sphere (shiny 0.85 (color (rgb 200 200 210))));
         move (-0.8) 1. (-1.3) (sphere (glassy 1.5 (color white))) ])

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
