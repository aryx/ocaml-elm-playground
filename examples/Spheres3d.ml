(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Unlike Cube3d/Cubes3d/PaintersAlgorithmFail3d etc., a cube's faces
 * each have their own independent, unshared corners (see
 * Playground3d.box_faces), so flat_shading/Gouraud/Phong all render a
 * cube pixel-for-pixel identically -- there's no vertex normal to
 * share or blend across a hard edge. A sphere is the shape where the
 * 4 shading modes ("m" to cycle, see notes_3d.md section 11) actually
 * look different from one another: flat_color is unlit, flat_shading
 * shows the individual UV-tessellation facets, Gouraud smooths that
 * out except right at the silhouette, and Phong is the smoothest of
 * all. Press "m" a few times while this is running to see it. *)
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 20. computer.time in
  let a_sphere = sphere red 1.2 |> move3d (-1.6) 0. 0. in
  let b_sphere = sphere yellow 1.2 |> move3d 1.6 0. 0. in
  let scene = group3d [ a_sphere; b_sphere ] |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(0., 2.5, 7.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
