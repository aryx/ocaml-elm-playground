(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* What a surface does with the light that reaches it, beyond its
 * colour: how much it mirrors, and whether light goes through it.
 * Data only -- a rasterizer has no use for either (it has no other
 * objects to reflect, no rays to bend), so graphics/3d's Render
 * carries a face's material without reading it, and only the ray
 * tracer (graphics/3d/raytrace/) makes it mean something: a
 * reflected ray, a refracted one (its Optics: Snell's law, Schlick's
 * approximation of Fresnel).
 *
 * Here rather than in the ray tracer's library so that Render.face can
 * have one without the rasterizer depending on the ray tracer: the
 * dependency goes the other way, the ray tracer reads faces. *)

type t = {
  (* 0. matte .. 1. a perfect mirror: the share of the colour that
   * comes from the reflected ray *)
  shiny : float;
  (* [Some n]: light goes through, bent by the index of refraction n
   * (1.5 glass, 1.33 water, 1. air: not bent at all) *)
  glassy : float option;
}

(* neither shiny nor glassy: every surface the rasterizer draws *)
val matte : t
