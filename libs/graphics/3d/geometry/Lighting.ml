(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lighting.mli *)

(* claude: this is a DIRECTIONAL light -- a "sun" -- not a light at a
 * position. There are 3 common kinds of light in 3D graphics, in
 * increasing order of realism/cost:
 *  - directional (what this is): infinitely far away, so its rays are
 *    effectively parallel everywhere in the scene -- there is no
 *    "origin point" to specify, only a *direction* it shines from,
 *    the same for every face regardless of where that face is. The
 *    real sun works this way for all practical purposes (it's ~150
 *    million km away), which is why this is the natural choice for an
 *    outdoor scene. Cheapest to compute: one constant vector, reused
 *    for every face, no per-face distance/attenuation math at all.
 *  - point light: sits at an actual 3D position (e.g. a lightbulb or
 *    torch); the direction *to* it, and therefore how a face is lit,
 *    is different for every face depending on where that face is
 *    relative to the light, and realistically its brightness also
 *    falls off with distance ("attenuation"). More expensive (a
 *    per-face, or per-pixel, direction+distance calculation instead of
 *    one shared constant) and not implemented here.
 *  - spotlight: a point light further restricted to a cone (a
 *    direction plus a cutoff angle) -- even more parameters, also not
 *    implemented here.
 *
 * The vector itself is a DIRECTION, not a position: by convention here
 * it points FROM a lit surface TOWARDS the light (so
 * [dot normal light_dir] below is large/positive exactly when a face's
 * normal points roughly *at* the light, i.e. is well-lit -- see
 * brightness_of_normal). (1., 1.3, 0.6) reads as "the sun sits up and
 * off to the +X/+Z side" -- an arbitrary but reasonable-looking choice,
 * not derived from anything; e.g. changing it to (0., 1., 0.) would put
 * the sun straight overhead instead (top faces bright, sides dimmer,
 * undersides at the `ambient` floor below). Not exposed to the public
 * API yet -- a game can't configure this per scene, only by editing
 * this constant and recompiling. *)
let light_dir : Vec3.t = Vec3.normalize (1., 1.3, 0.6)

(* claude: never fully black (a face directly facing away from the
 * light stays at least at ambient brightness) -- a real scene has some
 * ambient/bounced light even on surfaces not directly facing the sun,
 * and a fully-black face would look like a hole rather than a shaded
 * surface *)
let ambient = 0.25

(* claude: pure -- "how lit is a surface facing this direction",
 * independent of any shading mode: whether to call it once per face,
 * once per vertex, or once per pixel with an interpolated normal (or
 * not at all, without lighting) is the shading's decision, not the
 * lighting's (see the software rasterizer's make_shader). *)
let brightness_of_normal (normal : Vec3.t) : float =
  let lit = Stdlib.max 0. (Vec3.dot normal light_dir) in
  ambient +. ((1. -. ambient) *. lit)
