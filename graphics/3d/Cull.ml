(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cull.mli *)

let faces_camera ~(eye : Vec3.t) (points : Vec3.t list) : bool =
  (* claude: this winding-based normal is ONLY for backface culling (an
   * independent, whole-face notion of "which way does this face
   * point") -- it is unrelated to the per-vertex normals used for
   * shading (see Shading). For a flat face these two normals happen to
   * have the same value; for a smooth one (e.g. a sphere's) they don't,
   * since each of its points has its own, different normal. *)
  let normal = Vec3.face_normal points in
  let centroid = Vec3.centroid points in
  Vec3.dot normal (Vec3.sub eye centroid) > 0.
