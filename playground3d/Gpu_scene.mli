(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The GPU-API-independent scene preparation shared by the GPU
 * playground3d backends (playground3d/opengl/, and the planned
 * playground3d/webgl/): matrices and vertex data as plain OCaml float
 * arrays, which each backend then uploads with its own GPU API. See
 * Gpu_scene.ml's prelude for why this lives in elm_playground_3d. *)

type vec3 = float * float * float

(* Row-major 4x4 matrices, as a 16-element float array. *)

(* A view matrix, using the exact same right/up/forward camera basis as
 * the native software rasterizer's view_space. *)
val look_at : eye:vec3 -> target:vec3 -> float array

(* A projection matrix, with the same framing as the native software
 * rasterizer's project_vertex, and a z row mapping [near]..[far] to
 * normalized device coordinates -1..+1. *)
val perspective : fov_degrees:float -> aspect:float -> near:float -> far:float -> float array

(* [mat4_mul a b] applied to a point means "apply b first, then a". *)
val mat4_mul : float array -> float array -> float array

(* A GPU draw call binds at most one texture, so faces are grouped by
 * material, one draw call per group. *)
type material = Flat | Textured of string

(* position, normal, color, uv (uv is (0,0) and unused for Flat) *)
type vertex_data = vec3 * vec3 * Playground.color * (float * float)

(* At most one Flat group (always first) plus one group per distinct
 * texture src. Hud shapes contribute no geometry (see
 * Playground3d.collect_hud_shapes). *)
val group_by_material : Playground3d.shape3d list -> (material * vertex_data list) list

(* 11 = position (3) + normal (3) + color (3, as 0..1 floats) + uv (2),
 * interleaved in that order by vertex_floats_of_group. *)
val floats_per_vertex : int

(* The interleaved float data of a group, and its vertex count. *)
val vertex_floats_of_group : vertex_data list -> float array * int

(* The directional "sun" light, the same as the native software
 * rasterizer's light_dir, to upload as a shader uniform. *)
val light_dir : vec3
