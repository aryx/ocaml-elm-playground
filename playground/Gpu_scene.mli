(* The GPU-API-independent scene preparation shared by the GPU
 * playground3d backends (OpenGL and WebGL): vertex data as plain
 * OCaml float arrays, which each backend then uploads with its own GPU API (the camera matrices
 * are graphics/3d/geometry/Mat4's). See Gpu_scene.ml's prelude for why
 * this lives in elm_playground_3d. *)

type vec3 = float * float * float

(* A GPU draw call binds at most one texture, so faces are grouped by
 * material, one draw call per group. *)
type material = Flat | Textured of string

(* position, normal, color, uv (uv is (0,0) and unused for Flat) *)
type vertex_data = vec3 * vec3 * Playground.color * (float * float)

(* At most one Flat group (always first) plus one group per distinct
 * texture src. Hud shapes contribute no geometry (see
 * Playground3d.collect_hud_shapes). A Cached3d is flattened like a
 * group, unless [on_cached] is given: then it's passed to [on_cached]
 * instead (for the backend to draw its own buffers, see Mesh_cache),
 * and contributes no geometry. *)
val group_by_material :
  ?on_cached:(Playground3d.cached -> unit) -> Playground3d.shape3d list -> (material * vertex_data list) list

(* 11 = position (3) + normal (3) + color (3, as 0..1 floats) + uv (2),
 * interleaved in that order by vertex_floats_of_group. *)
val floats_per_vertex : int

(* The interleaved float data of a group, and its vertex count. *)
val vertex_floats_of_group : vertex_data list -> float array * int

(* The directional "sun" light, Lighting.light_dir, to upload as a
 * shader uniform. *)
val light_dir : vec3
