(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Clip.mli *)

type vertex = Vec3.t * (float * float) * Vec3.t

(* the point of the edge from [a] to [b] at depth [near] *)
let intersect ~(near : float) (((ax, ay, az), (au, av), an) : vertex) (((bx, by, bz), (bu, bv), bn) : vertex) :
    vertex =
  let t = (near -. az) /. (bz -. az) in
  let lerp x0 x1 = x0 +. (t *. (x1 -. x0)) in
  (* z exactly [near], not what the rounding of lerp az bz would give,
   * which could be a hair behind the plane *)
  ( (lerp ax bx, lerp ay by, near),
    (lerp au bu, lerp av bv),
    (* a mix of two unit normals is shorter than 1: renormalize *)
    Vec3.normalize (Vec3.add (Vec3.scale (1. -. t) an) (Vec3.scale t bn)) )

let near_plane ~(near : float) (polygon : vertex list) : vertex list =
  let in_front ((_, _, z), _, _) = z >= near in
  match polygon with
  | [] -> []
  (* claude: the common case, nothing to cut: the polygon as it is (the
   * walk below would give the same vertices, but starting from the
   * second one, and the triangle loop's rounding depends on the order) *)
  | _ when List.for_all in_front polygon -> polygon
  | first :: _ ->
      (* each edge, from [a] to [b], the last one back to the first vertex *)
      let rec edges = function
        | a :: (b :: _ as rest) -> (a, b) :: edges rest
        | [ last ] -> [ (last, first) ]
        | [] -> []
      in
      edges polygon
      |> List.concat_map (fun (a, b) ->
             match (in_front a, in_front b) with
             | true, true -> [ b ]
             | true, false -> [ intersect ~near a b ]
             | false, true -> [ intersect ~near a b; b ]
             | false, false -> [])
