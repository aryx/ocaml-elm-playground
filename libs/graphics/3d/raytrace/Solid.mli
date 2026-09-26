(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* What a ray tracer's scene is made of: solids, each something a ray
 * can be intersected with (Ray), and what its surface looks like.
 *
 * A rasterizer knows one kind of object, the triangle, because it has
 * to project it; a ray tracer knows anything a ray can meet, so a
 * sphere here is a centre and a radius, round at any distance, and a
 * plane goes on for ever -- where the rasterizer's are a few hundred
 * triangles and a finite quad (notes_raytracing.md). Triangles are one
 * solid among the others: the 3D Playground's scenes arrive as them
 * (Shape3d_render_software.solids).
 *
 * The solids are GML's and POV-Ray's (plan_raytracing_teaching.md,
 * phase 6): sphere and plane, and the triangle, as they were first;
 * then any primitive of size one at the origin -- ball, cube,
 * cylinder, cone, torus, half-space -- seen through a Transform (the
 * ray moved the other way, Transform.mli); and solids made of solids,
 * by union, intersection and difference (Csg.mli), from the intervals
 * along a ray where each solid is inside.
 *
 * The unit primitives, centred at the origin:
 *
 *   Ball         the sphere of radius 1
 *   Cube         from (-1, -1, -1) to (1, 1, 1)
 *   Cylinder     radius 1 around the y axis, from y = -1 to 1, capped
 *   Cone         its apex at (0, 1, 0), its base of radius 1 at y = -1
 *   Torus r      a ring of radius 1 around the y axis, its tube of
 *                radius r: (|p|^2 + 1 - r^2)^2 = 4 (x^2 + z^2), a
 *                quartic, whose roots are found by bracketing and
 *                bisection rather than Ferrari's formula (see Solid.ml)
 *   Half_space   y <= 0, the plane y = 0 and all below
 *)

(* the colour's pattern over the surface: one colour, or the
 * checkerboard of Whitted's picture, cubes of [size] in space in [color]
 * and the other one, alternating (a solid texture: Peachey 1985) --
 * the first of phase 7's patterns *)
type pattern = Plain | Checker of int * float

(* a surface: a 0xRRGGBB colour, its pattern, and how shiny or glassy
 * it is *)
type surface = { color : int; pattern : pattern; material : Material.t }

(* a surface's colour at a point of it. The checkerboard's squares are
 * cubes counted by floor (x / size) + floor (y / size) + ..., and a
 * floor at y = 0 meets their faces exactly: a hit point computed a
 * hair below (the shadow acne's rounding again) would fall in the cube
 * under it, the other colour, speckled. A millionth of a square is
 * added first, so that a face belongs to the cube above it. *)
val color_at : surface -> Vec3.t -> int

type primitive = Ball | Cube | Cylinder | Cone | Torus of float | Half_space

type t =
  (* a centre and a radius *)
  | Sphere of Vec3.t * float * surface
  (* the points p with n.p = d, as Ray.plane's; its inside, for CSG,
   * the side n points away from, n.p <= d *)
  | Plane of Vec3.t * float * surface
  (* three points, and the normal at each (the same three for a flat
   * face, the curved surface's for a smooth one, as Render.face's).
   * No inside: a triangle can be in a union, not in an intersection
   * or a difference ([csg] refuses it) *)
  | Triangle of { points : Vec3.t * Vec3.t * Vec3.t; normals : Vec3.t * Vec3.t * Vec3.t; surface : surface }
  (* a unit primitive, through a transform *)
  | Placed of { primitive : primitive; transform : Transform.t; surface : surface }
  (* A op B *)
  | Csg of Csg.op * t * t

(* [csg op a b]: A op B; an intersection or a difference with a
 * triangle in it is refused (Invalid_argument): a triangle has no
 * inside (a closed mesh could have one, by counting crossings: an
 * exercise) *)
val csg : Csg.op -> t -> t -> t

(* the surface of a solid that is not a CSG (a leaf); of a CSG, its
 * first operand's *)
val surface : t -> surface

(* [color leaf point]: the leaf's colour at a point of its surface; a
 * placed primitive's pattern is in its own space, and moves with it *)
val color : t -> Vec3.t -> int

(*****************************************************************************)
(* {1 A ray and a solid} *)
(*****************************************************************************)

(* [intervals ray solid]: where along the whole line the ray is inside
 * the solid, sorted and apart, each boundary with the leaf whose
 * surface it is (see Csg.mli); a triangle's is one point, entered and
 * left at once *)
val intervals : Ray.t -> t -> t Csg.interval list

(* [first_hit ray solid]: the first boundary beyond [min_t] (default
 * 0): where the ray enters the solid, or leaves it if it starts inside
 * (glass, seen from within):
 *
 *      a sphere seen from outside:    o ----> (  in     out  )    t_in
 *      from inside:                   ( in   o ---->    out  )    t_out
 *      behind:              (  in   out  )   o ---->              None
 *
 * [min_t] is also the camera's near plane (Raytrace), and the shadow
 * ray's step off its surface. A sphere, a plane and a triangle are
 * answered directly, the others through their intervals. *)
val first_hit : ?min_t:float -> Ray.t -> t -> t Csg.boundary option

(* [hit ray solid]: [first_hit]'s t *)
val hit : ?min_t:float -> Ray.t -> t -> float option

(* [normal leaf ray t]: the unit normal of a leaf where [ray] meets it
 * at [t], pointing out of it (a plane's: along n): the sphere's radius,
 * the plane's n, the triangle's three normals mixed by the hit's
 * barycentric coordinates (Ray.triangle's u and v) -- the rasterizer's
 * Phong shading does the same mix, per pixel, on the screen -- a placed
 * primitive's in its own space, then carried out by the transform's
 * inverse transpose. A boundary's [flipped] turns it round (Csg). A
 * CSG has no surface of its own (Invalid_argument): the leaf of the
 * boundary [first_hit] gives has. *)
val normal : t -> Ray.t -> float -> Vec3.t

(* [contains solid point]: is the point inside? Answered from each
 * solid's definition, not from its intervals, so that the one checks
 * the other (Requicha's point membership classification) *)
val contains : t -> Vec3.t -> bool

(* the box, sides parallel to the axes, around the solid: (lo, hi)
 * corners; [None] for what no box holds, a plane or a half-space *)
val bounds : t -> (Vec3.t * Vec3.t) option

(*****************************************************************************)
(* {1 Moving solids} *)
(*****************************************************************************)

(* [move offset solid]: the same solid, moved by [offset]: a sphere, a
 * plane and a triangle exactly, as what they are *)
val move : Vec3.t -> t -> t

(* [transform tr solid]: the solid transformed; a sphere stretched or
 * turned becomes a placed ball, a plane and a triangle stay what they
 * are, their points and normals carried *)
val transform : Transform.t -> t -> t
