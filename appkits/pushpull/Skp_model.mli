(* SketchUp's model: edges, and the faces they bound (SketchUp, Brad
 * Schell and Joe Esch, @Last Software, 2000).
 *
 * Every modeller before it made you choose: solids (TinyBlender's
 * primitives, CSG) or meshes of triangles. SketchUp's model is what
 * you would draw on paper -- lines -- and the faces appear by
 * themselves where lines close a flat loop. Three rules make it feel
 * like drawing rather than modelling:
 *
 * - **edges close faces**: an edge that closes a loop of edges in a
 *   plane fills it with a face ([add_edge]); an edge drawn across a
 *   face, from its boundary to its boundary, cuts it in two;
 * - **geometry is sticky**: vertices are shared, never copied. An
 *   edge ending on another edge splits it there, and the faces around
 *   it get the new corner too; move a vertex and every edge and face
 *   on it follows ([move]) -- the ridge of a box's top, lifted, makes
 *   a roof, the walls becoming pentagons by themselves;
 * - **push/pull**: a face swept along its normal, its sides made as it
 *   goes ([push_pull]) -- the one tool that turns a plan into a
 *   building, SketchUp's patent (US 6,628,279, 2003).
 *
 * A face is a loop of vertices, counterclockwise seen from its front
 * (the side its normal points to), and loops of holes, clockwise: a
 * rectangle drawn inside a wall is a hole in it and a face of its own,
 * the window. A face drawn on the ground faces down -- SketchUp's own
 * rule: its back is up, so that pulling it up makes a box whose faces
 * all face out.
 *
 * Push/pull is two cases. A face whose every side is shared with one
 * face square to it (the top of a box) just slides: its vertices move,
 * the sides stretch. Otherwise it is extruded: a copy of it at the
 * distance, a quad per side between them; and the face itself goes if
 * it was closed off (every side shared: it is inside the solid now),
 * the sides that fall in the plane of a neighbour merging into it
 * (pulled out) or cut out of it as a notch (pushed in), the vertices
 * left in the middle of a straight line healed away. A face on its own
 * stays, the bottom of what it made.
 *
 * Worked example, the house (TinySketchup's opening): a 6 x 4
 * rectangle on the ground faces down; pulled up 3 (a push of -3 along
 * its normal) it is a box, 8 vertices, 12 edges, 6 faces. A line from
 * the middle of the top's front edge to the middle of its back edge
 * splits both edges and the top: 10, 15, 7. The line lifted by 2: the
 * same numbers, a gable roof. V - E + F = 2, Euler's formula for a
 * closed solid, all along; a window (a hole in the front wall, pushed
 * in) makes it V - E + F - R = 2 with R the rings of holes, the
 * Euler-Poincare formula of every B-rep modeller. *)

type v3 = Vec3.t

(* an edge between two vertices; [curve]: part of a circle (drawn by
   the Circle tool); [soft]: not drawn, the seam between two faces of a
   curved surface (the sides of a pushed circle) *)
type edge = { a : int; b : int; soft : bool; curve : bool }

type face = { id : int; outer : int list; holes : int list list }

type t = { verts : (int * v3) list; edges : edge list; faces : face list; next : int }

val empty : t

(* how near two points must be to be the same: a millimetre, the model
   being in metres *)
val eps : float

val pos : t -> int -> v3
val face : t -> int -> face option

(* a face's loops, the outer one first; a loop's sides, the last one
   closing it *)
val loops : face -> int list list
val sides : int list -> (int * int) list

(* the unit normal of a face (Newell's, Vec3.face_normal), which its
   outer loop turns counterclockwise about *)
val normal : t -> face -> v3

(* the faces with the edge between two vertices as a side *)
val faces_on : t -> int -> int -> face list

(* [inside t f p]: p, in f's plane, is in f and not in a hole *)
val inside : t -> face -> v3 -> bool

(* [hit t origin dir]: the nearest face along the ray, as the multiple
   of [dir] to it, and the face *)
val hit : t -> v3 -> v3 -> (float * face) option

(* the vertex at a point: the one there, or a new one, splitting the
   edge it is on *)
val vertex_at : t -> v3 -> t * int

(* [add_edge t p q]: the edge between two points (their vertices by
   [vertex_at]); then the face it cuts in two, or the loop it closes,
   the shortest one in a plane, filled *)
val add_edge : ?curve:bool -> t -> v3 -> v3 -> t

(* a closed flat outline (the Rectangle and Circle tools): a face with
   a hole if it lies inside a face, touching nothing; otherwise its
   edges one by one *)
val add_polygon : ?curve:bool -> t -> v3 list -> t

(* [push_pull t face d]: the face swept d along its normal (backwards
   if negative) *)
val push_pull : t -> int -> float -> t

(* [move t vertices delta]: the vertices moved, and all on them *)
val move : t -> int list -> v3 -> t

(* an edge erased with the faces it bounded; a face erased alone *)
val erase_edge : t -> int -> int -> t
val erase_face : t -> int -> t

(* the vertices of the edges and faces given, once each *)
val vertices_of : edges:(int * int) list -> faces:int list -> t -> int list
