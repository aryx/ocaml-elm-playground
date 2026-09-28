(* The inference engine: a point in 3D from a point on the screen.
 *
 * The mouse gives two numbers and a 3D point needs three: which point
 * along the ray under the mouse is meant? A CAD program asks for
 * coordinates, or for a plane to work in first. SketchUp guesses --
 * its inventors' word was "inference" -- from what is near the mouse
 * on the screen, and says what it guessed, with a coloured dot and a
 * word:
 *
 *   Endpoint (green)    a vertex, within a few pixels
 *   Midpoint (cyan)     the middle of an edge
 *   On Red Axis ...     the line from the last point along x (red), y
 *                       (green) or z (blue): drawing square to the
 *                       world without a grid or an ORTHO mode
 *   On Edge (red)       the point of an edge nearest the ray
 *   On Face (blue)      where the ray meets the first face
 *
 * in that order: a point beats a line beats a surface, because the
 * rarer a thing is under the mouse, the likelier it was aimed at. With
 * nothing near, the ray meets the ground -- or, drawing from a point
 * above it, the level of that point.
 *
 * Only what can be seen is inferred: a vertex behind a face is not an
 * endpoint (the ray from the eye to it meets something first).
 *
 * Worked example: from (0, 0, 0), the mouse a pixel off the image of
 * the x axis, 3 metres along: On Red Axis, the point (3, 0, 0), even
 * though the ray under the mouse misses the axis by a centimetre. *)

type kind = Endpoint | Midpoint | On_axis of int | On_edge | On_face | Nowhere

(* the point, what it was inferred from, and what is under it: the
   vertex (Endpoint), the edge (Midpoint, On_edge), the face the ray
   meets first (whatever the kind: the face a rectangle is drawn on) *)
type found = { point : Vec3.t; kind : kind; vertex : int option; edge : (int * int) option; face : int option }

(* its word, as on SketchUp's tooltip *)
val name : kind -> string

(* [find ~project ~ray ?from model mouse]: [project] a world point on
   the screen (None behind the eye), [ray] the eye and the unit
   direction under the mouse, [from] the last point clicked, [tolerance]
   in screen units (default 10) *)
val find :
  project:(Vec3.t -> (float * float) option) -> ray:Vec3.t * Vec3.t -> ?from:Vec3.t -> ?tolerance:float -> Skp_model.t -> float * float -> found

(* [along (origin, dir) p u]: the point of the line p + s u nearest to
   the ray, as its s -- how far a push/pull or a move along an axis has
   gone, the mouse being anywhere *)
val along : Vec3.t * Vec3.t -> Vec3.t -> Vec3.t -> float

(* [on_plane (origin, dir) p n]: where the ray meets the plane through p
   of normal n, if in front of the eye *)
val on_plane : Vec3.t * Vec3.t -> Vec3.t -> Vec3.t -> Vec3.t option
