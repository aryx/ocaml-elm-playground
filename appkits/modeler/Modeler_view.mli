(* The four views of a modeller (Sketchpad III, Timothy Johnson, MIT,
 * 1963, the first; Blender's quad view, Ctrl-Alt-Q): the scene seen
 * from above, from the front, from the right -- three orthographic
 * projections, a draftsman's plan and elevations -- and through the
 * camera, in perspective.
 *
 * An orthographic view drops one coordinate: from the top, (x, y, z)
 * is drawn at (x, y); from the front at (x, z); from the right at
 * (y, z). Nothing shrinks with distance, so lengths can be read off
 * and a drag moves an object along the two axes the view shows. The
 * camera's view divides by the depth instead (Camera.ndc): the same
 * projection as the ray tracer's rays, so the wires drawn over the
 * rendered picture fall exactly on the solids.
 *
 * Worked example: from the right, the point (5, 2, 3) is at (2, 3);
 * dragged 10 pixels right at 20 pixels a unit, it moves by (0, 0.5, 0). *)

type view = Top | Front | Right | Camera_view

val name : view -> string

(* a view's two screen axes, as the world's axes 0, 1, 2 (x, y, z);
   and the axis it looks along (what R turns about), with its sign *)
val axes : view -> int * int
val normal : view -> int * float

(* [project view p]: an orthographic view's (u, v) of p, in world units *)
val project : view -> Modeler.v3 -> float * float

(* [unproject view (du, dv)]: a move on the screen, in world units, as
   a move in the world *)
val unproject : view -> float * float -> Modeler.v3

(* the scene's camera (its eye, looking at [target]), in the ray
   tracer's y-up world; vertical field of view in degrees *)
val camera : ?fov:float -> Modeler.t -> target:Modeler.v3 -> Camera.t

(* [perspective camera ~aspect p]: p (z up) in normalized device
   coordinates, -1..1 across the picture; None behind the eye *)
val perspective : Camera.t -> aspect:float -> Modeler.v3 -> (float * float) option

(* [pick ~to_screen ~tolerance t (x, y)]: the object whose wire passes
   nearest the point, within tolerance, of those not hidden; [to_screen]
   the view's projection, None for a point it cannot show *)
val pick : to_screen:(Modeler.v3 -> (float * float) option) -> tolerance:float -> Modeler.t -> float * float -> string option
