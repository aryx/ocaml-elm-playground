(* A Sketchpad drawing (Ivan Sutherland, "Sketchpad: A Man-Machine
 * Graphical Communication System", MIT, 1963; plan_cad.md): not a
 * picture but a model -- points, the lines and circles standing on
 * them, the constraints between them, and instances of other drawings.
 *
 * Three ideas, which every CAD program since has kept:
 *
 * - **Items share their points.** A line is two point ids, not two
 *   positions (Sutherland's "ring structure" linked each point to what
 *   stands on it). So the corner of a square is one point, and moving
 *   it moves both sides; two lines meet because they *are* joined, not
 *   because their ends happen to coincide.
 *
 *     p1 o------------o p2         Line (p1, p2)
 *        |                         Line (p1, p3): p1 moved,
 *        |                         both lines follow
 *     p3 o
 *
 * - **Constraints are relations, kept by the program**: this line
 *   horizontal, those two parallel, that point on that circle. The
 *   drawing says what should hold, and Relax makes it so -- a rough
 *   hexagon with equal sides and its corners on a circle becomes a
 *   regular one.
 *
 * - **A drawing can be placed in another**, as an instance: the
 *   master's items drawn moved, scaled and turned, and changed
 *   wherever it appears when the master is. Sutherland's masters and
 *   instances are the ancestors of AutoCAD's blocks, of the symbols of
 *   every drawing program, and of objects and classes (Alan Kay named
 *   Sketchpad among Smalltalk's).
 *
 * Coordinates are the drawing's own, y up. A document is a few sheets
 * (Sketchpad's drawings), numbered from 0, and an instance names its
 * master by that number. *)

(*****************************************************************************)
(* {1 A sheet} *)
(*****************************************************************************)

type pos = float * float

(* the points are named by id *)
type item =
  | Line of int * int
  | Circle of int * int (* its center, and a point on its rim: the radius *)

(* the lines and circles are named by id, from the same counter as the
   points *)
type constr =
  | Horizontal of int (* a line *)
  | Vertical of int
  | Parallel of int * int (* two lines *)
  | Perpendicular of int * int
  | Equal of int * int (* two lines of the same length *)
  | On_line of int * int (* a point on a line, or on its extension *)
  | On_circle of int * int (* a point on a circle *)

(* the master sheet, where its origin goes, how big and how turned
   (degrees, counterclockwise) *)
type instance = { master : int; at : pos; size : float; angle : float }

type sheet = {
  points : (int * pos) list;
  (* the points that relaxation must not move *)
  fixed : int list;
  items : (int * item) list;
  constraints : constr list;
  instances : instance list;
  next : int;
}

val empty : sheet

val add_point : pos -> sheet -> sheet * int
val add_item : item -> sheet -> sheet * int

(* a constraint already there is not added twice *)
val constrain : constr -> sheet -> sheet
val pos : sheet -> int -> pos
val set_pos : int -> pos -> sheet -> sheet
val toggle_fixed : int -> sheet -> sheet

(* the ends of a line; a circle's center and rim point *)
val ends : sheet -> int -> (int * int) option

(* the points a constraint's error depends on *)
val points_of : sheet -> constr -> int list

(* [merge ~drop ~onto s]: one point dropped on another becomes it --
   what stood on [drop] stands on [onto]; a line left with both ends
   on one point is gone *)
val merge : drop:int -> onto:int -> sheet -> sheet

(* a point (and what stands on it) or an item, and the constraints on
   what went *)
val delete : int -> sheet -> sheet

(*****************************************************************************)
(* {1 Instances, and the strokes a sheet is drawn with} *)
(*****************************************************************************)

type t = sheet list

val sheet : t -> int -> sheet
val set_sheet : int -> sheet -> t -> t

(* does sheet [a] show sheet [b], itself or through its instances? *)
val uses : t -> int -> int -> bool

(* [place i inst doc]: [inst] added to sheet [i]; None if its master
   shows sheet [i] already -- a drawing inside itself, forever *)
val place : int -> instance -> t -> t option

(* what is drawn: segments and circles (a circle stays one when scaled
   and turned alike in x and y) *)
type stroke = Seg of pos * pos | Round of pos * float

(* where a point of the master goes in an instance *)
val transform : instance -> pos -> pos

val instance_strokes : t -> instance -> stroke list

(* a sheet's own items, then its instances' *)
val strokes : t -> int -> stroke list

(*****************************************************************************)
(* {1 Aiming the pen} *)
(*****************************************************************************)

(* Sketchpad's light pen did not draw where it was: it *aimed*. Near a
   point, the pen was that point; near a line or a circle, it was the
   nearest place on it. So a line drawn to a corner ends on that very
   corner, and one drawn to a line has its end on that line (and a
   constraint saying so). Points win over items, items over instances,
   within [tolerance]. *)
type aim =
  | Nothing
  | At_point of int
  | On_item of int * pos (* the item, the nearest place on it *)
  | At_instance of int (* its index in the sheet's instances *)

(* [aim doc i ~tolerance ?except p]: what the pen at [p] aims at on
   sheet [i]; [except] is ignored (the point being dragged) *)
val aim : t -> int -> tolerance:float -> ?except:int -> pos -> aim

(* the distance from a point to a segment *)
val to_segment : pos -> pos -> pos -> float
