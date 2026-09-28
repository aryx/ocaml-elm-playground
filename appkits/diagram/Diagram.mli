(* Diagram: a page of shapes whose geometry is formulas, as Visio made
 * them (Shapeware, 1992).
 *
 * A drawing program keeps a rectangle as four numbers. Visio kept each
 * shape as a small **spreadsheet**, its ShapeSheet: named cells --
 * PinX and PinY (where it is), Width, Height, its outline's vertices,
 * its connection points -- each holding a formula over the others. A
 * block arrow's head is
 *
 *   User.Head     = MIN(0.5, Width*0.5)
 *   Geometry1.X2  = Width-User.Head
 *
 * so stretching the arrow lengthens its shaft and leaves its head the
 * size it was: the shape's behaviour is written in the shape, and
 * anybody who can write a spreadsheet formula can make a "smart
 * shape". Worked example, checked by the tests: the arrow 2 wide has
 * its head's base at 1.5; stretched to 4, at 3.5.
 *
 * Here the whole page is one sheet of appkits/sheet's engine: shape n
 * is column n, a cell a row, and the names in a formula are turned
 * into those cells before the engine sees it ([set_formula]). Another
 * shape's cell is written Sheet.3!PinX, as Visio wrote it. And that
 * is what **glue** is: a connector's end glued to a shape's
 * connection point holds a formula naming that shape's cells,
 *
 *   BeginX = Sheet.3!PinX-Sheet.3!Width*0.5+Sheet.3!Connections.X2
 *
 * so when the shape moves, the engine's dependency graph recomputes
 * the connector's end, and the connector follows -- nothing in this
 * module moves it (Visio's own formula said PAR(PNT(...)), the same
 * thing with its rotation). Worked example: a connector from a box's
 * right side to a diamond's left; the diamond moved up 2, the
 * connector's end rises 2. Deleting a shape leaves the connectors
 * glued to it where they were, their formulas replaced by their
 * values: unglued.
 *
 * Units are inches, y up, a shape's own coordinates from its
 * bottom-left corner. *)

type kind = Box | Connector
type ends = Begin | End

(* a stencil's shape: its cells in the ShapeSheet's order, each a
 * formula as typed; its outline, each vertex a pair of cells; its
 * connection points, each a pair of cells and the side it faces *)
type master = {
  name : string;
  kind : kind;
  cells : (string * string) list;
  geometry : (string * string) list;
  points : (string * string * Ortho_route.dir) list;
}

type shape = {
  id : int;
  master : string;
  kind : kind;
  rows : (string * string) list; (* each cell's name and its formula *)
  geometry : (string * string) list;
  points : (string * string * Ortho_route.dir) list;
  text : string;
  glue : (ends * int * int) list; (* a connector's end, glued to a shape's point *)
}

type t

val empty : t

(* the flowchart stencil, the block arrow, and the dynamic connector *)
val masters : master list

val shapes : t -> shape list
val shape : t -> int -> shape option

(* a master's instance, its pin (or a connector's middle) at a place *)
val drop : t -> master -> float * float -> t * int

(* [set_formula t id name text]: a cell given a number or a formula
 * with names in it; Error for a name the shape does not have, or a
 * formula that does not parse *)
val set_formula : t -> int -> string -> string -> (t, string) result

(* a cell's value, and as the ShapeSheet shows it (an error's reason,
 * a cycle's) *)
val value : t -> int -> string -> float option
val shown : t -> int -> string -> string

val set_text : t -> int -> string -> t

(* moved: its pin set; resized: its size set, its top-left corner kept *)
val move : t -> int -> float * float -> t
val resize : t -> int -> float * float -> t

(* a connector's end glued to a shape's connection point, or put
 * somewhere and unglued *)
val glue : t -> int -> ends -> target:int -> point:int -> t
val place_end : t -> int -> ends -> float * float -> t

(* the shape gone, what was glued to it left where it was *)
val delete : t -> int -> t

(* on the page: a box's outline and its connection points, its
 * bounds; a connector's ends, and its route *)
val outline : t -> shape -> (float * float) list
val connection_points : t -> shape -> ((float * float) * Ortho_route.dir) list
val bounds : t -> shape -> float * float * float * float
val ends_of : t -> shape -> (float * float) * (float * float)
val route : t -> shape -> (float * float) list
