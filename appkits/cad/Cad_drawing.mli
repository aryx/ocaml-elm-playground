(* An AutoCAD drawing (AutoCAD, Autodesk, 1982): entities on layers,
 * and blocks, in the drawing's own units, y up.
 *
 * Two ideas from the draftsman's office:
 *
 * - **Layers** are the transparent sheets a drawing was made on: the
 *   outline on one, the center lines on another (red), the dimensions
 *   on a third (green). An entity has its layer's colour, and a layer
 *   switched off hides everything on it -- the same drawing printed
 *   for the machinist without the notes.
 *
 * - **Blocks** are Sketchpad's masters again (appkits/sketch): a named
 *   group of entities with a base point, and INSERT places it, scaled
 *   and turned, as many times as needed. Change the block and every
 *   insertion changes. A bolt drawn once, inserted four times.
 *
 * A dimension is an entity too, the one that *measures*: its value is
 * computed from its two points, so moving what it measures and it
 * says the new length -- AutoCAD's associative dimensions, in their
 * simplest form. *)

type pt = Cad_geom.pt

type entity =
  | Line of pt * pt
  | Circle of pt * float
  | Arc of pt * float * float * float (* center, radius, start, end: degrees *)
  | Insert of string * pt * float * float (* the block, where, scale, rotation *)
  | Dimension of pt * pt * pt (* the two points measured, where the dimension line goes *)

type ent = { entity : entity; layer : string }

(* AutoCAD's colours are numbers (the ACI): 1 red, 2 yellow, 3 green,
   4 cyan, 5 blue, 6 magenta, 7 white *)
type layer = { name : string; color : int; on : bool }

type t = {
  ents : (int * ent) list; (* back to front, by id *)
  layers : layer list;
  current : string; (* the layer new entities go on *)
  blocks : (string * (pt * ent list)) list; (* a name: its base point, its entities *)
  next : int;
}

(* layer "0", white, current *)
val empty : t

val add : entity -> t -> t * int
val get : t -> int -> ent option
val remove : int list -> t -> t
val replace : int -> entity list -> t -> t (* one entity by others, on its layer *)
val ids : t -> int list
val layer : t -> string -> layer

(* the layer, made if it is not there *)
val ensure_layer : string -> t -> t
val set_layer : layer -> t -> t

(*****************************************************************************)
(* {1 What an entity is made of} *)
(*****************************************************************************)

(* an insertion's entities, where they land *)
val transform : pt -> float -> float -> pt -> pt
val move_entity : pt -> entity -> entity

(* the drawn pieces of an entity, blocks exploded (their entities on
   layer "0" taking the insertion's layer, as in AutoCAD), each with
   its layer: what is drawn, snapped to and picked *)
val pieces : t -> ent -> (Cad_geom.curve * string) list

(* a dimension: horizontal or vertical (by where its line is), its
   value, and its lines -- the dimension line and two extension lines *)
val dimension_value : pt -> pt -> pt -> float
val dimension_lines : pt -> pt -> pt -> (pt * pt) list

(* where its text goes, and the text *)
val dimension_text : pt -> pt -> pt -> pt * string

(* the entities' box, of those on layers switched on: None if none *)
val extents : t -> (pt * pt) option

(* [pick t tolerance p]: the entity nearest p, within tolerance, on a
   layer switched on; the one in front if two are as near *)
val pick : t -> float -> pt -> int option

(* [window t a b ~crossing]: the entities wholly inside the box -- or,
   crossing, the ones inside or touching it (AutoCAD's two windows:
   dragged to the right, to the left) *)
val window : t -> pt -> pt -> crossing:bool -> int list
