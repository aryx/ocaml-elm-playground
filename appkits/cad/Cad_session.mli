(* An AutoCAD session: the command line, as a machine that asks and is
 * answered (AutoCAD Release 12, Autodesk, 1992; plan_cad.md).
 *
 * AutoCAD's interface is a conversation. You type a command, and it
 * asks for what it needs, one prompt at a time, each saying what it
 * accepts:
 *
 *     Command: LINE
 *     From point: 0,0
 *     To point: @100,0             relative to the last point
 *     To point: @50<90             50 units at 90 degrees
 *     To point: C                  an option: close, and end
 *     Command:                     Enter: the last command again
 *
 * A point can be typed (absolute "x,y", relative "@dx,dy", polar
 * "@d<a") or picked with the mouse -- the machine does not care which;
 * the application snaps a pick first (Cad_snap), and keeps it square
 * with ORTHO. Enter (or the right button, or Space) ends a list or
 * takes the <default> offered; Escape cancels (Control-C in 1992).
 * Many commands first ask "Select objects:" and gather a selection --
 * a click on an entity, or a window between two clicks: dragged to the
 * right, what is wholly inside; to the left, what it crosses too.
 *
 * Every command is a state and a prompt; an input moves it on. The
 * application only turns keys and clicks into [input]s and draws
 * [prompt], [log], [preview] (the rubber band) and [selected] -- so
 * the whole of AutoCAD's behaviour is here, testable without a screen.
 *
 * Commands (their short names in brackets): LINE (L), CIRCLE (C), ARC
 * (A, through three points), ERASE (E), MOVE (M), COPY (CO), OFFSET
 * (O), TRIM (TR), EXTEND (EX), FILLET (F), DIMLINEAR (DLI), BLOCK (B),
 * INSERT (I), LAYER (LA), ZOOM (Z), PAN (P), ID, DIST (DI), U, REDO,
 * DXFOUT, DXFIN. Undo goes back a command at a time, the drawing kept
 * as a value before each. *)

type pt = Cad_geom.pt

(* the window onto the drawing: its middle, the drawing's units a
   pixel, and its size in pixels *)
type view = { center : pt; upp : float; w : float; h : float }

type input =
  | Text of string (* a line typed, then Enter; "" is Enter alone *)
  | Pick of pt (* a click, in the drawing's units *)
  | Cancel

(* what the prompt wants: the application snaps a Point, and not a pick
   of objects *)
type want = Idle | Point | Objects | Other

(* what the session asks the application to do with files *)
type io = Save of string * string (* a name, its text *) | Load of string

type t

val start : Cad_drawing.t -> view -> t
val drawing : t -> Cad_drawing.t
val view : t -> view
val resize : float -> float -> t -> t

(* [input ?cursor t i]; the cursor for a distance typed alone at a point
   prompt: that far from the last point, towards the cursor *)
val input : ?cursor:pt -> t -> input -> t

(* a command, from outside the command line (a menu): the one running
   cancelled *)
val command : string -> t -> t

(* ZOOM Extents *)
val zoom_extents : t -> t
val prompt : t -> string
val wants : t -> want

(* the point the rubber band starts from, for ORTHO *)
val anchor : t -> pt option

(* the command line's history, oldest first *)
val log : t -> string list

(* the rubber band: what the command would make with the cursor here *)
val preview : t -> pt -> Cad_drawing.entity list

(* the objects selected so far, and the window being dragged *)
val selected : t -> int list
val selection_box : t -> pt -> (pt * pt * bool) option (* its corners, crossing? *)

(* a file to write or read, asked for this input; [loaded] answers a
   Load with what was read *)
val io : t -> io option
val loaded : string -> string option -> t -> t

(* the name of the command running, if any, for the status line *)
val running : t -> string option
