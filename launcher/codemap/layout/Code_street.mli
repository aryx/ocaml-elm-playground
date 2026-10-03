(* Code_street: a file in its context, at the street level
   (plan_codemap_v2.md, step 6): the file looked at in the middle, what it
   uses on its left, what uses it on its right (the author: "on the right
   of the focused file the callers of this file, and on the left the
   callees ... so we have full context for a file"), a key cycling uses,
   users, both.

   Each side's files in panels, six at most (the others named at its
   foot), the program's own code first (its kits) and given thrice the
   room, then the most used; each
   laid out as its own ground (Code_ground) with its weights turned: on
   the left the definitions the focus uses tall, on the right the lines
   using the focus's definitions tall (the eight most tied; the others a
   little less), the rest squeezed thin, so that a file shows its shape
   and, large, what ties it to the focus.

   The ties: a mark in the margin, green at the user's line, red at the
   used definition (codemap's colours); and a road from the name where
   it is used to the name where it is defined (Code_road.road, green to
   red, the direction without an arrow), bundled near the panel so that
   the roads to one file read as one; faint, but the roads of the line
   under the mouse, lit.

   A use is a reference to a name defined in another file
   (Code_file.refs), found among the map's files by Code_names, the sure
   ones only (a guess would draw a road to a wrong place), and not an
   operator's (Basics' +.): noise, not a tie. *)

(* a use: in [src] at [from_line], [from_col], of [name] defined in
   [target] at [target_line], [target_col] *)
type edge = { src : string; from_line : int; from_col : int; target : string; target_line : int; target_col : int; name : string }

(* a file beside the focus, in its panel: its path, its lines laid out
   there, how many ties to the focus *)
type panel = { path : string; ground : Code_ground.t; count : int }

type mode = Uses | Users | Both

(* [uses]: the focus's uses of the others (on the left); [users]: the
   others' uses of the focus (on the right) *)
(* [left_more], [right_more]: the files tied but not shown (six a side
   at most), and their ties, for a line at the foot of their side *)
type t = {
  focus : Code_ground.t;
  left : panel list;
  right : panel list;
  uses : edge list;
  users : edge list;
  focus_path : string;
  left_more : (string * int) list;
  right_more : (string * int) list;
}

(* [uses ~index ~roots ~path f]: [f]'s uses of the other files' names *)
val uses : index:Code_names.index -> roots:string list -> path:string -> Code_file.t -> edge list

(* [layout ~mode ~first ~focus_path ~focus ~file ~uses ~users ~pw ~ph]:
   the focus's lines (their weights, Code_ground.weights) in the middle,
   the files [uses] go to on the left and those [users] come from on the
   right, as [mode] says; [file] giving a file's lexed text; the files
   [first] says first (the program's own code) *)
val layout :
  mode:mode ->
  ?first:(string -> bool) ->
  focus_path:string ->
  focus:float array ->
  file:(string -> Code_file.t option) ->
  uses:edge list ->
  users:edge list ->
  pw:int ->
  ph:int ->
  unit ->
  t

(* the panels, the left's and the right's *)
val panels : t -> panel list

(* the same, [q] times larger (Code_ground.scale) *)
val scale : t -> float -> t

(* the roads, on a map (its area): the line [hover] (a file and a line)
   lit, its roads bright, the others faint *)
val roads : ?hover:string * int -> Code_map_base.area -> t -> Playground.shape list

(* the lit roads' ends framed: the uses green, the definitions red *)
val ends : ?hover:string * int -> Code_map_base.area -> t -> Playground.shape list

(* the file and line under a pixel, the focus's or a panel's *)
val line_at : t -> float -> float -> (string * int) option

(* the ground a file is laid out in, the focus's or a panel's *)
val ground_of : t -> string -> Code_ground.t option
