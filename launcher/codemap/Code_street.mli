(* Code_street: a file and what it uses, at the street level
   (plan_codemap_v2.md, step 6). The file looked at keeps its ground
   (Code_ground) on the left of the map; on the right, a panel for each
   file it uses the most, six at most, each laid out as its own ground
   but with its weights turned: the definitions the focus uses tall, the
   rest squeezed thin, so that a kit's file shows its shape and, large,
   the few things the game takes from it. From each use to its
   definition, an edge: a road (Map_atlas.road), green at the user's
   end, red at the used's, the direction without an arrow; the roads to
   one file bundled, through one point before its panel (Holten's idea,
   flattened to two levels).

   The uses are the file's references to names defined elsewhere
   (Code_file.refs), found among the map's files by Code_names, the
   sure ones only: a guess would draw a road to a wrong place; and not
   an operator's (Basics' +.): noise, not an association. *)

(* a use: the focus's line, the file and line of the definition, its name *)
type edge = { from_line : int; target : string; target_line : int; name : string }

(* a file used, in its panel: its path, its lines laid out there, how
   many of the focus's uses go to it *)
type panel = { path : string; ground : Code_ground.t; count : int }

type t = { focus : Code_ground.t; panels : panel list; edges : edge list; split : float }

(* [uses ~index ~roots ~path f]: [f]'s uses of the other files' names *)
val uses : index:Code_names.index -> roots:string list -> path:string -> Code_file.t -> edge list

(* [layout ~focus ~file edges ~pw ~ph]: the focus's lines (their
   weights, Code_ground.weights) on the left, the files its [edges] go
   to on the right in panels, [file] giving a file's lexed text; the
   files [first] says first (the program's own code: its kits, Code_deps.own),
   then the most used *)
val layout :
  ?first:(string -> bool) -> focus:float array -> file:(string -> Code_file.t option) -> edge list -> pw:int -> ph:int -> t

(* the same, [q] times larger (Code_ground.scale) *)
val scale : t -> float -> t

(* the roads, on a map (its area) *)
val roads : Code_map_base.area -> t -> Playground.shape list

(* the file and line under a pixel, the focus's or a panel's *)
val line_at : t -> focus_path:string -> float -> float -> (string * int) option
