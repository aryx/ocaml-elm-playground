(* Map_cards: what Map_atlas says under the mouse, and beside the code.

   From afar, hovering a name gives its card, what the directories'
   .codemapconfig say of the unit (Code_guide), and its ties,
   codegraph's at the map's granularity: roads to the units using it
   and from those it uses, green to red, as wide as the uses.

   At the ground and the street, the words are on the code itself: the
   config's notes after the important lines, the panels' titles and the
   roads between a use and its definition, the line under the mouse
   framed, the name under it glowing wherever it is used; and a preview,
   a card of code, the first lines of the definition of the name
   hovered.

   The functions taking a [name list] want Map_names.names' result,
   computed once a frame by Map_atlas.labels. *)

(* From afar: a unit's card and ties *)

(* the card of the name under the mouse, beside it: its path and its
   description; "not described yet" where no config says anything *)
val hover_card : Code_map_base.t -> Code_map_base.camera -> Map_names.name list -> Playground.shape list

(* the roads between the unit whose name is under the mouse and the
   units tied to it, each grouped as the unit where it parts from the
   hovered one (a far file its region, a near one itself) *)
val unit_ties : Code_map_base.t -> Code_map_base.camera -> Map_names.name list -> Playground.shape list

(* the unit whose name is under the mouse, and the files tied to it, its
   users and what it uses: shift+click's view *)
val unit_with_ties : Code_map_base.t -> Code_map_base.camera -> (string * string list * string list) option

(* At the ground and the street: over the file [e] the map is *)

(* what the config says of an important line, a note after its end when
   the column has room for it *)
val notes : Code_map_base.t -> Code_map_base.camera -> Code_map_base.entry -> Playground.shape list

(* at the street, each panel's title, and the roads *)
val street_labels : Code_map_base.t -> Code_map_base.camera -> Code_map_base.entry -> Playground.shape list

(* the line under the mouse framed *)
val line_lit : Code_map_base.t -> Code_map_base.camera -> Code_map_base.entry -> Playground.shape list

(* the name under the mouse, its binding and its uses, glowing where
   they are in the layouts on the map (the ground's, the panels') *)
val names_glow : Code_map_base.t -> Code_map_base.camera -> Code_map_base.entry -> Playground.shape list

(* Cards of code *)

(* [code_card ?lit t c path first last title mx my]: a card of code
   beside the mouse (mx, my), [path]'s lines [first] to [last] readable
   under a title, painted once and kept; [lit], a line tinted in a
   colour (a match's) *)
val code_card :
  ?lit:int * Playground.color -> Code_map_base.t -> Code_map_base.camera -> string -> int -> int -> string -> float -> float -> Playground.shape list

(* [preview t c path f line col mx my]: the name at a line and column of
   a file, where it is defined elsewhere: a card of its definition's
   first lines (to a blank line, eight at most); nothing otherwise *)
val preview : Code_map_base.t -> Code_map_base.camera -> string -> Code_file.t -> int -> int -> float -> float -> Playground.shape list
