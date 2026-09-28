(* Map_streets: the code map as a street map (plan_codemap_google_maps.md),
   a style beside Map_classic. Each zoom answers its own question, by how
   high a line of code is on the screen (in the window's pixels):

     Z0  < 0.22       the parts: each file a flat block of its colour
     Z1-Z2  < 2.5     how much code, and its shape: the column hints, a
                      bar as long as each line, no colours of the code
     Z3  < text_px    where things are: SeeSoft's colours, a character a
                      pixel of its category's
     Z4               what the code says: the letters

   and between two, a band where one fades into the next, so that zooming
   is continuous. The files coloured by their role, as codemap's
   archi_code did: a program's main (Tiny*.ml, main.c), a test, a lexer
   or a parser; else their part's colour (Code_map_base.archi).

   The names over the map (the plan's steps 2 and 3): the directories',
   big and faint while they fill a good part of the screen (codemap's);
   and labels placed once, zoom by zoom (Code_labels), by their uses
   (Code_rank): the program's own file's tab always; the tricks of this
   game from Z1, landmarks; the files' tabs from Z1; the map's 3 most used
   definitions at Z0 and Z1, and each directory's 2; every definition and
   section at Z2 and Z3; at Z4, the code itself. *)

(* the level for a line this high on the screen (window pixels): 0 to 4 *)
val level : float -> int

(* claude: where the code's colours come in (Z3), and a step smoothed
   between two heights (0 below a, 1 above b) *)
val t_colours : float
val smooth : float -> float -> float -> float

(* a file's colour, its role's over its part's *)
val file_colour : Code_map_base.t -> string -> int * int * int

val paint : aa:bool -> Code_map_base.t -> Code_map_base.camera -> Rgba_image.t

(* a definition's weight on Highlight_code.emphasis's scale, from its
   uses (Code_rank.score); a section's title's, its category's *)
val emphasis : Code_map_base.t -> string -> int -> string -> Highlight_code.category -> float
val labels : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list

val style : Code_map_base.style
