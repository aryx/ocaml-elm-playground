(* Map_graph: the code map's codegraph view (the author: "dependencies
   are better visualized with a dependency structure matrix, like we do
   in codegraph"), a Code_dsm drawn over the map's area.

   The rows on the left, indented under the groups they were expanded
   from (a bar each, named); the columns their numbers; a cell shaded as
   it is used, blue below the diagonal (a use down the layers), magenta
   above it (a use against them: a cycle), its count written when it
   fits. The map's colours: a row hovered, what it uses red, its users
   green (a cell hovered: its row the user, green, its column red); a
   cycle magenta.

   The mouse: a row's name hovered, what it is; a cell hovered, its uses
   at the definitions' grain (who uses what, how many); a click on a row
   expands it into its parts (a folder into its folders and files, a
   file into its definitions), or collapses it; a click on a cell zooms into
   it: a matrix of its row's parts against its column's; a definition
   clicked, or any row shift+clicked, back to the map, there (Go).
   Backspace undoes an expansion, Escape leaves (Back). *)

type t

type action = Stay | Back | Go of string * int option (* a path, a definition's line *)

(* [make map ~title units]: the matrix of [units] (folders or files of
   [map]'s sources, the map's and those beyond it) *)
val make : Code_map_base.t -> title:string -> string list -> t

val update : Playground.computer -> pressed:(string -> bool) -> t -> t * action
val view : Playground.computer -> t -> Playground.shape list
