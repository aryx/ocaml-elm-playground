(* Map_search: / on Map_atlas, the search (Code_search), and the marks,
   searches kept.

   A query looks among the map's directories, its files and their
   top-level definitions (every file lexed once, at the first query; the
   sources beyond the map too, a program's map having the rest of the
   repository behind it). A prefix narrows it, as VS Code's: file:,
   dir:, def:, type:, view:, tour:, bone:, and two that look in the
   code rather than among names, text: (the lines' text; also a query
   starting with a double quote) and ref: (the code's references; also
   @). The hits are lit where they are, at any level: a directory or a
   file framed, a definition's line marked.

   ctrl+Enter keeps a search as a mark, in a colour of its own; the
   marks are lit all at once ("Cap.fork, Cap.exec ...: all the
   capabilities highlighted at the same time"), their legend in the
   map's bottom left corner. The configs give groups of marks too, which
   l cycles through. *)

(* The search *)

(* the hits of the query being typed (t.search), the best first; among
   the files shown only (the unit looked at, the street's panels) when
   the search says here. Kept until the query changes *)
val search_hits : Code_map_base.t -> Code_search.hit list

(* the directories a query ending in // takes together *)
val search_named : Code_map_base.t -> string list

(* all a search found, to see together (shift+Enter): its directories
   and files, a file under a directory found left out; else, if it found
   only definitions, their files *)
val search_set : Code_map_base.t -> string list

(* hits lit where they are on the map: a directory or file framed in
   [glow], a definition's line marked (a bar at the ground, a dot of
   radius [dot] above it from afar), [chosen] brighter and named *)
val search_lit :
  ?glow:Playground.color ->
  ?dot:Playground.number ->
  ?chosen:Code_search.hit ->
  Code_map_base.t ->
  Code_map_base.camera ->
  Code_search.hit list ->
  Playground.shape list

(* the box, under the title: the query typed, where it looks, the best
   hits, the chosen one lit, and what the keys do *)
val search_box : Code_map_base.t -> Code_map_base.camera -> Code_map_base.search -> Code_search.hit list -> Playground.shape list

(* The marks *)

(* the marks' colours, one each, in turn *)
val mark_colours : (int * int * int) list

(* a mark's hits: its query's, found once and kept in the mark *)
val mark_hits : Code_map_base.t -> Code_map_base.mark -> Code_search.hit list

(* the groups of marks l cycles through: those kept (ctrl+Enter in the
   search), then each config's, their names *)
val mark_groups : Code_map_base.t -> (string * Code_map_base.mark list) list

(* the group shown (t.mark_group): each mark's hits lit in its colour,
   and the legend *)
val marks_shapes : Code_map_base.t -> Code_map_base.camera -> Playground.shape list
