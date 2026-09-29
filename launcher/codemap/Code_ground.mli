(* Code_ground: a file at the ground level (plan_codemap_v2.md, step 5):
   its code on the whole map, each line as high as it matters. A
   definition's header, a section's title, a line the config calls
   important are tall enough to read from where the file is seen whole;
   a body's statements are thin, SeeSoft's bars (Eick et al., 1992); a
   blank line, a banner's rule of stars, thinner still. So the file's
   skeleton reads at once, and its flesh is there, in its shape.

   The heights, a line's weight times a unit:

     blank, a rule of stars    0.35
     a comment                 0.8
     a statement               1
     a definition's header     3
     a section's title         4
     important, weight w       2.5 + w   (the config's; w from 1 to 3)

   Laid out like a newspaper: the lines top to bottom in columns across
   the map, as many columns as keep a statement's 80 characters in a
   column's width at the unit's height (a cell half as wide as high),
   the unit as large as fits, 18 pixels at most (a short file, then, in
   one column, not filling the map).

   Worked example (the tests'): 30 statements and a header (33 units)
   on a map 800 by 600: 800 * 33 / (40 * 600) = 1.1, so one column; the
   unit 600 / (33 * 1.03), 17.65 (3% kept for lines not cut between
   columns), under 18; the header 53 high. *)

(* where a line is: its column, its top, its height, in pixels *)
type place = { col : int; y : float; h : float }

type t = { places : place array; cols : int; colw : float; unit : float }

(* [weights f ~important]: each line's weight, [important] the
   config's (a line from 0, its weight) *)
val weights : Code_file.t -> important:(int * int) list -> float array

(* [layout weights ~pw ~ph]: the lines in columns on a map [pw] by [ph] *)
val layout : float array -> pw:int -> ph:int -> t

(* [paint img f g ~bg ~aa]: the file's lines where [g] places them,
   their letters (VGA's font, anti-aliased if [aa]) when a line is 7
   pixels high or more, else its characters' colours *)
val paint : Rgba_image.t -> Code_file.t -> t -> bg:int * int * int -> aa:bool -> unit

(* the line under a pixel of the map, if any *)
val line_at : t -> float -> float -> int option

(* the same, [q] times larger: on a picture of more pixels than the
   window's (Code_map_base.at_ratio) *)
val scale : t -> float -> t

(* a line's box on the map: its left, top, width, height *)
val box : t -> int -> float * float * float * float
