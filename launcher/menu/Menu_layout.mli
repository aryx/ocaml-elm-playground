(* Menu_layout: where each part of the menu is, in the Playground's
 * units (the origin the screen's centre, y up). One place for the
 * numbers: Menu_view draws there, Menu_update asks what is under the
 * mouse there, and both open this module.
 *
 * The screen is 16:9, 1778 by 1000, scaled to the window:
 *
 *   TINYBOX  GAMES  APPS  / search                 <- the tabs
 *   < Platform >              3 / 15               <- the arrows
 *   b genre  p any  e any ...                      <- the filter bar
 *   +----+ +----+ +----+ +----+ +----+  +-------------+  its name
 *   |    | |    | |    | |    | |    |  | the chosen  |  what the
 *   +----+ +----+ +----+ +----+ +----+  | one (shot)  |  catalogue
 *   ...        the grid, 5 by 4         +-------------+  says
 *                                       +------------------------+
 *                                       |  its code (code_area)  |
 *                                       +------------------------+
 *)

open Menu_model

val screen_w : int
val screen_h : int

(* the left margin's end *)
val left_edge : float

(* The grid *)

val cols : int
val rows : int

(* a thumbnail's side, and a cell's width and height *)
val thumb : float
val cell_w : float
val cell_h : float

(* the first row shown: the chosen one's row kept in view *)
val first_row : model -> int

(* the centre of the thumbnail of the program at an index of the grid,
 * if it is in view *)
val cell_centre : model -> int -> (Playground.number * Playground.number) option

(* The chosen one *)

(* its screenshot's side and centre *)
val shot : float
val shot_x : float
val shot_y : float

(* what the catalogue says of it, right of the screenshot: the text's
 * left end and width *)
val text_x : float
val text_w : float

(* its code, under both: where Code_map draws it (its top left corner,
 * its width and height in pixels) *)
val code_area : float * float * int * int

val in_code_area : Playground.number * Playground.number -> bool

(* The buttons *)

(* at the top: the two shelves, the section's arrows *)
val games_tab : float * float
val apps_tab : float * float
val prev_arrow : float * float
val next_arrow : float * float

(* the filter bar: its height, each word's key and left end, a word's
 * width *)
val bar_y : float
val bar : (string * float) list
val bar_width : float

(* [near centre mouse ~w ~h]: the mouse within a box of that size round
 * a centre *)
val near : Playground.number * Playground.number -> Playground.number * Playground.number -> w:Playground.number -> h:Playground.number -> bool
