(* Code_map_base: the code map's types, and the tools every style of map
   draws with (plan_codemap_google_maps.md, step 0): the layout, the
   camera and its two spaces, the parts' colours, the pixels painted, a
   file's code painted as SeeSoft's picture or as letters, and the labels
   placed. Code_map includes it, and adds what every style shares (the
   moves, the names lit and clicked, the glass); a style (Map_classic)
   draws the picture and its names.

   Two spaces: the layout's, where the treemap is laid out once in a
   rectangle the map's size (units, y downwards), and the screen's, the
   map's pixels. The camera says which unit is at the map's centre and
   how many pixels a unit is (z):

     pixel (px, py)  =  ((u - cx) * z + pw/2,  (v - cy) * z + ph/2) *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type entry = { path : string; nlines : int; file : Code_file.t Lazy.t }

(* where the map is on the screen: its top left corner in the
   playground's coordinates, its size in pixels *)
type area = { left : float; top : float; pw : int; ph : int }

(* [a] rides along: every function given the camera knows where it draws *)
type camera = { cx : float; cy : float; z : float; a : area }

(* a file's geometry in its rectangle, in units: its [k] columns of [lpc]
   lines, a column's width, a character's cell *)
type geometry = { k : int; lpc : int; colw : float; cell_w : float; cell_h : float }

type t = {
  title : string;
  marked : string list;
  entries : entry list;
  algo : Treemap.algo;
  placed : entry Treemap.placed array;
  geometry : geometry option array; (* the files' *)
  cam : camera;
  target : camera;
  drag : (float * float * camera) option; (* where the press began, and the camera then *)
  dragged : bool; (* the press moved: its release is no click *)
  before_right : bool;
  mutable painted : (camera * float * Rgba_image.t) option; (* the picture of [cam], at a pixel ratio *)
  mutable last : camera option; (* the camera the frame before: is it still? *)
  mutable moving : bool; (* it was not, this frame (view): no glass *)
  mutable lens : (camera * Rgba_image.t) option; (* the magnifying glass's last picture, and its camera *)
  order : (string, int) Hashtbl.t; (* a file's place in the reading order, when numbered *)
  colours : (string * (int * int * int)) list; (* a .codemapconfig's (archi) *)
  mutable jumped : (int * (int * int)) option; (* the binding a click on a name went to *)
  mutable back : (camera * (int * (int * int)) option) list; (* where the jumps came from *)
  mutable choices : Code_names.candidate list option; (* the places to choose from *)
  mutable note : string; (* a word for the status line *)
  mutable found : ((string * int * int) * (Code_names.candidate list * bool)) option; (* the last search *)
  roots : string list; (* the projects' tops (Code_names.find) *)
  style : style; (* how the map is drawn *)
  mutable index : Code_names.index option; (* its files indexed, once (index_of) *)
  mutable rank : Code_rank.t option; (* its definitions' uses, once (rank_of) *)
  mutable search : string option; (* the query typed after /, while searching *)
  mutable flight : flight option; (* a smooth flight under way (a search's, a jump's) *)
  mutable pointer : (float * float) option; (* the layout's point under the mouse, when on the map, for a style's labels *)
  mutable focus : int; (* claude: the unit looked at, its index in [placed] (0: the root), when the style moves by units *)
  guide : Code_guide.t; (* claude: what the directories' .codemapconfig say (plan_codemap_v2.md) *)
  mutable street : bool; (* claude: at the ground, the file with what it uses (a: Code_street) *)
  mutable street_mode : int; (* claude: 1 what it uses, on the left; 2 what uses it, on the right; 3 both (a cycling) *)
  mutable clock : float; (* claude: the frame's time (view's), for what pulses *)
  mutable xray : bool; (* claude: the skeletons shown, the rest in the shade (x: Map_v2) *)
  mutable xray_n : int; (* claude: which of the skeletons at hand the X-ray shows (x again: the next) *)
  mutable peek : (string * int * int) option; (* claude: a definition's body shown readable over the map: its file, first and last lines (a click at the ground or the street) *)
  mutable peek_scroll : int; (* claude: the peek's first line shown, a long section's scrolled by the wheel *)
  mutable peek_stack : ((string * int * int) * int) list; (* claude: the peeks under it, and their scrolls: a peek of a peek (a click on a name in one) *)
  beyond : entry list; (* claude: sources not drawn but resolved against, peeked at (a program's map: the rest of the repository) *)
  mutable wheel_debt : float; (* claude: the wheel's notches not yet a step, and when the last step was *)
  mutable wheel_at : float;
}

(* a flight from one camera to another, zooming out and back in (van Wijk
   and Nuij), from a time on (nan: the next frame's) *)
and flight = { from : camera; dest : camera; mutable start : float; duration : float }

(* a style: the map's picture (the directories, the files, their code),
   painted at a camera, anti-aliased if [aa]; and the names over it, [q]
   the window's pixels a unit (at_ratio) *)
and style = {
  sname : string;
  paint : aa:bool -> t -> camera -> Rgba_image.t;
  labels : t -> camera -> float -> Playground.shape list;
  (* the definition a label under a pixel of the map ([q], px, py) names,
     if the style's labels name any: its file, line and name *)
  pick : t -> camera -> float -> float -> float -> (string * int * int) option; (* claude: its file, line and column (Map_v2's ground, street and region panels) *)
  (* claude: the directory or file whose name is under a pixel of the map,
     if the style's names are clickable (Map_v2's): a click flies to it *)
  unit_at : t -> camera -> float -> float -> float -> int option;
  (* claude: the camera moves a unit at a time (Code_units: Map_v2's), or
     freely (the others') *)
  units : bool;
}

(*****************************************************************************)
(* The layout, and the camera *)
(*****************************************************************************)

(* the layout's rectangle, the map's size: its units its pixels at the
   first zoom *)
val root_rect : area -> Treemap.rect

(* a file's columns: characters about twice as high as wide *)
val geometry_of : Treemap.rect -> int -> geometry
(* with [links] (Code_rank.links), layered: the users above the used
   (Code_layers) *)
val relayout : ?links:(string * string * int) list -> area -> Treemap.algo -> entry list -> entry Treemap.placed array * geometry option array

(* the camera fitting a rectangle; the whole map's *)
val fit : area -> Treemap.rect -> camera
val home : area -> camera

val make :
  ?numbered:bool ->
  ?colours:(string * (int * int * int)) list ->
  ?roots:string list ->
  ?guide:Code_guide.t ->
  ?beyond:entry list ->
  style:style ->
  area:float * float * int * int ->
  title:string ->
  marked:string list ->
  entry list ->
  t

(* the map's files, indexed for finding a name (Code_names.index) and
   their definitions' uses counted (Code_rank), each made once, when
   first asked for *)
val files_of : t -> (string * Code_file.t Lazy.t) list
val index_of : t -> Code_names.index
val rank_of : t -> Code_rank.t

(* the files shown and their lines; 12345 as "12,345 lines" *)
val lines_of : entry list -> int
val lines : t -> int
val lines_text : int -> string
val files : t -> int

(* the screen's pixels and the layout's units *)
val to_px : camera -> float -> float
val to_py : camera -> float -> float
val to_u : camera -> float -> float
val to_v : camera -> float -> float

(* the playground's coordinates of a pixel of the map, and back *)
val sx : area -> float -> Playground.number
val sy : area -> float -> Playground.number
val px_of : area -> Playground.number -> float
val py_of : area -> Playground.number -> float
val on : area -> float -> float -> bool
val inside : Treemap.rect -> float -> float -> bool

(* the deepest node under a point of the layout (its index in [placed]),
   the line of a file under it *)
val under : t -> float -> float -> int option
val line_at : geometry -> Treemap.rect -> float -> float -> int

(* where a line of a file is in the layout (its column's left, its top),
   and a name at a column of it *)
val line_pos : Treemap.rect -> geometry -> int -> float * float
val name_pos : Treemap.rect -> geometry -> int -> int -> float * float

(*****************************************************************************)
(* Colours *)
(*****************************************************************************)

(* a part's colour: a .codemapconfig's, then ours by name, then a role
   (tests, docs, lib...), then a hue from its name *)
val archi : (string * (int * int * int)) list -> string -> int * int * int
val mix : int * int * int -> float -> int * int * int -> int * int * int
val dark : int * int * int
val file_background : t -> string -> int * int * int
val dir_colour : t -> string -> int -> int * int * int

(* the categories' colours, Highlight_code's *)
val palette : (int * int * int) array

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(* the characters drawn from a cell this high on the screen; below, a
   cell is a block of its category's colour, SeeSoft's picture *)
val text_px : float
val readable : camera -> geometry -> bool

(* the same view, [q] times more pixels: the window's (pixel_ratio) *)
val at_ratio : camera -> float -> camera

(* a rectangle of pixels in a colour; a rectangle of the layout's pixels
   on the map, clipped (None: off it) *)
val fill : Rgba_image.t -> int -> int -> int -> int -> int * int * int -> unit
val clip : camera -> Treemap.rect -> (int * int * int * int) option

(* [paint_code ~aa img c r g f box bg]: file [f]'s code in [box], its
   pixels [img]'s: far, its cells' colours; near, its letters, anti-
   aliased if [aa] *)
val paint_code :
  aa:bool -> Rgba_image.t -> camera -> Treemap.rect -> geometry -> Code_file.t -> int * int * int * int -> int * int * int -> unit

(*****************************************************************************)
(* The labels' tools *)
(*****************************************************************************)

val yellow : Playground.color
val ink : Playground.color
val dim : Playground.color

(* a frame round a box of the map's pixels, [th] thick *)
val frame : area -> Playground.color -> float -> float -> float -> float -> float -> Playground.shape list

(* words centred at a pixel of the map, [size] high *)
(* the width of words [size] high, estimated (the font is never measured) *)
val text_width : float -> string -> float

val label : area -> ?alpha:Playground.number -> Playground.color -> float -> float -> float -> string -> Playground.shape
val basename : string -> string

(* labels placed greedily, the most important first, none over another *)
type candidate = { rank : float; box : float * float * float * float; shape : Playground.shape }

val place : area -> candidate list -> Playground.shape list
val candidate : area -> rank:float -> ?alpha:Playground.number -> Playground.color -> float -> float -> float -> string -> candidate

(* a name on a tab, [size] high, its box's top left corner at (tx, ty):
   the box, and the shape *)
val tab :
  area -> ?alpha:Playground.number -> Playground.color -> float -> float -> float -> string -> (float * float * float * float) * Playground.shape

val lighter : int * int * int -> Playground.color
