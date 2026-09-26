(* TinyMyst's island, as data: the scenes the ray tracer draws, the
 * cards (a place and a way of looking, a camera), and the HyperTalk
 * scripts that make them a game. Shared by the game (TinyMyst.ml), the
 * program that makes its stills (make_stills.ml), and the test that
 * checks the stills are still the scene's (tests/).
 *
 * Myst (Rand and Robyn Miller, Cyan, 1993) was a HyperCard stack of
 * 2,500 ray-traced pictures: a card per picture, a button per thing
 * that can be clicked on it, scripts saying what a click does. So is
 * this, twelve cards of it. *)

(* the size of every still: the Macintosh's 512 x 342 screen, Myst's *)
val width : int
val height : int

(*****************************************************************************)
(* {1 The stack's fields} *)
(*****************************************************************************)

(* the island's state, HyperCard's way: fields holding strings, "dock
 * switch" = "up", "dial" = "3", "shelf" = "open" *)
type fields = (string * string) list

(* the four marker switches, all down, the dial at 0, the shelf closed *)
val initial_fields : fields

(* a field's value ("" for no such field) *)
val field : fields -> string -> string

(* the marker switches' fields *)
val switches : string list

(*****************************************************************************)
(* {1 The cards} *)
(*****************************************************************************)

type place = Island | Library_room | Secret_room | Nowhere

type card = {
  name : string;
  (* which scene the camera is in; Nowhere, no picture (the end) *)
  place : place;
  eye : float * float * float;
  target : float * float * float;
  (* the fields its picture depends on: a still per value of them *)
  shows : string list;
  (* the named solids that are buttons on this card: a switch seen
   * from afar is not one, as a HyperCard button belongs to its card *)
  buttons : string list;
  (* fields drawn over the picture, HyperCard's card fields: the
   * field's name, where in the picture (pixels from its top left) *)
  labels : (string * float * float) list;
  (* its HyperTalk script: "mouseUp" for a click on nothing in
   * particular (going forward), "turnLeft" and "turnRight" for a
   * click on the left or right edge *)
  script : string;
}

(* the first card is where the game starts *)
val cards : card list
val find_card : string -> card option

(* the script every message ends up at, if no button or card answers
 * it *)
val stack_script : string

(* the buttons -- named solids -- and their scripts *)
val button_scripts : (string * string) list

(*****************************************************************************)
(* {1 The pictures} *)
(*****************************************************************************)

(* the card's scene in that state: only its [shows] fields count, the
 * others taken as they start (a switch too far to see is drawn down,
 * whatever it is -- one still per value of what the card shows, not
 * per value of the whole island) *)
val scene : card -> fields -> Povray.scene

(* how the stills are ray traced: 2 x 2 rays a pixel *)
val options : Raytrace.options

(* the still's name, e.g. "dock-up", "library-open", "path" *)
val still_name : card -> fields -> string

(* every still the game needs: each card with a picture, in each value
 * of the fields it shows *)
val stills : (card * fields) list

(* [pick card fields ~x ~y]: the name of the card's button seen at
 * (x, y) of its picture, in pixels from its top left, if any *)
val pick : card -> fields -> x:float -> y:float -> string option
