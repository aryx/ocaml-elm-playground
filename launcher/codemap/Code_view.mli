(* The code view: a source file, coloured as codemap colours it
   (Code_file's), with the whole file beside it as SeeSoft drew one
   (Eick, Steffen and Sumner, Bell Labs, 1992): a pixel per character,
   the colour of its category, a row per line, the part shown framed.

   The map (Code_map) opens it on a file; the arrows, Page Up and Down,
   Home and End and the wheel scroll it, and a click (or a drag) on the
   overview goes there. *)

type t

(* [make ?line ?lit file]: the view of [file], [line] (from 0, by
   default the first) in the middle; claude: with [lit], that line lit
   and near the top instead (Codemap's tour) *)
val make : ?line:int -> ?lit:int -> Code_file.t -> t

(* a frame's keys ([pressed]: down this frame and not the last; [arrow]:
   an arrow pressed, or held long enough to repeat) and the mouse *)
val update : Playground.computer -> pressed:(string -> bool) -> arrow:string option -> t -> t

val view : Playground.computer -> t -> Playground.shape list

(* claude: a name that must catch the eye, [w] by [h], centred on (0, 0),
   pulsing (a halo growing and fading), for the map too (Code_map) *)
val glow : Playground.computer -> Playground.color -> float -> float -> Playground.shape list
