(* A Scratch project run: sprites on a stage, each script a thread.
 *
 * The stage is 480 by 360, (0, 0) in the middle, y up; a direction is
 * in degrees clockwise from up, so 90 is right -- Scratch's
 * coordinates, which children meet as their first Cartesian plane and
 * first compass.
 *
 * Every script started by its hat (the green flag, a key, a click, a
 * broadcast) runs at once with all the others, and none of them has to
 * say so: Scratch's threads are green threads (there is one real one),
 * and a thread gives the others their turn only at chosen points --
 * the end of each turn of a loop, and a wait. After a frame's turn,
 * the stage is drawn. So
 *
 *   forever                      runs one move a frame, 30 or 60 a
 *     move (10) steps            second: an animation, though nothing
 *   end                          in the script says "frame"
 *
 * while a loop-less script of a thousand blocks runs whole in one
 * frame, and two forevers each get a turn per frame, which is why
 * children's programs are concurrent from their first day without
 * locks: nothing can be interrupted between two blocks that are not
 * the end of a loop. (Scratch 2 also yields when a frame has lasted
 * too long, and has a "turbo" and "run without screen refresh"; not
 * here: a frame's turn is bounded by a number of blocks instead.)
 *
 * Values are Scratch's: numbers and texts that turn into each other
 * as needed ("10" + 1 is 11, "cat" + 1 is 1), compared as numbers if
 * both are, else as texts ignoring case ("CAT" = "cat").
 *
 * Worked example: a sprite at (0, 0) facing 90 running "repeat (3)
 * move (10) steps end" is at x = 10 after the first frame's step, 20
 * after the second, 30 after the third, and its thread ends in the
 * fourth. *)

type value = Num of float | Str of string | Bool of bool

val number : value -> float
val text : value -> string

type rotation = All_around | Left_right | Dont_rotate

type sprite = {
  name : string;
  x : float;
  y : float;
  direction : float;
  size : float; (* per cent *)
  visible : bool;
  costume : int; (* from 0 *)
  costumes : int;
  rotation : rotation;
  radius : float; (* its costume's, at 100%: what edges and touching go by *)
  pen : bool;
  pen_hue : float; (* Scratch 2's pen colour, 0 to 200 round the hues *)
  pen_size : float;
  bubble : string option;
  scripts : Scratch_blocks.script list;
}

(* what the pen left on the stage *)
type ink = Line of (float * float) * (float * float) * float * float (* from, to, hue, size *) | Stamp of sprite

type frame
type thread = { sprite : string; script : int; frames : frame list }

type t = {
  sprites : sprite list;
  vars : (string * value) list;
  ink : ink list; (* the newest first *)
  threads : thread list;
  now : float; (* seconds *)
  timer_start : float;
  seed : int;
  broadcasts : string list; (* sent this frame, heard from the next *)
  halt : bool;
}

(* what the stage is told each frame: the mouse in stage coordinates,
   the keys held by Scratch's names ("space", "left arrow", "a"), the
   time in seconds *)
type input = { mouse_x : float; mouse_y : float; mouse_down : bool; keys : string list; time : float }

val sprite : name:string -> costumes:int -> radius:float -> Scratch_blocks.script list -> sprite
val stage : sprite list -> t

val green_flag : t -> t
val stop : t -> t
val key : string -> t -> t
val click : string -> t -> t

(* a script clicked in the editor, run from its first block *)
val run_script : string -> int -> t -> t

(* one frame: each thread run to its next yield *)
val step : input -> t -> t

val find : t -> string -> sprite
val update : t -> sprite -> t
val variable : t -> string -> value
