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
 * Snap!'s additions (Scratch_blocks.mli) make it a real language, and
 * need three things Scratch's runtime had no use for:
 *
 * - **a heap**: a list is an object, [List id], which two variables
 *   can share, so that "add" to one is seen through the other; and a
 *   variable that a parameter or "script variables" makes is a cell
 *   there too. Nothing is ever freed: a garbage collector is the
 *   exercise;
 * - **environments**: each frame a thread runs knows its names -- a
 *   custom block's parameters, its script variables -- as cells; a
 *   name not there is a global. A ring made in a frame keeps that
 *   frame's environment, and so its cells: a closure, as Scheme's
 *   (Snap! is Scheme with blocks: its authors teach SICP with it);
 * - **reporters that run scripts**: a custom reporter's body, or a
 *   command ring called, runs at once to its "report", without
 *   yielding, since its value is wanted now; its loops turn without the
 *   stage being drawn. (Snap! yields there too, a reporter being a
 *   process of its own; not here.) A custom command's body runs in the
 *   thread like any stack, yielding in its loops.
 *
 * Worked example: a sprite at (0, 0) facing 90 running "repeat (3)
 * move (10) steps end" is at x = 10 after the first frame's step, 20
 * after the second, 30 after the third, and its thread ends in the
 * fourth. And "map ({(() * ())}) over (numbers from (1) to (4))" is a
 * new list, (1 4 9 16): the ring's two empty slots both filled with
 * each item. *)

type value = Num of float | Str of string | Bool of bool | List of int | Ring of ring

(* a ring's reporter (or a command ring's script), and the environment
   it was made in *)
and ring

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
  bubble : value option; (* a list shown as Snap! shows it, a table *)
  scripts : Scratch_blocks.script list;
}

(* what the pen left on the stage *)
type ink = Line of (float * float) * (float * float) * float * float (* from, to, hue, size *) | Stamp of sprite

type frame
type thread = { sprite : string; script : int; frames : frame list; result : value option }

type t = {
  sprites : sprite list;
  vars : (string * value) list; (* the globals *)
  cells : (int * value) list; (* the heap's variables *)
  lists : (int * value list) list; (* and its lists *)
  next_id : int;
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

(* a reporter clicked: its value, now *)
val report : input -> t -> string -> Scratch_blocks.block -> value * t

val find : t -> string -> sprite
val update : t -> sprite -> t
val variable : t -> string -> value

(* a list's items; a value as text, a list's items in parentheses *)
val items : t -> int -> value list
val show : t -> value -> string
