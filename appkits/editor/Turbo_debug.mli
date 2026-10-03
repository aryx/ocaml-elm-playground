(* Turbo_debug: the text compiled and its program run, stepped and
   paused: what made Turbo Pascal an environment and not an editor. The
   compiler is Pascal_compile, in one pass to P-code; the machine is
   Pmachine, which can be paused; the steps are Pdebug's.

   A program started is a session (Turbo_model.session). It is not run
   to its end in one call: [advance] drives its machine a slice a frame
   towards the session's goal (the next line, the cursor's line, a
   breakpoint, the end), so that the IDE stays alive, Ctrl-C can break
   a loop, and a readln waits for keys. When the goal is reached the
   session is paused, the execution bar on the line to run next.

   The program's screen (the user screen) is shown only while the
   program writes or reads, Turbo's "smart" screen swapping: a step
   that prints nothing doesn't flash it. *)

open Turbo_model

(* Compiling *)

(* the text compiled: its code, kept in the model while the text is
   unchanged; or the model with the red bar saying the first error and
   the cursor on it *)
val compile : model -> (Pcode.program * model, model) result

(* the box after F9: the lines compiled and the code's size *)
val compiled_box : model -> Pcode.program -> model

(* Running and stepping *)

(* [go m step]: a step taken (F7 trace into, F8 step over, F4 to the
   cursor) or a run (Ctrl-F9), the program compiled and started first
   if it wasn't *)
val go : model -> Pdebug.step -> model

(* a frame while the program runs: its machine run towards the goal, a
   slice at most. Then paused, finished (the user screen kept, and a
   run-time error's line), waiting for a line to read, or still going *)
val advance : model -> session -> model

(* a key while the program runs: typed on the user screen for the line
   the program reads; Control-C, Turbo's Ctrl-Break, pauses it where it
   is *)
val executing_key : model -> session -> string -> model

(* the line (from 0) where a paused program is, the execution bar's;
   None when no program is paused *)
val execution_line : model -> int option

(* the word under the cursor: what Ctrl-F7 offers to watch *)
val word_at : model -> string
