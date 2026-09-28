(* A program's entry point, run at once or kept for later.
 *
 * Every game and app ends with its main, which OCaml runs when the
 * module is initialized:
 *
 *   let main = Program.main __MODULE__ (fun () ->
 *     Playground_platform.run_app app)
 *
 * Alone, in its own executable (TinyMario.exe, TinyMario.bc.js), the
 * program starts right there, as a plain [let main = run_app app]
 * would. Linked with every other program in one binary, tinybox (the
 * launcher, plan_launcher.md), each would start in turn at link time,
 * so there [main] only records the entry under the program's name,
 * and the launcher runs the one chosen.
 *
 * The capabilities stay the program's own business: a program that
 * needs some calls Cap.main inside its entry, as before --
 *
 *   let main = Program.main __MODULE__ (fun () -> Cap.main (fun caps ->
 *     Playground_platform.run_app (app (caps :> < Cap.open_in >))))
 *
 * -- and a process of tinybox runs at most one entry, so Cap.main
 * is still called once per process.
 *)

(* [main name entry]: [entry ()] now, or recorded under [name] once
 * [collect] was called. [name] is the program's module name,
 * [__MODULE__]. *)
val main : string -> (unit -> unit) -> unit

(* From now on, [main] records instead of running. Called by the
 * launcher's library, whose initialization comes before any program's
 * (a library's modules are initialized before the executable's). *)
val collect : unit -> unit

(* the entries recorded, in link order *)
val collected : unit -> (string * (unit -> unit)) list

(* [run name ~argv]: the launcher runs the entry recorded under [name],
 * with [argv] as the program's command line (see [argv]). Raises
 * Not_found if no program has that name. *)
val run : string -> argv:string array -> unit

(* The program's command line, what the platform parses (its dashed
 * options, and the app's flags, Playground_platform.flags) instead of
 * Sys.argv: Sys.argv alone; in tinybox, the one [run] was given, as if
 * the program had been started alone --
 *
 *   tinybox TinyWinamp dir=~/Music   ->  [| "TinyWinamp"; "dir=~/Music" |]
 *
 * -- so that the launcher's own words never reach the program.
 * Fails if called between [collect] and [run]: a program reading its
 * command line at its top level instead of in its main. *)
val argv : unit -> string array
