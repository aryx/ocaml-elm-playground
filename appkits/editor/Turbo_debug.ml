(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Turbo_debug.mli *)

open Turbo_model

(*****************************************************************************)
(* Compiling and running *)
(*****************************************************************************)

(* the text compiled: its code, or the red bar and the cursor on the
   error *)
let compile (m : model) : (Pcode.program * model, model) result =
  match Pascal_compile.compile (Turbo_edit.text m) with
  | Ok p -> Ok (p, { m with compiled = Some p; error = None })
  | Error e -> Error { m with error = Some ("Error: " ^ e.message ^ "."); row = e.line - 1; col = e.col - 1; mode = Editing }

let compiled_box (m : model) (p : Pcode.program) : model =
  { m with
    mode =
      Info
        ( "Compiling",
          [ "Main file: " ^ m.file; ""; "Done."; ""; Printf.sprintf "Lines compiled: %d" (Turbo_edit.nlines m);
            Printf.sprintf "P-code: %d instructions" (Array.length p.code) ] ) }

(*****************************************************************************)
(* The debugger *)
(*****************************************************************************)

let note = "\r\n\x1b[7m Press any key to return to Turbo Pascal \x1b[0m"

(* the program compiled and started, paused before its first
   instruction; its user screen blank *)
let start (m : model) : (session * model, model) result =
  match compile m with
  | Error m -> Error m
  | Ok (program, m) ->
      let machine = Pmachine.start program in
      Ok
        ( { program; machine; user = Vt.create ~rows:24 ~cols:80; typing = None; goal = None; pause = (fun _ -> false);
            seed = Lehmer.of_int (m.runs + 1); swapped = false },
          { m with runs = m.runs + 1 } )

(* the line where a paused program is: the execution bar's *)
let execution_line (m : model) : int option =
  match m.session with Some s when s.goal = None -> Some (Pdebug.line s.program s.machine - 1) | _ -> None

(* the execution bar's line, and the cursor put on it *)
let paused_at (m : model) (s : session) : model =
  { m with session = Some { s with goal = None; swapped = false; typing = None }; mode = Editing; row = Pdebug.line s.program s.machine - 1; col = 0 }

(* the machine run towards its goal, a slice at most: paused, done,
   reading, or still going (the next frame goes on) *)
let rec advance (m : model) (s : session) : model =
  match s.goal with
  | None -> paused_at m s
  | Some _ when s.typing <> None -> { m with session = Some s; mode = Executing }
  | Some _ -> (
      (* a hundred thousand instructions a frame: some 4 ms natively,
         Wirth's queens (677,000) in seven frames *)
      let stop = Pmachine.resume ~pause:s.pause s.machine 100_000 in
      let out = Pmachine.output s.machine in
      let s = if out = "" then s else { s with user = Vt.feed s.user (Line_discipline.output out); swapped = true } in
      match stop with
      | Paused -> paused_at m s
      | Slice_over -> { m with session = Some s; mode = Executing }
      | Need_line -> { m with session = Some { s with typing = Some ""; swapped = true }; mode = Executing }
      | Need_random n ->
          let seed = Lehmer.next s.seed in
          Pmachine.give_random s.machine (int_of_float (Lehmer.to_unit seed *. float_of_int n));
          advance m { s with seed }
      | Halted -> { m with session = None; mode = Finished (Vt.feed s.user note, None); last_screen = Some s.user }
      | Failed (code, msg) ->
          let line = s.program.lines.(max 0 (Pmachine.pc s.machine - 1)) in
          let user = Vt.feed s.user (Printf.sprintf "\r\nRuntime error %d at line %d: %s" code line msg) in
          { m with session = None; mode = Finished (Vt.feed user note, Some (line, Printf.sprintf "Runtime error %d: %s." code msg)); last_screen = Some user })

(* a step taken (F7, F8, F4) or a run (Ctrl-F9), the program started
   first if it wasn't *)
let go (m : model) (step : Pdebug.step) : model =
  let started = match m.session with Some s -> Ok (s, m) | None -> start m in
  match started with
  | Error m -> m
  | Ok (s, m) -> advance m { s with goal = Some step; pause = Pdebug.pause_for s.program step s.machine; swapped = false }

(* a key while the program runs: the line it reads typed on the user
   screen, and Control-C, Turbo's Ctrl-Break, pausing it where it is *)
let executing_key (m : model) (s : session) (k : string) : model =
  match (k, s.typing) with
  | "\x03", _ -> paused_at m s
  | "\r", Some line ->
      Pmachine.give_line s.machine line;
      advance m { s with typing = None; user = Vt.feed s.user "\r\n" }
  | ("\x7f" | "\b"), Some line when line <> "" ->
      { m with session = Some { s with typing = Some (String.sub line 0 (String.length line - 1)); user = Vt.feed s.user "\b \b" } }
  | _, Some line when String.length k = 1 && k.[0] >= ' ' -> { m with session = Some { s with typing = Some (line ^ k); user = Vt.feed s.user k } }
  | _ -> m

(* the word under the cursor: what Ctrl-F7 offers to watch *)
let word_at (m : model) : string =
  let s = Turbo_edit.line m m.row in
  let rec back i = if i > 0 && Turbo_edit.is_word s.[i - 1] then back (i - 1) else i in
  let rec forth i = if i < String.length s && Turbo_edit.is_word s.[i] then forth (i + 1) else i in
  let c = min m.col (String.length s) in
  let a = back c and b = forth c in
  String.sub s a (b - a)
