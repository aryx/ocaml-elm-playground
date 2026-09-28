(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Talk.mli *)

(* the seeded numbers: Lehmer's generator, drawn exactly as
   Playground.random_int draws them, so that a seed gives the same game
   in a test and on the Playground *)
type seed = Lehmer.t

let initial_seed (n : int) : seed = Lehmer.scramble n

let random_int (lo : int) (hi : int) (seed : seed) : int * seed =
  let seed = Lehmer.next seed in
  (lo + int_of_float (float_of_int (hi - lo + 1) *. Lehmer.to_unit seed), seed)

(*****************************************************************************)
(* Programs *)
(*****************************************************************************)

type status = Exited | Interrupted

type 'a talk =
  | Done of 'a
  | Print of string * 'a talk
  | Read_line of (string -> 'a talk)
  | Read_key of (string -> 'a talk)
  | Random of int * (int -> 'a talk)
  | Spawn of unit talk * (status -> 'a talk)
  | Step of (unit -> 'a talk)

let return x = Done x
let print s = Print (s, Done ())
let read_line = Read_line (fun l -> Done l)
let read_key = Read_key (fun k -> Done k)
let random n = Random (n, fun i -> Done i)
let spawn child = Spawn (child, fun st -> Done st)
let step = Step (fun () -> Done ())

(* the rest of the program glued after the end of [m]: the requests
   stay the same, and where [m] was Done, [f] takes over *)
let rec ( let* ) (m : 'a talk) (f : 'a -> 'b talk) : 'b talk =
  match m with
  | Done x -> f x
  | Print (s, k) -> Print (s, ( let* ) k f)
  | Read_line k -> Read_line (fun l -> ( let* ) (k l) f)
  | Read_key k -> Read_key (fun key -> ( let* ) (k key) f)
  | Random (n, k) -> Random (n, fun i -> ( let* ) (k i) f)
  | Spawn (child, k) -> Spawn (child, fun st -> ( let* ) (k st) f)
  (* [f] applied only when the step is taken: what follows is built as
     late as that *)
  | Step k -> Step (fun () -> ( let* ) (k ()) f)

let ask (question : string) : string talk =
  let* () = print question in
  read_line

(*****************************************************************************)
(* Running without a screen *)
(*****************************************************************************)

let run ?(seed = 1) (program : 'a talk) (answers : string list) : string =
  let out = Buffer.create 256 in
  let steps = ref 0 in
  (* the answers and the seed left, if the program reached its end: a
     spawned child's end is where its parent carries on *)
  let rec go : 'b. 'b talk -> string list -> seed -> (string list * seed) option =
   fun p answers seed ->
    match p, answers with
    | Done _, _ -> Some (answers, seed)
    | Print (s, k), _ -> Buffer.add_string out s; go k answers seed
    | Random (n, k), _ ->
        let i, seed = random_int 0 (n - 1) seed in
        go (k i) answers seed
    (* the answer echoed, as the paper would show it *)
    | Read_line k, a :: rest -> Buffer.add_string out (a ^ "\n"); go (k a) rest seed
    | Read_key k, a :: rest -> go (k a) rest seed
    | (Read_line _ | Read_key _), [] -> None
    | Spawn (child, k), _ -> (
        match go child answers seed with
        | Some (answers, seed) -> go (k Exited) answers seed
        | None -> None)
    (* a million steps: a program that doesn't end *)
    | Step k, _ ->
        incr steps;
        if !steps > 1_000_000 then None else go (k ()) answers seed
  in
  ignore (go program answers (initial_seed seed));
  Buffer.contents out

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

type machine = {
  program : unit talk;
  (* the parents waiting for their spawned child, the innermost first *)
  parents : (status -> unit talk) list;
  vt : Vt.t;
  tty : Line_discipline.t;
  (* bytes printed but not yet on the screen, at a baud rate *)
  (* newest first, joined when it goes to the screen: a string appended
     to at each print would be copied whole each time, and a BASIC loop
     prints thousands of lines a frame *)
  outbox : string list;
  (* characters a second, None: at once *)
  cps : float option;
  budget : float;
  seed : seed;
  (* the steps taken since the last frame *)
  steps : int;
}

(* bytes put in the outbox, and the outbox's bytes in order *)
let add (s : string) (outbox : string list) : string list = if s = "" then outbox else s :: outbox
let pending (m : machine) : string = String.concat "" (List.rev m.outbox)

(* everything in the outbox on the screen, unless a baud rate
   rations it (then [tick] does it) *)
let flush (m : machine) : machine =
  match m.cps with
  | None -> { m with vt = Vt.feed m.vt (pending m); outbox = [] }
  | Some _ -> m

(* the steps a frame may take: a program computing at length (BASIC's
   10 GOTO 10) runs this much a frame, then the screen is drawn and the
   keyboard read, Control-C included *)
let steps_per_frame = 20_000

(* at a baud rate, the bytes waiting for the line: past this, a program
   printing in a loop waits for them, as write(2) blocks on a tty whose
   output buffer is full *)
let output_buffer = 256
let full (m : machine) : bool = m.cps <> None && List.fold_left (fun n s -> n + String.length s) 0 m.outbox >= output_buffer

(* the program run until it reads, ends, or has taken its steps for
   this frame: its prints into the outbox, its random numbers drawn,
   the tty set to the mode its read needs *)
let rec advance (m : machine) : machine =
  match m.program with
  | Step k when m.steps < steps_per_frame && not (full m) -> advance { m with program = k (); steps = m.steps + 1 }
  | Step _ -> flush { m with tty = Line_discipline.set_mode m.tty Cooked }
  | Print (s, k) -> advance { m with program = k; outbox = add (Line_discipline.output s) m.outbox }
  | Random (n, k) ->
      let i, seed = random_int 0 (n - 1) m.seed in
      advance { m with program = k i; seed }
  | Spawn (child, k) -> advance { m with program = child; parents = k :: m.parents }
  | Done () when m.parents <> [] -> exit_child m Exited
  | Read_key _ -> flush { m with tty = Line_discipline.set_mode m.tty Raw }
  | Read_line _ | Done () -> flush { m with tty = Line_discipline.set_mode m.tty Cooked }

(* the innermost program over: its parent carries on, told how *)
and exit_child (m : machine) (st : status) : machine =
  match m.parents with
  | k :: parents -> advance { m with program = k st; parents }
  | [] -> advance { m with program = Done () }

let start ?baud ~(seed : int) ~(rows : int) ~(cols : int) (program : unit talk) : machine =
  advance
    {
      program;
      parents = [];
      vt = Vt.create ~rows ~cols;
      tty = Line_discipline.create ();
      outbox = [];
      (* a character is 10 bits on the line (a start bit, 8, a stop
         bit); the Model 33 had two stop bits, so its 110 baud were
         exactly 10 characters a second *)
      cps = Option.map (fun b -> float_of_int b /. if b = 110 then 11. else 10.) baud;
      budget = 0.;
      seed = initial_seed seed;
      steps = 0;
    }

(* a key at a time, since a line or a key given to the program can
   change the tty's mode for the next one *)
let input (m : machine) (bytes : string) : machine =
  let one (m : machine) (key : string) : machine =
    let tty, echo, events = Line_discipline.input m.tty key in
    let m = { m with tty; outbox = add echo m.outbox } in
    List.fold_left
      (fun (m : machine) (ev : Line_discipline.event) ->
        match ev, m.program with
        | Line l, Read_line k -> advance { m with program = k l }
        | Key s, Read_key k -> advance { m with program = k s }
        | (Interrupt | End_of_file), (Read_line _ | Read_key _ | Step _) -> exit_child m Interrupted
        (* typed ahead of a question, or after the end: dropped *)
        | _ -> m)
      m events
  in
  flush (List.fold_left one m (Line_discipline.split_keys bytes))

let tick (m : machine) (dt : float) : machine =
  (* a new frame: the steps counted again, a computing program resumed *)
  let m = match m.program with Step _ -> advance { m with steps = 0 } | _ -> { m with steps = 0 } in
  match m.cps with
  | None -> m
  | Some _ when m.outbox = [] -> { m with budget = 0. }
  | Some cps ->
      let budget = m.budget +. (dt *. cps) in
      let text = pending m in
      let n = min (int_of_float budget) (String.length text) in
      let rest = String.sub text n (String.length text - n) in
      { m with vt = Vt.feed m.vt (String.sub text 0 n); outbox = add rest []; budget = budget -. float_of_int n }

let screen (m : machine) = m.vt
let reading (m : machine) = m.outbox = [] && match m.program with Read_line _ | Read_key _ -> true | _ -> false
let finished (m : machine) = m.outbox = [] && match m.program with Done () -> true | _ -> false
