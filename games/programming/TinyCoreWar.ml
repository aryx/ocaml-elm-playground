(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Core War (A. K. Dewdney and D. G. Jones, 1984,
 * made famous by Dewdney's "Computer Recreations" column in Scientific
 * American that May): two programs, written in an assembly language
 * called Redcode, share one circular memory, the core, and take turns
 * running one instruction each. A program that executes a DAT
 * instruction dies; the last one running wins. You do not play Core
 * War, you write the program that plays it -- here, you pick the fight
 * and watch.
 *
 *   1 2 3      a fight: Dwarf against Imp, Mice against Dwarf, Imp
 *              against Mice
 *   space      pause, and on pause, right: one cycle at a time
 *   up/down    faster, slower           r: the fight again
 *
 * The core is drawn cell by cell, in the color of the warrior who wrote
 * there last, and each program's running processes as white squares.
 *
 * Its ancestor is Darwin (Victor Vyssotsky, Robert Morris and Doug
 * McIlroy, at Bell Labs, 1961), the same fight between programs on an
 * IBM 7090; after Dewdney's column came the International Core War
 * Society, its standards of 1986 and 1988 (this game's Redcode is the
 * 1988 one, without the 1994 standard's modifiers), and the "hills"
 * where programs still fight each other by e-mail. (Names and dates
 * from memory, to check.)
 *
 * The machine, MARS (the Memory Array Redcode Simulator), is [step]:
 *
 *   an instruction  OPCODE  A-operand, B-operand      e.g.  MOV 0, 1
 *   an operand      #n  immediate: the number n itself
 *                   $n  direct (or just n): the cell n cells from here
 *                   @n  indirect: from the cell n cells from here, as
 *                       many more as its B-field says
 *                   <n  the same, the B-field decremented first
 *   the opcodes     DAT (die), MOV, ADD, SUB, JMP, JMZ, JMN, DJN, CMP
 *                   (skip the next instruction if equal) and SPL (a
 *                   new process, as a thread)
 *
 * Every address is relative to the instruction and taken modulo the
 * core's size, so a program can be loaded anywhere and cannot tell
 * where it is: the core has no beginning. The tournaments' core has
 * 8000 cells; this one 800, so that each can be seen.
 *
 * The three warriors are the classics, as they were printed:
 *
 *   Imp (Dewdney)   MOV 0, 1        copies itself one cell ahead, then
 *                                   runs the copy: a program that walks
 *   Dwarf (Dewdney) drops a DAT "bomb" every 4 cells, forever; a
 *                   program walking into one dies
 *   Mice (Chip Wendell, winner of the first tournament, 1986) copies
 *                   itself far away and starts the copy with SPL: more
 *                   and more mice, which a bomb can no longer all kill
 *
 * What it uses: nothing but the Playground and Scene2d (the keys
 * pressed): the assembler and the machine are here, a page each.
 *
 * Exercises: writing your own warrior (an editor in the game: the gui
 * library's text field, then [assemble]); the 1994 standard's
 * modifiers (MOV.A, MOV.I ...) and its SEQ/SNE/SLT; a tournament (every
 * warrior against every other, 100 fights each from random places, the
 * scores in a table); the famous strategies: the stone (a faster
 * bomber), the paper (a faster replicator, like Mice), the scissors (a
 * scanner looking for the enemy before bombing it), and why each beats
 * the next.
 *)
open Playground

(*****************************************************************************)
(* Redcode *)
(*****************************************************************************)

type opcode = DAT | MOV | ADD | SUB | JMP | JMZ | JMN | DJN | CMP | SPL
type mode = Immediate | Direct | Indirect | Predecrement
type instr = { op : opcode; amode : mode; a : int; bmode : mode; b : int }

let dat0 = { op = DAT; amode = Immediate; a = 0; bmode = Immediate; b = 0 }

let opcodes =
  [ ("DAT", DAT); ("MOV", MOV); ("ADD", ADD); ("SUB", SUB); ("JMP", JMP); ("JMZ", JMZ); ("JMN", JMN);
    ("DJN", DJN); ("CMP", CMP); ("SPL", SPL) ]

(* [assemble text]: the instructions of a program, one a line, with ';'
 * starting a comment and an optional label before the opcode; an
 * operand is a number or a label (the distance to its line). With one
 * operand, DAT's is the B-field, the others' the A-field. *)
let assemble (text : string) : instr list =
  let lines =
    String.split_on_char '\n' text
    |> List.map (fun l -> match String.index_opt l ';' with Some i -> String.sub l 0 i | None -> l)
    |> List.map (fun l -> String.map (fun c -> if c = ',' || c = '\t' then ' ' else c) l)
    |> List.map (fun l -> String.split_on_char ' ' l |> List.filter (( <> ) ""))
    |> List.filter (( <> ) [])
  in
  (* the labels: a first word that is not an opcode *)
  let is_op w = List.mem_assoc (String.uppercase_ascii w) opcodes in
  let labels = List.mapi (fun i words -> match words with w :: _ when not (is_op w) -> [ (w, i) ] | _ -> []) lines in
  let labels = List.concat labels in
  let operand i s =
    let mode, rest =
      match s.[0] with
      | '#' -> (Immediate, String.sub s 1 (String.length s - 1))
      | '$' -> (Direct, String.sub s 1 (String.length s - 1))
      | '@' -> (Indirect, String.sub s 1 (String.length s - 1))
      | '<' -> (Predecrement, String.sub s 1 (String.length s - 1))
      | _ -> (Direct, s)
    in
    let value =
      match int_of_string_opt rest with
      | Some n -> n
      | None -> ( match List.assoc_opt rest labels with Some line -> line - i | None -> failwith ("no label " ^ rest))
    in
    (mode, value)
  in
  List.mapi
    (fun i words ->
      let words = match words with w :: rest when not (is_op w) -> rest | ws -> ws in
      match words with
      | op :: args -> (
          let op = List.assoc (String.uppercase_ascii op) opcodes in
          match (op, List.map (operand i) args) with
          | DAT, [ (bm, b) ] -> { op; amode = Immediate; a = 0; bmode = bm; b }
          | _, [ (am, a) ] -> { op; amode = am; a; bmode = Immediate; b = 0 }
          | _, [ (am, a); (bm, b) ] -> { op; amode = am; a; bmode = bm; b }
          | _ -> failwith "an instruction takes one or two operands")
      | [] -> dat0)
    lines

(*****************************************************************************)
(* The warriors *)
(*****************************************************************************)

(* a warrior's program, and the line it starts at (Redcode's "END
 * start", later ORG) *)
type program = { name : string; code : instr list; entry : int }

let imp = { name = "IMP"; code = assemble "MOV 0, 1"; entry = 0 }

let dwarf =
  { name = "DWARF";
    entry = 0;
    code =
    assemble
      "        ADD #4, bomb   ; the next place to bomb, 4 cells further\n\
      \        MOV bomb, @bomb ; the bomb, to where it points\n\
      \        JMP -2\n\
       bomb    DAT #0" }

let mice =
  { name = "MICE";
    entry = 1 (* start *);
    code =
    assemble
      "ptr     DAT #0\n\
       start   MOV #12, ptr    ; 12 cells to copy\n\
       loop    MOV @ptr, <copy ; one, from the end\n\
      \        DJN loop, ptr\n\
      \        SPL @copy, 0    ; the copy runs too\n\
      \        ADD #653, copy  ; the next copy, far away\n\
      \        JMZ start, ptr\n\
       copy    DAT 833" }

let fights = [| (dwarf, imp); (mice, dwarf); (imp, mice) |]

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

let size = 800
let wrap n = ((n mod size) + size) mod size

type warrior = { name : string; code : instr list; queue : int list (* the processes' next instructions *) }

type mars = {
  core : instr array;
  owner : int array; (* who wrote each cell last: 0, 1 or 2 *)
  warriors : warrior array;
  turn : int; (* whose instruction is next: 0 or 1 *)
  cycles : int;
}

let max_processes = 64
let max_cycles = 8000

(* the two warriors loaded half the core apart *)
let load ((p1, p2) : program * program) : mars =
  let core = Array.make size dat0 and owner = Array.make size 0 in
  let put at w (p : program) =
    List.iteri (fun i ins -> core.(wrap (at + i)) <- ins; owner.(wrap (at + i)) <- w) p.code;
    { name = p.name; code = p.code; queue = [ at + p.entry ] }
  in
  let w1 = put 0 1 p1 in
  let w2 = put (size / 2) 2 p2 in
  { core; owner; turn = 0; cycles = 0; warriors = [| w1; w2 |] }

(* an operand's address: for an immediate one, the instruction itself *)
let address (m : mars) (pc : int) (mode : mode) (field : int) : int =
  match mode with
  | Immediate -> pc
  | Direct -> wrap (pc + field)
  | Indirect -> let at = wrap (pc + field) in wrap (at + m.core.(at).b)
  | Predecrement ->
      let at = wrap (pc + field) in
      m.core.(at) <- { (m.core.(at)) with b = m.core.(at).b - 1 };
      wrap (at + m.core.(at).b)

(* [step m]: one instruction of the warrior whose turn it is; the core
 * changes in place, the rest is a new value *)
let step (m : mars) : mars =
  let me = m.turn in
  let w = m.warriors.(me) in
  match w.queue with
  | [] -> { m with turn = 1 - me }
  | pc :: rest ->
      let ins = m.core.(pc) in
      let aptr = address m pc ins.amode ins.a in
      let ra = m.core.(aptr) in
      let bptr = address m pc ins.bmode ins.b in
      let write at i = m.core.(at) <- i; m.owner.(at) <- me + 1 in
      let next = wrap (pc + 1) in
      let continue_at =
        match ins.op with
        | DAT -> []
        | MOV ->
            write bptr (if ins.amode = Immediate then { (m.core.(bptr)) with b = ins.a } else ra);
            [ next ]
        | ADD | SUB ->
            let f x y = if ins.op = ADD then x + y else x - y in
            let d = m.core.(bptr) in
            write bptr
              (if ins.amode = Immediate then { d with b = f d.b ins.a } else { d with a = f d.a ra.a; b = f d.b ra.b });
            [ next ]
        | JMP -> [ aptr ]
        | JMZ -> [ (if m.core.(bptr).b = 0 then aptr else next) ]
        | JMN -> [ (if m.core.(bptr).b <> 0 then aptr else next) ]
        | DJN ->
            let d = m.core.(bptr) in
            write bptr { d with b = d.b - 1 };
            [ (if d.b - 1 <> 0 then aptr else next) ]
        | CMP ->
            let equal = if ins.amode = Immediate then ins.a = m.core.(bptr).b else ra = m.core.(bptr) in
            [ (if equal then wrap (pc + 2) else next) ]
        | SPL -> if List.length rest + 2 <= max_processes then [ next; aptr ] else [ next ]
      in
      let warriors = Array.copy m.warriors in
      warriors.(me) <- { w with queue = rest @ continue_at };
      { m with warriors; turn = 1 - me; cycles = (if me = 1 then m.cycles + 1 else m.cycles) }

(* the winner (0 or 1) once the other has no process left, a draw after
 * [max_cycles] *)
type result = Running | Wins of int | Draw

let result (m : mars) : result =
  match (m.warriors.(0).queue, m.warriors.(1).queue) with
  | [], [] -> Draw
  | [], _ -> Wins 1
  | _, [] -> Wins 0
  | _ -> if m.cycles >= max_cycles then Draw else Running

(* [run m n]: [n] cycles, both warriors' turns, unless the fight ends *)
let rec run (m : mars) (n : int) : mars =
  if n = 0 || result m <> Running then m else run (step (step m)) (n - 1)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = { fight : int; mars : mars; speed : int; paused : bool }
type model = play Scene2d.t

let start fight = { fight; mars = load fights.(fight); speed = 4; paused = false }
let initial_model : model = Scene2d.start (start 0)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed f = Scene2d.pressed f s in
  let key name = pressed (fun k -> Set_.mem name k.keys) in
  let p = s.scene in
  let p =
    if key "1" then start 0
    else if key "2" then start 1
    else if key "3" then start 2
    else if key "r" then start p.fight
    else if pressed (fun k -> k.kspace) then { p with paused = not p.paused }
    else if pressed (fun k -> k.kup) then { p with speed = min 64 (p.speed * 2) }
    else if pressed (fun k -> k.kdown) then { p with speed = max 1 (p.speed / 2) }
    else if p.paused && pressed (fun k -> k.kright) then { p with mars = run p.mars 1 }
    else if p.paused then p
    else { p with mars = run p.mars p.speed }
  in
  { s with scene = p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let colors = [| rgb 40 40 50; rgb 60 200 90; rgb 230 80 70 |]
let cell = 22.
let columns = 40
let cell_xy i = (-429. +. (cell *. float_of_int (i mod columns)), 440. -. (cell *. float_of_int (i / columns)))

let view (_computer : computer) (s : model) : shape list =
  let p = s.scene and m = s.scene.mars in
  let cells =
    List.init size (fun i ->
        let x, y = cell_xy i in
        move x y (square colors.(m.owner.(i)) (cell -. 3.)))
  in
  let bombs =
    (* a DAT written by a warrior: a dark dot, what kills *)
    List.init size Fun.id
    |> List.filter (fun i -> m.core.(i).op = DAT && m.owner.(i) <> 0)
    |> List.map (fun i -> let x, y = cell_xy i in move x y (square (rgb 20 20 20) 7.))
  in
  let processes =
    Array.to_list m.warriors
    |> List.concat_map (fun w -> List.map (fun pc -> let x, y = cell_xy pc in move x y (square white 9.)) w.queue)
  in
  let listing i =
    let w = m.warriors.(i) in
    let x = if i = 0 then -300. else 150. in
    let title = Printf.sprintf "%s  %d process%s" w.name (List.length w.queue) (if List.length w.queue = 1 then "" else "es") in
    move x (-40.) (words colors.(i + 1) title)
    :: List.mapi
         (fun j ins ->
           let name = fst (List.find (fun (_, o) -> o = ins.op) opcodes) in
           let operand mode v =
             (match mode with Immediate -> "#" | Direct -> "" | Indirect -> "@" | Predecrement -> "<") ^ string_of_int v
           in
           move x (-80. -. (26. *. float_of_int j))
             (scale 0.8 (words white (Printf.sprintf "%s %s, %s" name (operand ins.amode ins.a) (operand ins.bmode ins.b)))))
         w.code
  in
  let status =
    match result m with
    | Running -> Printf.sprintf "cycle %d    %d cycles a frame%s" m.cycles p.speed (if p.paused then "    PAUSED" else "")
    | Wins w -> Printf.sprintf "%s WINS, cycle %d    r: again" m.warriors.(w).name m.cycles
    | Draw -> Printf.sprintf "A DRAW after %d cycles    r: again" m.cycles
  in
  [ rectangle black 1000. 1000. ] @ cells @ bombs @ processes @ listing 0 @ listing 1
  @ [ move 0. (-400.) (words white status);
      move 0. (-440.) (scale 0.8 (words gray "1 2 3: a fight    space: pause    up/down: speed")) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = game view update initial_model
let main = Playground_platform.run_app app
