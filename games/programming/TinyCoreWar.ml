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
 * War, you write the program that plays it.
 *
 * So the screen is an editor and a machine. Each warrior's Redcode is
 * text you type into (a menu loads one of the classics to start from);
 * Fight assembles both and loads them into the core, and the fight
 * runs, drawn cell by cell in the color of the warrior who wrote there
 * last, the running processes as white squares. A mistake in a
 * program is shown under it, with its line, and there is no fight
 * until both assemble. As in the tournaments, a program is fixed once
 * loaded: an edit counts from the next Fight.
 *
 *   Fight      assemble, load, run       Pause, Step: one cycle
 *   speed      how many cycles a frame
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
 *   the rest        a label before an opcode; an operand may be one (the
 *                   distance to its line); ';' starts a comment; END,
 *                   or END and a label, where the program starts
 *
 * Every address is relative to the instruction and taken modulo the
 * core's size, so a program can be loaded anywhere and cannot tell
 * where it is: the core has no beginning. The tournaments' core has
 * 8000 cells; this one 800, so that each can be seen.
 *
 * The classics in the menu, as they were printed:
 *
 *   Imp (Dewdney)   MOV 0, 1        copies itself one cell ahead, then
 *                                   runs the copy: a program that walks
 *   Dwarf (Dewdney) drops a DAT "bomb" every 4 cells, forever; a
 *                   program walking into one dies
 *   Mice (Chip Wendell, winner of the first tournament, 1986) copies
 *                   itself far away and starts the copy with SPL: more
 *                   and more mice, which a bomb can no longer all kill
 *
 * The Imp walking through the Dwarf turns it into an Imp (a draw: two
 * imps cannot kill each other); the Mice outbreed the Dwarf.
 *
 * What it uses: the Playground's Gui (the text areas -- gui/Text_edit,
 * a piece table with its own undo -- the menus, the buttons, the
 * slider); the assembler and the machine are here, a page each.
 *
 * Exercises: saving a warrior to a file and opening one (the office
 * apps' File_menu, over the store); the 1994 standard's modifiers
 * (MOV.A, MOV.I ...) and its SEQ/SNE/SLT; a tournament (every warrior
 * against every other, 100 fights each from random places, the scores
 * in a table); the famous strategies, written in the editor: the stone
 * (a faster bomber), the paper (a faster replicator, like Mice), the
 * scissors (a scanner looking for the enemy before bombing it), and why
 * each beats the next.
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

(* a warrior's program, and the instruction it starts at *)
type program = { code : instr list; entry : int }

exception Wrong of int * string

(* [assemble text]: the program, or the first mistake and its line
 * (counted from 1, as an editor shows them). One instruction a line,
 * with an optional label before the opcode; ';' starts a comment; an
 * operand is a number or a label (the distance to its line); with one
 * operand, DAT's is the B-field, the others' the A-field; END, alone
 * or with a label, says where the program starts. *)
let assemble (text : string) : (program, int * string) result =
  let lines =
    String.split_on_char '\n' text
    |> List.mapi (fun i l -> (i + 1, l))
    |> List.map (fun (n, l) -> (n, match String.index_opt l ';' with Some i -> String.sub l 0 i | None -> l))
    |> List.map (fun (n, l) -> (n, String.map (fun c -> if c = ',' || c = '\t' || c = '\r' then ' ' else c) l))
    |> List.map (fun (n, l) -> (n, String.split_on_char ' ' l |> List.filter (( <> ) "")))
    |> List.filter (fun (_, words) -> words <> [])
  in
  let is_op w = List.mem_assoc (String.uppercase_ascii w) opcodes in
  let is_end w = String.uppercase_ascii w = "END" in
  (* the instructions' lines, up to END, and END's label and line if
   * any *)
  let rec split acc = function
    | [] -> (List.rev acc, None)
    | (n, w :: rest) :: _ when is_end w -> (List.rev acc, match rest with l :: _ -> Some (n, l) | [] -> None)
    | line :: tl -> split (line :: acc) tl
  in
  let code_lines, start = split [] lines in
  let labels =
    List.concat (List.mapi (fun i (_, words) -> match words with w :: _ when not (is_op w) -> [ (w, i) ] | _ -> []) code_lines)
  in
  let where n label = match List.assoc_opt label labels with Some i -> i | None -> raise (Wrong (n, "no label " ^ label)) in
  let operand n i s =
    let mode, rest =
      match s.[0] with
      | '#' -> (Immediate, String.sub s 1 (String.length s - 1))
      | '$' -> (Direct, String.sub s 1 (String.length s - 1))
      | '@' -> (Indirect, String.sub s 1 (String.length s - 1))
      | '<' -> (Predecrement, String.sub s 1 (String.length s - 1))
      | _ -> (Direct, s)
    in
    if rest = "" then raise (Wrong (n, "an operand with no value"));
    (mode, match int_of_string_opt rest with Some v -> v | None -> where n rest - i)
  in
  try
    let code =
      List.mapi
        (fun i (n, words) ->
          let words = match words with w :: rest when not (is_op w) -> rest | ws -> ws in
          match words with
          | [] -> raise (Wrong (n, "a label with no instruction"))
          | op :: args -> (
              match List.assoc_opt (String.uppercase_ascii op) opcodes with
              | None -> raise (Wrong (n, "no opcode " ^ op))
              | Some op -> (
                  match (op, List.map (operand n i) args) with
                  | DAT, [ (bm, b) ] -> { op; amode = Immediate; a = 0; bmode = bm; b }
                  | _, [ (am, a) ] -> { op; amode = am; a; bmode = Immediate; b = 0 }
                  | _, [ (am, a); (bm, b) ] -> { op; amode = am; a; bmode = bm; b }
                  | _ -> raise (Wrong (n, "one or two operands")))))
        code_lines
    in
    if code = [] then raise (Wrong (1, "no instruction"));
    let entry = match start with Some (n, l) -> where n l | None -> 0 in
    Ok { code; entry }
  with Wrong (n, msg) -> Error (n, msg)

(*****************************************************************************)
(* The classics *)
(*****************************************************************************)

let classics =
  [ ("Imp", "; Dewdney: copy yourself one cell\n\
             ; ahead, and run the copy\n\
             MOV 0, 1\n");
    ("Dwarf",
     "; Dewdney: a bomb every 4 cells\n\
      \        ADD #4, bomb    ; where next\n\
      \        MOV bomb, @bomb ; drop it\n\
      \        JMP -2\n\
       bomb    DAT #0\n");
    ("Mice",
     "; Chip Wendell, 1986: copy yourself\n\
      ; and run the copy too\n\
      ptr     DAT #0\n\
      start   MOV #12, ptr    ; 12 to copy\n\
      loop    MOV @ptr, <copy ; copy one\n\
      \        DJN loop, ptr\n\
      \        SPL @copy, 0    ; run it too\n\
      \        ADD #653, copy  ; far away\n\
      \        JMZ start, ptr\n\
      copy    DAT 833\n\
      \        END start\n") ]

let classic name = Result.get_ok (assemble (List.assoc name classics))
let imp = classic "Imp"
let dwarf = classic "Dwarf"
let mice = classic "Mice"

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

let size = 800
let wrap n = ((n mod size) + size) mod size

(* a warrior loaded: its processes' next instructions, in turn *)
type warrior = { queue : int list }

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
    { queue = [ wrap (at + p.entry) ] }
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
      warriors.(me) <- { queue = rest @ continue_at };
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

(* for each side, the classic last chosen in its menu and its text; the
 * fight, with the names it was loaded under *)
type model = {
  chosen : int array;
  texts : Text_edit.t array;
  mars : mars;
  names : string array;
  running : bool;
  speed : number;
}

let names = List.map fst classics
let text_of i = Text_edit.of_string (snd (List.nth classics i))

(* Dwarf against Imp, running *)
let initial_model : model =
  { chosen = [| 1; 0 |]; texts = [| text_of 1; text_of 0 |]; mars = load (dwarf, imp);
    names = [| "Dwarf"; "Imp" |]; running = true; speed = 4. }

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

let box x y w h : Widget.box = { x; y; w; h }
let side_x i = if i = 0 then -250. else 250.
let colors = [| rgb 40 40 50; rgb 60 200 90; rgb 230 80 70 |]
let cell = 16.
let columns = 40
let cell_xy i = (-312. +. (cell *. float_of_int (i mod columns)), 460. -. (cell *. float_of_int (i / columns)))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  (* the two warriors: a menu of the classics over each text *)
  let chosen = Array.copy m.chosen and texts = Array.copy m.texts in
  for i = 0 to 1 do
    let c = Gui.menu_in computer (box (side_x i +. 110.) 100. 220. 30.) names m.chosen.(i) in
    if c <> m.chosen.(i) then (chosen.(i) <- c; texts.(i) <- text_of c);
    texts.(i) <- Gui.text_area_in computer (box (side_x i) (-120.) 460. 360.) texts.(i)
  done;
  let programs = Array.map (fun t -> assemble (Text_edit.to_string t)) texts in
  let ready = Array.for_all Result.is_ok programs in
  let fight = Gui.button_in ~enabled:ready computer (box (-420.) (-385.) 110. 36.) "Fight" in
  let pause = Gui.button_in computer (box (-295.) (-385.) 110. 36.) (if m.running then "Pause" else "Run") in
  let one = Gui.button_in ~enabled:(not m.running) computer (box (-170.) (-385.) 110. 36.) "Step" in
  let speed = Gui.slider_in computer (box 60. (-385.) 200. 36.) ~from:1. ~to_:32. m.speed in
  let m = { m with chosen; texts; speed } in
  if fight then
    let p i = Result.get_ok programs.(i) in
    let name i =
      let t = Text_edit.to_string texts.(i) and c = List.nth classics chosen.(i) in
      if t = snd c then fst c else fst c ^ ", edited"
    in
    { m with mars = load (p 0, p 1); names = [| name 0; name 1 |]; running = true }
  else if pause then { m with running = not m.running }
  else if one then { m with mars = run m.mars 1 }
  else if m.running then { m with mars = run m.mars (int_of_float speed) }
  else m

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (_computer : computer) (m : model) : shape list =
  let core = m.mars in
  let cells =
    List.init size (fun i ->
        let x, y = cell_xy i in
        move x y (square colors.(core.owner.(i)) (cell -. 2.)))
  in
  let bombs =
    (* a DAT written by a warrior: a dark dot, what kills *)
    List.init size Fun.id
    |> List.filter (fun i -> core.core.(i).op = DAT && core.owner.(i) <> 0)
    |> List.map (fun i -> let x, y = cell_xy i in move x y (square (rgb 20 20 20) 5.))
  in
  let processes =
    Array.to_list core.warriors
    |> List.concat_map (fun (w : warrior) -> List.map (fun pc -> let x, y = cell_xy pc in move x y (square white 7.)) w.queue)
  in
  (* each side's title, in its color, and its program's mistake or its
   * processes *)
  let side i =
    let programs = assemble (Text_edit.to_string m.texts.(i)) in
    let n = List.length core.warriors.(i).queue in
    let below =
      match programs with
      | Error (line, msg) -> (rgb 250 120 100, Printf.sprintf "line %d: %s" line msg)
      | Ok _ -> (gray, Printf.sprintf "%s: %d process%s" m.names.(i) n (if n = 1 then "" else "es"))
    in
    [ move (side_x i -. 150.) 100. (words colors.(i + 1) (Printf.sprintf "WARRIOR %d" (i + 1)));
      move (side_x i) (-322.) (words (fst below) (snd below)) ]
  in
  let status =
    match result core with
    | Running -> Printf.sprintf "cycle %d, %d a frame%s" core.cycles (int_of_float m.speed) (if m.running then "" else ", paused")
    | Wins w -> Printf.sprintf "%s WINS, cycle %d" (String.uppercase_ascii m.names.(w)) core.cycles
    | Draw -> Printf.sprintf "A DRAW, cycle %d" core.cycles
  in
  (* the widgets last: an open menu over everything *)
  let gui = Gui.draw () in
  [ rectangle black 1000. 1000. ] @ cells @ bombs @ processes @ side 0 @ side 1
  @ [ move 330. (-385.) (words white status) ]
  @ gui

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = game view update initial_model
let main = Playground_platform.run_app app
