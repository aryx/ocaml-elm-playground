(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Squeak (Dan Ingalls, Ted Kaehler, John Maloney,
 * Scott Wallace, Alan Kay, Apple then Disney, 1996; "Back to the
 * Future", OOPSLA 1997): Smalltalk-80 made live again, in colour, its
 * environment Morphic (plan_tiny_squeak.md, notes_squeak.md).
 *
 *   the left button (Smalltalk's red one) picks up what does not want
 *   the mouse -- a shape, a window by its title -- and puts it down;
 *   in a text it puts the caret and selects, in a list it picks. On
 *   the grey world itself: the world's menu, new morphs and the tools.
 *   The right button (the yellow one) in a text: do it, print it,
 *   inspect it, accept.
 *   The middle button, or Control and the left one (the blue one): a
 *   halo around the morph -- delete (pink), pick up (black), duplicate
 *   (green), resize (yellow), inspect (blue), its viewer (turquoise).
 *   Etoys: the car drives by its script, two phrases of tiles done
 *   each cycle. Click a number and type another; the script's button
 *   pauses it; drag a phrase out of it, or out of the car's viewer
 *   into it, or onto the world for a new script.
 *   Control-C interrupts what runs too long.
 *   Flag: kernel=mini boots MiniMorphic instead, Morphic in one file
 *   of Smalltalk, to read before the real one: fifty squares bouncing
 *   in black and white, the left button picking one up.
 *
 * TinySmalltalk80 is the Blue Book's system, and its windows are
 * OCaml. Here everything on the screen is Smalltalk: the windows, the
 * Browser, the menus, the text you type in, the atoms that bounce are
 * morphs, drawn by Smalltalk with BitBlt on a Form of 32 bits, the
 * Display. The Browser opens on EllipseMorph>>drawOn:, what draws the
 * bouncing atoms: change it, accept (the right button's menu), and
 * they are drawn the new way at once, while they bounce. Do the same
 * to BorderedMorph>>drawOn: and it is the windows, the Browser's own
 * among them (the world's menu: restore display).
 *
 * The trick of this app is how little of it there is: this file is the
 * whole host. It gives Smalltalk the mouse and the keys, sends the
 * world doOneCycle once a frame, and shows the Display. Squeak's
 * lesson, which is its title's: a Smalltalk written in itself, where
 * what the machine must provide keeps shrinking (Squeak's own virtual
 * machine is written in Smalltalk and translated to C; here it stays
 * OCaml: plan_tiny_squeak.md's last phase).
 *
 * The Smalltalk is languages/smalltalk, TinySmalltalk80's, booted from
 * a second kernel (St_kernel.squeak: the Blue Book's files, then
 * kernel/squeak/'s -- closures, Color and Forms with a depth, a font
 * drawn from Hershey's strokes, Morphic, the tools).
 *
 * What it uses: the Playground, graphics_rgba (the Display's pixels as
 * a picture) and languages/smalltalk. Not libs/gui, not graphics/font:
 * the widgets and the text are Smalltalk's.
 *
 * What it deliberately does not do (exercises): the image saved and
 * loaded (St_image.mli does it: the screen menu's save, as
 * TinySmalltalk80's); a debugger as a morph (an error is said in the
 * Transcript, and the world goes on); scroll bars; the mouse wheel;
 * copy and paste (Playground_platform.clipboard); Squeak's painting
 * tools (the car is drawn by a method, not painted); Etoys' tests and
 * variables.
 *)
open Playground
module M = St_memory
module I = St_interp

(*****************************************************************************)
(* The host *)
(*****************************************************************************)

(* the Smalltalk screen, the Display: 800 by 600 pixels, y downwards;
 * scaled to the window *)
let screen_w = 800
let screen_h = 600

(* what the virtual machine reads: the mouse (x, y, the buttons: 4
 * red, 2 yellow, 1 blue) and the characters typed, waiting *)
type io = { mutable mouse : int * int * int; keys : int Queue.t }

let host (io : io) : I.host =
  {
    St_boot.quiet_host with
    mouse = (fun () -> io.mouse);
    keyboard = (fun () -> Queue.take_opt io.keys);
  }

(* the first things Smalltalk is told: a Display in colour, a world on
 * it, and what is on the screen when the program starts. Each is
 * evaluated alone: a method has room for 64 literals. *)
let startup =
  [
    {st|Smalltalk at: #Display put: (Form extent: 800 @ 600 depth: 32).
Smalltalk at: #World put: (PasteUpMorph on: Display)|st};
    {st|| browser |
browser := Browser open.
browser window position: 8 @ 8; extent: 580 @ 330.
browser categoryList selectItem: 'Morphic-Basic'.
browser classList selectItem: #EllipseMorph.
browser protocolList selectItem: 'drawing'.
browser selectorList selectItem: #drawOn:|st};
    {st|| workspace return |
return := String with: (Character value: 13).
workspace := Workspace open.
workspace position: 8 @ 346; extent: 400 @ 246.
workspace submorphs first contents:
	'"Select a line, the right button: print it"', return,
	'3 + 4 * 2', return,
	'100 factorial printString size', return,
	'(1 to: 10) inject: 0 into: [:a :b | a + b]', return,
	'World color: (Color r: 3/5 g: 4/5 b: 3/5)', return,
	'World hand attachMorph: EllipseMorph new', return,
	'Transcript show: ''Hello, Squeak''; cr', return,
	'World inspect'|st};
    {st|Transcript open position: 416 @ 446; extent: 376 @ 146.
Transcript show: 'TinySqueak: everything here is a morph.'; cr.
World addMorph: (BouncingAtomsMorph new position: 594 @ 36; yourself).
World addMorph: (PartsBinMorph new position: 594 @ 226; yourself)|st};
    (* the first Etoy: a car, and its script already ticking *)
    {st|| car script |
car := CarMorph new.
World addMorph: car.
car position: 610 @ 345.
script := ScriptEditorMorph on: car.
World addMorph: script.
script position: 416 @ 350.
script acceptDroppedMorph: (PhraseTileMorph target: car selector: #forward: label: 'forward by' argument: 4).
script acceptDroppedMorph: (PhraseTileMorph target: car selector: #turn: label: 'turn by' argument: 5).
script toggle|st};
  ]

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type model = {
  (* the system, booted at the second frame: seconds of work in a
   * browser, so not at the program's top level (tinybox links it),
   * and after a first frame that says so *)
  squeak : (I.vm * io) option;
  shown : bool; (* that first frame was *)
  (* the world's cycle, when it did not end in its frame's budget: run
   * on, the next frames *)
  cycle : I.process option;
  (* the Display's picture, and St_bitblt.changes when it was taken *)
  picture : (int * Rgba_image.t) option;
  held : string Set_.t; (* the keys down the frame before *)
}

let initial : model = { squeak = None; shown = false; cycle = None; picture = None; held = Set_.empty }

(* a global's value *)
let global (vm : I.vm) (name : string) : M.oop option =
  Option.map (fun a -> M.fetch (I.memory vm) a 1) (St_class.global (I.memory vm) name)

(* MiniMorphic's start, the flag kernel=mini: Morphic in one file
 * (kernel/morphic/MiniMorphic.st), fifty squares bouncing on the Blue
 * Book's Display, black and white; the left button picks one up *)
let startup_mini = [ "Smalltalk at: #World put: (WorldMorph bouncingAtoms: 50)" ]

let boot ~(mini : bool) : I.vm * io =
  let io = { mouse = (0, 0, 0); keys = Queue.create () } in
  let vm = St_boot.boot ~host:(host io) ~kernel:(if mini then St_kernel.mini_morphic else St_kernel.squeak) () in
  List.iter
    (fun text -> match I.evaluate vm ~budget:200_000_000 text with Ok _ -> () | Error e -> prerr_endline ("TinySqueak: " ^ e))
    (if mini then startup_mini else startup);
  (vm, io)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* bytecodes a frame: a cycle of the world is a few tens of thousands;
 * a long computation is run on over the next frames *)
let budget = 2_000_000

let scale_of (computer : computer) : float =
  Float.min (computer.screen.width /. float_of_int screen_w) (computer.screen.height /. float_of_int screen_h)

(* the mouse, as Smalltalk reads it: the Display's point under it, and
 * its buttons by their Smalltalk-80 colours *)
let mouse (computer : computer) : int * int * int =
  let s = scale_of computer in
  let x = int_of_float ((computer.mouse.mx /. s) +. (float_of_int screen_w /. 2.))
  and y = int_of_float ((float_of_int screen_h /. 2.) -. (computer.mouse.my /. s)) in
  let control = Set_.mem "Control" computer.keyboard.keys in
  let red = computer.mouse.mdown && not control
  and yellow = computer.mouse.mrdown
  and blue = computer.mouse.mmdown || (computer.mouse.mdown && control) in
  (x, y, (if red then 4 else 0) lor (if yellow then 2 else 0) lor if blue then 1 else 0)

(* the keys that are not characters, as Squeak's codes *)
let key_code : string -> int option = function
  | "Enter" -> Some 13
  | "Tab" -> Some 9
  | "Backspace" -> Some 8
  | "ArrowLeft" -> Some 28
  | "ArrowRight" -> Some 29
  | "ArrowUp" -> Some 30
  | "ArrowDown" -> Some 31
  | _ -> None

(* why the cycle stopped, said by Smalltalk itself, in its Transcript;
 * by the host if it cannot (MiniMorphic's kernel has no window for it) *)
let report (vm : I.vm) (why : string) : unit =
  let said =
    match global vm "Transcript" with
    | Some transcript -> Result.is_ok (I.call vm ~budget transcript "showError:" [ M.new_string (I.memory vm) why ])
    | None -> false
  in
  if not said then prerr_endline ("TinySqueak: " ^ why)

(* the world's cycle: a new one if the last one ended, run for a
 * frame's budget. An error ends it, and the next frame starts
 * another: the world goes on (HandMorph>>processEvents). *)
let run_cycle (vm : I.vm) (m : model) ~(interrupt : bool) : model =
  let p = match m.cycle with Some p -> Some p | None -> Option.map (fun w -> I.spawn vm w "doOneCycle" []) (global vm "World") in
  match p with
  | None -> m
  | Some p -> (
      if interrupt && m.cycle <> None then I.suspend p "Interrupted";
      I.run vm p ~budget;
      match p.state with
      | I.Runnable -> { m with cycle = Some p }
      | I.Suspended why ->
          I.terminate vm p;
          report vm why;
          { m with cycle = None }
      | I.Finished _ | I.Terminated -> { m with cycle = None })

(* the Display's pixels as a picture, when BitBlt drew since the last *)
let take_picture (vm : I.vm) (m : model) : model =
  let changes = St_bitblt.changes () in
  match m.picture with
  | Some (c, _) when c = changes -> m
  | _ -> (
      match Option.bind (global vm "Display") (St_colorblt.rgba (I.memory vm)) with
      | Some (w, h, bytes) ->
          let img = Rgba_image.create ~width:w ~height:h in
          for i = 0 to Bytes.length bytes - 1 do
            Bigarray.Array1.unsafe_set img.rgba i (Char.code (Bytes.unsafe_get bytes i))
          done;
          { m with picture = Some (changes, img) }
      | None -> m)

let update (computer : computer) (m : model) : model =
  let mini = List.assoc_opt "kernel" computer.flags = Some "mini" in
  let m = match m.squeak with None when m.shown -> { m with squeak = Some (boot ~mini) } | _ -> m in
  match m.squeak with
  | None -> { m with shown = true }
  | Some (vm, io) ->
      let keys = computer.keyboard.keys in
      let went_down = Set_.elements (Set_.diff keys m.held) in
      let control = Set_.mem "Control" keys in
      io.mouse <- mouse computer;
      if not control then begin
        String.iter (fun c -> if Char.code c >= 32 && Char.code c < 127 then Queue.add (Char.code c) io.keys) computer.keyboard.typed;
        List.iter (fun k -> Option.iter (fun c -> Queue.add c io.keys) (key_code k)) went_down
      end;
      let m = run_cycle vm m ~interrupt:(control && List.mem "c" went_down) in
      { (take_picture vm m) with held = keys }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the Display, and nothing else: all there is to see is Smalltalk's *)
let view (computer : computer) (m : model) : shape list =
  let s = scale_of computer in
  rectangle black computer.screen.width computer.screen.height
  ::
  (match m.picture with
  | Some (_, img) -> [ bitmap (float_of_int screen_w *. s) (float_of_int screen_h *. s) img ]
  | None -> [ words white "Squeak is booting..." ])

let app = game view update initial
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
