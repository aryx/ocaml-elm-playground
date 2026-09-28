(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyPostScript: a page described as a program
 * (John Warnock and Chuck Geschke, Adobe, 1984; the Apple LaserWriter,
 * 1985, the first printer to run it).
 *
 * A program on the left, the page it describes on the right. The page
 * is not a picture sent to the printer but a program the printer runs:
 * Run, and it is drawn; Print slowly, and it is drawn an operator at a
 * time, as a 1985 LaserWriter's 12 MHz 68000 took its minute over a
 * page; Step, one object at a time, the stack below showing what each
 * left there. Next program loads the next one from the disk
 * (Ps_disk), each a lesson of the Blue Book's kind:
 *
 *   - tree: a procedure calling itself, its arguments on the stack,
 *     gsave and grestore to come back to where it was;
 *   - star: translate and rotate move the axes, not the drawing -- the
 *     current transformation matrix (Ps_graphics.mli);
 *   - rosette: text is shapes too, turned and shaded like any path;
 *   - pie: arrays, and arc (a circle made of Bezier curves);
 *   - curve: a Bezier curve and the four points that make it;
 *   - calculator: no page at all -- a programming language first, the
 *     stack and = printing to the transcript.
 *
 * Edit the program and Run again: an error stops it with the message
 * a LaserWriter printed (%%[ Error: undefined; OffendingCommand: ...),
 * the line it was on under the buttons.
 *
 * The language is languages/postscript (Ps_lexer, Ps_graphics,
 * Ps_machine, Ps_disk, tested without a screen), where the ideas are
 * explained: the three stacks, procedures as data, the CTM, paths,
 * Bezier curves and their flattening. PostScript is Forth's
 * descendant (Moore, 1970, through Warnock's JaM at PARC, 1978), and
 * a Forth is small enough to be a program of its own some day.
 *
 * What it uses: languages/postscript; Hershey's letters (graphics/font)
 * as the host's font -- every name findfont is given draws in them;
 * the gui's text area and buttons. What it does not use: the 2D
 * software rasterizer directly -- the paths are drawn as the
 * playground's polygons and lines, the PostScript machine giving them
 * already flattened.
 *
 * What it deliberately does not do: fonts (outlines, filled, hinted --
 * Adobe's Type 1 was the other half of the business), clip paths,
 * images, dashes, joins and caps, even-odd against non-zero winding
 * (eofill is fill here), several pages shown, and PostScript Level 2
 * (dictionaries written << >>, forms, patterns).
 *
 * Exercises: clip; setdash; the line joins, drawn at each corner;
 * charpath, the letters as a path to fill; a Forth (TinyForth) beside
 * it, the language without the graphics; EPS, the page's bounding box
 * and a picture placed in a document (TinyPageMaker's business).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type speed = Paused | Slowly | Fast

type model = {
  program : Text_edit.t;
  chosen : int; (* which program of the disk *)
  machine : Ps_machine.t;
  speed : speed;
  flags_read : bool;
}

(* Hershey's letters, as the machine wants them: ems, y up from the
   baseline (Hershey's is at 9, y down) *)
let hershey : Ps_machine.host =
  {
    glyph =
      (fun c ->
        let g = Hershey.glyph c in
        let em v = float_of_int v /. Hershey.units_per_em in
        (List.map (List.map (fun (x, y) -> (em (x - g.left), em (9 - y)))) g.strokes, em (g.right - g.left)));
  }

let load i =
  let _, text = List.nth Ps_disk.programs i in
  { program = Text_edit.of_string text; chosen = i; machine = Ps_machine.start ~host:hershey text; speed = Fast; flags_read = true }

let initial = { (load 0) with flags_read = false }
let restart speed m = { m with machine = Ps_machine.start ~host:hershey (Text_edit.to_string m.program); speed }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let editor_box : Widget.box = { x = -222.; y = 232.; w = 540.; h = 530. }
let button_box i : Widget.box = { x = -432. +. (float_of_int i *. 118.); y = -60.; w = 110.; h = 30. }
let buttons = [ "Run"; "Print slowly"; "Step"; "Next program" ]

(* how many objects a frame: a LaserWriter's minute a page, or at once *)
let per_frame = function Paused -> 0 | Slowly -> 3 | Fast -> 5000

let update computer m =
  (* the program asked for by name, sample=rosette *)
  let m =
    if m.flags_read then m
    else
      let wanted = List.assoc_opt "sample" computer.flags in
      let rec index i = function [] -> 0 | (name, _) :: rest -> if Some name = wanted then i else index (i + 1) rest in
      load (index 0 Ps_disk.programs)
  in
  let m = { m with program = Gui.text_area_in computer editor_box m.program } in
  let pressed i = Gui.button_in computer (button_box i) (List.nth buttons i) in
  let m =
    if pressed 0 then restart Fast m
    else if pressed 1 then restart Slowly m
    else if pressed 2 then
      (* the first Step starts the program over, paused *)
      let m = if m.speed <> Paused || Ps_machine.status m.machine <> Ps_machine.Running then restart Paused m else m in
      { m with machine = Ps_machine.step m.machine }
    else if pressed 3 then load ((m.chosen + 1) mod List.length Ps_disk.programs)
    else m
  in
  { m with machine = Ps_machine.run ~budget:(per_frame m.speed) m.machine }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the page, US Letter's 612 by 792 points, on the right *)
let page_center = (275., 180.)
let page_scale = 0.69
let on_page (x, y) = (fst page_center +. ((x -. 306.) *. page_scale), snd page_center +. ((y -. 396.) *. page_scale))
let colour (r, g, b) = rgb (int_of_float (r *. 255.)) (int_of_float (g *. 255.)) (int_of_float (b *. 255.))

let segment color w (x1, y1) (x2, y2) =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.sqrt ((dx *. dx) +. (dy *. dy)) +. (w *. 0.5)) w
  |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* a paint as the playground's shapes: a filled outline a polygon, a
   stroked one its segments, round where they meet when they are
   thick *)
let paint_shapes (p : Ps_machine.paint) : shape list =
  let color = colour p.rgb in
  List.concat_map
    (fun (points, closed) ->
      let points = List.map on_page points in
      match p.how with
      | Ps_machine.Fill -> if List.length points >= 3 then [ polygon color points ] else []
      | Ps_machine.Stroke w ->
          let w = Float.max 1. (w *. page_scale) in
          let points = if closed then points @ [ List.hd points ] else points in
          let rec segs = function a :: (b :: _ as rest) -> segment color w a b :: segs rest | _ -> [] in
          let joints = if w >= 3. then List.map (fun (x, y) -> circle color (w /. 2.) |> move x y) points else [] in
          segs points @ joints)
    p.lines

let left_text ?(size = 16.) color x y s =
  words color s |> scale (size /. words_font_size) |> move (x +. (Widget.text_width ~size s /. 2.)) y

let line_of text pos =
  let n = ref 1 in
  String.iteri (fun i c -> if i < pos && c = '\n' then incr n) text;
  !n

let view computer m =
  let text = Text_edit.to_string m.program in
  let machine = m.machine in
  let status, status_color =
    match Ps_machine.status machine with
    | Ps_machine.Running -> ((match m.speed with Paused -> "paused: Step for the next object" | _ -> "running"), rgb 60 60 60)
    | Ps_machine.Done -> (Printf.sprintf "done, %d objects executed" (Ps_machine.steps machine), rgb 20 110 40)
    | Ps_machine.Failed e -> (e, rgb 190 30 30)
  in
  let where =
    match Ps_machine.span machine with
    | Some (a, b) when b <= String.length text ->
        (* the object's text, its first line: a procedure spans many *)
        let s = String.sub text a (b - a) in
        let first = match String.index_opt s '\n' with Some i -> String.sub s 0 i ^ " ..." | None -> s in
        Printf.sprintf "line %d: %s" (line_of text a) first
    | _ -> ""
  in
  let px, py = page_center in
  (* the page being painted, or else the last one showpage finished *)
  let shown = match (Ps_machine.page machine, List.rev (Ps_machine.pages machine)) with [], p :: _ -> p | current, _ -> current in
  let page = List.concat_map paint_shapes shown in
  (* a procedure on the stack can be long: its start is enough *)
  let cut s = if String.length s > 48 then String.sub s 0 45 ^ "..." else s in
  let stack = List.map cut (Ps_machine.stack machine) in
  let printed = Ps_machine.output machine in
  let last n l = List.filteri (fun i _ -> i >= List.length l - n) l in
  [ rectangle (rgb 200 200 205) computer.screen.width computer.screen.height;
    (* the paper, and its shadow *)
    rectangle (rgb 150 150 155) (612. *. page_scale) (792. *. page_scale) |> move (px +. 6.) (py -. 6.);
    rectangle white (612. *. page_scale) (792. *. page_scale) |> move px py ]
  @ page
  @ [ left_text ~size:13. (rgb 90 90 90) (px -. 210.) (py -. 292.) (Printf.sprintf "%s -- US Letter, 612 x 792 points" (fst (List.nth Ps_disk.programs m.chosen)));
      left_text status_color (-490.) (-95.) status;
      left_text (rgb 60 60 110) (-490.) (-120.) where;
      left_text ~size:15. (rgb 40 40 40) (-490.) (-155.) "operand stack, top first:";
      left_text ~size:15. (rgb 40 40 40) (-60.) (-155.) "transcript:" ]
  @ List.mapi (fun i s -> left_text ~size:14. (rgb 20 20 20) (-470.) (-180. -. (float_of_int i *. 18.)) s) (List.filteri (fun i _ -> i < 15) stack)
  @ List.mapi (fun i s -> left_text ~size:14. (rgb 20 20 20) (-40.) (-180. -. (float_of_int i *. 18.)) s) (last 15 printed)
  @ Gui.draw ()

let app = game view update initial
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
