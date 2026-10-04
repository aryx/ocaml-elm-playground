(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A GPT learning to make up names while you watch (Gpt.mli,
 * notes_ai_learning.md section 14). After Andrej Karpathy's microgpt:
 * its sizes (4,192 numbers), its names, one name a step.
 *
 * Three things to look at:
 *
 *  - the names on the right, a new one every third of a second:
 *    letters at random at first, names within a minute;
 *  - the curve at the bottom: the loss on names it never learns from,
 *    crossing the line of the table of letter pairs (2.454, what
 *    AiNames' "c" scores) after a few hundred names;
 *  - the square on the left, which is the new thing: **attention**.
 *    A name is read a letter at a time, top to bottom; each row is
 *    one letter being read, and the row's cells are how much it looks
 *    at each letter before it (and at itself, the last cell). Before
 *    any training every row is flat: it looks everywhere alike. As it
 *    learns the rows sharpen, each head its own way: "1" to "4" show
 *    one head, "0" all four averaged.
 *
 * The keys that are the lesson: "a" trains it again without
 * attention, "p" without positions (Gpt.mli's table says what each
 * costs in the long run; here, watch how alike the first minute is).
 * "-" and "+" change the temperature the names are drawn at: low, the
 * same safe names; high, wild ones. Space pauses, "r" starts over.
 *
 * What it uses: Gpt, Tokenizer, Corpus and Sampling (ai's language
 * folder), the names of data/names, Scene2d (the keys). *)
open Playground

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

type data = {
  tokens : Tokenizer.t;
  learn : int list array; (* each name as its tokens, between boundaries *)
  held : int list list; (* names it never learns from *)
}

(* read once, when first needed: nothing of it at the program's top *)
let data : data Lazy.t =
  lazy
    (let tokens = Tokenizer.of_text Makemore_names.text in
     let corpus = Corpus.split (Tokenizer.words Makemore_names.text) in
     let bounded = List.map (Tokenizer.bounded tokens) in
     { tokens; learn = Array.of_list (bounded corpus.learn); held = List.filteri (fun i _ -> i < 150) (bounded corpus.held) })

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  gpt : Gpt.t;
  steps : int;
  losses : float list; (* on the held-out names, newest first *)
  names : string list; (* newest first *)
  draws : Lehmer.state;
  temperature : float;
  shown : int; (* the head whose attention is drawn, 0 for all *)
  (* where it looked, reading the word in the square: a row per
   * letter read, redone every few frames *)
  looked : float array list;
  running : bool;
  frame : int;
}

(* the word in the square *)
let word = "isabella"

let fresh (attention : bool) (positions : bool) : model =
  let d = Lazy.force data in
  {
    gpt = Gpt.make ~seed:1 (Gpt.config ~attention ~positions (Tokenizer.size d.tokens));
    steps = 0;
    losses = [];
    names = [];
    draws = Lehmer.make 1;
    temperature = 0.5;
    shown = 0;
    looked = [];
    running = true;
    frame = 0;
  }

let initial_model : model Lazy.t = lazy (fresh true true)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let steps_a_frame = 2
let kept = 300

(* the rate falls over the first 5,000 names, as microgpt's does over
 * its run, and stays low after *)
let rate (step : int) : float = 0.01 *. Float.max 0.1 (1. -. (float_of_int step /. 5000.))

(* each letter of the word read in turn: the shares of the head
 * shown, or the mean of all of them *)
let attention_of (m : model) : float array list =
  let d = Lazy.force data in
  if not m.gpt.config.attention then []
  else
    let tokens = Tokenizer.boundary :: Tokenizer.encode d.tokens word in
    List.init (List.length tokens) (fun read ->
        let (_, heads) = Gpt.next m.gpt (List.filteri (fun i _ -> i <= read) tokens) in
        match m.shown with
        | 0 ->
            Array.init (read + 1) (fun i ->
                List.fold_left (fun sum (h : float array) -> sum +. h.(i)) 0. heads /. float_of_int (List.length heads))
        | h -> List.nth heads (h - 1))

let learn_a_frame (m : model) : model =
  let d = Lazy.force data in
  let m = { m with frame = m.frame + 1 } in
  let rec go (gpt : Gpt.t) (steps : int) (n : int) : Gpt.t * int =
    if n = 0 then (gpt, steps)
    else go (fst (Gpt.step ~rate:(rate steps) gpt d.learn.(steps mod Array.length d.learn))) (steps + 1) (n - 1)
  in
  let (gpt, steps) = go m.gpt m.steps steps_a_frame in
  let m = { m with gpt; steps } in
  (* the loss on 150 held-out names is 150 texts read: every 40 steps *)
  let m =
    if steps mod 40 = 0 then { m with losses = List.filteri (fun i _ -> i < kept) (Gpt.loss gpt d.held :: m.losses) } else m
  in
  let m =
    if m.frame mod 20 = 1 then
      { m with names = List.filteri (fun i _ -> i < 16) (Gpt.sample ~temperature:m.temperature m.draws d.tokens gpt :: m.names) }
    else m
  in
  if m.frame mod 10 = 1 then { m with looked = attention_of m } else m

let update (computer : computer) (s : model Lazy.t Scene2d.t) : model Lazy.t Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = Lazy.force scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let c = m.gpt.config in
  let m = if key "a" then fresh (not c.attention) c.positions else m in
  let m = if key "p" then fresh c.attention (not c.positions) else m in
  let m = if key "r" then fresh c.attention c.positions else m in
  let m = if key "-" then { m with temperature = Float.max 0.1 (m.temperature -. 0.1) } else m in
  let m = if key "=" || key "+" then { m with temperature = Float.min 2. (m.temperature +. 0.1) } else m in
  let m =
    List.fold_left (fun m h -> if key (string_of_int h) then { m with shown = h; looked = attention_of { m with shown = h } } else m) m [ 0; 1; 2; 3; 4 ]
  in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  let m = if m.running then learn_a_frame m else m in
  { scenes with scene = Lazy.from_val m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let gold = rgb 240 210 120

(* a share as a colour *)
let heat (p : float) : color =
  let a = Float.min 1. (Float.max 0. p) in
  rgb (int_of_float (30. +. (a *. 225.))) (int_of_float (33. +. (a *. 177.))) (int_of_float (48. +. (a *. 40.)))

(* the square: a row per letter read, a cell per letter it could look
 * at -- those before it, and itself *)
let square (m : model) : shape list =
  let letters = "." ^ word in
  let n = String.length letters in
  let cell = 52. in
  let left = -430. and top = 300. in
  let x c = left +. (cell *. (float_of_int c +. 0.5)) and y r = top -. (cell *. (float_of_int r +. 0.5)) in
  let letter i = String.make 1 letters.[i] in
  List.concat
    (List.init n (fun r ->
         (text white 1.5 (letter r) |> move (left -. 22.) (y r))
         :: (text grey 1.3 (letter r) |> move (x r) (top +. 20.))
         :: List.init (r + 1) (fun c ->
                let share = match List.nth_opt m.looked r with Some row when c < Array.length row -> row.(c) | _ -> 0. in
                rectangle (heat share) (cell -. 2.) (cell -. 2.) |> move (x c) (y r))))
  @ [ text grey 1.1
        (if not m.gpt.config.attention then "attention is off: no letter looks at another"
         else
           Printf.sprintf "reading down: each row, how much that letter looks at each one before (%s)"
             (if m.shown = 0 then "the four heads averaged" else Printf.sprintf "head %d" m.shown))
      |> move (left +. (cell *. 4.5)) (top -. (cell *. float_of_int n) -. 22.) ]

let names (m : model) : shape list =
  (text white 1.4 (Printf.sprintf "names it makes up, at temperature %.1f" m.temperature) |> move 290. 330.)
  :: List.mapi (fun i n -> text (rgb 200 225 200) 1.5 n |> move 290. (290. -. (32. *. float_of_int i))) m.names

(* the loss, oldest on the left, between 2.2 and knowing nothing; the
 * table of pairs' 2.454 as a line to cross *)
let curve (m : model) : shape list =
  let n = List.length m.losses in
  let bottom = -400. and height = 120. and wide = 900. in
  let low = 2.2 and high = log 27. in
  let y v = bottom +. (height *. Float.min 1. (Float.max 0. ((v -. low) /. (high -. low)))) in
  let x i = (-.wide /. 2.) +. (wide *. float_of_int (n - 1 - i) /. float_of_int (kept - 1)) in
  [ rectangle (rgb 30 33 48) wide height |> move_y (bottom +. (height /. 2.));
    rectangle (rgb 90 95 120) wide 1. |> move_y (y 2.454);
    text grey 1. "2.454, the table of letter pairs" |> move 330. (y 2.454 +. 10.) ]
  @ List.mapi (fun i v -> rectangle gold 3. 3. |> move (x i) (y v)) m.losses

let view (computer : computer) (s : model Lazy.t Scene2d.t) : shape list =
  let m = Lazy.force s.scene and screen = computer.screen in
  let c = m.gpt.config in
  let now = match m.losses with l :: _ -> Printf.sprintf "%.3f" l | [] -> "..." in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ square m @ names m @ curve m
  @ [ text white 2.2 "A GPT LEARNING NAMES" |> move_y 450.;
      text grey 1.4
        (Printf.sprintf "%d numbers   %d names read   %s%s" (Gpt.parameters m.gpt) m.steps
           (if c.attention then "attention" else "NO attention")
           (if c.positions then ", positions" else ", NO positions"))
      |> move_y 405.;
      text gold 1.8 (Printf.sprintf "loss on names never read: %s" now) |> move 290. (-250.);
      text grey 1.3 "a: attention on/off   p: positions on/off   0-4: which head   - +: temperature   r: again   space: pause"
      |> move_y (-475.) ]

let app = game view update (Scene2d.start initial_model)

let main =
  (* claude: a step's graph dies at its end; room for it in the minor
     heap, or it is promoted first and collected later, at several
     times the cost (notes_opti_ocaml.md). No effect in a browser. *)
  Gc.set { (Gc.get ()) with minor_heap_size = 8 * 1024 * 1024 };
  Playground_platform.run_app app
