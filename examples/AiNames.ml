(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A machine learning to make up names, three ways (Bigram.mli,
 * Ngram_mlp.mli, notes_ai_learning.md sections 11 and 12). After
 * Andrej Karpathy's makemore, on its own 32,033 names, so the numbers
 * on the screen are his.
 *
 * The keys are the lesson, in this order:
 *
 *  - "c": **counted**. The table of which letter follows which, a row
 *    per letter before, a column per letter after, the brighter the
 *    likelier. The first row is how names start, the first column how
 *    they end. Names are drawn from it on the right: not names, and
 *    not noise. Its loss, 2.454, is how surprised it is by a real
 *    name's next letter, on average.
 *  - "g": **learned**. The same table forgotten, every cell alike,
 *    and found again by walking downhill on that loss. Watch it
 *    become the counted table, and the number under it -- how far the
 *    furthest cell still is -- go to 0.000. Nothing was counted. A
 *    count and a learned weight are the same thing.
 *  - "m": **a network**, reading three letters back instead of one.
 *    No table could: 19,683 rows. It gives each letter a place, two
 *    numbers, drawn here as they move. The vowels drift together,
 *    nobody having said what a vowel is. Its loss on names it never
 *    trained on passes the table's after a few thousand batches, and
 *    the names start to look like names.
 *
 * Space pauses, "r" starts the learning over.
 *
 * It is slow in "m", a batch a frame: every number of the network is
 * a node of a graph (Grad.mli), and that is what a later module,
 * working on whole arrays, is for.
 *
 * What it uses: Tokenizer, Bigram, Ngram_mlp and Sampling (ai's
 * language folder), the names of data/names, Scene2d (the keys). *)
open Playground

(*****************************************************************************)
(* The names *)
(*****************************************************************************)

type data = {
  tokens : Tokenizer.t;
  counts : Matrix.t;
  counted : Matrix.t; (* the table, by counting *)
  train : Ngram_mlp.example array;
  held : Ngram_mlp.example array; (* names the network never learns from *)
}

(* read once, when first needed: nothing of it at the program's top *)
let data : data Lazy.t =
  lazy
    (let words = Tokenizer.words Makemore_names.text in
     let tokens = Tokenizer.of_text Makemore_names.text in
     let counts = Bigram.counts tokens words in
     (* shuffled from a seed: the names come most frequent first *)
     let corpus = Corpus.split words in
     let part words = Ngram_mlp.examples tokens ~context:3 words in
     { tokens; counts; counted = Bigram.probabilities counts; train = part corpus.learn; held = Array.sub (part corpus.held) 0 1500 })

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode = Counted | Learned | Network

type model = {
  mode : mode;
  learned : Bigram.learned;
  net : Ngram_mlp.t;
  steps : int; (* of the mode being learned *)
  losses : float list; (* newest first *)
  names : string list; (* newest first *)
  draws : Lehmer.state; (* the names' and the batches' dice *)
  running : bool;
  frame : int;
}

let fresh (mode : mode) : model =
  {
    mode;
    learned = Bigram.start 27;
    net = Ngram_mlp.make ~seed:1 27;
    steps = 0;
    losses = [];
    names = [];
    draws = Lehmer.make 1;
    running = true;
    frame = 0;
  }

let initial_model : model = fresh Counted

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the table on the screen: counted, or what the learning has so far *)
let table (m : model) : Matrix.t =
  match m.mode with Learned -> Bigram.learned_probabilities m.learned | Counted | Network -> (Lazy.force data).counted

let loss (m : model) : float =
  let d = Lazy.force data in
  match m.mode with
  | Counted -> Bigram.loss d.counted d.counts
  | Learned -> Bigram.loss (Bigram.learned_probabilities m.learned) d.counts
  | Network -> Ngram_mlp.loss m.net d.held

let a_name (m : model) : string =
  let d = Lazy.force data in
  match m.mode with
  | Counted | Learned -> Bigram.sample m.draws d.tokens (table m)
  | Network -> Ngram_mlp.sample m.draws d.tokens m.net

let kept = 300

let learn_a_frame (m : model) : model =
  let d = Lazy.force data in
  let m = { m with frame = m.frame + 1 } in
  (* a name every third of a second, whatever the mode *)
  let m = if m.frame mod 20 = 1 then { m with names = List.filteri (fun i _ -> i < 16) (a_name m :: m.names) } else m in
  let keep (m : model) : model = { m with losses = List.filteri (fun i _ -> i < kept) (loss m :: m.losses) } in
  match m.mode with
  | Counted -> m
  | Learned ->
      (* a step is a millisecond; one a frame so that it can be watched *)
      if m.steps >= kept then m else keep { m with learned = Bigram.step d.counts m.learned; steps = m.steps + 1 }
  | Network ->
      let net = Ngram_mlp.step m.net (Ngram_mlp.batch m.draws d.train 32) in
      let m = { m with net; steps = m.steps + 1 } in
      (* the loss on the held-out names is 1,500 forward passes: every
         twentieth batch *)
      if m.steps mod 20 = 1 then keep m else m

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = if key "c" then fresh Counted else m in
  let m = if key "g" then fresh Learned else m in
  let m = if key "m" then fresh Network else m in
  let m = if key "r" then fresh m.mode else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  { scenes with scene = (if m.running then learn_a_frame m else m) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let gold = rgb 240 210 120

(* the table: 27 by 27 cells, the row the letter before, the column
 * the letter after *)
let cell = 21.
let left = -455.
let top = 330.

(* a probability as a colour: its square root, so that the many small
 * ones are not all black beside the few large *)
let heat (p : float) : color =
  let a = sqrt (Float.min 1. p /. 0.6) |> Float.min 1. in
  rgb (int_of_float (24. +. (a *. 231.))) (int_of_float (28. +. (a *. 180.))) (int_of_float (48. +. (a *. 40.)))

let heat_map (m : model) : shape list =
  let d = Lazy.force data and p = table m in
  let x c = left +. (cell *. (float_of_int c +. 0.5)) and y r = top -. (cell *. (float_of_int r +. 0.5)) in
  let letter i = String.make 1 (Tokenizer.char d.tokens i) in
  List.concat
    (List.init 27 (fun r ->
         (text grey 1. (letter r) |> move (left -. 14.) (y r))
         :: (text grey 1. (letter r) |> move (x r) (top +. 14.))
         :: List.init 27 (fun c -> rectangle (heat (Matrix.get p r c)) (cell -. 1.) (cell -. 1.) |> move (x c) (y r))))
  @ [ text grey 1.1 "before (rows), after (columns); . is where a name starts and ends" |> move (left +. (cell *. 13.5)) (top -. (cell *. 27.) -. 18.) ]

(* the network's places for the letters: its embedding's two numbers,
 * as a point each *)
let places (m : model) : shape list =
  let d = Lazy.force data and e = m.net.embedding in
  let side = cell *. 27. in
  let (cx, cy) = (left +. (side /. 2.), top -. (side /. 2.)) in
  let reach = Array.fold_left (fun a x -> Float.max a (Float.abs x)) 1. e.data in
  let vowel c = String.contains "aeiouy" c in
  (rectangle (rgb 30 33 48) side side |> move cx cy)
  :: List.init 27 (fun i ->
         let c = Tokenizer.char d.tokens i in
         let color = if i = 0 then gold else if vowel c then rgb 240 130 110 else rgb 120 180 250 in
         text color 1.8 (String.make 1 c)
         |> move (cx +. (Matrix.get e i 0 /. reach *. side *. 0.46)) (cy +. (Matrix.get e i 1 /. reach *. side *. 0.46)))
  @ [ text grey 1.1 "each letter where the network put it: vowels red, consonants blue" |> move cx (top -. side -. 18.) ]

let names (m : model) : shape list =
  (text white 1.4 "names it makes up" |> move 300. 330.)
  :: List.mapi (fun i n -> text (rgb 200 225 200) 1.5 n |> move 300. (290. -. (32. *. float_of_int i))) m.names

(* the loss, oldest on the left, between the table's 2.454 (a line)
 * and knowing nothing, log 27 *)
let curve (m : model) : shape list =
  let n = List.length m.losses in
  let bottom = -400. and height = 110. and wide = 900. in
  let low = 2.2 and high = log 27. in
  let y v = bottom +. (height *. Float.min 1. (Float.max 0. ((v -. low) /. (high -. low)))) in
  let x i = (-.wide /. 2.) +. (wide *. float_of_int (n - 1 - i) /. float_of_int (kept - 1)) in
  [ rectangle (rgb 30 33 48) wide height |> move_y (bottom +. (height /. 2.));
    rectangle (rgb 90 95 120) wide 1. |> move_y (y 2.454);
    text grey 1. "2.454, the counted table" |> move 350. (y 2.454 +. 10.) ]
  @ List.mapi (fun i v -> rectangle gold 3. 3. |> move (x i) (y v)) m.losses

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen and d = Lazy.force data in
  let now = match m.losses with l :: _ -> l | [] -> loss { m with mode = Counted } in
  let furthest =
    let p = table m in
    let worst = ref 0. in
    Array.iteri (fun i x -> worst := Float.max !worst (Float.abs (x -. d.counted.data.(i)))) p.data;
    !worst
  in
  let (title, line) =
    match m.mode with
    | Counted -> ("COUNTED: WHICH LETTER FOLLOWS WHICH", "228,146 pairs of letters in 32,033 names, counted")
    | Learned ->
        ( "LEARNED: THE SAME TABLE, BY WALKING DOWNHILL",
          Printf.sprintf "step %d   the furthest cell from the counted table: %.3f" m.steps furthest )
    | Network ->
        ( "A NETWORK, THREE LETTERS BACK",
          Printf.sprintf "%d numbers   %d batches of 32   loss on names never trained on" (Ngram_mlp.parameters m.net)
            m.steps )
  in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ (match m.mode with Counted | Learned -> heat_map m | Network -> places m)
  @ names m @ curve m
  @ [ text white 2.2 title |> move_y 450.;
      text grey 1.4 line |> move_y 405.;
      text gold 1.8 (Printf.sprintf "loss %.3f   %.2f bits a letter" now (Bigram.bits now)) |> move 300. (-250.);
      text grey 1.3 "c: counted    g: learned    m: a network    r: again    space: pause" |> move_y (-475.) ]

let app = game view update (Scene2d.start initial_model)

let main =
  (* claude: a step's graph is a few megabytes of nodes that all die
     at its end. In the default minor heap (256k words) they are
     promoted first and collected by the major collector later, 45 ms
     a batch; with room for a whole step they die young, 12 ms
     (notes_opti_ocaml.md). No effect in a browser. *)
  Gc.set { (Gc.get ()) with minor_heap_size = 8 * 1024 * 1024 };
  Playground_platform.run_app app
