(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Shannon's guessing game, against a language model. A name is
 * hidden; guess its first letter. Wrong: guess again. Right: it is
 * shown, and you guess the next, and so on to the end -- which has to
 * be guessed too: "." (or space) says "the name stops here". Type the
 * letters, or click them. Eight names make a round; fewer guesses a
 * letter wins.
 *
 * The computer plays the same names with the same rule, and its
 * guesses are no mystery: it always tries the letters in the order
 * of the probabilities its model gives them. So the number of guesses
 * it needs for a letter is that letter's rank in its opinion, and
 * after each letter the game shows the opinion it had. "1" plays
 * against the table of letter pairs (Bigram.mli: it knows the letter
 * before, nothing else), "2" against the network that reads three
 * letters back (Ngram_mlp.mli), "3" against the GPT, which reads the
 * whole name so far (Gpt.mli), the default and the strongest: their
 * losses on names never seen are 2.45, 2.33 and 2.22. The same eight
 * names for all three, so they can be compared, and you with each.
 *
 * The game is Claude Shannon's, of 1951: he had people guess English
 * a letter at a time to measure how much of it is already known
 * before it is read, and found about one bit a letter where a table
 * of pairs needs three and a half. The count of guesses is his
 * measure; the model's *loss*, which every program of
 * notes_ai_learning.md sections 11 and on is about bringing down, is
 * the same thing weighed more finely, and is shown beside it in bits.
 * Playing is the quickest way to feel what that number means: 2.3
 * guesses a letter is hard to beat on names, and you will know why
 * the first letter costs ten guesses and the last one.
 *
 * The names are makemore's (data/names), and none of the hidden ones
 * was ever shown to either model: they come from the tenth that
 * Corpus.split keeps out of all training, the trainer's included.
 *
 * The network and the GPT are not trained here: eleven minutes for
 * one, half a minute for the other. Each was trained once by a program of
 * scripts/train (train_names, train_names_gpt), and what it learned is
 * a file of data/weights (Weights.mli), embedded at build time as
 * Weights_names_mlp and Weights_names_gpt; a file's first lines say
 * how it was made and how well it did, and data/weights/README.md how
 * to make it again.
 *
 * What it uses: ai's language folder (Tokenizer, Corpus, Bigram,
 * Ngram_mlp, Gpt) and Weights; the names and the weights of data/; Scene2d
 * (the keys pressed). Not Mcts or
 * Minimax: there is no opponent's move to foresee, both players face
 * the same hidden name.
 *
 * Exercises: sentences instead of names (Shannon's own game: a text
 * and a model trained on it); the model's guesses shown as you play,
 * a hint that costs a guess; your own guesses kept, and a table of
 * pairs learned from them -- what do you think follows a q?
 *)
open Playground

(*****************************************************************************)
(* The names and the two models *)
(*****************************************************************************)

type opponent = Pairs | Network | Transformer

type data = {
  tokens : Tokenizer.t;
  pairs : Matrix.t; (* the table's probabilities, a row per letter before *)
  net : Ngram_mlp.t;
  gpt : Gpt.t;
  hidden : string array; (* names neither model was shown *)
}

(* made when first needed: nothing of it at the program's top *)
let data : data Lazy.t =
  lazy
    (let tokens = Tokenizer.of_text Makemore_names.text in
     let corpus = Corpus.split (Tokenizer.words Makemore_names.text) in
     (* smoothed by 1: a held-out name may have a pair the others
        never had, and "impossible" would be a rank among ties *)
     let pairs = Bigram.probabilities ~smoothing:1. (Bigram.counts tokens corpus.learn) in
     let net =
       match Result.bind (Weights.of_string Weights_names_mlp.bytes) Ngram_mlp.of_weights with
       | Ok net -> net
       | Error why -> failwith ("names_mlp.weights: " ^ why)
     in
     let gpt =
       match Result.bind (Weights.of_string Weights_names_gpt.bytes) Gpt.of_weights with
       | Ok gpt -> gpt
       | Error why -> failwith ("names_gpt.weights: " ^ why)
     in
     { tokens; pairs; net; gpt; hidden = Array.of_list corpus.held })

(* what a model thinks comes after the first [at] letters of [name] *)
let opinion (o : opponent) (name : string) (at : int) : float array =
  let d = Lazy.force data in
  let before = Array.of_list (Tokenizer.encode d.tokens (String.sub name 0 at)) in
  let back k = if at - k >= 0 then before.(at - k) else Tokenizer.boundary in
  match o with
  | Pairs -> Matrix.row d.pairs (back 1)
  | Network -> Ngram_mlp.probabilities d.net [| back 3; back 2; back 1 |]
  (* the whole name so far, from the boundary it starts at *)
  | Transformer -> fst (Gpt.next d.gpt (Tokenizer.boundary :: Array.to_list before))

(* the token that really comes at [at]: a letter, or the boundary
 * after the last *)
let truth (name : string) (at : int) : int =
  let d = Lazy.force data in
  if at >= String.length name then Tokenizer.boundary else List.hd (Tokenizer.encode d.tokens (String.make 1 name.[at]))

(* how many guesses the model needs: the true token's rank in its
 * opinion, 1 for its first choice *)
let rank (p : float array) (token : int) : int = 1 + Array.fold_left (fun n x -> if x > p.(token) then n + 1 else n) 0 p

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let names_a_round = 8

type stage = Guessing | Shown (* the name is out: space for the next *) | Over

type model = {
  opponent : opponent;
  series : int; (* which eight names: "r" takes the next eight *)
  nth : int; (* the name being guessed, 0 to 7 *)
  at : int; (* letters of it found *)
  tried : int list; (* wrong guesses at this letter *)
  (* guesses for each letter found so far, this name's, newest first *)
  mine : int list;
  theirs : int list;
  (* the round's totals *)
  letters : int;
  my_guesses : int;
  their_guesses : int;
  their_bits : float; (* the sum of -log2 p of each true letter *)
  last : (float array * int) option; (* its opinion of the letter just found, and that letter *)
  stage : stage;
}

let start (opponent : opponent) (series : int) : model =
  { opponent; series; nth = 0; at = 0; tried = []; mine = []; theirs = []; letters = 0; my_guesses = 0;
    their_guesses = 0; their_bits = 0.; last = None; stage = Guessing }

let initial_model : model = start Transformer 0

(* the hidden names in an order of their own, far apart in the list *)
let name (m : model) : string =
  let d = Lazy.force data in
  d.hidden.((((m.series * names_a_round) + m.nth) * 397) mod Array.length d.hidden)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let guess (m : model) (token : int) : model =
  if m.stage <> Guessing || List.mem token m.tried then m
  else
    let name = name m in
    let right = truth name m.at in
    if token <> right then { m with tried = token :: m.tried }
    else
      let p = opinion m.opponent name m.at in
      let (mine, theirs) = (List.length m.tried + 1, rank p right) in
      { m with
        at = m.at + 1;
        tried = [];
        mine = mine :: m.mine;
        theirs = theirs :: m.theirs;
        letters = m.letters + 1;
        my_guesses = m.my_guesses + mine;
        their_guesses = m.their_guesses + theirs;
        their_bits = m.their_bits -. (log p.(right) /. log 2.);
        last = Some (p, right);
        stage = (if right = Tokenizer.boundary then Shown else Guessing) }

let next (m : model) : model =
  if m.nth + 1 >= names_a_round then { m with stage = Over }
  else { m with nth = m.nth + 1; at = 0; tried = []; mine = []; theirs = []; last = None; stage = Guessing }

(* the letters to click: three rows of nine, the boundary last *)
let key_size = 62.
let key_at (token : int) : number * number =
  (* a to z are tokens 1 to 26, "." is 0: shown after z *)
  let k = if token = Tokenizer.boundary then 26 else token - 1 in
  ((float_of_int (k mod 9) -. 4.) *. (key_size +. 8.), -150. -. (float_of_int (k / 9) *. (key_size +. 8.)))

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let d = Lazy.force data in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes || Scene2d.pressed (fun k -> k.kenter) scenes in
  let m = if key "1" then start Pairs m.series else m in
  let m = if key "2" then start Network m.series else m in
  let m = if key "3" then start Transformer m.series else m in
  let m =
    match m.stage with
    | Shown -> if space then next m else m
    | Over -> if space then start m.opponent (m.series + 1) else m
    | Guessing ->
        (* a letter typed, or clicked *)
        let typed =
          List.filter
            (fun token -> key (String.make 1 (Tokenizer.char d.tokens token)))
            (List.init (Tokenizer.size d.tokens) (fun i -> i))
        in
        let typed = if space then Tokenizer.boundary :: typed else typed in
        let clicked =
          if not computer.mouse.mclick then []
          else
            List.filter
              (fun token ->
                let (x, y) = key_at token in
                Float.abs (computer.mouse.mx -. x) < key_size /. 2. && Float.abs (computer.mouse.my -. y) < key_size /. 2.)
              (List.init (Tokenizer.size d.tokens) (fun i -> i))
        in
        List.fold_left guess m (typed @ clicked)
  in
  { scenes with scene = m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let gold = rgb 240 210 120
let green = rgb 140 220 160
let who (o : opponent) : string =
  match o with Pairs -> "the table of pairs" | Network -> "the network" | Transformer -> "the GPT"

(* the name so far, a letter a cell, the guesses each cost under it:
 * yours in green, the model's in gold *)
let the_name (m : model) : shape list =
  let name = name m in
  let cells = String.length name + 1 in
  let wide = 64. in
  let x i = (float_of_int i -. (float_of_int (cells - 1) /. 2.)) *. wide in
  let mine = Array.of_list (List.rev m.mine) and theirs = Array.of_list (List.rev m.theirs) in
  List.concat
    (List.init cells (fun i ->
         let found = i < m.at in
         let letter = if i < String.length name then String.make 1 name.[i] else "." in
         [ rectangle (if i = m.at && m.stage = Guessing then gold else rgb 60 66 92) (wide -. 8.) 4. |> move (x i) 205.;
           (if found then text white 3.4 letter |> move (x i) 245. else group []);
           (if found then text green 1.5 (string_of_int mine.(i)) |> move (x i) 175. else group []);
           (if found then text gold 1.5 (string_of_int theirs.(i)) |> move (x i) 140. else group []) ]))
  @ [ text green 1.3 "you" |> move (x 0 -. 90.) 175.; text gold 1.3 "it" |> move (x 0 -. 90.) 140. ]

let keyboard (m : model) : shape list =
  let d = Lazy.force data in
  List.concat
    (List.init (Tokenizer.size d.tokens) (fun token ->
         let (x, y) = key_at token in
         let tried = List.mem token m.tried in
         [ rectangle (if tried then rgb 70 36 40 else rgb 44 50 74) key_size key_size |> move x y;
           text (if tried then rgb 120 80 80 else white) 2. (String.make 1 (Tokenizer.char d.tokens token)) |> move x y ]))

(* what the model thought of the letter just found: its five first
 * choices as bars, the true one lit *)
let opinion_bars (m : model) : shape list =
  match m.last with
  | None -> []
  | Some (p, right) ->
      let d = Lazy.force data in
      let order = List.sort (fun a b -> compare p.(b) p.(a)) (List.init (Array.length p) (fun i -> i)) in
      let shown = List.filteri (fun i _ -> i < 5) order in
      let shown = if List.mem right shown then shown else List.filteri (fun i _ -> i < 4) shown @ [ right ] in
      (text grey 1.2 (who m.opponent ^ " expected") |> move 365. 80.)
      :: List.concat
           (List.mapi
              (fun i token ->
                let y = 45. -. (30. *. float_of_int i) and wide = 130. *. p.(token) /. p.(List.hd order) in
                let color = if token = right then gold else rgb 90 100 140 in
                [ text color 1.4 (String.make 1 (Tokenizer.char d.tokens token)) |> move 285. y;
                  rectangle color wide 16. |> move (305. +. (wide /. 2.)) y;
                  text grey 1.1 (Printf.sprintf "%.0f%%" (100. *. p.(token))) |> move 470. y ])
              shown)

let per (total : int) (letters : int) : float = if letters = 0 then 0. else float_of_int total /. float_of_int letters

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let mine = per m.my_guesses m.letters and theirs = per m.their_guesses m.letters in
  let prompt =
    match m.stage with
    | Guessing when m.at = 0 -> "a name neither of you has seen: what is its first letter?"
    | Guessing -> "and the next?  ( . or space if the name ends here )"
    | Shown -> Printf.sprintf "%s.  space: the next name" (name m)
    | Over ->
        if mine < theirs then "you guessed better than " ^ who m.opponent ^ ".  space: eight more names"
        else if mine > theirs then who m.opponent ^ " guessed better than you.  space: eight more names"
        else "a draw.  space: eight more names"
  in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ the_name m @ keyboard m @ opinion_bars m
  @ [ text white 2.2 "SHANNON'S GUESSING GAME" |> move_y 450.;
      text grey 1.4
        (Printf.sprintf "against %s      name %d of %d" (who m.opponent) (min names_a_round (m.nth + 1)) names_a_round)
      |> move_y 405.;
      text white 1.5 prompt |> move_y 330.;
      text green 1.6 (Printf.sprintf "you: %.2f guesses a letter" mine) |> move (-250.) (-20.);
      text gold 1.6 (Printf.sprintf "it: %.2f" theirs) |> move (-250.) (-55.);
      text grey 1.2
        (if m.letters = 0 then ""
         else Printf.sprintf "its loss on these letters: %.2f bits each" (m.their_bits /. float_of_int m.letters))
      |> move (-250.) (-90.);
      text grey 1.3 "against  1: the table of pairs    2: the network    3: the GPT        the same names for all" |> move_y (-410.) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let app = game view update (Scene2d.start initial_model)

(* claude: run at once in its own .exe, recorded under its name in
 * tinybox, the launcher of every game (Program.mli) *)
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
