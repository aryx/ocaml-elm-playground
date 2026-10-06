(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Chess against the computer, which thinks 3 moves ahead with
 * alpha-beta (Minimax.mli, the same search as AiOthello.ml).
 * You are white: click a piece, then where it goes (or move the cursor
 * with the arrows, and space twice); a pawn reaching the last rank
 * becomes a queen. u takes back your last move and the computer's
 * answer; space after the end plays again. "a" changes who plays
 * black: this computer, or a network that taught itself the game (its
 * section below).
 *
 * Chess is the game the field was named for. Claude Shannon's
 * "Programming a Computer for Playing Chess" (1950) set out minimax
 * with an evaluation function, and said a program would have to choose
 * which moves to look at; Alan Turing played a game with a program he
 * ran by hand, on paper (1952); Alex Bernstein's program (IBM, 1957)
 * played the whole game; Richard Greenblatt's Mac Hack VI (MIT, 1967)
 * played in tournaments; Belle (Ken Thompson and Joe Condon, Bell
 * Labs, 1980) was chess in hardware; and Deep Blue (IBM) beat Garry
 * Kasparov in 1997, searching 200 million positions a second, with the
 * same alpha-beta as here. (Names and dates from memory, to check.)
 *
 * Othello's rules fit in 20 lines; chess's are what most of this file
 * is, and what a chess program gets wrong first. So they are checked
 * the way chess programmers check theirs, with **perft** ([perft]):
 * count every position n moves ahead, and compare with the numbers
 * everyone agrees on -- 20, 400, 8902 from the start; and from
 * "Kiwipete" and other positions chosen to hold every rule at once
 * (castling through check, en passant that uncovers a check,
 * promotions that capture). One wrong rule, and a count is off
 * (tests/games/Unit_games.ml).
 *
 * The rules, as the move generator sees them ([pseudo_moves], [legal]):
 * each piece's moves are generated as if its own king did not matter,
 * and each is then played and thrown away if it leaves that king
 * attacked ([attacked]). The simplest correct way, and the slowest;
 * real programs keep track of pins instead.
 *
 * Two things more than AiOthello, each with its key to turn it off and
 * see what it is for:
 *
 *  - **Move ordering** ([order], the key o). Alpha-beta cuts a branch
 *    as soon as one move refutes it, so it cuts most when the best move
 *    comes first. Captures first, the biggest victim taken by the
 *    smallest attacker ("MVV-LVA": most valuable victim, least valuable
 *    attacker): a pawn taking a queen is tried before a queen taking a
 *    pawn. The count of positions visited, under the board, shows what
 *    it saves -- the same search, the same move, several times fewer
 *    positions.
 *
 *  - **Quiescence** ([quiesce], the key c). A search that stops 3 moves
 *    ahead stops in the middle of things: its last move may be a queen
 *    taking a pawn, and the reply that takes the queen is one move too
 *    far to be seen -- the "horizon effect". So the evaluation does not
 *    look at the board until it is quiet: at the leaves, the captures
 *    are searched on, captures only, until nobody wants to take
 *    anything (each side may also stop: "stand pat").
 *
 *      3 moves ahead    ... Qxe5     the leaf: +1 pawn, says the board
 *      quiescence       ... Qxe5 dxe5   -8: the queen was lost
 *
 *    That search at the leaves has to know alpha-beta's window there
 *    (Minimax.alphabeta's [leaf], [quiet]): with no window, a leaf
 *    plays every exchange out to the end: one position full of
 *    captures ("Kiwipete", the tests' favourite) took 1.9 seconds
 *    natively and 5 in JavaScript, and takes 0.16 and 1.5 with the
 *    window.
 *
 * The evaluation ([static]): material (a pawn 100, a knight 320, a
 * bishop 330, a rook 500, a queen 900) and a table per piece of what
 * each square is worth -- knights in the centre, pawns forward, the king
 * behind its pawns -- Tomasz Michniewski's "Simplified Evaluation
 * Function" (the tables as remembered here), the same idea as
 * AiOthello's table of squares. A checkmate is worth more than any
 * material, and more the sooner it comes ([mate]).
 *
 * What it uses: the boards kit's Chess (the rules and the computer,
 * shared with the trainer), ai/'s Minimax (alpha-beta: the ordering is
 * the order of [moves], the quiescence its [leaf]), Alphazero,
 * Policy_value and Mcts for the other computer, whose weights are a
 * file of data/weights, Scene2d (the
 * keys pressed). Not the puzzle kit, nor Tilemap: a board of 64
 * squares is an array. Positions can be written in FEN, chess's own
 * text format ([of_fen]), which is how the tests set them up.
 *
 * Left as exercises: draws by repetition and by the 50-move rule (the
 * game only knows stalemate); choosing the piece to promote to (the
 * generator knows all four, the click takes a queen); an opening book;
 * iterative deepening with a time limit; a transposition table (the
 * same position reached by two move orders, searched once); showing the
 * moves in standard notation (Nf3, exd5, O-O).
 *)
open Playground

(* the rules and the computer, checked by perft and the tests: the
 * boards kit's Chess, shared with the program that trains a network
 * to play (scripts/train/train_chess) *)
include Chess

(*****************************************************************************)
(* The other computer: a network that taught itself (ai=network) *)
(*****************************************************************************)
(* The computer above was told what a position is worth: the table of
 * [static], a century of players' judgment in a few hundred numbers.
 * This one was told the rules, and that pieces are worth having; it
 * played against itself (Alphazero.mli, notes_ai_learning.md section
 * 16) and learned a guess at which moves matter and a guess at who is
 * winning, which guide a search (Mcts.mli) in the place of alpha-beta.
 * It is AlphaZero (DeepMind, 2017), which after nine hours of such
 * games beat the best alpha-beta program there was -- in miniature,
 * and the miniature is the lesson: theirs was 44 million games on
 * thousands of processors, this one a few thousand games on a desk.
 * scripts/train/train_chess did it, and data/weights' README says how
 * it fares against the computer above. Do not expect it to win.
 *
 * "a" changes who plays black, and the flag ai= who starts: classic
 * (alpha-beta, the default), network (the search guided by the
 * network), policy (the network alone, its first idea, no search). *)

type engine = Classic | Network | Policy

let engine_of (flags : (string * string) list) : engine =
  match List.assoc_opt "ai" flags with Some "network" -> Network | Some "policy" -> Policy | _ -> Classic

let engine_name (e : engine) : string =
  match e with
  | Classic -> "alpha-beta, 3 moves ahead"
  | Network -> "a network that taught itself, and a search"
  | Policy -> "the network alone, no search"

let net : Policy_value.t Lazy.t =
  lazy
    (match Result.bind (Weights.of_string Weights_chess.bytes) Policy_value.of_weights with
    | Ok net -> net
    | Error why -> failwith ("chess.weights: " ^ why))

(* the game as the network was taught it, never stopped: a position
 * and the half-moves played *)
let learned : (position * int, move) Alphazero.board = Chess.board ~longest:max_int

(* a playout is a pass through the network, a few milliseconds: so
 * many a frame, and so many before it moves, a second or two. The
 * search is anytime (Mcts.mli's think), so the game goes on drawing *)
let network_playouts = 240
let playouts_a_frame = 4

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  position : position;
  before : position list; (* before each of your moves, for u *)
  cursor : int;
  selected : int option;
  last : move option;
  wait : int; (* frames before the computer plays *)
  nodes : int option; (* the positions its last search visited *)
  ordered : bool;
  quiescence : bool;
  engine : engine option; (* who plays black; None until the flags are seen *)
  (* the network's search, while it is black's turn, and its playouts
   * so far *)
  mind : (position * int, move) Mcts.thinking option;
  thought : int;
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; before = []; cursor = 52; selected = None; last = None; wait = 0; nodes = None;
    ordered = true; quiescence = true; engine = None; mind = None; thought = 0 }

let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let square_size = 100.

let square_at (x : float) (y : float) : int option =
  let c = int_of_float (Float.floor ((x +. 400.) /. square_size))
  and r = int_of_float (Float.floor ((400. -. y) /. square_size)) in
  if on_board r c then Some ((r * 8) + c) else None

let over (p : position) = legal p = []

(* your move from [from] to [dest], if there is one (a queen, when a
 * pawn promotes) *)
let your_move (p : position) (from : int) (dest : int) : move option =
  List.find_opt
    (fun m -> m.from = from && m.dest = dest && (m.promotion = None || m.promotion = Some Queen))
    (legal p)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed f = Scene2d.pressed f scenes in
  let key k = pressed (fun kb -> Set_.mem k kb.keys) in
  let m = computer.mouse in
  let g = if key "o" then { g with ordered = not g.ordered } else g in
  let g = if key "c" then { g with quiescence = not g.quiescence } else g in
  let g =
    match g.before with
    | p :: rest when key "u" ->
        { g with position = p; before = rest; selected = None; last = None; wait = 0; mind = None; thought = 0 }
    | _ -> g
  in
  let engine = match g.engine with Some e -> e | None -> engine_of computer.flags in
  let engine = if key "a" then (match engine with Classic -> Network | Network -> Policy | Policy -> Classic) else engine in
  let g = { g with engine = Some engine } in
  let g = { g with wait = max 0 (g.wait - 1) } in
  let p = g.position in
  if over p then g
  else if p.turn = Black then
    if g.wait > 0 then g
    else
      match engine with
      | Classic -> (
          let a = search ~ordered:g.ordered ~quiescence:g.quiescence ~depth p in
          match a.best with
          | Some mv -> { g with position = play p mv; last = Some mv; nodes = Some a.nodes; mind = None; thought = 0 }
          | None -> g)
      | Policy -> (
          match Alphazero.instinct learned (Lazy.force net) (p, 0) with
          | Some mv -> { g with position = play p mv; last = Some mv; nodes = None; mind = None; thought = 0 }
          | None -> g)
      | Network -> (
          (* a frame's worth of the search; when it has had enough, the
             move its tree visited most *)
          let t =
            match g.mind with
            | Some t -> t
            | None ->
                let (prior, evaluate) = Alphazero.guides learned (Lazy.force net) in
                Mcts.start ~seed:p.ply ~prior ~evaluate learned.game (p, 0)
          in
          let t = Mcts.think ~playouts:playouts_a_frame t in
          let thought = g.thought + playouts_a_frame in
          if thought < network_playouts then { g with mind = Some t; thought }
          else
            match (Mcts.plan t).best with
            | Some mv -> { g with position = play p mv; last = Some mv; nodes = None; mind = None; thought = 0 }
            | None -> g)
  else
    (* the cursor: the mouse when it moves, the arrows *)
    let r = row g.cursor and c = col g.cursor in
    let step k d = if pressed k then d else 0 in
    let clamp v = max 0 (min 7 v) in
    let r = clamp (r + step (fun k -> k.kdown) 1 + step (fun k -> k.kup) (-1)) in
    let c = clamp (c + step (fun k -> k.kright) 1 + step (fun k -> k.kleft) (-1)) in
    let cursor = (r * 8) + c in
    let cursor = if m.mdx <> 0. || m.mdy <> 0. || m.mclick then Option.value (square_at m.mx m.my) ~default:cursor else cursor in
    let g = { g with cursor } in
    if pressed (fun k -> k.kspace) || m.mclick then
      match g.selected with
      | Some from when your_move p from cursor <> None ->
          let mv = Option.get (your_move p from cursor) in
          { g with position = play p mv; before = p :: g.before; last = Some mv; selected = None; wait = 30 }
      | _ ->
          let mine = match p.board.(cursor) with Some q -> q.color = White | None -> false in
          { g with selected = (if mine then Some cursor else None) }
    else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let g = s.scene in
  if over g.position && Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (new_game ()) s
  else { s with scene = update_game computer s g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : Playground.color) (size : float) (s : string) : shape = words color s |> scale size

let square_center (i : int) : float * float =
  (-350. +. (square_size *. float_of_int (col i)), 350. -. (square_size *. float_of_int (row i)))

(* each piece's silhouette, about 70 pixels high, in one color *)
let silhouette (kind : kind) (color : Playground.color) : shape list =
  let base = rectangle color 52. 10. |> move_y (-32.) in
  match kind with
  | Pawn -> [ base; polygon color [ (-11., 6.); (11., 6.); (18., -28.); (-18., -28.) ]; circle color 13. |> move_y 14. ]
  | Rook ->
      [ base; rectangle color 34. 44. |> move_y (-8.); rectangle color 44. 10. |> move_y 16.;
        rectangle color 10. 10. |> move (-17.) 25.; rectangle color 10. 10. |> move_y 25.; rectangle color 10. 10. |> move 17. 25. ]
  | Knight ->
      [ base;
        polygon color [ (-18., -28.); (22., -28.); (22., 4.); (14., 24.); (0., 34.); (-6., 28.); (-24., 12.); (-26., 2.);
                        (-18., -2.); (-6., 8.); (-12., -12.) ] ]
  | Bishop ->
      [ base; polygon color [ (-14., -28.); (14., -28.); (7., 2.); (-7., 2.) ]; oval color 28. 36. |> move_y 14.;
        circle color 5. |> move_y 34. ]
  | Queen ->
      [ base; polygon color [ (-18., -28.); (18., -28.); (24., 20.); (11., 4.); (0., 26.); (-11., 4.); (-24., 20.) ];
        circle color 5. |> move (-24.) 22.; circle color 5. |> move_y 29.; circle color 5. |> move 24. 22. ]
  | King ->
      [ base; polygon color [ (-16., -28.); (16., -28.); (20., 12.); (-20., 12.) ]; rectangle color 32. 8. |> move_y 16.;
        rectangle color 6. 20. |> move_y 30.; rectangle color 18. 6. |> move_y 32. ]

(* the silhouette, over itself a little bigger in the outline's color *)
let view_piece (p : piece) : shape =
  let fill, outline = if p.color = White then (rgb 250 248 240, rgb 30 30 30) else (rgb 35 35 35, rgb 150 150 150) in
  group [ group (silhouette p.kind outline) |> scale 1.1; group (silhouette p.kind fill) ]

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let p = g.position in
  let at i shape = let x, y = square_center i in move x y shape in
  let moves = legal p in
  let status =
    match (moves, in_check p) with
    | [], true -> if p.turn = White then "CHECKMATE: THE COMPUTER WINS (space: again)" else "CHECKMATE: YOU WIN! (space: again)"
    | [], false -> "STALEMATE: A DRAW (space: again)"
    | _ ->
        let check = if in_check p then "check! " else "" in
        if p.turn = White then check ^ (if g.selected = None then "your move: pick a piece" else "your move: where to?")
        else check ^ (if g.thought > 0 then Printf.sprintf "the computer thinks... %d" g.thought else "the computer thinks...")
  in
  let targets = match g.selected with Some from -> List.filter (fun m -> m.from = from) moves | None -> [] in
  let light = rgb 238 216 180 and dark = rgb 180 135 100 in
  [ rectangle (rgb 45 40 35) screen.width screen.height ]
  @ List.init 64 (fun i -> at i (square (if (row i + col i) mod 2 = 0 then light else dark) square_size))
  @ (match g.last with
    | Some mv -> [ at mv.from (square (rgb 230 210 80) square_size |> fade 0.45); at mv.dest (square (rgb 230 210 80) square_size |> fade 0.45) ]
    | None -> [])
  @ (if in_check p then [ at (king_square p.board p.turn) (circle (rgb 220 40 40) 45. |> fade 0.6) ] else [])
  @ (match g.selected with Some i -> [ at i (square (rgb 90 170 90) square_size |> fade 0.6) ] | None -> [])
  @ List.filter_map (fun i -> Option.map (fun q -> at i (view_piece q)) p.board.(i)) (List.init 64 Fun.id)
  @ List.map (fun m -> at m.dest (circle (rgb 40 110 50) (if p.board.(m.dest) = None then 12. else 44.) |> fade 0.5)) targets
  @ (if p.turn = White && moves <> [] then
       let frame = rgb 40 90 200 in
       [ at g.cursor (group [ rectangle frame 100. 5. |> move_y 47.5; rectangle frame 100. 5. |> move_y (-47.5);
                              rectangle frame 5. 100. |> move_x 47.5; rectangle frame 5. 100. |> move_x (-47.5) ]) ]
     else [])
  @ List.init 8 (fun k -> text (rgb 200 190 170) 2. (String.make 1 (Char.chr (Char.code 'a' + k))) |> move (-350. +. (100. *. float_of_int k)) (-415.))
  @ List.init 8 (fun k -> text (rgb 200 190 170) 2. (string_of_int (8 - k)) |> move (-418.) (350. -. (100. *. float_of_int k)))
  @ [ text white 3. "you (white) against the computer (black)" |> move_y 460.;
      text (rgb 200 190 170) 1.5
        ("black is " ^ engine_name (match g.engine with Some e -> e | None -> engine_of computer.flags) ^ "   (a: another)")
      |> move_y 425.;
      text white 2.5 (match g.last with Some mv -> status ^ Printf.sprintf "   last: %s-%s" (name mv.from) (name mv.dest) | None -> status)
      |> move_y (-445.);
      text (rgb 200 220 200) 1.8
        (Printf.sprintf "%so: moves ordered (%s)   c: quiescence (%s)   u: undo"
           (match g.nodes with Some n -> Printf.sprintf "%d moves ahead: %d positions   " depth n | None -> "")
           (if g.ordered then "on" else "off") (if g.quiescence then "on" else "off"))
      |> move_y (-478.) ]

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
