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
 * alpha-beta (ai/Minimax.mli, the same search as games/AiOthello.ml).
 * You are white: click a piece, then where it goes (or move the cursor
 * with the arrows, and space twice); a pawn reaching the last rank
 * becomes a queen. u takes back your last move and the computer's
 * answer; space after the end plays again.
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
 * What it uses: ai/'s Minimax (alpha-beta: the ordering is the order
 * of [moves], the quiescence its [leaf]), Scene2d (the
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

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

type color = White | Black
type kind = Pawn | Knight | Bishop | Rook | Queen | King
type piece = { color : color; kind : kind }

(* the 64 squares, row by row from the top: 0 is a8, 63 is h1 *)
type position = {
  board : piece option array;
  turn : color;
  (* who may still castle, king side and queen side *)
  white_short : bool;
  white_long : bool;
  black_short : bool;
  black_long : bool;
  (* the square a pawn just jumped over, where it can be taken *)
  en_passant : int option;
  ply : int; (* the half-moves played *)
}

type move = { from : int; dest : int; promotion : kind option }

let other = function White -> Black | Black -> White
let row (i : int) = i / 8
let col (i : int) = i mod 8
let on_board r c = r >= 0 && r < 8 && c >= 0 && c < 8

(* which way a pawn walks: white up the board, to row 0 *)
let forward = function White -> -1 | Black -> 1

let name (i : int) : string = Printf.sprintf "%c%d" (Char.chr (Char.code 'a' + col i)) (8 - row i)

let knight_jumps = [ (-2, -1); (-2, 1); (-1, -2); (-1, 2); (1, -2); (1, 2); (2, -1); (2, 1) ]
let straight = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
let diagonal = [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
let around = straight @ diagonal

(* FEN, e.g. the start: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w
 * KQkq - 0 1": the rows from the top, a digit for empty squares, white
 * in capitals; who plays; who may castle; the en passant square *)
let of_fen (fen : string) : position =
  let fields = String.split_on_char ' ' fen in
  let field n = match List.nth_opt fields n with Some f -> f | None -> "-" in
  let board = Array.make 64 None in
  let i = ref 0 in
  String.iter
    (fun ch ->
      match ch with
      | '/' -> ()
      | '1' .. '8' -> i := !i + (Char.code ch - Char.code '0')
      | _ ->
          let color = if Char.uppercase_ascii ch = ch then White else Black in
          let kind =
            match Char.lowercase_ascii ch with
            | 'p' -> Pawn | 'n' -> Knight | 'b' -> Bishop | 'r' -> Rook | 'q' -> Queen | _ -> King
          in
          board.(!i) <- Some { color; kind };
          incr i)
    (field 0);
  let rights = field 2 in
  let en_passant =
    match field 3 with
    | "-" -> None
    | s -> Some (((8 - (Char.code s.[1] - Char.code '0')) * 8) + (Char.code s.[0] - Char.code 'a'))
  in
  { board; turn = (if field 1 = "b" then Black else White);
    white_short = String.contains rights 'K'; white_long = String.contains rights 'Q';
    black_short = String.contains rights 'k'; black_long = String.contains rights 'q';
    en_passant; ply = 0 }

let start = of_fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

(* is square [i] attacked by [by]'s pieces? Looked at from the square:
 * a knight's jump away, a king's step, a pawn's diagonal, and along
 * each line up to the first piece *)
let attacked (board : piece option array) (i : int) (by : color) : bool =
  let r = row i and c = col i in
  let is r c kinds =
    on_board r c && match board.((r * 8) + c) with Some p -> p.color = by && List.mem p.kind kinds | None -> false
  in
  let steps deltas kinds = List.exists (fun (dr, dc) -> is (r + dr) (c + dc) kinds) deltas in
  let rec ray r c (dr, dc) kinds =
    let r = r + dr and c = c + dc in
    on_board r c && match board.((r * 8) + c) with None -> ray r c (dr, dc) kinds | Some _ -> is r c kinds
  in
  steps knight_jumps [ Knight ]
  || steps around [ King ]
  || is (r - forward by) (c - 1) [ Pawn ]
  || is (r - forward by) (c + 1) [ Pawn ]
  || List.exists (fun d -> ray r c d [ Rook; Queen ]) straight
  || List.exists (fun d -> ray r c d [ Bishop; Queen ]) diagonal

let king_square (board : piece option array) (color : color) : int =
  let rec find i = if i = 64 || board.(i) = Some { color; kind = King } then i else find (i + 1) in
  find 0

let in_check (p : position) : bool = attacked p.board (king_square p.board p.turn) (other p.turn)

(* every move of the player to play, its own king forgotten *)
let pseudo_moves (p : position) : move list =
  let b = p.board and me = p.turn in
  let moves = ref [] in
  let add from dest = moves := { from; dest; promotion = None } :: !moves in
  let add_pawn from dest =
    if row dest = 0 || row dest = 7 then
      List.iter (fun k -> moves := { from; dest; promotion = Some k } :: !moves) [ Knight; Bishop; Rook; Queen ]
    else add from dest
  in
  let enemy i = match b.(i) with Some q -> q.color <> me | None -> false in
  for i = 0 to 63 do
    match b.(i) with
    | Some { color; kind } when color = me -> (
        let r = row i and c = col i in
        let steps deltas =
          List.iter
            (fun (dr, dc) ->
              let r' = r + dr and c' = c + dc in
              if on_board r' c' then
                let j = (r' * 8) + c' in
                if b.(j) = None || enemy j then add i j)
            deltas
        in
        let slide dirs =
          List.iter
            (fun (dr, dc) ->
              let rec go r c =
                let r = r + dr and c = c + dc in
                if on_board r c then begin
                  let j = (r * 8) + c in
                  if b.(j) = None then (add i j; go r c) else if enemy j then add i j
                end
              in
              go r c)
            dirs
        in
        match kind with
        | Pawn ->
            let f = forward me in
            let one = ((r + f) * 8) + c in
            if on_board (r + f) c && b.(one) = None then begin
              add_pawn i one;
              let home = if me = White then 6 else 1 in
              let two = ((r + (2 * f)) * 8) + c in
              if r = home && b.(two) = None then add i two
            end;
            List.iter
              (fun dc ->
                if on_board (r + f) (c + dc) then
                  let j = ((r + f) * 8) + c + dc in
                  if enemy j || p.en_passant = Some j then add_pawn i j)
              [ -1; 1 ]
        | Knight -> steps knight_jumps
        | Bishop -> slide diagonal
        | Rook -> slide straight
        | Queen -> slide around
        | King ->
            steps around;
            (* castling: the squares between empty, and the king neither
             * in check nor crossing an attacked square (where it lands
             * is [legal]'s business, as for any move) *)
            let home = if me = White then 60 else 4 in
            let short, long = if me = White then (p.white_short, p.white_long) else (p.black_short, p.black_long) in
            let free js = List.for_all (fun j -> b.(j) = None) js in
            let safe js = List.for_all (fun j -> not (attacked b j (other me))) js in
            if i = home then begin
              if short && free [ i + 1; i + 2 ] && safe [ i; i + 1 ] then add i (i + 2);
              if long && free [ i - 1; i - 2; i - 3 ] && safe [ i; i - 1 ] then add i (i - 2)
            end)
    | _ -> ()
  done;
  List.rev !moves

let play (p : position) (m : move) : position =
  let b = Array.copy p.board in
  let piece = Option.get b.(m.from) in
  (* en passant: the pawn taken is beside, not on the square moved to *)
  if piece.kind = Pawn && p.en_passant = Some m.dest && col m.from <> col m.dest then
    b.((row m.from * 8) + col m.dest) <- None;
  b.(m.dest) <- Some (match m.promotion with Some kind -> { piece with kind } | None -> piece);
  b.(m.from) <- None;
  (* castling: the king moved two squares, the rook jumps over it *)
  if piece.kind = King && abs (col m.dest - col m.from) = 2 then begin
    let r = row m.from in
    let rook_from, rook_dest = if col m.dest = 6 then ((r * 8) + 7, (r * 8) + 5) else (r * 8, (r * 8) + 3) in
    b.(rook_dest) <- b.(rook_from);
    b.(rook_from) <- None
  end;
  (* the rights to castle go when the king moves, or a rook leaves (or
   * is taken on) its corner *)
  let untouched corner = m.from <> corner && m.dest <> corner in
  let king_stays color = not (piece.kind = King && piece.color = color) in
  { board = b;
    turn = other p.turn;
    white_short = p.white_short && king_stays White && untouched 63;
    white_long = p.white_long && king_stays White && untouched 56;
    black_short = p.black_short && king_stays Black && untouched 7;
    black_long = p.black_long && king_stays Black && untouched 0;
    en_passant = (if piece.kind = Pawn && abs (row m.dest - row m.from) = 2 then Some ((m.from + m.dest) / 2) else None);
    ply = p.ply + 1 }

(* a move that doesn't leave your own king attacked *)
let is_legal (p : position) (m : move) : bool =
  let q = play p m in
  not (attacked q.board (king_square q.board p.turn) q.turn)

let legal (p : position) : move list = List.filter (is_legal p) (pseudo_moves p)

(* whether there is one: the first found is enough *)
let can_move (p : position) : bool = List.exists (is_legal p) (pseudo_moves p)

(* the positions [depth] moves ahead: the move generator's test *)
let rec perft (p : position) (depth : int) : int =
  if depth = 0 then 1 else List.fold_left (fun n m -> n + perft (play p m) (depth - 1)) 0 (legal p)

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

let value = function Pawn -> 100 | Knight -> 320 | Bishop -> 330 | Rook -> 500 | Queen -> 900 | King -> 20000

(* what each square is worth to a white piece, row 0 the far side;
 * black's are the same, upside down *)
let table = function
  | Pawn ->
      [| 0; 0; 0; 0; 0; 0; 0; 0;
         50; 50; 50; 50; 50; 50; 50; 50;
         10; 10; 20; 30; 30; 20; 10; 10;
         5; 5; 10; 25; 25; 10; 5; 5;
         0; 0; 0; 20; 20; 0; 0; 0;
         5; -5; -10; 0; 0; -10; -5; 5;
         5; 10; 10; -20; -20; 10; 10; 5;
         0; 0; 0; 0; 0; 0; 0; 0 |]
  | Knight ->
      [| -50; -40; -30; -30; -30; -30; -40; -50;
         -40; -20; 0; 0; 0; 0; -20; -40;
         -30; 0; 10; 15; 15; 10; 0; -30;
         -30; 5; 15; 20; 20; 15; 5; -30;
         -30; 0; 15; 20; 20; 15; 0; -30;
         -30; 5; 10; 15; 15; 10; 5; -30;
         -40; -20; 0; 5; 5; 0; -20; -40;
         -50; -40; -30; -30; -30; -30; -40; -50 |]
  | Bishop ->
      [| -20; -10; -10; -10; -10; -10; -10; -20;
         -10; 0; 0; 0; 0; 0; 0; -10;
         -10; 0; 5; 10; 10; 5; 0; -10;
         -10; 5; 5; 10; 10; 5; 5; -10;
         -10; 0; 10; 10; 10; 10; 0; -10;
         -10; 10; 10; 10; 10; 10; 10; -10;
         -10; 5; 0; 0; 0; 0; 5; -10;
         -20; -10; -10; -10; -10; -10; -10; -20 |]
  | Rook ->
      [| 0; 0; 0; 0; 0; 0; 0; 0;
         5; 10; 10; 10; 10; 10; 10; 5;
         -5; 0; 0; 0; 0; 0; 0; -5;
         -5; 0; 0; 0; 0; 0; 0; -5;
         -5; 0; 0; 0; 0; 0; 0; -5;
         -5; 0; 0; 0; 0; 0; 0; -5;
         -5; 0; 0; 0; 0; 0; 0; -5;
         0; 0; 0; 5; 5; 0; 0; 0 |]
  | Queen ->
      [| -20; -10; -10; -5; -5; -10; -10; -20;
         -10; 0; 0; 0; 0; 0; 0; -10;
         -10; 0; 5; 5; 5; 5; 0; -10;
         -5; 0; 5; 5; 5; 5; 0; -5;
         0; 0; 5; 5; 5; 5; 0; -5;
         -10; 5; 5; 5; 5; 5; 0; -10;
         -10; 0; 5; 0; 0; 0; 0; -10;
         -20; -10; -10; -5; -5; -10; -10; -20 |]
  | King ->
      [| -30; -40; -40; -50; -50; -40; -40; -30;
         -30; -40; -40; -50; -50; -40; -40; -30;
         -30; -40; -40; -50; -50; -40; -40; -30;
         -30; -40; -40; -50; -50; -40; -40; -30;
         -20; -30; -30; -40; -40; -30; -30; -20;
         -10; -20; -20; -20; -20; -20; -20; -10;
         20; 20; 0; 0; 0; 0; 20; 20;
         20; 30; 10; 0; 0; 10; 30; 20 |]

let tables = List.map (fun k -> (k, table k)) [ Pawn; Knight; Bishop; Rook; Queen; King ]

(* the board alone, for white: its material and squares, minus black's *)
let static (p : position) : int =
  let total = ref 0 in
  Array.iteri
    (fun i sq ->
      match sq with
      | Some { color = White; kind } -> total := !total + value kind + (List.assoc kind tables).(i)
      | Some { color = Black; kind } ->
          total := !total - value kind - (List.assoc kind tables).(((7 - row i) * 8) + col i)
      | None -> ())
    p.board;
  !total

(* MVV-LVA: the most valuable victim first, by the least valuable
 * attacker; a promotion as a capture of a queen; quiet moves last *)
let order (p : position) (moves : move list) : move list =
  let key m =
    let victim =
      match p.board.(m.dest) with
      | Some q -> value q.kind
      | None -> if p.en_passant = Some m.dest && (Option.get p.board.(m.from)).kind = Pawn then value Pawn else 0
    in
    let promoted = match m.promotion with Some k -> value k | None -> 0 in
    let attacker = value (Option.get p.board.(m.from)).kind in
    if victim + promoted = 0 then 0 else ((victim + promoted) * 10) - (attacker / 10)
  in
  List.stable_sort (fun a b -> compare (key b) (key a)) moves

let is_capture (p : position) (m : move) : bool =
  p.board.(m.dest) <> None || m.promotion <> None
  || (p.en_passant = Some m.dest && (Option.get p.board.(m.from)).kind = Pawn)

(* the captures played on until the board is quiet: alpha-beta over
 * captures only, for the player to play ("negamax": each side's score
 * is the other's, negated), where standing pat -- capturing nothing --
 * is always allowed; at most [limit] captures deep *)
let rec quiesce (p : position) (alpha : int) (beta : int) (limit : int) : int =
  let stand = if p.turn = White then static p else -static p in
  if stand >= beta || limit = 0 then stand
  else
    let rec loop alpha = function
      | [] -> alpha
      | m :: rest ->
          let v = -quiesce (play p m) (-beta) (-alpha) (limit - 1) in
          if v >= beta then v else loop (max alpha v) rest
    in
    (* the captures only, checked for legality only them *)
    loop (max alpha stand) (order p (List.filter (is_legal p) (List.filter (is_capture p) (pseudo_moves p))))

let mate = 100000

(* for white, MAX, a game over: a checkmate beats everything, the
 * sooner the better; a stalemate is a draw *)
let ended (p : position) : float =
  if in_check p then float_of_int (if p.turn = White then -(mate - p.ply) else mate - p.ply) else 0.

let score (p : position) : float = if can_move p then float_of_int (static p) else ended p

(* the leaves searched on, captures only, within the window alpha-beta
 * has there (Minimax.alphabeta's [leaf]): turned round for black,
 * since [quiesce] counts for the player to play *)
let quiet (p : position) ~(alpha : float) ~(beta : float) : float =
  if not (can_move p) then ended p
  else
    let bound x = if x >= float_of_int mate then mate else if x <= float_of_int (-mate) then -mate else int_of_float x in
    if p.turn = White then float_of_int (quiesce p (bound alpha) (bound beta) 6)
    else -.float_of_int (quiesce p (-bound beta) (-bound alpha) 6)

let chess ~(ordered : bool) : (position, move) Minimax.game =
  { moves = (fun p -> if ordered then order p (legal p) else legal p);
    play;
    score;
    max_to_play = (fun p -> p.turn = White) }

let depth = 3

let search ~(ordered : bool) ~(quiescence : bool) ~(depth : int) (p : position) : move Minimax.result =
  if quiescence then Minimax.alphabeta ~leaf:quiet (chess ~ordered) ~depth p
  else Minimax.alphabeta (chess ~ordered) ~depth p

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
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; before = []; cursor = 52; selected = None; last = None; wait = 0; nodes = None;
    ordered = true; quiescence = true }

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
    | p :: rest when key "u" -> { g with position = p; before = rest; selected = None; last = None; wait = 0 }
    | _ -> g
  in
  let g = { g with wait = max 0 (g.wait - 1) } in
  let p = g.position in
  if over p then g
  else if p.turn = Black then
    if g.wait > 0 then g
    else
      let a = search ~ordered:g.ordered ~quiescence:g.quiescence ~depth p in
      match a.best with
      | Some mv -> { g with position = play p mv; last = Some mv; nodes = Some a.nodes }
      | None -> g
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
        else check ^ "the computer thinks..."
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
  @ [ text white 3. "you (white) against the computer (black)" |> move_y 450.;
      text white 2.5 (match g.last with Some mv -> status ^ Printf.sprintf "   last: %s-%s" (name mv.from) (name mv.dest) | None -> status)
      |> move_y (-445.);
      text (rgb 200 220 200) 1.8
        (Printf.sprintf "%so: moves ordered (%s)   c: quiescence (%s)   u: undo"
           (match g.nodes with Some n -> Printf.sprintf "%d moves ahead: %d positions   " depth n | None -> "")
           (if g.ordered then "on" else "off") (if g.quiescence then "on" else "off"))
      |> move_y (-478.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
