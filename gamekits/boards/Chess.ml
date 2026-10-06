(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Chess: the rules, the computer of AiChess (alpha-beta with move
 * ordering and quiescence, an evaluation by material and squares),
 * and how a network reads a position. See the .mli. *)

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
(* As a network reads it *)
(*****************************************************************************)

(* a square seen from the other side of the board: a8 is a1 *)
let mirrored (i : int) : int = ((7 - row i) * 8) + col i

(* where the player to play sees a square: white as it is, black from
 * its own side, so that one's own pawns always walk up the planes *)
let seen (turn : color) (i : int) : int = if turn = White then i else mirrored i

let number = function Pawn -> 0 | Knight -> 1 | Bishop -> 2 | Rook -> 3 | Queen -> 4 | King -> 5
let planes = 17

let encode (p : position) : float array =
  let a = Array.make (planes * 64) 0. in
  Array.iteri
    (fun i sq ->
      match sq with
      | Some { color; kind } -> a.((((if color = p.turn then 0 else 6) + number kind) * 64) + seen p.turn i) <- 1.
      | None -> ())
    p.board;
  let whole plane yes = if yes then Array.fill a (plane * 64) 64 1. in
  let (my_short, my_long, their_short, their_long) =
    if p.turn = White then (p.white_short, p.white_long, p.black_short, p.black_long)
    else (p.black_short, p.black_long, p.white_short, p.white_long)
  in
  whole 12 my_short;
  whole 13 my_long;
  whole 14 their_short;
  whole 15 their_long;
  (match p.en_passant with Some i -> a.((16 * 64) + seen p.turn i) <- 1. | None -> ());
  a

let index (p : position) (m : move) : int = (seen p.turn m.from * 64) + seen p.turn m.dest

(* a pawn reaching the last rank becomes a queen, and nothing else:
 * the three other promotions would be three more moves from the same
 * square to the same square, which [index] cannot tell apart *)
let queening : (position, move) Minimax.game =
  let chess = chess ~ordered:false in
  { chess with moves = (fun p -> List.filter (fun m -> m.promotion = None || m.promotion = Some Queen) (legal p)) }

(* the pieces alone, white's less black's, the kings left out *)
let material (p : position) : int =
  Array.fold_left
    (fun total sq ->
      match sq with
      | Some { kind = King; _ } | None -> total
      | Some { color = White; kind } -> total + value kind
      | Some { color = Black; kind } -> total - value kind)
    0 p.board

let board ~(longest : int) : (position * int, move) Alphazero.board =
  {
    game =
      {
        moves = (fun (p, n) -> if n >= longest then [] else queening.moves p);
        play = (fun (p, n) m -> (play p m, n + 1));
        score =
          (fun (p, _) ->
            if not (can_move p) then ended p
            else
              (* stopped, not finished: whoever is a piece ahead has it *)
              let m = material p in
              if abs m >= value Knight then float_of_int m else 0.);
        max_to_play = (fun (p, _) -> p.turn = White);
      };
    start = (start, 0);
    inputs = planes * 64;
    moves = 64 * 64;
    encode = (fun (p, _) -> encode p);
    index = (fun (p, _) m -> index p m);
  }
