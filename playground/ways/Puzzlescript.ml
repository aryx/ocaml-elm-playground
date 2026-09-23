(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Puzzlescript.mli *)

(*****************************************************************************)
(* The four directions *)
(*****************************************************************************)

type dir = Up | Down | Left | Right

let every = [ Up; Down; Left; Right ]
let horizontal = [ Left; Right ]
let vertical = [ Up; Down ]

(* a map's rows go down the screen, as in Tilemap *)
let delta (d : dir) : int * int =
  match d with Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0)

let opposite (d : dir) : dir =
  match d with Up -> Down | Down -> Up | Left -> Right | Right -> Left

(*****************************************************************************)
(* Things, and the layers they live on *)
(*****************************************************************************)

type thing = { glyph : char; color : color; layer : int; art : string list }

let thing ?(art = []) (glyph : char) (color : color) ~(layer : int) : thing = { glyph; color; layer; art }

(*****************************************************************************)
(* Rules *)
(*****************************************************************************)

(* what a token says about the thing it names, besides its being there *)
type spec =
  | Any (* the thing, however it is *)
  | Forward (* moving the way the rule is being read *)
  | Backward
  | Moving (* moving some way *)
  | Stationary
  | Absent (* "no": the thing is not here *)

type token = { spec : spec; glyph : char }
type cellpat = token list
type rule = { before : cellpat list; after : cellpat list; dirs : dir list; late : bool }

(* [parse_cell "> @"]: the tokens of one cell of a pattern. A word is
 * either a thing's character or one of the prefixes below, which then
 * says how to read the word after it. *)
let parse_cell (s : string) : cellpat =
  let words = String.split_on_char ' ' s |> List.concat_map (String.split_on_char '\t') |> List.filter (fun w -> w <> "") in
  let rec go (spec : spec) (words : string list) : cellpat =
    match words with
    | [] -> []
    | w :: rest -> (
        match w with
        | ">" -> go Forward rest
        | "<" -> go Backward rest
        | "moving" -> go Moving rest
        | "stationary" -> go Stationary rest
        | "no" -> go Absent rest
        | g when String.length g = 1 -> { spec; glyph = g.[0] } :: go Any rest
        | g -> failwith (Printf.sprintf "Puzzlescript: %S is not a thing's character, nor a prefix" g))
  in
  go Any words

(* the cells of one side of a rule, "a | b | c" *)
let parse_side (s : string) : cellpat list = String.split_on_char '|' s |> List.map parse_cell

(* [split_arrow "a -> b"]: the two sides, at the first arrow *)
let split_arrow (s : string) : string * string =
  let n = String.length s in
  let rec find i = if i + 1 >= n then None else if s.[i] = '-' && s.[i + 1] = '>' then Some i else find (i + 1) in
  match find 0 with
  | None -> failwith (Printf.sprintf "Puzzlescript: no -> in the rule %S" s)
  | Some i -> (String.sub s 0 i, String.sub s (i + 2) (n - i - 2))

let rule ?(dirs = every) ?(late = false) (s : string) : rule =
  let before, after = split_arrow s in
  let before = parse_side before and after = parse_side after in
  if List.length before <> List.length after then
    failwith (Printf.sprintf "Puzzlescript: the two sides of %S have different numbers of cells" s);
  { before; after; dirs; late }

(*****************************************************************************)
(* Winning *)
(*****************************************************************************)

type win = All_on of char * char | Some_on of char * char | No_on of char * char | None_left of char

let all_on (a : char) (b : char) : win = All_on (a, b)
let some_on (a : char) (b : char) : win = Some_on (a, b)
let no_on (a : char) (b : char) : win = No_on (a, b)
let none_left (a : char) : win = None_left a

(*****************************************************************************)
(* A game *)
(*****************************************************************************)

type t = {
  things : thing list;
  player : char;
  legend : (char * string) list;
  rules : rule list;
  wins : win list;
  levels : string list list;
  background : color;
}

let make ~(things : thing list) ~(player : char) ?(legend = []) ~(rules : rule list) ~(wins : win list)
    ?(background = rgb 24 24 30) (levels : string list list) : t =
  { things; player; legend; rules; wins; levels; background }

let thing_of (g : t) (glyph : char) : thing =
  match List.find_opt (fun (th : thing) -> th.glyph = glyph) g.things with
  | Some th -> th
  | None -> failwith (Printf.sprintf "Puzzlescript: no thing for the character %C" glyph)

let layer_of (g : t) (glyph : char) : int = (thing_of g glyph).layer
let layers (g : t) : int = List.fold_left (fun n (th : thing) -> max n (th.layer + 1)) 1 g.things

(*****************************************************************************)
(* The board *)
(*****************************************************************************)

(* one thing, and where it is trying to go this turn *)
type slot = { what : char; moving : dir option }
type board = { w : int; h : int; nlayers : int; cells : slot option array array }

let at_index (b : board) (x : int) (y : int) : int = (y * b.w) + x
let inside (b : board) (x : int) (y : int) : bool = x >= 0 && y >= 0 && x < b.w && y < b.h
let get (b : board) (x : int) (y : int) (l : int) : slot option = b.cells.(at_index b x y).(l)
let set (b : board) (x : int) (y : int) (l : int) (s : slot option) : unit = b.cells.(at_index b x y).(l) <- s
let copy (b : board) : board = { b with cells = Array.map Array.copy b.cells }
let same (a : board) (b : board) : bool = a.cells = b.cells

(* what a map character puts in a cell: what the legend says, or the
 * thing of that character, or nothing *)
let spelled (g : t) (c : char) : char list =
  match List.assoc_opt c g.legend with
  | Some glyphs -> List.init (String.length glyphs) (String.get glyphs)
  | None -> if List.exists (fun (th : thing) -> th.glyph = c) g.things then [ c ] else []

let board (g : t) (level : int) : board =
  let rows = List.nth g.levels level in
  let w = List.fold_left (fun n row -> max n (String.length row)) 0 rows in
  let h = List.length rows in
  let nlayers = layers g in
  let b = { w; h; nlayers; cells = Array.init (w * h) (fun _ -> Array.make nlayers None) } in
  List.iteri
    (fun y row ->
      String.iteri (fun x c -> List.iter (fun glyph -> set b x y (layer_of g glyph) (Some { what = glyph; moving = None })) (spelled g c)) row)
    rows;
  b

(* the things in a cell, from the lowest layer up *)
let at (b : board) (x : int) (y : int) : char list =
  if not (inside b x y) then []
  else Array.to_list b.cells.(at_index b x y) |> List.filter_map (Option.map (fun s -> s.what))

(* the board back as rows of characters, the legend used where a cell
 * holds exactly what one of its entries spells *)
let to_strings (g : t) (b : board) : string list =
  let char_of x y =
    let here = List.sort compare (at b x y) in
    match List.find_opt (fun (c, _) -> List.sort compare (spelled g c) = here) g.legend with
    | Some (c, _) -> c
    | None -> ( match List.rev (at b x y) with top :: _ -> top | [] -> ' ')
  in
  List.init b.h (fun y -> String.init b.w (fun x -> char_of x y))

(*****************************************************************************)
(* Matching a rule *)
(*****************************************************************************)

(* does this cell hold what the tokens ask for? *)
let cell_matches (g : t) (b : board) (d : dir) (x : int) (y : int) (pat : cellpat) : bool =
  List.for_all
    (fun tok ->
      match get b x y (layer_of g tok.glyph) with
      | Some s when s.what = tok.glyph -> (
          match tok.spec with
          | Absent -> false
          | Any -> true
          | Forward -> s.moving = Some d
          | Backward -> s.moving = Some (opposite d)
          | Moving -> s.moving <> None
          | Stationary -> s.moving = None)
      | _ -> tok.spec = Absent)
    pat

(* the cells of [pat], read from (x, y) in the direction [d] *)
let matches (g : t) (b : board) (d : dir) (x : int) (y : int) (pat : cellpat list) : bool =
  let dx, dy = delta d in
  List.for_all
    (fun (i, cell) ->
      let cx = x + (i * dx) and cy = y + (i * dy) in
      inside b cx cy && cell_matches g b d cx cy cell)
    (List.mapi (fun i cell -> (i, cell)) pat)

(* writing one cell of the right-hand side. A thing named in the left
 * side but not in the right one, on the same layer, is taken away --
 * that is how a rule makes something disappear. *)
let write_cell (g : t) (b : board) (d : dir) (x : int) (y : int) (before : cellpat) (after : cellpat) : unit =
  List.iter
    (fun tok ->
      if tok.spec <> Absent && not (List.exists (fun t2 -> layer_of g t2.glyph = layer_of g tok.glyph) after) then
        set b x y (layer_of g tok.glyph) None)
    before;
  List.iter
    (fun tok ->
      let l = layer_of g tok.glyph in
      let was = match get b x y l with Some s when s.what = tok.glyph -> s.moving | _ -> None in
      match tok.spec with
      | Absent -> set b x y l None
      | Any -> set b x y l (Some { what = tok.glyph; moving = was })
      | Moving -> set b x y l (Some { what = tok.glyph; moving = (match was with Some _ -> was | None -> Some d) })
      | Stationary -> set b x y l (Some { what = tok.glyph; moving = None })
      | Forward -> set b x y l (Some { what = tok.glyph; moving = Some d })
      | Backward -> set b x y l (Some { what = tok.glyph; moving = Some (opposite d) }))
    after

let apply (g : t) (b : board) (d : dir) (x : int) (y : int) (r : rule) : unit =
  let dx, dy = delta d in
  List.iteri (fun i (before, after) -> write_cell g b d (x + (i * dx)) (y + (i * dy)) before after)
    (List.map2 (fun a b -> (a, b)) r.before r.after)

(* every rule, at every cell, in every direction it may be read: once *)
let pass (g : t) (b : board) ~(late : bool) : unit =
  List.iter
    (fun r ->
      if r.late = late then
        List.iter
          (fun d ->
            for y = 0 to b.h - 1 do
              for x = 0 to b.w - 1 do
                if matches g b d x y r.before then apply g b d x y r
              done
            done)
          r.dirs)
    g.rules

(* Rules are applied again and again until the board stops changing --
 * PuzzleScript's loop. A rule that undoes its own work would spin for
 * ever, so the number of passes is capped. *)
let max_passes = 40

let run_rules (g : t) (b : board) ~(late : bool) : unit =
  let rec go n =
    if n < max_passes then begin
      let was = copy b in
      pass g b ~late;
      if not (same was b) then go (n + 1)
    end
  in
  go 0

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

(* Whatever is marked as moving now tries to go one cell that way. It
 * can if the cell it is going to has room on its own layer -- which is
 * the only collision rule there is, and why two things that must not
 * share a cell (a wall and a crate) are put on the same layer.
 *
 * A thing that moves loses its mark, so it moves once per turn; then
 * the whole board is swept again, because the cell one thing leaves may
 * be the one another was waiting for:
 *
 *      > @ > $ .      the crate has room, the hero has not: the crate
 *      . @ > $ .      goes first, and the hero follows on the next
 *      . . @ $ .      sweep. Push, from one rule and this loop. *)
let resolve_moves (b : board) : unit =
  let moved = ref true in
  while !moved do
    moved := false;
    for y = 0 to b.h - 1 do
      for x = 0 to b.w - 1 do
        for l = 0 to b.nlayers - 1 do
          match get b x y l with
          | Some { what; moving = Some d } ->
              let dx, dy = delta d in
              let nx = x + dx and ny = y + dy in
              if inside b nx ny && get b nx ny l = None then begin
                set b nx ny l (Some { what; moving = None });
                set b x y l None;
                moved := true
              end
          | _ -> ()
        done
      done
    done
  done;
  (* what could not go anywhere simply stays: the mark is dropped *)
  Array.iter (fun cell -> Array.iteri (fun l s -> match s with Some s -> cell.(l) <- Some { s with moving = None } | None -> ()) cell) b.cells

(*****************************************************************************)
(* A turn *)
(*****************************************************************************)

(* The whole game loop, and there is no more to it than this: mark what
 * the player asked to move, let the rules have their say, move what is
 * still marked, then let the late rules tidy up. *)
let turn (g : t) (input : dir option) (b : board) : board =
  let b = copy b in
  (match input with
  | None -> ()
  | Some d ->
      for y = 0 to b.h - 1 do
        for x = 0 to b.w - 1 do
          for l = 0 to b.nlayers - 1 do
            match get b x y l with
            | Some s when s.what = g.player -> set b x y l (Some { s with moving = Some d })
            | _ -> ()
          done
        done
      done);
  run_rules g b ~late:false;
  resolve_moves b;
  run_rules g b ~late:true;
  b

(*****************************************************************************)
(* Winning *)
(*****************************************************************************)

let cells_with (b : board) (glyph : char) : (int * int) list =
  List.concat_map (fun y -> List.filter_map (fun x -> if List.mem glyph (at b x y) then Some (x, y) else None) (List.init b.w Fun.id)) (List.init b.h Fun.id)

let won (g : t) (b : board) : bool =
  let on a bb = List.filter (fun (x, y) -> List.mem bb (at b x y)) (cells_with b a) in
  g.wins <> []
  && List.for_all
       (fun w ->
         match w with
         | All_on (a, bb) -> List.length (on a bb) = List.length (cells_with b a) && cells_with b a <> []
         | Some_on (a, bb) -> on a bb <> []
         | No_on (a, bb) -> on a bb = []
         | None_left a -> cells_with b a = [])
       g.wins

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

(* the level being played, the boards before this one (for undo), and
 * whether it is over. The history is the same one-line trick as
 * gamekits/puzzle/Undo.mli, written out here so that playground/ needs
 * nothing from gamekits/. *)
type level = { n : int; now : board; past : board list; over : bool }
type state = level Scene2d.t

let load (g : t) (n : int) : level = { n; now = board g n; past = []; over = false }

let key (k : keyboard) : dir option =
  if k.kup || k.kw then Some Up
  else if k.kdown || k.ks then Some Down
  else if k.kleft || k.ka then Some Left
  else if k.kright || k.kd then Some Right
  else None

let update_game (g : t) (computer : computer) (s : state) : state =
  let s = Scene2d.update computer s in
  let l = s.scene in
  let pressed f = Scene2d.pressed f s in
  if l.over then
    (* the next level, or around again *)
    if pressed (fun k -> k.kspace) then { s with scene = load g ((l.n + 1) mod List.length g.levels) } else s
  else if pressed (fun k -> Set_.mem "r" k.keys) then { s with scene = load g l.n }
  else if pressed (fun k -> Set_.mem "z" k.keys) then
    match l.past with [] -> s | b :: past -> { s with scene = { l with now = b; past } }
  else
    match List.find_opt (fun d -> pressed (fun k -> key k = Some d)) every with
    | None -> s
    | Some d ->
        let next = turn g (Some d) l.now in
        if same next l.now then s
        else { s with scene = { l with now = next; past = l.now :: l.past; over = won g next } }

(*****************************************************************************)
(* Drawing it *)
(*****************************************************************************)

(* a thing is a square of its colour, or its own little picture *)
let thing_shape (th : thing) (size : number) : shape =
  match th.art with
  | [] -> square th.color (size *. 0.94)
  | rows ->
      let pixel = size /. float_of_int (List.length rows) in
      Sprite.pixels pixel (List.map (fun c -> (c, th.color)) [ '#'; 'x'; 'o'; '0'; '1'; '*' ]) rows

let view_board (g : t) (computer : computer) (b : board) : shape list =
  let screen = computer.screen in
  let size = Float.min (screen.width *. 0.9 /. float_of_int b.w) (screen.height *. 0.8 /. float_of_int b.h) in
  let left = -.(float_of_int b.w *. size) /. 2. and top = float_of_int b.h *. size /. 2. in
  let place x y shape = shape |> move (left +. ((float_of_int x +. 0.5) *. size)) (top -. ((float_of_int y +. 0.5) *. size)) in
  List.concat_map
    (fun y ->
      List.concat_map
        (fun x -> List.map (fun glyph -> place x y (thing_shape (thing_of g glyph) size)) (at b x y))
        (List.init b.w Fun.id))
    (List.init b.h Fun.id)

let view_game (g : t) (computer : computer) (s : state) : shape list =
  let screen = computer.screen in
  let l = s.scene in
  let text c size str = words c str |> scale size in
  (rectangle g.background screen.width screen.height :: view_board g computer l.now)
  @ [ text (rgb 150 150 160) 1.6
        (Printf.sprintf "level %d of %d    arrows move    z undo    r restart" (l.n + 1) (List.length g.levels))
      |> move_y (screen.bottom +. 30.) ]
  @
  if not l.over then []
  else
    [ rectangle black 600. 120. |> fade 0.85 |> move_y 0.; text (rgb 250 220 120) 4. "SOLVED" |> move_y 20. ]
    @ Scene2d.blink 1. s [ text white 2. "press space" |> move_y (-30.) ]

let play (g : t) : (state Playground.game, msg) app =
  if g.levels = [] then failwith "Puzzlescript: a game needs at least one level";
  game (view_game g) (update_game g) (Scene2d.start (load g 0))
