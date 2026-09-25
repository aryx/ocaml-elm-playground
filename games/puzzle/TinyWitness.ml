(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Witness (Jonathan Blow, Thekla, 2016): an
 * island of panels, each a grid on which you draw one line, from the
 * round start to the rounded end. Arrow keys draw it (back over itself
 * to take it back), space at the end submits it, r starts again. A
 * right line lights the panel and powers the next one down the cable.
 *
 * The Witness never says a word: no tutorial, no text, each rule is
 * taught by a row of panels, the first so easy that only the rule can
 * be what it is about, the next ones turning it round. Its island had
 * about 650 panels, a dozen kinds of symbols, and puzzles hidden in the
 * landscape itself (a line of trees, seen from the right place). Jonathan
 * Blow made it after Braid (2008, see TinyBraid), in seven years.
 * (Names and dates from memory, to check.) Here, seven panels and three
 * rules:
 *   - the maze: some edges are broken, the line can't cross a gap;
 *   - the dots: the line must pass over every one;
 *   - the black and white squares: the line must separate them, no
 *     region with both colours in it.
 *
 * The new idea here is a rule about regions: the line, with the
 * panel's border, cuts the grid into regions, and some rules are about
 * what each region holds. The regions are found by a flood fill over
 * the cells ([regions]), a cell reaching its neighbour unless the line
 * runs between them -- the line as walls. The rules are checked only
 * at the end, on the whole line ([check]): the line itself is only a
 * self-avoiding walk on the grid's corners, and the puzzle is all in
 * the checking.
 *
 *      +-+=E     a 2 x 2 panel: the line (= and ") from S up the
 *      |B"W|     middle cuts it in two regions, the B's and the W's:
 *      +-"-+     right. Up the left side and along the top instead,
 *      |B"W|     the whole panel is one region holding both colours:
 *      S=+-+     wrong, and the squares blink.
 *
 * And a panel is typed as text, its grid drawn in characters ([panels]):
 * the corners at even positions, the edges between them, the cells at
 * odd positions -- the level as data again, and a text any editor
 * shows as the picture it is.
 *
 * What it uses: Scene2d (the title, the panels, the end), Audio. Not
 * Tilemap: the panel's text is read directly, its three kinds of
 * positions not the tiles of one grid.
 *
 * Exercises: the mouse, as the original (the line follows the pointer
 * along the grid); the other symbols (the stars, a pair of each colour
 * per region; the Tetris pieces, a region exactly their shape; the
 * elimination mark); symmetry panels, two lines drawn at once; a panel
 * whose answer is in the landscape behind it.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The panels *)
(*****************************************************************************)

(* a panel as text: at even (x, y) the corners, '+' (or S the start, E
 * the end, o with a dot); between two corners the edges, '-' and '|'
 * (o with a dot, ' ' broken); at odd (x, y) the cells, B and W the
 * squares. Every panel checked solvable by a search over all its lines;
 * the maze (a spanning tree of the corners, so one way only) generated
 * the same way. *)
let panels : string array list =
  List.map Array.of_list
    [ [ "+-+-E";
        "| | |";
        "+-+-+";
        "| | |";
        "S-+-+" ];
      [ "+ +-+-+-E";
        "|   |    ";
        "+-+ +-+-+";
        "  | |   |";
        "+ + + +-+";
        "| |   | |";
        "+ +-+-+ +";
        "|       |";
        "S-+-+-+-+" ];
      [ "+-+-+-E";
        "| | | |";
        "o-+-+-+";
        "| | o |";
        "+-+-+-+";
        "| | | |";
        "S-+-o-+" ];
      [ "+ + +-+-E";
        "| | |   |";
        "+ +-o +-+";
        "| | | | |";
        "+-+ +-+ +";
        "o | | | |";
        "+-+ + + +";
        "| | | | o";
        "S-+-+ +-+" ];
      [ "+-+-E";
        "|B|W|";
        "+-+-+";
        "|B|W|";
        "S-+-+" ];
      [ "+-+-+-E";
        "|B|B|W|";
        "+-+-+-+";
        "|W| |B|";
        "+-+-+-+";
        "|W|W|B|";
        "S-+-+-+" ];
      [ "+-+-+-+-E";
        "|W| | |B|";
        "+-+-+-+-+";
        "| |W| | |";
        "+-+-o-+-+";
        "| | |B| |";
        "+-+-+-+-+";
        "|B| | |W|";
        "S-+-+-o-+" ] ]

type pos = int * int

let at (p : string array) ((x, y) : pos) : char =
  if y < 0 || y >= Array.length p || x < 0 || x >= String.length p.(y) then ' ' else p.(y).[x]

let width (p : string array) : int = Array.fold_left (fun w r -> max w (String.length r)) 0 p
let height (p : string array) : int = Array.length p

let all_positions (p : string array) : pos list =
  List.concat (List.init (height p) (fun y -> List.init (width p) (fun x -> (x, y))))

let find (p : string array) (c : char) : pos list = List.filter (fun q -> at p q = c) (all_positions p)
let is_corner ((x, y) : pos) : bool = x mod 2 = 0 && y mod 2 = 0
let is_cell ((x, y) : pos) : bool = x mod 2 = 1 && y mod 2 = 1
let start (p : string array) : pos = List.hd (find p 'S')

(* the edge between two neighbouring corners, or two neighbouring cells:
 * the position halfway *)
let between ((x1, y1) : pos) ((x2, y2) : pos) : pos = ((x1 +.. x2) /.. 2, (y1 +.. y2) /.. 2)

(*****************************************************************************)
(* The line, and the rules *)
(*****************************************************************************)

(* the line, its end first *)
type line = pos list

let edges (l : line) : pos list =
  let rec go = function a :: (b :: _ as rest) -> between a b :: go rest | _ -> [] in
  go l

(* [extend p l (dx, dy)]: the line one corner further, if the edge is
 * there (not broken, not outside) and the corner not already on it;
 * back onto the corner before, it's shortened instead *)
let extend (p : string array) (l : line) ((dx, dy) : pos) : line =
  match l with
  | [] -> l
  | (x, y) :: rest ->
      let next = (x +.. (2 *.. dx), y +.. (2 *.. dy)) in
      (match rest with
       | prev :: _ when prev = next -> rest
       | _ -> if at p (between (x, y) next) = ' ' || at p next = ' ' || List.mem next l then l else next :: l)

(* [regions p l]: the cells in groups, the line's edges as walls: a
 * flood fill from each cell not yet in a group, stepping to the four
 * neighbours unless the edge between is on the line *)
let regions (p : string array) (l : line) : pos list list =
  let walls = edges l in
  let cells = List.filter is_cell (all_positions p) in
  let rec fill seen group = function
    | [] -> (seen, group)
    | c :: todo ->
        let x, y = c in
        let next =
          List.filter
            (fun n -> List.mem n cells && (not (List.mem n seen)) && not (List.mem (between c n) walls))
            [ (x +.. 2, y); (x -.. 2, y); (x, y +.. 2); (x, y -.. 2) ]
        in
        fill (next @ seen) (c :: group) (next @ todo)
  in
  let _, groups =
    List.fold_left
      (fun (seen, groups) c -> if List.mem c seen then (seen, groups) else let seen, g = fill (c :: seen) [] [ c ] in (seen, g :: groups))
      ([], []) cells
  in
  groups

(* [check p l]: what's wrong with a line ended at E, [] if nothing: the
 * dots not passed over, the squares of every region holding both
 * colours *)
let check (p : string array) (l : line) : pos list =
  let on_line = l @ edges l in
  let missed = List.filter (fun d -> not (List.mem d on_line)) (find p 'o') in
  let mixed =
    List.concat_map
      (fun region ->
        let squares = List.filter (fun c -> at p c = 'B' || at p c = 'W') region in
        let colours = List.sort_uniq compare (List.map (at p) squares) in
        if List.length colours > 1 then squares else [])
      (regions p l)
  in
  missed @ mixed

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state = Drawing | Wrong of pos list * int (* what's wrong, frames since *) | Right of int

type game = { index : int; line : line; state : state; frames : int }
type scene = Title | Playing of game | Island
type model = scene Scene2d.t

let panel (g : game) : string array = List.nth panels g.index
let new_panel (i : int) : game = { index = i; line = [ start (List.nth panels i) ]; state = Drawing; frames = 0 }
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (scenes : model) (g : game) : game =
  let pressed k = Scene2d.pressed k scenes in
  let p = panel g in
  let g = { g with frames = g.frames +.. 1 } in
  match g.state with
  | Right n -> { g with state = Right (n +.. 1) }
  | Wrong (bad, n) -> if n > 80 then { g with state = Drawing; line = [ start p ] } else { g with state = Wrong (bad, n +.. 1) }
  | Drawing ->
      if pressed (fun k -> Set_.mem "r" k.keys) then { g with line = [ start p ] }
      else if pressed (fun k -> k.kspace) then
        if at p (List.hd g.line) <> 'E' then g
        else (
          match check p g.line with
          | [] -> Audio.play Audio.coin; { g with state = Right 0 }
          | bad -> Audio.play Audio.hit; { g with state = Wrong (bad, 0) })
      else
        let dir =
          if pressed (fun k -> k.kleft) then Some (-1, 0) else if pressed (fun k -> k.kright) then Some (1, 0)
          else if pressed (fun k -> k.kup) then Some (0, -1) else if pressed (fun k -> k.kdown) then Some (0, 1) else None
        in
        match dir with
        | Some d ->
            let line = extend p g.line d in
            if line <> g.line then Audio.play (Audio.square 660. |> Audio.lasting 0.03 |> Audio.fading);
            { g with line }
        | None -> g

let update (_computer : computer) (s : model) : model =
  let s = Scene2d.update _computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_panel 0)) s else s
  | Playing g -> (
      let g = update_game s g in
      match g.state with
      (* the next panel powered, after a moment to look at the right line *)
      | Right n when n > 60 -> if g.index +.. 1 >= List.length panels then Scene2d.go Island s else Scene2d.go (Playing (new_panel (g.index +.. 1))) s
      | _ -> { s with scene = Playing g })
  | Island -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let panel_size = 440.

(* the spacing between two corners, and where a position of the text is
 * drawn: the panel centered on (0, [cy]) *)
let spacing (p : string array) : number = panel_size / float_of_int (max (width p -.. 1) (height p -.. 1) /.. 2)
let cy = 60.

let place (p : string array) ((x, y) : pos) : number * number =
  let sp = spacing p / 2. in
  ((float_of_int x - (float_of_int (width p -.. 1) / 2.)) * sp, cy + ((float_of_int (height p -.. 1) / 2.) - float_of_int y) * sp)

(* a thick segment between two points, rounded by circles at its ends *)
let segment (c : color) (w : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  group [ rectangle c (Float.abs (x2 - x1) + w) (Float.abs (y2 - y1) + w) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.) ]

(* the end's nub: a stub out of the border, the way the end corner faces *)
let nub (p : string array) ((x, y) : pos) : number * number =
  let px, py = place p (x, y) in
  let len = spacing p * 0.3 in
  if y = 0 then (px, py + len) else if y = height p -.. 1 then (px, py - len) else if x = 0 then (px - len, py) else (px + len, py)

let view_panel (g : game) : shape list =
  let p = panel g in
  let sp = spacing p in
  let lw = sp * 0.2 in
  let track = rgb 100 130 140 in
  let corners = List.filter is_corner (all_positions p) |> List.filter (fun q -> at p q <> ' ') in
  (* the grid: its edges, the broken ones as two stubs *)
  let grid =
    List.concat_map
      (fun q ->
        let x, y = q in
        List.filter_map
          (fun n ->
            if not (List.mem n corners) then None
            else
              let a, b = (place p q, place p n) in
              match at p (between q n) with
              | ' ' ->
                  let stub (ax, ay) (bx, by) = segment track lw (ax, ay) (ax + ((bx - ax) * 0.3), ay + ((by - ay) * 0.3)) in
                  Some (group [ stub a b; stub b a ])
              | _ -> Some (segment track lw a b))
          [ (x +.. 2, y); (x, y +.. 2) ])
      corners
  in
  let e = List.hd (find p 'E') in
  let blink_bad = match g.state with Wrong (bad, n) when n mod 16 < 8 -> bad | _ -> [] in
  let symbols =
    List.filter_map
      (fun q ->
        let x, y = place p q in
        let bad = List.mem q blink_bad in
        match at p q with
        | 'o' -> Some (hexagon (if bad then red else rgb 30 30 30) (lw * 0.38) |> move x y)
        | 'B' -> Some (square (if bad then red else rgb 20 20 20) (sp * 0.32) |> move x y)
        | 'W' -> Some (square (if bad then red else rgb 245 245 245) (sp * 0.32) |> move x y)
        | _ -> None)
      (all_positions p)
  in
  (* the line: bright while drawn, red and fading when wrong, glowing
   * when right; into the nub once it's there *)
  let colour, alpha =
    match g.state with Drawing -> (rgb 250 240 190, 1.) | Wrong (_, n) -> (rgb 230 60 50, 1. - (float_of_int n / 90.)) | Right _ -> (rgb 255 255 230, 1.)
  in
  let points = List.map (place p) g.line in
  let points = if at p (List.hd g.line) = 'E' && g.state <> Drawing then nub p e :: points else points in
  let rec pairs = function a :: (b :: _ as rest) -> segment colour (lw * 0.9) a b :: pairs rest | _ -> [] in
  let sx, sy = place p (start p) in
  let hx, hy = List.hd points in
  let w = (float_of_int (width p -.. 1) / 2. * sp / 2.) + 70. in
  let h = (float_of_int (height p -.. 1) / 2. * sp / 2.) + 70. in
  [ rectangle (rgb 90 90 90) 30. 300. |> move_y (cy - 300.);
    rectangle (rgb 25 45 55) (w * 2.) (h * 2.) |> move_y cy;
    rectangle (rgb 40 70 80) ((w * 2.) - 16.) ((h * 2.) - 16.) |> move_y cy ]
  @ grid
  @ [ segment track lw (place p e) (nub p e); circle track (lw * 1.25) |> move sx sy ]
  @ symbols
  @ [ group ((circle colour (lw * 1.25) |> move sx sy) :: (circle colour (lw * 0.45) |> move hx hy) :: pairs points) |> fade alpha ]

(* the panels in a row below, a cable between them, lit up to the one
 * being played *)
let view_progress (g : game) : shape list =
  let n = List.length panels in
  let x i = (float_of_int i - (float_of_int (n -.. 1) / 2.)) * 70. in
  let lit i = i < g.index || (i = g.index && match g.state with Right _ -> true | _ -> false) in
  [ rectangle (rgb 60 60 60) (x (n -.. 1) - x 0) 4. |> move_y (-380.) ]
  @ List.concat
      (List.init n (fun i ->
           [ rectangle (if lit i then rgb 255 220 120 else rgb 60 60 60) 70. 4. |> move ((x i) + 35.) (-380.) |> fade (if i < n -.. 1 then 1. else 0.);
             square (if i = g.index then rgb 220 220 220 else rgb 80 80 80) 34. |> move (x i) (-380.);
             square (if lit i then rgb 255 220 120 else rgb 40 70 80) 26. |> move (x i) (-380.) ]))

let backdrop (screen : screen) : shape list =
  [ rectangle (rgb 150 200 230) screen.width screen.height; rectangle (rgb 60 120 170) screen.width 200. |> move_y (-300.);
    oval (rgb 120 170 90) 1400. 300. |> move_y (-380.); circle (rgb 255 250 220) 50. |> move 380. 380. ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  backdrop screen
  @
  match s.scene with
  | Title ->
      view_panel (new_panel 0) @ view_progress (new_panel 0)
      @ [ rectangle black 700. 200. |> fade 0.7 |> move_y 60.; text white 6. "TINY WITNESS" |> move_y 110.;
          text white 2.2 "arrows draw the line   space at the end   r again" |> move_y 50. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-5.) ]
  | Playing g -> view_panel g @ view_progress g
  | Island ->
      [ text (rgb 40 70 80) 5. "THE ISLAND IS QUIET" |> move_y 60.; text (rgb 40 70 80) 2.5 "every panel lit, and not one word said" |> move_y (-10.) ]
      @ Scene2d.blink 1. s [ text (rgb 40 70 80) 3. "PRESS SPACE" |> move_y (-100.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app