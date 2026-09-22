(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Rogue (Michael Toy, Glenn Wichman, and later Ken
 * Arnold, 1980): down through a dungeon made anew each game, room by
 * room, to the Amulet of Yendor on the third level. You are the @;
 * letters are monsters (B a bat, K a kobold, S a snake, H a hobgoblin, O
 * an orc), $ gold, ! a potion, > the stairs down, , the Amulet. Arrows
 * (or h j k l, and y u b n for the diagonals, as the original) to walk,
 * and into a monster to fight it; q to drink a potion; space to go down
 * the stairs, or to rest a turn.
 *
 * Rogue was written on the Unix machines of UC Santa Cruz and Berkeley,
 * for terminals, with the curses library Ken Arnold wrote for it; it
 * was shipped with BSD Unix, and played in every computer science
 * department. Its descendants are a genre, the roguelikes: Hack,
 * NetHack, Angband, and today Spelunky, The Binding of Isaac, Hades.
 * Its two ideas: the dungeon is generated, never the same twice, and
 * death is final. (Names and dates from memory, to check.)
 *
 * What's new here:
 *
 *  - Time is turns: the world waits for the player; one key, one move,
 *    then every monster's move ([turn]). The playground calls update 60
 *    times a second, and nothing happens in most of them: a game's clock
 *    doesn't have to be the display's.
 *
 *  - The dungeon is generated ([generate]), the way Rogue did it: the
 *    map cut in 3 x 3 cells, a room of random size and place in each (or
 *    only a crossing of corridors), each room joined to its neighbors by
 *    a corridor bent once or twice. From the seed in the model: the same
 *    seed, the same dungeon, which is what makes a roguelike's run
 *    shareable (and golden frames possible).
 *
 *  - What you see ([visible]): a room is lit, seen whole as you step in;
 *    in a corridor, only the squares next to you. What has been seen
 *    stays on the map, dimmed ([seen]), the monsters only while they're
 *    in sight: the fog of war, before it had a name. (Recursive
 *    shadowcasting, Björn Bergström's, the field of view of modern
 *    roguelikes, is an exercise.)
 *
 * What it uses: Tilemap (the dungeon, and what's been seen: grids of
 * characters), Scene2d (title, play, the tombstone, the win). No sprite,
 * no camera, no physics: the terminal's characters, in the playground's
 * text (and, for speed, the floors as dots and the walls as lines).
 *
 * Exercises: more levels and monsters, weapons and armor to wear, scrolls
 * to read, hunger, traps, the monsters' own ways (Rogue's leprechauns
 * steal gold, its nymphs your things), shadowcasting, saving (Rogue let
 * you save, and deleted the save when you loaded it: death is final).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Randomness *)
(*****************************************************************************)

(* a linear congruential generator, the state in the model: [roll seed
 * n] a number from 0 to n - 1, and the next seed *)
let roll (seed : int) (n : int) : int * int =
  let seed = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
  ((seed /.. 65536) mod max 1 n, seed)

(* several rolls in a row, the seed threaded along *)
let rolls (seed : int) (ns : int list) : int list * int =
  List.fold_left (fun (acc, seed) n -> let r, seed = roll seed n in (acc @ [ r ], seed)) ([], seed) ns

(*****************************************************************************)
(* The dungeon *)
(*****************************************************************************)

let cols = 50
let rows = 33
let cell = 20.

(* a room: its floor from (x0, y0) to (x1, y1), in cells; its walls
 * around *)
type room = { x0 : int; y0 : int; x1 : int; y1 : int }

let empty_map : Tilemap.t = Tilemap.of_strings cell (List.init rows (fun _ -> String.make cols ' '))

let set (m : Tilemap.t) (c : int) (r : int) (ch : char) : Tilemap.t = Tilemap.set m c r ch
let get (m : Tilemap.t) (c : int) (r : int) : char = Option.value (Tilemap.get m c r) ~default:' '

(* a room's floor and walls: - above and below, | on the sides *)
let dig_room (m : Tilemap.t) (r : room) : Tilemap.t =
  let m = ref m in
  for c = r.x0 -.. 1 to r.x1 +.. 1 do
    for rw = r.y0 -.. 1 to r.y1 +.. 1 do
      let ch = if rw < r.y0 || rw > r.y1 then '-' else if c < r.x0 || c > r.x1 then '|' else '.' in
      m := set !m c rw ch
    done
  done;
  !m

(* a corridor square: dug in the rock, a door where it goes through a
 * wall, nothing in a room *)
let dig (m : Tilemap.t) ((c, r) : int * int) : Tilemap.t =
  match get m c r with ' ' -> set m c r '#' | '-' | '|' -> set m c r '+' | _ -> m

(* the squares from a to b: straight, then turning once halfway, then
 * straight (an L, or an S if they're not aligned) *)
let corridor (horizontal : bool) ((ax, ay) : int * int) ((bx, by) : int * int) : (int * int) list =
  let line (x0, y0) (x1, y1) =
    let n = max (abs (x1 -.. x0)) (abs (y1 -.. y0)) in
    List.init (n +.. 1) (fun i -> (x0 +.. (compare x1 x0 *.. i), y0 +.. (compare y1 y0 *.. i)))
  in
  if horizontal then
    let mx = (ax +.. bx) /.. 2 in
    line (ax, ay) (mx, ay) @ line (mx, ay) (mx, by) @ line (mx, by) (bx, by)
  else
    let my = (ay +.. by) /.. 2 in
    line (ax, ay) (ax, my) @ line (ax, my) (bx, my) @ line (bx, my) (bx, by)

type item = Gold of int | Potion | Stairs | Amulet

type monster = { letter : char; mx : int; my : int; hp : int; awake : bool }

(* the monsters of each level: letter, hit points, most damage *)
let bestiary (depth : int) : (char * int * int) list =
  match depth with 1 -> [ ('B', 3, 2); ('K', 5, 4) ] | 2 -> [ ('K', 5, 4); ('S', 8, 3); ('H', 10, 6) ] | _ -> [ ('H', 10, 6); ('O', 12, 8); ('S', 8, 3) ]

let damage_of (letter : char) : int = match letter with 'B' -> 2 | 'K' -> 4 | 'S' -> 3 | 'H' -> 6 | _ -> 8

type level = { map : Tilemap.t; rooms : room list; items : ((int * int) * item) list; monsters : monster list; start : int * int }

(* [generate seed depth]: a level, and the next seed. The map in 3 x 3
 * cells of 16 x 11; in each, a room (from 4 x 3 to its cell's size), or
 * one in four times, a crossing of corridors only (a room of one
 * square, without walls); each cell joined to its right neighbor, and
 * down, in two columns out of three. *)
let generate (seed : int) (depth : int) : level * int =
  let cw = cols /.. 3 and ch = rows /.. 3 in
  let seed = ref seed in
  let r n = let v, s = roll !seed n in seed := s; v in
  let rooms =
    List.init 9 (fun i ->
        let gx = (i mod 3) *.. cw and gy = (i /.. 3) *.. ch in
        if r 4 = 0 && i <> 4 then
          let x = gx +.. 3 +.. r (cw -.. 6) and y = gy +.. 2 +.. r (ch -.. 4) in
          { x0 = x; y0 = y; x1 = x; y1 = y }
        else
          let w = 4 +.. r (cw -.. 8) and h = 3 +.. r (ch -.. 6) in
          let x = gx +.. 2 +.. r (cw -.. w -.. 3) and y = gy +.. 2 +.. r (ch -.. h -.. 3) in
          { x0 = x; y0 = y; x1 = x +.. w -.. 1; y1 = y +.. h -.. 1 })
  in
  let crossing (rm : room) = rm.x0 = rm.x1 && rm.y0 = rm.y1 in
  let m = List.fold_left (fun m rm -> if crossing rm then set m rm.x0 rm.y0 '#' else dig_room m rm) empty_map rooms in
  (* the corridors: from a door on one room's side to one on the other's *)
  let door (rm : room) side =
    if crossing rm then (rm.x0, rm.y0)
    else
      match side with
      | `Right -> (rm.x1 +.. 1, rm.y0 +.. r (rm.y1 -.. rm.y0 +.. 1))
      | `Left -> (rm.x0 -.. 1, rm.y0 +.. r (rm.y1 -.. rm.y0 +.. 1))
      | `Down -> (rm.x0 +.. r (rm.x1 -.. rm.x0 +.. 1), rm.y1 +.. 1)
      | `Up -> (rm.x0 +.. r (rm.x1 -.. rm.x0 +.. 1), rm.y0 -.. 1)
  in
  let room i = List.nth rooms i in
  let links =
    List.concat (List.init 3 (fun row -> List.init 2 (fun c -> (true, (row *.. 3) +.. c, (row *.. 3) +.. c +.. 1))))
    @ List.filter_map (fun (i, j) -> if r 3 <> 0 || i mod 3 = 1 then Some (false, i, j) else None) (List.init 6 (fun i -> (i, i +.. 3)))
  in
  let m =
    List.fold_left
      (fun m (horizontal, i, j) ->
        let a = door (room i) (if horizontal then `Right else `Down) and b = door (room j) (if horizontal then `Left else `Up) in
        List.fold_left dig m (corridor horizontal a b))
      m links
  in
  (* what's in the rooms: the stairs (or the Amulet, at the bottom), gold,
   * potions, monsters; the player in the middle room *)
  let real = List.filter (fun rm -> not (crossing rm)) rooms in
  let spot () =
    let rm = List.nth real (r (List.length real)) in
    (rm.x0 +.. r (rm.x1 -.. rm.x0 +.. 1), rm.y0 +.. r (rm.y1 -.. rm.y0 +.. 1))
  in
  let middle = room 4 in
  let start = ((middle.x0 +.. middle.x1) /.. 2, (middle.y0 +.. middle.y1) /.. 2) in
  let rec free used = let p = spot () in if List.mem p used || p = start then free used else p in
  let items =
    List.fold_left
      (fun acc it -> (free (List.map fst acc), it) :: acc)
      [] ([ (if depth = 3 then Amulet else Stairs) ] @ List.init 3 (fun _ -> Gold (10 +.. r (20 *.. depth))) @ List.init 2 (fun _ -> Potion))
  in
  let kinds = bestiary depth in
  let monsters =
    List.fold_left
      (fun acc _ ->
        let letter, hp, _ = List.nth kinds (r (List.length kinds)) in
        let x, y = free (List.map fst items @ List.map (fun m -> (m.mx, m.my)) acc) in
        { letter; mx = x; my = y; hp; awake = false } :: acc)
      [] (List.init (3 +.. depth) Fun.id)
  in
  ({ map = m; rooms = real; items; monsters; start }, !seed)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type game = {
  level : level;
  depth : int;
  seen : Tilemap.t; (* 's' where seen *)
  px : int;
  py : int;
  hp : int;
  max_hp : int;
  xp : int;
  rank : int; (* the player's level: more hit points, more damage *)
  gold : int;
  potions : int;
  message : string;
  seed : int;
  turns : int;
}

type scene = Title | Playing of game | Dead of game * string (* killed by *) | Won of game
type model = scene Scene2d.t

let enter (depth : int) (g : game) : game =
  let level, seed = generate g.seed depth in
  let px, py = level.start in
  { g with level; depth; seen = empty_map; px; py; seed }

let new_game (seed : int) : game =
  enter 1 { level = { map = empty_map; rooms = []; items = []; monsters = []; start = (0, 0) }; depth = 1; seen = empty_map; px = 0; py = 0; hp = 12; max_hp = 12;
            xp = 0; rank = 1; gold = 0; potions = 0; message = "Welcome to the Dungeons of Doom."; seed; turns = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Seeing *)
(*****************************************************************************)

let in_room (rm : room) (c : int) (r : int) : bool = c >= rm.x0 -.. 1 && c <= rm.x1 +.. 1 && r >= rm.y0 -.. 1 && r <= rm.y1 +.. 1

(* [visible g c r]: the room the player is in, walls included, lit; and
 * the squares around the player *)
let visible (g : game) (c : int) (r : int) : bool =
  (abs (c -.. g.px) <= 1 && abs (r -.. g.py) <= 1)
  || List.exists (fun rm -> in_room rm g.px g.py && in_room rm c r && get g.level.map g.px g.py <> '+') g.level.rooms

let look (g : game) : game =
  let seen = ref g.seen in
  for c = 0 to cols -.. 1 do
    for r = 0 to rows -.. 1 do
      if visible g c r && get !seen c r <> 's' then seen := set !seen c r 's'
    done
  done;
  { g with seen = !seen }

(*****************************************************************************)
(* A turn *)
(*****************************************************************************)

let walkable (g : game) (c : int) (r : int) : bool = match get g.level.map c r with '.' | '#' | '+' -> true | _ -> false
let monster_at (g : game) (c : int) (r : int) : monster option = List.find_opt (fun m -> m.mx = c && m.my = r) g.level.monsters

let name (letter : char) : string =
  match letter with 'B' -> "bat" | 'K' -> "kobold" | 'S' -> "snake" | 'H' -> "hobgoblin" | _ -> "orc"

let with_monsters (g : game) (ms : monster list) : game = { g with level = { g.level with monsters = ms } }

(* the player hits a monster three times in four, for 1 to 6, more by
 * rank; a monster dies at 0, and its hit points are experience *)
let attack (g : game) (m : monster) : game =
  let (hit, dmg), seed = match rolls g.seed [ 4; 6 ] with [ h; d ], s -> ((h, d), s) | _, s -> ((0, 0), s) in
  let g = { g with seed } in
  if hit = 0 then { g with message = Printf.sprintf "You miss the %s." (name m.letter) }
  else
    let hp = m.hp -.. (dmg +.. g.rank) in
    if hp <= 0 then
      let xp = g.xp +.. (match List.find_opt (fun (l, _, _) -> l = m.letter) (bestiary g.depth) with Some (_, h, _) -> h | None -> 5) in
      let rank = if xp >= 40 then 4 else if xp >= 20 then 3 else if xp >= 10 then 2 else 1 in
      let g = { (with_monsters g (List.filter (fun m' -> m' != m) g.level.monsters)) with xp; message = Printf.sprintf "You defeated the %s." (name m.letter) } in
      if rank > g.rank then { g with rank; max_hp = g.max_hp +.. 4; hp = g.hp +.. 4; message = g.message ^ " Welcome to level " ^ string_of_int rank ^ "." } else g
    else { (with_monsters g (List.map (fun m' -> if m' == m then { m with hp; awake = true } else m') g.level.monsters)) with message = Printf.sprintf "You hit the %s." (name m.letter) }

(* the monsters' turn: those that see the player wake up; awake, next to
 * the player, they attack (hit 3 times in 5); otherwise they step closer
 * (a bat, flying about, one time in two anywhere) *)
let monsters_turn (g : game) : game * string option =
  List.fold_left
    (fun (g, killer) m ->
      let m = if visible g m.mx m.my then { m with awake = true } else m in
      let g = with_monsters g (List.map (fun m' -> if m'.mx = m.mx && m'.my = m.my then m else m') g.level.monsters) in
      if not m.awake || killer <> None then (g, killer)
      else if abs (m.mx -.. g.px) <= 1 && abs (m.my -.. g.py) <= 1 then
        let (hit, dmg), seed = match rolls g.seed [ 5; damage_of m.letter ] with [ h; d ], s -> ((h, d), s) | _, s -> ((0, 0), s) in
        let g = { g with seed } in
        if hit >= 3 then ({ g with message = g.message ^ Printf.sprintf " The %s misses." (name m.letter) }, None)
        else
          let hp = g.hp -.. (dmg +.. 1) in
          ({ g with hp; message = g.message ^ Printf.sprintf " The %s hits." (name m.letter) }, if hp <= 0 then Some (name m.letter) else None)
      else
        let (dir, bat_wander), seed = match rolls g.seed [ 8; 2 ] with [ d; w ], s -> ((d, w), s) | _, s -> ((0, 0), s) in
        let g = { g with seed } in
        let dx, dy =
          if m.letter = 'B' && bat_wander = 0 then [| (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, 1); (1, -1); (-1, -1) |].(dir)
          else (compare g.px m.mx, compare g.py m.my)
        in
        let free (c, r) = walkable g c r && monster_at g c r = None && (c, r) <> (g.px, g.py) in
        match List.find_opt free [ (m.mx +.. dx, m.my +.. dy); (m.mx +.. dx, m.my); (m.mx, m.my +.. dy) ] with
        | Some (mx, my) -> (with_monsters g (List.map (fun m' -> if m' == m || (m'.mx = m.mx && m'.my = m.my) then { m with mx; my } else m') g.level.monsters), None)
        | None -> (g, None))
    (g, None) g.level.monsters

type action = Move of int * int | Rest | Quaff | Descend

(* [turn g a]: the player's move, then the monsters'; and who killed the
 * player, if they did *)
let turn (g : game) (a : action) : game * string option =
  let g = { g with message = ""; turns = g.turns +.. 1 } in
  let g =
    match a with
    | Move (dx, dy) -> (
        let c = g.px +.. dx and r = g.py +.. dy in
        match monster_at g c r with
        | Some m -> attack g m
        | None when walkable g c r -> (
            let g = { g with px = c; py = r } in
            match List.assoc_opt (c, r) g.level.items with
            | Some (Gold n) -> { g with gold = g.gold +.. n; level = { g.level with items = List.remove_assoc (c, r) g.level.items }; message = Printf.sprintf "You found %d gold pieces." n }
            | Some Potion -> { g with potions = g.potions +.. 1; level = { g.level with items = List.remove_assoc (c, r) g.level.items }; message = "You pick up a potion." }
            | Some Stairs -> { g with message = "There are stairs down here (space)." }
            | Some Amulet | None -> g)
        | None -> g)
    | Rest -> { g with hp = min g.max_hp (g.hp +.. 1) }
    | Quaff when g.potions > 0 -> { g with potions = g.potions -.. 1; hp = min g.max_hp (g.hp +.. 10); message = "You feel better." }
    | Quaff -> { g with message = "You have no potion." }
    | Descend -> enter (g.depth +.. 1) { g with message = "You go down the stairs." }
  in
  let g = look g in
  monsters_turn g

let on_stairs (g : game) : bool = List.assoc_opt (g.px, g.py) g.level.items = Some Stairs
let on_amulet (g : game) : bool = List.assoc_opt (g.px, g.py) g.level.items = Some Amulet

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the key pressed this frame: a turn, or none *)
let action_of (s : model) (g : game) : action option =
  let p k = Scene2d.pressed k s in
  let key c = p (fun k -> Set_.mem c k.keys) in
  let dirs = [ ("h", (-1, 0)); ("l", (1, 0)); ("k", (0, -1)); ("j", (0, 1)); ("y", (-1, -1)); ("u", (1, -1)); ("b", (-1, 1)); ("n", (1, 1)) ] in
  if p (fun k -> k.kleft) then Some (Move (-1, 0))
  else if p (fun k -> k.kright) then Some (Move (1, 0))
  else if p (fun k -> k.kup) then Some (Move (0, -1))
  else if p (fun k -> k.kdown) then Some (Move (0, 1))
  else if p (fun k -> k.kspace) then Some (if on_stairs g then Descend else Rest)
  else if key "q" then Some Quaff
  else List.find_map (fun (c, (dx, dy)) -> if key c then Some (Move (dx, dy)) else None) dirs

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Playing (look (new_game (1 +.. s.frames)))) s else s
  | Playing g -> (
      match action_of s g with
      | None -> s
      | Some a -> (
          let g, killer = turn g a in
          match killer with
          | Some k -> Scene2d.go (Dead (g, k)) s
          | None -> if on_amulet g then Scene2d.go (Won g) s else { s with scene = Playing g }))
  | Dead _ | Won _ -> if Scene2d.pressed (fun k -> k.kspace) s && s.elapsed > 1. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let top = 330.

let at (c : int) (r : int) : number * number = (-500. + ((float_of_int c + 0.5) * cell), top - ((float_of_int r + 0.5) * cell))

(* a square of the map, as the terminal drew it, dimmed when remembered
 * only *)
let square_shape (lit : bool) (ch : char) (c : int) (r : int) : shape list =
  let x, y = at c r in
  let color = if lit then rgb 200 200 200 else rgb 90 90 110 in
  match ch with
  | '.' -> [ rectangle color 3. 3. |> move x y ]
  | '#' -> [ rectangle (if lit then rgb 160 160 160 else rgb 70 70 80) (cell - 4.) (cell - 4.) |> move x y ]
  | '-' -> [ rectangle color cell 3. |> move x y ]
  | '|' -> [ rectangle color 3. cell |> move x y ]
  | '+' -> [ text (if lit then rgb 200 160 80 else rgb 110 90 60) 2. "+" |> move x (y - 6.) ]
  | _ -> []

let item_glyph (it : item) : string * color =
  match it with Gold _ -> ("$", yellow) | Potion -> ("!", rgb 200 90 255) | Stairs -> (">", white) | Amulet -> (",", rgb 255 200 60)

let view_game (g : game) : shape list =
  let squares =
    List.concat
      (List.init rows (fun r ->
           List.concat (List.init cols (fun c -> if get g.seen c r = 's' then square_shape (visible g c r) (get g.level.map c r) c r else []))))
  in
  let glyph str color (c, r) = let x, y = at c r in text color 2.2 str |> move x (y - 7.) in
  squares
  @ List.filter_map (fun ((c, r), it) -> if get g.seen c r = 's' then let s, col = item_glyph it in Some (glyph s col (c, r)) else None) g.level.items
  @ List.filter_map (fun m -> if visible g m.mx m.my then Some (glyph (String.make 1 m.letter) (rgb 255 120 120) (m.mx, m.my)) else None) g.level.monsters
  @ [ glyph "@" (rgb 255 255 120) (g.px, g.py);
      text white 2.3 g.message |> move_y 420.;
      text (rgb 200 200 200) 2.3
        (Printf.sprintf "Level: %d  Gold: %d  Hp: %d(%d)  Rank: %d  Exp: %d  Potions: %d" g.depth g.gold g.hp g.max_hp g.rank g.xp g.potions)
      |> move_y (-420.) ]

(* the tombstone, as Rogue drew it *)
let view_tomb (g : game) (killer : string) : shape list =
  [ polygon (rgb 150 150 160) [ (-180., -250.); (180., -250.); (180., 150.); (120., 230.); (-120., 230.); (-180., 150.) ];
    text black 3.5 "REST" |> move_y 170.; text black 3.5 "IN" |> move_y 120.; text black 3.5 "PEACE" |> move_y 70.;
    text black 2.2 (Printf.sprintf "%d Au" g.gold) |> move_y 10.; text black 2.2 "killed by a" |> move_y (-40.); text black 2.2 killer |> move_y (-80.);
    text black 2.2 "on level" |> move_y (-130.); text black 2.2 (string_of_int g.depth) |> move_y (-170.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text white 7. "TINY ROGUE" |> move_y 200.; text (rgb 200 200 200) 2.3 "arrows or h j k l (y u b n diagonals) walk and fight" |> move_y 100.;
        text (rgb 200 200 200) 2.3 "q drink a potion   space: down the stairs, or rest" |> move_y 60.;
        text (rgb 255 200 60) 2.3 "find the Amulet of Yendor, three levels down" |> move_y 20. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ]
  | Playing g -> view_game g
  | Dead (g, killer) -> view_tomb g killer
  | Won g ->
      [ text (rgb 255 200 60) 5. "THE AMULET OF YENDOR!"; text white 3. (Printf.sprintf "%d gold, %d turns" g.gold g.turns) |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
