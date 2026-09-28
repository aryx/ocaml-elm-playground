(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Adventure (Warren Robinett, Atari 2600, 1980): an
 * evil magician has stolen the enchanted chalice and hidden it in the
 * black castle; bring it back to the gold castle. You are a square.
 * Arrows to move, walk into a thing to pick it up, space to drop it.
 * Three dragons: Yorgle (yellow, afraid of the gold key), Grundle
 * (green) and Rhindle (red, the fastest); a sword kills them.
 *
 * Adventure was the first action-adventure: Robinett had played Will
 * Crowther's text Adventure on a PDP-10 and wanted the same on a
 * console with 128 bytes of RAM and no keyboard -- rooms joined by
 * exits, things to carry, locks and keys, monsters -- but seen instead
 * of read. Atari did not credit its programmers, so he hid his name in
 * a secret room, reached with a grey dot: the first Easter egg. Zelda
 * (1986, TinyZelda) grew out of the same design. (Names and dates from
 * memory, to check.)
 *
 * What's new here:
 *
 *  - The world as a graph of screens, not a grid ([rooms]): each room
 *    names the room past each of its edges, so the map need not be flat
 *    -- the mazes' sides lead back into themselves, and both sides of
 *    the lower maze lead to the black castle. TinyZelda's world was one
 *    big tilemap cut into screens; here there is no big map at all.
 *
 *  - The 2600's playfield ([playfield]): the walls of a room are 20
 *    bits a band, the left half of the screen only; the chip draws the
 *    right half either as their mirror image (the castles, the halls)
 *    or as a copy (the mazes, hence their repeating look). A dead end,
 *    closed on one side only, needs a wall drawn by something else: a
 *    thin wall ([closed]).
 *
 *  - Carrying ([carried], [offset]): one thing at a time, and it stays
 *    where it touched you -- a key held above your head, a sword held
 *    behind you -- so how you pick a thing up matters: a sword held in
 *    front meets the dragon first. Taking another drops the first.
 *
 *  - Dragons that bite, then swallow ([Biting]): a dragon touching you
 *    opens its mouth, and if you are still there when it closes, you
 *    are eaten; the moment between is the whole game. They fly over
 *    the walls, but not out of their room, and each flees or chases
 *    ([dragon_move]): Yorgle runs from the gold key in your hands.
 *
 * What it uses: Scene2d (the title, eaten, won), Sprite.pixels (the
 * dragons, the keys, the sword and the chalice, 8 pixels wide as on the
 * 2600). Not Tilemap: a room is 7 bands of 40 walls, the collisions a
 * few lines ([blocked]); not Camera2d: one room shown at a time, no
 * scrolling; not the adventure kit (TinyZork's world of words): here the
 * world is places, not rules. Not Physics.
 *
 * Exercises: the bat, who steals what you carry and swaps it for
 * something else; the magnet, pulling things to it; the bridge, to
 * cross a wall; the white castle; dragons that follow you out of their
 * room and guard their treasure; the dark catacombs, lit around you
 * only; the grey dot and the secret room; Game 3's random placement.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

type item = Sword | Gold_key | Black_key | Chalice

type side = Left | Right

type room = {
  color : color; (* of its walls, and of you in it *)
  bands : string list; (* the playfield: 7 bands of the 20 left columns, X a wall *)
  mirror : bool; (* the right half the left one reflected, or repeated *)
  closed : side option; (* a thin wall on a side the playfield leaves open *)
  up : int option; (* the room past each edge *)
  down : int option;
  left : int option;
  right : int option;
  gate : (int * item) option; (* a castle: the room inside, the key that opens it *)
}

(* the castles' top: two towers, the keep, and the gate in the middle of
 * the bottom band, 4 columns wide *)
let castle = [ "X   X X X   X X X X "; "X   XXXXX   XXXXXXXX"; "X   XXXXXXXXXXXXXXXX"; "X   XXXXXXXXXXXXXX  " ]

let gold = rgb 210 200 60
let blue = rgb 60 80 220
let black_castle = rgb 0 0 0

let room color bands ?(mirror = true) ?closed ?up ?down ?left ?right ?gate () : room = { color; bands; mirror; closed; up; down; left; right; gate }

let rooms : room array =
  [| (* 0: before the gold castle, where you start *)
     room gold (castle @ [ "X                   "; "X                   "; "XXXXXXXXXXXXXXXX    " ]) ~down:2 ~gate:(1, Gold_key) ();
     (* 1: in the gold castle, where the chalice belongs *)
     room gold [ "XXXXXXXXXXXXXXXXXXXX"; "X                   "; "X   XXXXX           "; "X                   "; "X   XXXXX           "; "X                   "; "XXXXXXXXXXXXXXXX    " ] ~down:0 ();
     (* 2: the crossroads *)
     room (rgb 80 170 60)
       [ "XXXXXXXXXXXXXXXX    "; "X                   "; "X                   "; "                    "; "X                   "; "X                   "; "XXXXXXXXXXXXXXXX    " ]
       ~up:0 ~down:5 ~left:3 ~right:4 ();
     (* 3: the west hall, a dead end *)
     room (rgb 220 120 40)
       [ "XXXXXXXXXXXXXXXXXXXX"; "X                   "; "X      XXXXXX       "; "                    "; "X      XXXXXX       "; "X                   "; "XXXXXXXXXXXXXXXXXXXX" ]
       ~closed:Left ~right:2 ();
     (* 4: the east hall, a dead end *)
     room (rgb 160 80 200)
       [ "XXXXXXXXXXXXXXXXXXXX"; "X                   "; "X   XX    XX    XX  "; "                    "; "X   XX    XX    XX  "; "X                   "; "XXXXXXXXXXXXXXXXXXXX" ]
       ~closed:Right ~left:2 ();
     (* 5: the blue maze, its sides leading back into itself *)
     room blue
       [ "    XXXXXXXXXXXX    "; "XX      XX      XX  "; "XX  XX  XX  XX  XX  "; "    XX      XX      "; "XXXXXX  XXXXXXXXXX  "; "        XX          "; "XXXXXXXXXX  XXXXXXXX" ]
       ~mirror:false ~up:2 ~down:6 ~left:5 ~right:5 ();
     (* 6: the maze's bottom, both sides to the black castle *)
     room blue
       [ "XXXXXXXXXX  XXXXXXXX"; "X                  X"; "X  XXXXXXXXXXXXXX  X"; "X  X            X  X"; "   X  XXXXXXXX  X   "; "   X            X   "; "XXXXXXXXXXXXXXXXXXXX" ]
       ~mirror:false ~up:5 ~left:7 ~right:7 ();
     (* 7: before the black castle, open on both sides *)
     room black_castle (castle @ [ "                    "; "                    "; "XXXXXXXXXXXXXXXXXXXX" ]) ~left:6 ~right:6 ~gate:(8, Black_key) ();
     (* 8: in the black castle *)
     room black_castle [ "XXXXXXXXXXXXXXXXXXXX"; "X                   "; "X          XXXXXXXXX"; "X                   "; "X          XXXXXXXXX"; "X                   "; "XXXXXXXXXXXXXXXX    " ] ~down:7 () |]

let start_room = 0
let home = 1 (* the chalice brought here wins *)

(* the screen: the room is 40 columns of 24 by 7 bands of 120, centered *)
let col_w = 24.
let band_h = 120.
let half_w = 480.
let half_h = 420.

(* [playfield r]: the room's 40 columns, band by band: the 20 given, then
 * again, reflected or repeated, as the 2600's playfield registers did *)
let playfield (r : room) : string list =
  List.map
    (fun b ->
      let right = if r.mirror then String.init 20 (fun i -> b.[19 -.. i]) else b in
      let row = Bytes.of_string (b ^ right) in
      (match r.closed with Some Left -> Bytes.set row 0 'X' | Some Right -> Bytes.set row 39 'X' | None -> ());
      Bytes.to_string row)
    r.bands

let wall_at (r : room) (x : number) (y : number) : bool =
  let c = int_of_float (Float.floor ((x + half_w) / col_w)) and b = int_of_float (Float.floor ((half_h - y) / band_h)) in
  c >= 0 && c < 40 && b >= 0 && b < 7 && (List.nth (playfield r) b).[c] = 'X'

(* the gate: the notch at the bottom of the castle, columns 18 to 21 of
 * band 3 *)
let in_gate (x : number) (y : number) : bool = Float.abs x < 48. && Float.abs y < 60.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type thing = { what : item; troom : int; tx : number; ty : number }

type kind = Yorgle | Grundle | Rhindle
type state = Alive | Biting of int (* frames before the mouth closes *) | Dead | Full (* it ate you *)
type dragon = { kind : kind; droom : int; dx : number; dy : number; state : state }

type game = {
  room : int;
  x : number; (* you, a square *)
  y : number;
  things : thing list;
  carried : item option;
  offset : number * number; (* where the carried thing is, from you *)
  ignoring : item option; (* just dropped: not taken again until you step off it *)
  dragons : dragon list;
  open_gates : int list; (* the castles whose gate is up *)
  deaths : int;
  frames : int;
}

type scene = Title | Playing of game | Eaten of game | Won of game
type model = scene Scene2d.t

let new_game () : game =
  { room = start_room; x = 0.; y = -250.;
    things =
      [ { what = Sword; troom = 1; tx = 200.; ty = 0. }; { what = Gold_key; troom = 3; tx = -400.; ty = 0. }; { what = Black_key; troom = 4; tx = 300.; ty = 0. };
        { what = Chalice; troom = 8; tx = 0.; ty = 250. } ];
    carried = None; offset = (0., 0.); ignoring = None;
    dragons =
      [ { kind = Yorgle; droom = 3; dx = -100.; dy = 250.; state = Alive }; { kind = Grundle; droom = 4; dx = 200.; dy = 250.; state = Alive };
        { kind = Rhindle; droom = 8; dx = -300.; dy = -250.; state = Alive } ];
    open_gates = []; deaths = 0; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Boxes *)
(*****************************************************************************)

let you_half = 9.

(* half the width and height of each thing's box *)
let half_of (it : item) : number * number = match it with Sword -> (24., 15.) | Gold_key | Black_key -> (24., 9.) | Chalice -> (24., 24.)
let dragon_half = (22., 36.)

let touch (x1, y1) (w1, h1) (x2, y2) (w2, h2) : bool = Float.abs (x1 - x2) < w1 + w2 && Float.abs (y1 - y2) < h1 + h2

let you_touch (g : game) (pos : number * number) (half : number * number) : bool = touch (g.x, g.y) (you_half, you_half) pos half

(* [blocked g room x y]: a corner of the square in a wall, or in a shut
 * gate *)
let blocked (g : game) (i : int) (x : number) (y : number) : bool =
  let r = rooms.(i) in
  let shut = match r.gate with Some _ -> not (List.mem i g.open_gates) | None -> false in
  let e = you_half - 0.1 in
  List.exists (fun (cx, cy) -> wall_at r cx cy || (shut && in_gate cx cy)) [ (x - e, y - e); (x + e, y - e); (x - e, y + e); (x + e, y + e) ]

(*****************************************************************************)
(* Moving between rooms *)
(*****************************************************************************)

(* [cross g]: past an edge into the room there, on the other side of
 * the screen, unless nothing is there or a wall is where you would
 * arrive; into a castle by its open gate, and out of it below the gate *)
let cross (g : game) : game =
  let r = rooms.(g.room) in
  let stay = { g with x = Float.max (-.half_w + you_half) (Float.min (half_w - you_half) g.x); y = Float.max (-.half_h + you_half) (Float.min (half_h - you_half) g.y) } in
  let go target x y =
    match target with
    | None -> stay
    | Some i ->
        (* out of a castle: in front of its gate *)
        let x, y = match rooms.(i).gate with Some (inside, _) when inside = g.room -> (0., -90.) | _ -> (x, y) in
        if blocked g i x y then stay else { g with room = i; x; y }
  in
  match r.gate with
  | Some (inside, _) when List.mem g.room g.open_gates && in_gate g.x g.y && g.y > 20. -> { g with room = inside; x = 0.; y = -360. }
  | _ ->
      if g.x < -.half_w then go r.left (g.x + (2. * half_w)) g.y
      else if g.x > half_w then go r.right (g.x - (2. * half_w)) g.y
      else if g.y > half_h then go r.up g.x (g.y - (2. * half_h))
      else if g.y < -.half_h then go r.down g.x (g.y + (2. * half_h))
      else g

(*****************************************************************************)
(* Dragons *)
(*****************************************************************************)

let speed (k : kind) : number = match k with Yorgle -> 2.5 | Grundle -> 3. | Rhindle -> 3.8
let fears (k : kind) : item option = match k with Yorgle -> Some Gold_key | Grundle | Rhindle -> None
let bite_frames = 24

(* [dragon_move g d]: towards you, or away from what it fears if you
 * hold it; over the walls, but kept in its room *)
let dragon_move (g : game) (d : dragon) : dragon =
  let feared = match fears d.kind with Some it when g.carried = Some it -> List.find_opt (fun t -> t.what = it) g.things | _ -> None in
  let tx, ty, sign = match feared with Some t -> (t.tx, t.ty, -1.) | None -> (g.x, g.y, 1.) in
  let dist = Float.max 1. (Float.hypot (tx - d.dx) (ty - d.dy)) in
  let s = speed d.kind in
  let clamp lim v = Float.max (-.lim) (Float.min lim v) in
  { d with dx = clamp (half_w - 30.) (d.dx + (sign * s * (tx - d.dx) / dist)); dy = clamp (half_h - 40.) (d.dy + (sign * s * (ty - d.dy) / dist)) }

(* [dragon_step g d]: the dragons of your room move and bite; a bite
 * that closes on you is the end *)
let dragon_step (g : game) (d : dragon) : dragon =
  if d.droom <> g.room then d
  else
    match d.state with
    | Dead | Full -> d
    | Alive ->
        let d = dragon_move g d in
        if you_touch g (d.dx, d.dy) dragon_half then (Audio.play Audio.hit; { d with state = Biting bite_frames }) else d
    | Biting 0 -> if you_touch g (d.dx, d.dy) dragon_half then { d with state = Full } else { d with state = Alive }
    | Biting n -> { d with state = Biting (n -.. 1) }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  let k = computer.keyboard in
  (* you: along x, then along y, each stopped by the walls *)
  let s = 4. in
  let vx = (if k.kright then s else 0.) - (if k.kleft then s else 0.) and vy = (if k.kup then s else 0.) - (if k.kdown then s else 0.) in
  let g = if blocked g g.room (g.x + vx) g.y then g else { g with x = g.x + vx } in
  let g = if blocked g g.room g.x (g.y + vy) then g else { g with y = g.y + vy } in
  let g = cross g in
  (* what you carry comes along, into the next room too *)
  let ox, oy = g.offset in
  let g = { g with things = List.map (fun t -> if Some t.what = g.carried then { t with troom = g.room; tx = g.x + ox; ty = g.y + oy } else t) g.things } in
  (* space drops it; walking into another thing swaps them *)
  let g =
    if Scene2d.pressed (fun k -> k.kspace) scenes && g.carried <> None then (Audio.play Audio.blip; { g with carried = None; ignoring = g.carried }) else g
  in
  let here = List.filter (fun t -> t.troom = g.room && you_touch g (t.tx, t.ty) (half_of t.what)) g.things in
  let g = match g.ignoring with Some it when not (List.exists (fun t -> t.what = it) here) -> { g with ignoring = None } | _ -> g in
  let g =
    match List.find_opt (fun t -> Some t.what <> g.carried && Some t.what <> g.ignoring) here with
    | Some t -> Audio.play Audio.coin; { g with carried = Some t.what; offset = (t.tx - g.x, t.ty - g.y); ignoring = g.carried }
    | None -> g
  in
  (* a key at its castle's gate raises it *)
  let g =
    match rooms.(g.room).gate with
    | Some (_, key) when not (List.mem g.room g.open_gates) ->
        if List.exists (fun t -> t.what = key && t.troom = g.room && touch (t.tx, t.ty) (half_of key) (0., 0.) (48., 60.)) g.things then
          (Audio.play Audio.powerup; { g with open_gates = g.room :: g.open_gates })
        else g
    | _ -> g
  in
  (* the dragons; the sword, carried or lying, kills one it touches *)
  let sword = List.find (fun t -> t.what = Sword) g.things in
  let slain (d : dragon) = d.droom = sword.troom && (match d.state with Alive | Biting _ -> true | Dead | Full -> false) && touch (d.dx, d.dy) dragon_half (sword.tx, sword.ty) (half_of Sword) in
  let dragons = List.map (fun d -> if slain d then (Audio.play Audio.explosion; { d with state = Dead }) else dragon_step g d) g.dragons in
  { g with dragons }

(* [reincarnate g]: after being eaten, as the 2600's reset switch did:
 * back before the gold castle, what you carried left where you died,
 * the dragons alive again *)
let reincarnate (g : game) : game =
  { g with room = start_room; x = 0.; y = -250.; carried = None; ignoring = None; deaths = g.deaths +.. 1;
    dragons = List.map (fun d -> { d with state = Alive }) g.dragons }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if List.exists (fun t -> t.what = Chalice && t.troom = home) g.things then (Audio.play Audio.powerup; Scene2d.go (Won g) s)
      else if List.exists (fun d -> d.state = Full) g.dragons then (Audio.play Audio.laser; Scene2d.go (Eaten g) s)
      else { s with scene = Playing g }
  | Eaten g -> if space then Scene2d.go (Playing (reincarnate g)) s else s
  | Won _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let grey = rgb 170 170 170

(* 8 pixels wide, as a 2600's sprite; a dragon facing left *)
let dragon_rows =
  [ "...XX..."; "..XXXX.."; "..X.XX.."; "..XXXX.."; "XXXX...."; "..XX...."; "..XXX..."; ".XXXXX.."; "XXXXXXX."; "XXXXXXXX"; ".XXXXXX."; "..X..X.."; ".XX.XX.." ]

let dragon_biting = List.mapi (fun i row -> match i with 3 -> "XXXXXX.." | 4 -> "X......." | 5 -> "XXXX...." | _ -> row) dragon_rows

let item_rows (it : item) : string list =
  match it with
  | Sword -> [ "..X....."; ".X......"; "XXXXXXXX"; ".X......"; "..X....." ]
  | Gold_key | Black_key -> [ ".XX....."; "X..XXXXX"; ".XX..X.X" ]
  | Chalice -> [ "X......X"; "X......X"; "XX....XX"; ".XXXXXX."; "..XXXX.."; "...XX..."; "...XX..."; ".XXXXXX." ]

let item_color (frames : int) (it : item) : color =
  match it with
  | Sword -> rgb 230 200 60
  | Gold_key -> gold
  | Black_key -> black
  (* the chalice flashes through the colors *)
  | Chalice -> Sprite.cycle (frames /.. 8) [ rgb 230 60 60; rgb 230 160 40; rgb 230 230 60; rgb 60 200 60; rgb 60 120 230; rgb 180 60 220 ]

let dragon_color (k : kind) : color = match k with Yorgle -> rgb 230 220 60 | Grundle -> rgb 60 180 60 | Rhindle -> rgb 210 50 50

let view_dragon (g : game) (d : dragon) : shape =
  let rows = match d.state with Biting _ -> dragon_biting | Dead -> List.rev dragon_rows | Alive | Full -> dragon_rows in
  let rows = if g.x > d.dx && d.state <> Dead then Sprite.flip rows else rows in
  let body = Sprite.pixels 6. [ ('X', dragon_color d.kind) ] rows in
  (* the one that ate you shows you in its belly *)
  group (body :: (if d.state = Full then [ square rooms.(g.room).color 18. |> move_y (-12.) ] else [])) |> move d.dx d.dy

(* the room's walls, a rectangle per run of X in each band *)
let view_walls (r : room) : shape list =
  List.concat
    (List.mapi
       (fun b row ->
         Sprite.runs row
         |> List.filter_map (fun (c, len, ch) ->
                if ch <> 'X' then None
                else
                  let w = float_of_int len * col_w in
                  Some (rectangle r.color w band_h |> move (-.half_w + (float_of_int c * col_w) + (w / 2.)) (half_h - (float_of_int b * band_h) - (band_h / 2.)))))
       (playfield r))

let view_room (g : game) (eaten : bool) : shape list =
  let r = rooms.(g.room) in
  let gate =
    match r.gate with
    | Some _ when not (List.mem g.room g.open_gates) ->
        List.init 4 (fun i -> rectangle r.color 8. 120. |> move (-36. + (float_of_int i * 24.)) 0.) @ [ rectangle r.color 96. 8. |> move_y 30.; rectangle r.color 96. 8. |> move_y (-30.) ]
    | Some _ -> [ rectangle r.color 96. 16. |> move_y 52. ]
    | None -> []
  in
  let things = List.filter (fun t -> t.troom = g.room) g.things |> List.map (fun t -> Sprite.pixels 6. [ ('X', item_color g.frames t.what) ] (item_rows t.what) |> move t.tx t.ty) in
  let dragons = List.filter (fun d -> d.droom = g.room) g.dragons |> List.map (view_dragon g) in
  let you = if eaten then [] else [ square r.color 18. |> move g.x g.y ] in
  (rectangle grey (2. * half_w) (2. * half_h) :: view_walls r) @ gate @ things @ dragons @ you

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text gold 7. "TINY ADVENTURE" |> move_y 200.; text white 2.5 "arrows move   walk into a thing to take it   space drops it" |> move_y 100.;
        text white 2.5 "bring the enchanted chalice back to the gold castle" |> move_y 60.;
        Sprite.pixels 8. [ ('X', dragon_color Yorgle) ] dragon_rows |> move (-120.) (-60.);
        Sprite.pixels 8. [ ('X', item_color 0 Sword) ] (item_rows Sword) |> move 40. (-60.);
        Sprite.pixels 8. [ ('X', item_color 16 Chalice) ] (item_rows Chalice) |> move 160. (-60.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-250.) ]
  | Playing g -> view_room g false
  | Eaten g -> view_room g true @ Scene2d.blink 1. s [ text white 3. "EATEN   PRESS SPACE" |> move_y (-460.) ]
  | Won g ->
      (* the victory: the walls flash through the colors *)
      let r = { (rooms.(home)) with color = item_color s.frames Chalice } in
      (rectangle grey (2. * half_w) (2. * half_h) :: view_walls r)
      @ [ text black 5. "THE CHALICE IS HOME" |> move_y 60.;
          text black 3. (Printf.sprintf "%.0f seconds   eaten %d times" (float_of_int g.frames / 60.) g.deaths) |> move_y (-40.) ]
      @ Scene2d.blink 1. s [ text black 3. "PRESS SPACE" |> move_y (-140.) ])

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
