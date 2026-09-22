(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Maniac Mansion (Ron Gilbert and Gary Winnick,
 * Lucasfilm Games, 1987): Dave has to get into Dr. Fred's mansion, past
 * a purple tentacle, and down to the lab where Sandy is held.
 *
 *   click a verb, then an object (in the room, or in the inventory):
 *   "Pick up" "key"; "Use" "key" "front door"; "Give" "soda" "tentacle".
 *   Click the floor to walk.
 *
 * The text adventure given a picture. Where TinyZork's player types
 * "put the egg in the case", Maniac Mansion's clicks Use, the egg, the
 * case: the parser became a menu of verbs at the bottom of the screen,
 * and the objects are what is drawn. Its engine, SCUMM (Script Creation
 * Utility for Maniac Mansion, Ron Gilbert and Aric Wilmunder), ran
 * every LucasArts adventure after it -- Zak McKracken, Monkey Island,
 * Day of the Tentacle, this game's sequel. (Names and dates from
 * memory, to check.)
 *
 * What the picture needs that the words did not:
 *
 *   - The sentence line ([sentence_line]): the sentence being built,
 *     and while the mouse is over something, what a click would make of
 *     it ("Walk to fridge", "Use key with front door") -- the parser's
 *     job, done before the click instead of after the enter.
 *   - Walkboxes ([walkboxes], [route]): the floor of each room as boxes
 *     the kid may stand in; two boxes that touch are a way from one to
 *     the other. To walk somewhere is a search among the boxes
 *     (breadth-first, ai/Pathfind), then a straight line to the middle
 *     of each edge crossed, then to the spot. The hall's table stands in
 *     the middle of its floor, and Dave walks round it:
 *
 *        +----------+          +----------+
 *        |   left   |  table   |  right   |    a click on the right,
 *        |          |          |      * <-+--  from the left: through
 *        +----o-----+----------+----o-----+    the front strip, by the
 *        |              front             |    middle of each edge (o)
 *        +--------------------------------+
 *
 *   - Walking up to things: a verb on an object sends Dave to the
 *     object's spot first ([walk_to]), and the sentence is done when he
 *     gets there -- a menu of verbs, and a body that has to be where
 *     the verb is.
 *   - The rules, grouped by object ([rules]): SCUMM's "script per
 *     object", each object answering the verbs it knows; the same
 *     adventure kit as TinyZork underneath (gamekits/adventure), the
 *     world and the rules unchanged, only the way a sentence is made.
 *
 * What it uses: gamekits/adventure's Adventure (the world, and the rules
 * answering a sentence), ai/Pathfind (the search among the walkboxes),
 * Scene2d. Not Tilemap nor Camera2d: a room is one picture.
 *
 * Left undone, exercises: the three kids and "New kid" -- Maniac
 * Mansion's own idea, puzzles that need two of them in two places;
 * the cut-scenes, in which the Edisons go about their business while
 * you play; the other endings (it had several, depending on the kids);
 * walkboxes that are trapezoids, and a kid who shrinks as he walks
 * away, as the later SCUMM games scaled him; Dr. Fred himself.
 *)
open Playground

(*****************************************************************************)
(* The mansion *)
(*****************************************************************************)

(* a rectangle of the room's picture: its center and size; the room is
 * 900 x 440, centered on (0, 0), its floor at the bottom *)
type rect = { cx : float; cy : float; w : float; h : float }

let inside (r : rect) (x : float) (y : float) : bool =
  Float.abs (x -. r.cx) <= r.w /. 2. && Float.abs (y -. r.cy) <= r.h /. 2.

(* an object of the picture: its room, what it is called, where it is
 * drawn (and clicked), and where Dave stands to use it *)
type hotspot = { room : string; name : string; at : rect; spot : float * float }

let hotspots : (string * hotspot) list =
  let h room name cx cy w hh spot = { room; name; at = { cx; cy; w; h = hh }; spot } in
  [ ("mat", h "yard" "doormat" 0. (-105.) 110. 16. (0., -130.));
    ("key", h "yard" "key" 30. (-105.) 26. 12. (30., -130.));
    ("front door", h "yard" "front door" 0. 20. 110. 190. (0., -110.));
    ("door out", h "hall" "front door" (-400.) 0. 80. 180. (-380., -140.));
    ("kitchen door", h "hall" "kitchen door" 400. 0. 80. 180. (380., -140.));
    ("library door", h "hall" "library door" (-250.) 20. 90. 160. (-250., -110.));
    ("tentacle", h "hall" "purple tentacle" (-160.) (-60.) 50. 150. (-250., -140.));
    ("hall door", h "kitchen" "door" (-400.) 0. 80. 180. (-380., -140.));
    ("fridge", h "kitchen" "refrigerator" 280. (-10.) 120. 200. (240., -150.));
    ("soda", h "kitchen" "soda" 280. 10. 20. 36. (240., -150.));
    ("drawer", h "kitchen" "drawer" (-120.) (-60.) 120. 30. (-120., -150.));
    ("flashlight", h "kitchen" "flashlight" (-120.) (-35.) 50. 14. (-120., -150.));
    ("library exit", h "library" "door" (-400.) 0. 80. 180. (-380., -140.));
    ("bookcase", h "library" "bookcase" 100. 30. 260. 220. (100., -130.));
    ("odd book", h "library" "odd-looking book" 150. 60. 18. 34. (100., -130.));
    ("secret door", h "library" "secret passage" 100. 20. 100. 180. (100., -110.)) ]

let hotspot (o : string) : hotspot = List.assoc o hotspots

(* the floor, as boxes Dave may stand in *)
let walkboxes (room : string) : rect list =
  match room with
  | "yard" -> [ { cx = 0.; cy = -165.; w = 880.; h = 90. }; { cx = 0.; cy = -100.; w = 140.; h = 40. } ]
  | "hall" ->
      (* the table in the middle: a box on each side of it, and the front *)
      [ { cx = -295.; cy = -125.; w = 290.; h = 70. }; { cx = 295.; cy = -125.; w = 290.; h = 70. };
        { cx = 0.; cy = -185.; w = 880.; h = 50. } ]
  | _ -> [ { cx = 0.; cy = -160.; w = 880.; h = 100. } ]

(* where Dave arrives, coming from a room *)
let arrival (room : string) (from : string) : float * float =
  match (room, from) with
  | "hall", "yard" -> (-380., -140.)
  | "hall", "kitchen" -> (380., -140.)
  | "hall", _ -> (-250., -120.)
  | "yard", _ -> (0., -110.)
  | _ -> (-380., -140.)

let dark (room : string) : bool = room = "library"

let start_places : (string * Adventure.place) list =
  Adventure.
    [ ("mat", Room "yard"); ("key", Nowhere); ("front door", Room "yard"); ("door out", Room "hall");
      ("kitchen door", Room "hall"); ("library door", Room "hall"); ("tentacle", Room "hall");
      ("hall door", Room "kitchen"); ("fridge", Room "kitchen"); ("soda", Inside "fridge");
      ("drawer", Room "kitchen"); ("flashlight", Inside "drawer"); ("library exit", Room "library");
      ("bookcase", Room "library"); ("odd book", Room "library"); ("secret door", Nowhere) ]

let lit (w : Adventure.world) : bool =
  (not (dark w.here)) || (Adventure.where w "flashlight" = Adventure.Carried && Adventure.has w "flashlight on")

(*****************************************************************************)
(* The rules, a script per object *)
(*****************************************************************************)

let rule verb obj with_ test act : Adventure.rule = { verb; obj; with_; test; act }
let reply (msg : string) = fun (_ : Adventure.sentence) w -> (w, msg)
let flagged f = fun (_ : Adventure.sentence) w -> Adventure.has w f
let lacks f = fun (_ : Adventure.sentence) w -> not (Adventure.has w f)
let the_obj (s : Adventure.sentence) : string = Option.get s.obj
let enter (room : string) = fun (_ : Adventure.sentence) (w : Adventure.world) -> (Adventure.go room w, "")

let rules : Adventure.rule list =
  let open Adventure in
  [ (* the doormat *)
    rule "look" (Some "mat") None always (reply "It says WELCOME. It's a little lumpy.");
    rule "push" (Some "mat") None (lacks "mat moved")
      (fun _ w -> (set "mat moved" w |> put "key" (Room "yard"), "Hey, there's a key under here!"));
    rule "pull" (Some "mat") None (lacks "mat moved")
      (fun _ w -> (set "mat moved" w |> put "key" (Room "yard"), "Hey, there's a key under here!"));
    (* the front door *)
    rule "look" (Some "front door") None always (reply "A big old door. Somebody lives here, sort of.");
    rule "use" (Some "key") (Some "front door") (lacks "front door unlocked")
      (fun _ w -> (set "front door unlocked" w, "Unlocked it."));
    rule "open" (Some "front door") None (lacks "front door unlocked") (reply "It's locked.");
    rule "open" (Some "front door") None (lacks "front door open") (fun _ w -> (set "front door open" w, ""));
    rule "walk" (Some "front door") None (flagged "front door open") (enter "hall");
    rule "walk" (Some "door out") None always (enter "yard");
    (* the purple tentacle, and the library door behind it *)
    rule "look" (Some "tentacle") None always (reply "A purple tentacle. It looks thirsty.");
    rule "give" (Some "soda") (Some "tentacle") always
      (fun _ w -> (put "soda" Nowhere w |> put "tentacle" Nowhere, "Purple Tentacle: Soda! Mmm! (it slithers off, burping)"));
    rule "walk" (Some "library door") None (fun _ w -> where w "tentacle" <> Nowhere)
      (reply "Purple Tentacle: Stop right there, kid! Nobody goes in the library.");
    rule "walk" (Some "library door") None always (enter "library");
    rule "walk" (Some "kitchen door") None always (enter "kitchen");
    (* the kitchen *)
    rule "walk" (Some "hall door") None always (enter "hall");
    rule "look" (Some "fridge") None always (reply "Dr. Fred's refrigerator. It hums.");
    rule "look" (Some "drawer") None always (reply "A kitchen drawer.");
    rule "turn on" (Some "flashlight") None (fun _ w -> where w "flashlight" = Carried)
      (fun _ w -> (set "flashlight on" w, "Let there be light."));
    (* the library *)
    rule "walk" (Some "library exit") None always (enter "hall");
    rule "look" (Some "bookcase") None always (reply "Lots of books. One of them sticks out.");
    rule "look" (Some "odd book") None always (reply "\"Pull me\", it seems to say.");
    rule "pull" (Some "odd book") None (lacks "passage open")
      (fun _ w -> (set "passage open" w |> put "secret door" (Room "library") |> put "bookcase" Nowhere |> put "odd book" Nowhere,
                   "Whoa! The bookcase slid away!"));
    rule "walk" (Some "secret door") None always (enter "lab");
    (* anything *)
    rule "pick up" any None (fun s w -> List.mem (the_obj s) [ "key"; "soda"; "flashlight" ] && where w (the_obj s) <> Carried)
      (fun s w -> (put (the_obj s) Carried w, ""));
    rule "pick up" any None always (reply "I don't need that.");
    rule "open" any None (fun s _ -> List.mem (the_obj s) [ "fridge"; "drawer" ]) (fun s w -> (set (the_obj s ^ " open") w, ""));
    rule "close" any None (fun s _ -> List.mem (the_obj s) [ "fridge"; "drawer" ]) (fun s w -> (unset (the_obj s ^ " open") w, ""));
    rule "walk" any None always (reply "");
    rule "look" any None always (reply "It's just a thing.");
    rule "push" any None always (reply "It won't budge.");
    rule "pull" any None always (reply "It won't budge.") ]

(*****************************************************************************)
(* Walking: the walkboxes *)
(*****************************************************************************)

let box_of (room : string) (x : float) (y : float) : int option =
  List.mapi (fun i b -> (i, b)) (walkboxes room) |> List.find_map (fun (i, b) -> if inside b x y then Some i else None)

(* the point of box [b] nearest (x, y) *)
let clamp_to (b : rect) (x : float) (y : float) : float * float =
  (Float.max (b.cx -. (b.w /. 2.)) (Float.min (b.cx +. (b.w /. 2.)) x), Float.max (b.cy -. (b.h /. 2.)) (Float.min (b.cy +. (b.h /. 2.)) y))

(* the middle of the edge two boxes share, if they touch *)
let shared_edge (a : rect) (b : rect) : (float * float) option =
  let ax0, ax1, ay0, ay1 = (a.cx -. (a.w /. 2.), a.cx +. (a.w /. 2.), a.cy -. (a.h /. 2.), a.cy +. (a.h /. 2.)) in
  let bx0, bx1, by0, by1 = (b.cx -. (b.w /. 2.), b.cx +. (b.w /. 2.), b.cy -. (b.h /. 2.), b.cy +. (b.h /. 2.)) in
  let near u v = Float.abs (u -. v) < 0.5 in
  let x_lo, x_hi = (Float.max ax0 bx0, Float.min ax1 bx1) and y_lo, y_hi = (Float.max ay0 by0, Float.min ay1 by1) in
  if (near ay1 by0 || near ay0 by1) && x_lo < x_hi then Some ((x_lo +. x_hi) /. 2., if near ay1 by0 then ay1 else ay0)
  else if (near ax1 bx0 || near ax0 bx1) && y_lo < y_hi then Some ((if near ax1 bx0 then ax1 else ax0), (y_lo +. y_hi) /. 2.)
  else None

(* [route room from to]: the points to walk through, from where Dave
 * is to the floor point nearest (tx, ty): the boxes' path found by a
 * breadth-first search, then the middle of each edge crossed *)
let route (room : string) ((fx, fy) : float * float) ((tx, ty) : float * float) : (float * float) list =
  let boxes = Array.of_list (walkboxes room) in
  let nearest x y =
    let d i = let px, py = clamp_to boxes.(i) x y in Float.hypot (px -. x) (py -. y) in
    List.fold_left (fun a i -> if d i < d a then i else a) 0 (List.init (Array.length boxes) Fun.id)
  in
  let start = match box_of room fx fy with Some i -> i | None -> nearest fx fy in
  let goal = match box_of room tx ty with Some i -> i | None -> nearest tx ty in
  let problem : int Pathfind.problem =
    { neighbors = (fun i -> List.filter_map (fun j -> if j <> i && shared_edge boxes.(i) boxes.(j) <> None then Some (j, 1.) else None) (List.init (Array.length boxes) Fun.id));
      goal = (fun i -> i = goal);
      estimate = (fun _ -> 0.) }
  in
  let path = (Pathfind.breadth_first problem start).path in
  let rec edges = function a :: (b :: _ as rest) -> Option.get (shared_edge boxes.(a) boxes.(b)) :: edges rest | _ -> [] in
  edges path @ [ clamp_to boxes.(goal) tx ty ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let verbs = [| "Walk to"; "Look at"; "Pick up"; "Open"; "Close"; "Push"; "Pull"; "Use"; "Give"; "Turn on" |]

(* a verb of the menu, to the kit's *)
let verb_word (v : string) : string =
  match v with "Walk to" -> "walk" | "Look at" -> "look" | "Pick up" -> "pick up" | "Turn on" -> "turn on" | v -> String.lowercase_ascii v

type play = {
  world : Adventure.world;
  x : float; (* Dave's feet *)
  y : float;
  path : (float * float) list;
  verb : string;
  first : string option; (* "Use key ..." waiting for its second object *)
  doing : Adventure.sentence option; (* walking to do it *)
  saying : string;
  said : int; (* frames the line stays *)
}

type scene = Title | Playing of play | Rescued
type model = scene Scene2d.t

let start () : play =
  { world = Adventure.start "yard" start_places; x = -300.; y = -170.; path = []; verb = "Walk to"; first = None;
    doing = None; saying = ""; said = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the objects to be clicked in the room: lying there, or inside
 * something open there; in the dark, only the way out *)
let clickable (w : Adventure.world) : string list =
  List.filter_map
    (fun (o, _) ->
      if (not (lit w)) && o <> "library exit" then None
      else if Adventure.visible w o && Adventure.where w o <> Adventure.Carried then Some o
      else None)
    w.places

(* the second word of a two-object verb *)
let preposition (verb : string) : string option = match verb with "Use" -> Some "with" | "Give" -> Some "to" | _ -> None

(* where Dave goes to do [s]: the spot of the object it is done to,
 * the second one if the first is carried (use key with door: to the
 * door) *)
let walk_to (w : Adventure.world) (s : Adventure.sentence) : (float * float) option =
  let spot o = if Adventure.where w o = Adventure.Carried then None else Some (hotspot o).spot in
  match s.with_ with Some o2 -> spot o2 | None -> Option.bind s.obj spot

(* a sentence done, where Dave now is: the rules; a room changed puts
 * him at its door *)
let do_sentence (s : Adventure.sentence) (p : play) : play =
  let before = p.world.here in
  let w, reply = Adventure.run rules s p.world in
  let p = { p with world = w; doing = None; saying = reply; said = (if reply = "" then 0 else 180); verb = "Walk to"; first = None } in
  if w.here <> before then
    let x, y = arrival w.here before in
    { p with x; y; path = [] }
  else p

(* a click on object [o] (in the room or the inventory) with the verb
 * chosen: a sentence, done now or once walked to; or half of one *)
let click_object (o : string) (p : play) : play =
  match (preposition p.verb, p.first) with
  | Some _, None -> { p with first = Some o }
  | _ ->
      let obj, with_ = match p.first with Some f -> (Some f, Some o) | None -> (Some o, None) in
      let s : Adventure.sentence = { verb = verb_word p.verb; obj; with_ } in
      match walk_to p.world s with
      | Some target -> { p with path = route p.world.here (p.x, p.y) target; doing = Some s; verb = "Walk to"; first = None }
      | None -> do_sentence s p

(* one frame of walking: towards the next point, 4 pixels *)
let step_walk (p : play) : play =
  match p.path with
  | [] -> ( match p.doing with Some s -> do_sentence s p | None -> p)
  | (tx, ty) :: rest ->
      let d = Float.hypot (tx -. p.x) (ty -. p.y) in
      if d <= 4. then { p with x = tx; y = ty; path = rest } else { p with x = p.x +. ((tx -. p.x) /. d *. 4.); y = p.y +. ((ty -. p.y) /. d *. 4.) }

(* the screen's layout: the room's picture at the top, the sentence,
 * the verbs and the inventory below *)
let room_center (screen : screen) : float * float = (0., screen.top -. 260.)
let verb_at (screen : screen) (i : int) : float * float = (-330. +. (float_of_int (i mod 5) *. 120.), screen.top -. 590. -. (float_of_int (i / 5) *. 50.))
let inventory_at (screen : screen) (i : int) : float * float = (330. +. (float_of_int (i mod 2) *. 150.) -. 60., screen.top -. 590. -. (float_of_int (i / 2) *. 40.))

(* what is under the mouse: a verb, an object in the room or carried,
 * or the floor *)
type target = Verb of string | Object of string | Floor of float * float | Outside

let under (screen : screen) (mx : float) (my : float) (p : play) : target =
  let near (x, y) w h = Float.abs (mx -. x) < w /. 2. && Float.abs (my -. y) < h /. 2. in
  let rcx, rcy = room_center screen in
  let rx, ry = (mx -. rcx, my -. rcy) in
  let verb = List.find_opt (fun i -> near (verb_at screen i) 110. 40.) (List.init (Array.length verbs) Fun.id) in
  let carried = Adventure.carried p.world in
  let item = List.find_opt (fun (i, _) -> near (inventory_at screen i) 140. 32.) (List.mapi (fun i o -> (i, o)) carried) in
  match (verb, item) with
  | Some i, _ -> Verb verbs.(i)
  | _, Some (_, o) -> Object o
  | None, None ->
      if Float.abs rx > 450. || Float.abs ry > 220. then Outside
      else
        (* the last drawn on top: the smaller ones come after *)
        match List.find_opt (fun o -> inside (hotspot o).at rx ry) (List.rev (clickable p.world)) with
        | Some o -> Object o
        | None -> Floor (rx, ry)

let click (screen : screen) (mx : float) (my : float) (p : play) : play =
  match under screen mx my p with
  | Verb v -> { p with verb = v; first = None }
  | Object o -> click_object o p
  | Floor (x, y) -> { p with path = route p.world.here (p.x, p.y) (x, y); doing = None; verb = "Walk to"; first = None }
  | Outside -> p

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title | Rescued -> if computer.mouse.mclick || Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing (start ())) scenes else scenes
  | Playing p ->
      let p = if computer.mouse.mclick then click computer.screen computer.mouse.mx computer.mouse.my p else p in
      let p = step_walk p in
      let p = { p with said = max 0 (p.said - 1) } in
      if p.world.here = "lab" then Scene2d.go Rescued scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : float) (s : string) : shape = words color s |> scale size
let purple = rgb 150 60 200
let dave = rgb 230 230 120

(* what the sentence line says: the verb, the first object, and what a
 * click on the thing under the mouse would add *)
let sentence_line (p : play) (hover : target) : string =
  let name o = (hotspot o).name in
  let hovered = match hover with Object o -> Some (name o) | _ -> None in
  let parts =
    [ Some p.verb ] @ (match p.first with Some f -> [ Some (name f); preposition p.verb ] | None -> []) @ [ hovered ]
  in
  String.concat " " (List.filter_map Fun.id parts)

let object_shape (o : string) (w : Adventure.world) : shape =
  let r = (hotspot o).at in
  let box c = rectangle c r.w r.h |> move r.cx r.cy in
  match o with
  | "mat" -> box (rgb 150 70 50)
  | "key" -> group [ circle (rgb 240 200 60) 6. |> move (r.cx -. 7.) r.cy; rectangle (rgb 240 200 60) 16. 4. |> move (r.cx +. 4.) r.cy ]
  | "front door" | "door out" | "kitchen door" | "hall door" | "library exit" ->
      let opened = (o = "front door" && Adventure.has w "front door open") || o <> "front door" in
      if opened then group [ box (rgb 30 20 20); rectangle (rgb 110 70 40) 12. r.h |> move (r.cx -. (r.w /. 2.)) r.cy ]
      else group [ box (rgb 110 70 40); circle (rgb 230 200 90) 5. |> move (r.cx +. 30.) r.cy ]
  | "library door" -> group [ box (rgb 30 20 20); rectangle (rgb 90 60 40) r.w 10. |> move r.cx (r.cy +. (r.h /. 2.)) ]
  | "tentacle" ->
      group [ oval purple 44. 150. |> move r.cx r.cy; circle white 9. |> move r.cx (r.cy +. 45.); circle black 4. |> move (r.cx +. 3.) (r.cy +. 45.);
              rectangle (rgb 90 30 120) 36. 6. |> move r.cx (r.cy +. 20.) ]
  | "fridge" ->
      if Adventure.has w "fridge open" then group [ box (rgb 230 230 230); rectangle (rgb 250 250 220) (r.w -. 16.) (r.h -. 16.) |> move r.cx r.cy ]
      else group [ box (rgb 220 220 225); rectangle (rgb 150 150 160) 6. 40. |> move (r.cx -. 45.) (r.cy +. 20.) ]
  | "soda" -> group [ box (rgb 200 30 40); rectangle white r.w 6. |> move r.cx r.cy ]
  | "drawer" -> group [ rectangle (rgb 140 100 60) 260. 90. |> move r.cx (r.cy -. 30.); box (rgb 120 80 50); circle (rgb 60 40 20) 4. |> move r.cx r.cy ]
  | "flashlight" -> group [ box (rgb 60 60 70); rectangle (rgb 230 230 160) 8. r.h |> move (r.cx +. (r.w /. 2.)) r.cy ]
  | "bookcase" ->
      group
        (box (rgb 100 60 30)
        :: List.concat
             (List.init 4 (fun row ->
                  List.init 10 (fun k ->
                      rectangle (List.nth [ rgb 160 40 40; rgb 40 90 150; rgb 60 130 60; rgb 190 160 60 ] ((row + k) mod 4)) 18. 40.
                      |> move (r.cx -. 110. +. (float_of_int k *. 24.)) (r.cy +. 80. -. (float_of_int row *. 52.))))))
  | "odd book" -> rectangle (rgb 220 120 30) r.w r.h |> move (r.cx +. 6.) r.cy
  | "secret door" -> group [ box (rgb 20 10 20); text (rgb 120 250 120) 1.2 "LAB" |> move r.cx (r.cy +. 60.) ]
  | _ -> box white

let room_shape (p : play) : shape list =
  let w = p.world in
  let wall, floor =
    match w.here with
    | "yard" -> (rgb 40 50 90, rgb 60 110 50)
    | "hall" -> (rgb 120 60 60, rgb 90 60 40)
    | "kitchen" -> (rgb 190 190 150, rgb 150 120 90)
    | _ -> (rgb 70 50 40, rgb 80 60 50)
  in
  let backdrop =
    [ rectangle wall 900. 440.; rectangle floor 900. 130. |> move_y (-155.) ]
    @ (if w.here = "yard" then [ rectangle (rgb 140 140 150) 500. 260. |> move_y 60.; rectangle (rgb 100 100 110) 140. 50. |> move_y (-100.) ]
       else if w.here = "hall" then [ rectangle (rgb 60 40 30) 300. 60. |> move_y (-110.); rectangle (rgb 80 55 40) 280. 10. |> move_y (-80.) ]
       else [])
  in
  let things = List.map (fun o -> object_shape o w) (clickable w) in
  let kid =
    group [ rectangle (rgb 40 60 140) 26. 40. |> move_y 20.; rectangle dave 30. 36. |> move_y 58.; circle (rgb 240 200 160) 14. |> move_y 88.;
            rectangle (rgb 60 40 20) 30. 8. |> move_y 100. ]
    |> move p.x p.y
  in
  if lit w then backdrop @ things @ [ kid ]
  else [ rectangle black 900. 440. ] @ things @ [ kid; text (rgb 200 200 200) 2. "It's too dark to see anything!" |> move_y 100. ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let bg = rectangle (rgb 10 10 20) screen.width screen.height in
  match model.scene with
  | Title ->
      [ bg; text purple 5. "TINY MANIAC MANSION" |> move_y 200.;
        text white 2. "Dr. Fred has Sandy in his lab. Dave has to get her out." |> move_y 80.;
        text white 2. "click a verb, then things; click the floor to walk" |> move_y 40. ]
      @ Scene2d.blink 1. model [ text white 3. "CLICK TO START" |> move_y (-200.) ]
  | Rescued ->
      [ bg; text (rgb 120 250 120) 4. "SANDY IS FREE!" |> move_y 100.;
        text white 2. "Dave got past the tentacle, into the lab, and out with her." |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "CLICK TO PLAY AGAIN" |> move_y (-200.) ]
  | Playing p ->
      let rcx, rcy = room_center screen in
      let hover = under screen computer.mouse.mx computer.mouse.my p in
      let panel_y = screen.top -. 540. in
      [ bg; group (room_shape p) |> move rcx rcy ]
      @ (if p.said > 0 then [ text dave 2. p.saying |> move 0. (screen.top -. 20.) ] else [])
      @ [ text (rgb 120 200 250) 2. (sentence_line p hover) |> move 0. panel_y ]
      @ List.mapi
          (fun i v ->
            let x, y = verb_at screen i in
            let c = if v = p.verb then rgb 250 250 120 else if hover = Verb v then rgb 150 250 150 else rgb 60 170 60 in
            text c 1.8 v |> move x y)
          (Array.to_list verbs)
      @ List.mapi
          (fun i o ->
            let x, y = inventory_at screen i in
            text (if hover = Object o then rgb 250 200 250 else rgb 200 120 220) 1.8 (hotspot o).name |> move x y)
          (Adventure.carried p.world)

let help = {|TinyManiacMansion
  click a verb, then an object; click the floor to walk
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
