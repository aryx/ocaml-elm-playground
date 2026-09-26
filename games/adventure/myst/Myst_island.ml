(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Myst_island.mli *)

open Playground
open Povray

let width = 512
let height = 342

(*****************************************************************************)
(* The stack's fields *)
(*****************************************************************************)

type fields = (string * string) list

let switches = [ "dock switch"; "pool switch"; "dome switch"; "gear switch" ]
let initial_fields : fields = List.map (fun s -> (s, "down")) switches @ [ ("dial", "0"); ("shelf", "closed") ]
let field (fields : fields) (name : string) : string = try List.assoc name fields with Not_found -> ""

(*****************************************************************************)
(* The surfaces *)
(*****************************************************************************)

let grass = marble ~size:4. (rgb 70 120 50) (rgb 105 150 65)
let stone = marble ~size:1.5 (rgb 200 195 180) (rgb 160 155 145)
let marble_white = marble ~size:0.8 (rgb 235 230 220) (rgb 190 185 175)
let planks = wood (rgb 150 105 60) (rgb 110 75 40)
let sea = shiny 0.35 (marble ~size:6. (rgb 40 80 120) (rgb 30 65 100))
let metal = shiny 0.5 (color (rgb 190 195 200))
let leaves = color (rgb 40 85 40)
let bark = color (rgb 90 60 35)
let white_paint = color (rgb 240 240 235)
let lever_red = color (rgb 180 30 30)

(*****************************************************************************)
(* The island *)
(*****************************************************************************)

(* the sun low in the west, late afternoon, as Myst's island was lit *)
let outdoor_lights = [ sun (rgb 255 240 215) 1. (-0.8) (-0.4); lamp (rgb 70 80 100) 0. 40. 40. ]

(* a marker switch at (x, y, z): a white post and its lever, standing
 * when up and lying on its side when down, which reads from any
 * angle; [turn] faces it in degrees *)
let marker_switch (fields : fields) (name : string) ~(turn : float) (x : float) (y : float) (z : float) : obj =
  let up = field fields name = "up" in
  let lever =
    scale 0.06 0.4 0.06 (box lever_red) |> move 0. 0.4 0. |> rotate 0. 0. (if up then 0. else 80.) |> move 0. 0.7 0.
  in
  union [ scale 0.3 0.35 0.3 (box white_paint) |> move 0. 0.35 0.; lever ] |> rotate 0. turn 0. |> move x y z

(* a tree: a trunk and two cones of leaves *)
let tree (x : float) (z : float) (h : float) : obj =
  union
    [ scale 0.25 (h *. 0.3) 0.25 (cylinder bark) |> move x (2. +. (h *. 0.3)) z;
      scale (h *. 0.35) (h *. 0.45) (h *. 0.35) (cone leaves) |> move x (2. +. (h *. 0.85)) z;
      scale (h *. 0.25) (h *. 0.35) (h *. 0.25) (cone leaves) |> move x (2. +. (h *. 1.3)) z ]

(* the library, a temple on the hill: a floor, a room, a portico of
 * columns, a roof and its pediment *)
let library_door = scale 0.8 1.4 0.1 (box planks) |> move 0. 3.8 (-0.45)

let library : obj =
  let room = diff (scale 3. 1.8 2.5 (box marble_white)) (scale 0.9 1.5 0.5 (box marble_white) |> move 0. (-0.4) 2.3) in
  let column x z = scale 0.3 1.9 0.3 (cylinder marble_white) |> move x 4.3 z in
  let pediment =
    inter [ scale 2.6 2.6 3.6 (box stone) |> rotate 0. 0. 45. |> scale 1.4 0.35 1. |> move 0. 6.5 (-2.); scale 5. 1.5 4. (box stone) |> move 0. 8. (-2.) ]
  in
  union
    ([ scale 4.5 0.4 4.2 (box stone) |> move 0. 2.2 (-2.);
       room |> move 0. 4.2 (-3.);
       scale 4.8 0.2 4.4 (box stone) |> move 0. 6.4 (-2.);
       pediment ]
    @ List.map (fun x -> column x 1.7) [ -3.8; -1.9; 1.9; 3.8 ])
  |> move 0. 0. (-4.)

(* the planetarium's dome, half a sphere of metal on a drum *)
let dome : obj =
  union [ inter [ scale 3. 3. 3. (sphere metal); scale 4. 4. 4. (box metal) |> move 0. 4. 0. ]; scale 3.2 0.5 3.2 (cylinder stone) ]
  |> move (-15.) 2.3 0.

(* the gear, standing on its edge: a wheel, its teeth, its hole *)
let gear : obj =
  let tooth angle = scale 0.45 0.6 0.35 (box stone) |> move 0. 2.4 0. |> rotate 0. 0. angle in
  diff (union ((scale 2.2 0.35 2.2 (cylinder stone) |> rotate 90. 0. 0.) :: List.init 10 (fun i -> tooth (36. *. float_of_int i))))
    (scale 0.6 1. 0.6 (cylinder stone) |> rotate 90. 0. 0.)
  |> rotate 0. (-30.) 0. |> move 15. 5. 1.

(* the pool, and the model of a ship floating in it *)
let pool : obj =
  union
    [ torus 0.15 stone |> scale 2.2 1.5 2.2 |> move 0. 2.25 0.;
      scale 2.2 0.1 2.2 (cylinder (shiny 0.6 (color (rgb 50 80 90)))) |> move 0. 2.15 0.;
      union [ scale 0.6 0.12 0.2 (box planks) |> move 0. 2.35 0.; scale 0.03 0.4 0.03 (cylinder bark) |> move 0. 2.8 0. ]
      |> rotate 0. 30. 0. ]
  |> move 3. 0. 9.

(* the dock, on its posts *)
let dock : obj =
  union
    ((scale 1.4 0.12 5. (box (wood ~size:0.04 (rgb 150 105 60) (rgb 110 75 40))) |> move 0. 0.8 26.)
    :: List.map (fun (x, z) -> scale 0.12 0.8 0.12 (cylinder bark) |> move x 0.3 z)
         [ (-1.3, 22.); (1.3, 22.); (-1.3, 26.); (1.3, 26.); (-1.3, 30.); (1.3, 30.) ])

(* the wreck out at sea, its mast sticking out *)
let wreck : obj =
  union [ scale 1.5 0.6 4. (box planks) |> rotate 0. 20. 15. |> move 6. 0.2 44.; scale 0.12 3. 0.12 (cylinder bark) |> rotate 0. 0. 20. |> move 5.2 2.4 44. ]

(* the path from the cliff's top to the library, and the ramp down to
 * the dock *)
let path : obj =
  union
    [ scale 1.2 0.02 7.5 (box (color (rgb 170 150 120))) |> move 0. 2.01 11.5;
      scale 1. 0.1 1.8 (box planks) |> rotate 20. 0. 0. |> move 0. 1.4 20.5 ]

(* a plateau, flat at y = 2, over cliffs down to the sea *)
let ground : obj = union [ scale 20. 1.5 17. (cylinder grass) |> move 0. 0.5 2.; scale 23. 2. 20. (sphere grass) |> move 0. 0. 2. ]

let island (fields : fields) : (string option * obj) list =
  [ (None, plane sea);
    (None, ground);
    (None, dock);
    (None, wreck);
    (None, path);
    (None, library);
    (Some "library door", library_door |> move 0. 0. (-4.));
    (None, dome);
    (None, gear);
    (Some "pool", pool);
    (Some "dock switch", marker_switch fields "dock switch" ~turn:0. 0.9 0.92 23.);
    (Some "pool switch", marker_switch fields "pool switch" ~turn:(-40.) 5.8 2. 9.);
    (Some "dome switch", marker_switch fields "dome switch" ~turn:(-60.) (-11.) 2. 2.);
    (Some "gear switch", marker_switch fields "gear switch" ~turn:60. 12. 2. 3.5) ]
  @ List.map (fun (x, z, h) -> (None, tree x z h))
      [ (-8., 8., 3.); (-10., -6., 4.); (-6., -12., 3.5); (9., -8., 3.2); (14., -9., 3.8); (-18., 8., 3.); (7., 14., 2.6) ]

(*****************************************************************************)
(* The library, inside *)
(*****************************************************************************)

let indoor_lights = [ lamp (rgb 255 225 180) 0. 3.2 1.; lamp (rgb 90 80 70) 0. 2. 4. ]

(* a shelf of books, [n] of them in turn of these colours *)
let books (y : float) : obj =
  let colours = [| rgb 120 30 30; rgb 30 50 100; rgb 90 70 30; rgb 40 80 50; rgb 110 90 70 |] in
  union
    (List.init 14 (fun i ->
         let h = 0.28 +. (0.06 *. float_of_int (i * 7 mod 3)) in
         scale 0.1 h 0.2 (box (color colours.(i mod 5))) |> move (-1.35 +. (0.21 *. float_of_int i)) (y +. h) 0.))

let bookshelf (fields : fields) : obj =
  let shift = if field fields "shelf" = "open" then -2.8 else 0. in
  union
    ([ scale 1.6 1.5 0.25 (box planks) |> move 0. 1.5 (-0.3) ]
    @ List.map (fun y -> scale 1.55 0.04 0.3 (box planks) |> move 0. y 0.) [ 0.3; 1.; 1.7; 2.4 ]
    @ List.map books [ 0.34; 1.04; 1.74 ])
  |> move shift 0. (-3.7)

(* a lectern with a book on it, of this colour *)
let lectern (x : float) (c : color) : obj * obj =
  ( union [ scale 0.1 0.55 0.1 (cylinder planks) |> move x 0.55 0.; scale 0.4 0.05 0.3 (box planks) |> rotate (-20.) 0. 0. |> move x 1.15 0. ],
    scale 0.28 0.06 0.2 (box (color c)) |> rotate (-20.) 0. 0. |> move x 1.25 0. )

let library_room (fields : fields) : (string option * obj) list =
  let red_stand, red_book = lectern (-2.2) (rgb 170 25 25) and blue_stand, blue_book = lectern 2.2 (rgb 30 50 170) in
  let hall = diff (scale 4. 2. 4.5 (box marble_white)) (scale 3.8 1.9 4.3 (box marble_white)) |> move 0. 1.9 0. in
  [ (None, hall);
    (None, scale 3.8 0.02 4.3 (box (checker ~size:0.8 (rgb 120 40 40) (rgb 200 180 140))) |> move 0. 0. 0.);
    (Some "bookshelf", bookshelf fields);
    (* behind the shelf, the way down *)
    (Some "passage", scale 1.2 1.3 0.1 (box (color (rgb 10 10 10))) |> move 0. 1.3 (-4.05));
    (None, red_stand);
    (Some "red book", red_book);
    (None, blue_stand);
    (Some "blue book", blue_book);
    (Some "dial", union [ scale 0.2 0.5 0.2 (cylinder stone) |> move 0. 0.5 (-1.2); scale 0.35 0.05 0.35 (cylinder metal) |> move 0. 1.05 (-1.2) ]);
    (Some "note", scale 0.25 0.01 0.18 (box white_paint) |> rotate 0. 15. 0. |> move (-1.) 0.62 0.8);
    (None, union [ scale 0.5 0.03 0.4 (box planks) |> move (-1.) 0.6 0.8; scale 0.05 0.3 0.05 (cylinder planks) |> move (-1.) 0.3 0.8 ]) ]

(* the secret room: the green book on its pedestal, under a glass ball *)
let secret_room : (string option * obj) list =
  [ (None, diff (scale 2.5 1.8 2.5 (box stone)) (scale 2.3 1.7 2.3 (box stone)) |> move 0. 1.7 0.);
    (None, scale 0.25 0.5 0.25 (cylinder marble_white) |> move 0. 0.5 (-0.8));
    (Some "green book", scale 0.25 0.05 0.18 (box (color (rgb 30 120 50))) |> move 0. 1.05 (-0.8));
    (None, scale 0.18 0.4 0.18 (cylinder marble_white) |> move 1.1 0.4 (-1.4));
    (None, scale 0.3 0.3 0.3 (sphere (glassy 1.5 (color white))) |> move 1.1 1.1 (-1.4)) ]

(*****************************************************************************)
(* The cards *)
(*****************************************************************************)

type place = Island | Library_room | Secret_room | Nowhere

type card = {
  name : string;
  place : place;
  eye : float * float * float;
  target : float * float * float;
  shows : string list;
  buttons : string list;
  labels : (string * float * float) list;
  script : string;
}

let card ?(shows = []) ?(buttons = []) ?(labels = []) name place eye target script =
  { name; place; eye; target; shows; buttons; labels; script }

(* HyperTalk's layers: a button's script, then its card's, then the
 * stack's *)
let stack_script =
  {|
-- a switch clicked: flipped
on flip
  answer "The switch clicks."
end flip

-- the shelf opens when every switch is up and the dial says how many
on checkShelf
  put 0 into n
  if field "dock switch" is "up" then add 1 to n
  if field "pool switch" is "up" then add 1 to n
  if field "dome switch" is "up" then add 1 to n
  if field "gear switch" is "up" then add 1 to n
  if n = 4 and field "dial" = n then put "open" into field "shelf" else put "closed" into field "shelf"
end checkShelf
|}

let switch_script (name : string) =
  Printf.sprintf
    {|
on mouseUp
  if field "%s" is "up" then put "down" into field "%s" else put "up" into field "%s"
  checkShelf
end mouseUp
|}
    name name name

let button_scripts : (string * string) list =
  List.map (fun s -> (s, switch_script s)) switches
  @ [ ("library door", {|
on mouseUp
  go to card "library"
end mouseUp
|});
      ("pool", {|
on mouseUp
  go to card "pool"
end mouseUp
|});
      ("dial", {|
on mouseUp
  put (field "dial" + 1) mod 10 into field "dial"
  checkShelf
end mouseUp
|});
      ("note",
        {|
on mouseUp
  answer "Four switches keep this island. Raise them all, and set the dial to their number."
end mouseUp
|});
      ("red book",
        {|
on mouseUp
  answer "Sirrus: Bring me the red pages! Do not touch the green book!"
end mouseUp
|});
      ("blue book",
        {|
on mouseUp
  answer "Achenar: Blue pages... please... the green book is a trap!"
end mouseUp
|});
      ("passage", {|
on mouseUp
  if field "shelf" is "open" then go to card "secret"
end mouseUp
|});
      ("green book", {|
on mouseUp
  go to card "linked"
end mouseUp
|}) ]

(* a card's script: where its edges and its middle lead *)
let moves ?forward ?left ?right () =
  let handler msg dest = match dest with None -> "" | Some d -> Printf.sprintf "on %s\n  go to card %S\nend %s\n" msg d msg in
  handler "mouseUp" forward ^ handler "turnLeft" left ^ handler "turnRight" right

let cards : card list =
  [ card "dock" Island (0., 1.9, 30.) (0., 3.5, 0.) ~shows:[ "dock switch" ] ~buttons:[ "dock switch" ]
      (moves ~forward:"path" ~left:"sea" ~right:"sea" ());
    card "sea" Island (0., 1.9, 26.) (5., 1., 44.) (moves ~left:"dock" ~right:"dock" ());
    card "path" Island (0., 3.6, 15.) (0., 4., -4.) ~buttons:[ "pool" ] (moves ~forward:"library outside" ~left:"path west" ~right:"path east" ());
    card "path west" Island (0., 3.6, 13.) (-15., 3.5, 1.) (moves ~forward:"dome" ~left:"path south" ~right:"path" ());
    card "path east" Island (0., 3.6, 13.) (15., 4., 1.) (moves ~forward:"gear" ~left:"path" ~right:"path south" ());
    card "path south" Island (0., 3.6, 13.) (0., 1., 30.) (moves ~forward:"dock" ~left:"path east" ~right:"path west" ());
    card "dome" Island (-7., 3.4, 5.) (-13., 3., 1.) ~shows:[ "dome switch" ] ~buttons:[ "dome switch" ] (moves ~left:"path" ~right:"path" ());
    card "gear" Island (7., 3.4, 6.) (13., 3.5, 2.) ~shows:[ "gear switch" ] ~buttons:[ "gear switch" ] (moves ~left:"path" ~right:"path" ());
    card "pool" Island (1., 3.8, 13.) (4.5, 2., 8.5) ~shows:[ "pool switch" ] ~buttons:[ "pool switch" ] (moves ~left:"path" ~right:"path" ());
    card "library outside" Island (0., 4., 3.) (0., 4.5, -6.) ~buttons:[ "library door" ] (moves ~left:"path south" ~right:"path south" ());
    card "library" Library_room (0., 1.7, 3.9) (0., 1.2, -3.) ~shows:[ "shelf" ] ~buttons:[ "dial"; "note"; "red book"; "blue book"; "passage" ]
      ~labels:[ ("dial", 256., 176.) ]
      (moves ~left:"library outside" ~right:"library outside" ());
    card "secret" Secret_room (0., 1.8, 2.1) (0.2, 0.8, -1.) ~buttons:[ "green book" ] (moves ~left:"library" ~right:"library" ());
    card "linked" Nowhere (0., 0., 1.) (0., 0., 0.) "" ]

let find_card (name : string) : card option = List.find_opt (fun c -> c.name = name) cards

(*****************************************************************************)
(* The pictures *)
(*****************************************************************************)

(* what the card's picture depends on: the fields it shows, the others
 * as they start -- a switch too far to see is drawn down, whatever it
 * is, so that a card has one still per value of what it shows *)
let restricted (c : card) (fields : fields) : fields =
  List.map (fun (k, v) -> if List.mem k c.shows then (k, field fields k) else (k, v)) initial_fields

let objects (c : card) (fields : fields) : (string option * obj) list =
  let fields = restricted c fields in
  match c.place with
  | Island -> island fields
  | Library_room -> library_room fields
  | Secret_room -> secret_room
  | Nowhere -> []

let scene_of (c : card) (objs : (string option * obj) list) : scene =
  let camera = camera ~fov:55. ~eye:c.eye ~target:c.target () in
  match c.place with
  | Island | Nowhere -> scene ~ambient:0.3 ~sky:(rgb 150 185 220) ~camera outdoor_lights (List.map snd objs)
  | Library_room -> scene ~ambient:0.25 ~sky:black ~camera indoor_lights (List.map snd objs)
  | Secret_room -> scene ~ambient:0.2 ~sky:black ~camera [ lamp (rgb 220 255 225) 0. 3. 1.5; lamp (rgb 90 90 110) (-1.5) 1. (-1.8) ] (List.map snd objs)

let scene (c : card) (fields : fields) : scene = scene_of c (objects c fields)

let options = { Raytrace.default_options with samples = 2 }

let slug (s : string) : string = String.map (fun ch -> if ch = ' ' then '_' else ch) s

let still_name (c : card) (fields : fields) : string =
  String.concat "-" (slug c.name :: List.map (fun k -> slug (field fields k)) c.shows)

(* every value of a card's shown fields *)
let values (name : string) : string list =
  if List.mem name switches then [ "down"; "up" ] else if name = "shelf" then [ "closed"; "open" ] else [ field initial_fields name ]

let stills : (card * fields) list =
  List.concat_map
    (fun c ->
      if c.place = Nowhere then []
      else
        let rec combos = function
          | [] -> [ initial_fields ]
          | k :: rest -> List.concat_map (fun f -> List.map (fun v -> (k, v) :: List.remove_assoc k f) (values k)) (combos rest)
        in
        List.map (fun f -> (c, f)) (combos c.shows))
    cards

let pick (c : card) (fields : fields) ~(x : float) ~(y : float) : string option =
  let objs = objects c fields in
  match Povray.pick (scene_of c objs) ~width ~height x y with
  | None -> None
  | Some hit -> (
      match List.find_opt (fun (_, o) -> o == hit) objs with
      | Some (Some name, _) when List.mem name c.buttons -> Some name
      | _ -> None)
