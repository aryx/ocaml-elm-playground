(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Myst (Rand and Robyn Miller, Cyan, Broderbund,
 * 1993): an island, deserted, a library on its hill, four marker
 * switches, two brothers trapped in two books, and a secret.
 *
 *   click the middle of the picture to go forward, its left or right
 *   edge to turn; click a thing to use it (a switch, a book, a door)
 *
 * Myst was a slideshow. About 2,500 still pictures, each ray traced
 * overnight on the Millers' Macintoshes (StrataVision 3D), shown one
 * at a time; a click on the right part of one showed the next. And the
 * program showing them was a HyperCard stack: a card per picture,
 * buttons over the parts that could be clicked, HyperTalk scripts
 * saying what a click did. (Names and numbers from memory, to check.)
 *
 * So is this, with the repository's own pieces: the island is a scene
 * of the Povray way (myst/Myst_island.ml), its stills made once by our
 * ray tracer (myst/make_stills.exe, a few minutes) and embedded in the
 * program as JPEGs; the scripts are HyperTalk, run by TinyHyperCard's
 * interpreter (Hypertalk), the messages going as HyperCard's did,
 *
 *     button  ->  card  ->  stack
 *
 * a switch's script flipping its field, a card's script saying where
 * its edges lead, the stack's checking the puzzle for all of them.
 *
 * The trick of this game (Myst's own): nothing is computed while you
 * play. Every picture was made before, in every state it can show --
 * the dock with its switch up, and with it down -- so a click only
 * ever chooses a picture. The price is the pictures: one per place,
 * per way of looking, per state seen from there (a switch too far to
 * see is drawn down whatever it is: Myst_island.restricted).
 *
 * What is ours and not Cyan's: the buttons. Cyan drew their hotspots
 * as rectangles over each picture; here a click is a ray, cast from
 * the card's camera through the pixel clicked, and the solid it meets
 * is the button (Povray.pick) -- the renderer's primitive used again,
 * so the switch is clickable exactly where it is drawn.
 *
 * With the flag render=live the stills are not read but ray traced as
 * you arrive, coarse to fine (Raytrace.start): the making of the game
 * inside the game, and slow on purpose. A still missing from the
 * embedded ones is made so too.
 *
 * What it uses: Povray (the scenes, and pick), Raytrace (the live
 * pictures), Hypertalk (the scripts), Jpeg (the stills). Not the
 * adventure kit (gamekits/adventure): the world is HyperCard's, fields
 * and cards, and the rules are scripts.
 *
 * Left undone, exercises: the sounds (Myst's were half its
 * atmosphere: a loop of wind and sea per place, from audio/); the
 * brothers' pages, and the books' flickering faces (Myst's QuickTime
 * movies); more of the island (the clock tower, the rocket, the
 * cabin), each a few cards and a scene; the Ages; the zip mode.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  (* the card shown, by name *)
  card : string;
  (* the stack's fields: the island's state *)
  fields : Myst_island.fields;
  (* what the last "answer" said, shown until the next click *)
  dialog : string option;
  (* the picture left behind, fading out over the new one, and the
   * frames it has left: Myst's dissolve *)
  leaving : (Rgba_image.t * int) option;
}

let initial : model =
  { card = (List.hd Myst_island.cards).name; fields = Myst_island.initial_fields; dialog = None; leaving = None }

let dissolve_frames = 20

let card (m : model) : Myst_island.card =
  match Myst_island.find_card m.card with Some c -> c | None -> failwith ("TinyMyst: no card " ^ m.card)

(*****************************************************************************)
(* The pictures *)
(*****************************************************************************)

(* claude: memo tables, by still name -- the stills decoded once, the
 * live pictures made a slice per frame in place (a Raytrace.progress
 * is mutable, as in Povray) *)
let decoded : (string, Rgba_image.t) Hashtbl.t = Hashtbl.create 16
let live : (string, Raytrace.progress) Hashtbl.t = Hashtbl.create 16

let rendering_live (computer : computer) : bool = List.assoc_opt "render" computer.flags = Some "live"

(* the progress making that picture live, started if not yet *)
let progress (c : Myst_island.card) (fields : Myst_island.fields) : Raytrace.progress =
  let name = Myst_island.still_name c fields in
  match Hashtbl.find_opt live name with
  | Some p -> p
  | None ->
      let p =
        Raytrace.start (Povray.raytrace_scene (Myst_island.scene c fields)) ~width:Myst_island.width
          ~height:Myst_island.height
      in
      Hashtbl.replace live name p;
      p

(* the card's picture in that state: its still, or as far as its live
 * rendering has got; None for a card with no picture *)
let picture ~(live_mode : bool) (c : Myst_island.card) (fields : Myst_island.fields) : Rgba_image.t option =
  if c.place = Nowhere then None
  else
    let name = Myst_island.still_name c fields in
    match (live_mode, List.assoc_opt name Myst_stills.all) with
    | false, Some jpeg ->
        Some
          (match Hashtbl.find_opt decoded name with
          | Some img -> img
          | None ->
              let img = Jpeg.decode jpeg in
              Hashtbl.replace decoded name img;
              img)
    | _ -> Some (Raytrace.picture (progress c fields))

(*****************************************************************************)
(* The scripts *)
(*****************************************************************************)

let parsed : (string, Hypertalk.script) Hashtbl.t = Hashtbl.create 16

let parse (text : string) : Hypertalk.script =
  match Hashtbl.find_opt parsed text with
  | Some s -> s
  | None ->
      let s = Hypertalk.parse text in
      Hashtbl.replace parsed text s;
      s

(* the island as HyperTalk sees it: the fields, the cards, "answer" *)
let world : model Hypertalk.world =
  { get_field =
      (fun m name ->
        match List.assoc_opt name m.fields with
        | Some v -> v
        | None -> raise (Hypertalk.Error (Printf.sprintf "no field %S" name)));
    set_field = (fun m name v -> { m with fields = (name, v) :: List.remove_assoc name m.fields });
    go =
      (fun m r ->
        match r with
        | Named name when Myst_island.find_card name <> None -> { m with card = name }
        | _ -> raise (Hypertalk.Error "go: to a card by its name"));
    answer = (fun m s -> { m with dialog = Some s });
    beep = (fun m -> m);
    number_of_cards = (fun _ -> List.length Myst_island.cards);
    card_number =
      (fun m ->
        let rec index i = function [] -> 0 | (c : Myst_island.card) :: rest -> if c.name = m.card then i else index (i + 1) rest in
        index 1 Myst_island.cards);
    card_name = (fun m -> m.card) }

(* [send m ~button msg]: the message along the path from the button (if
 * any) to the card and the stack; a script's mistake is shown as
 * HyperCard did, in a dialog *)
let send (m : model) ?button (msg : string) : model =
  let from = match Option.bind button (fun b -> List.assoc_opt b Myst_island.button_scripts) with Some s -> [ s ] | None -> [] in
  try Hypertalk.send world (List.map parse (from @ [ (card m).script; Myst_island.stack_script ])) msg m
  with Hypertalk.Error e -> { m with dialog = Some ("Script error: " ^ e) }

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

(* the picture drawn as large as the window holds, centred *)
let zoom (computer : computer) : float =
  Float.min (computer.screen.width /. float_of_int Myst_island.width) (computer.screen.height /. float_of_int Myst_island.height)

(* the mouse in the picture's pixels, from its top left *)
let in_picture (computer : computer) : float * float =
  let z = zoom computer in
  ((computer.mouse.mx /. z) +. (float_of_int Myst_island.width /. 2.), (float_of_int Myst_island.height /. 2.) -. (computer.mouse.my /. z))

(* the pixels of either edge that turn *)
let edge = 64.

(* what a click there would do *)
type target = Turn_left | Turn_right | Button of string | Forward | Nothing

let target (m : model) (computer : computer) : target =
  let c = card m in
  let x, y = in_picture computer in
  let has msg = List.mem (String.lowercase_ascii msg) (Hypertalk.handlers (parse c.script)) in
  if c.place = Nowhere || x < 0. || y < 0. || x > float_of_int Myst_island.width || y > float_of_int Myst_island.height then Nothing
  else if x < edge && has "turnLeft" then Turn_left
  else if x > float_of_int Myst_island.width -. edge && has "turnRight" then Turn_right
  else
    match Myst_island.pick c m.fields ~x ~y with
    | Some b when List.mem_assoc b Myst_island.button_scripts -> Button b
    | _ -> if has "mouseUp" then Forward else Nothing

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (m : model) : model =
  let live_mode = rendering_live computer in
  let before = m in
  let m = { m with leaving = (match m.leaving with Some (img, n) when n > 1 -> Some (img, n - 1) | _ -> None) } in
  let m =
    if not computer.mouse.mclick then m
    else if m.dialog <> None then { m with dialog = None }
    else
      match target m computer with
      | Turn_left -> send m "turnLeft"
      | Turn_right -> send m "turnRight"
      | Button b -> send m ~button:b "mouseUp"
      | Forward -> send m "mouseUp"
      | Nothing -> m
  in
  (* a new picture: the old one dissolves over it *)
  let m =
    if Myst_island.still_name (card before) before.fields = Myst_island.still_name (card m) m.fields then m
    else
      match picture ~live_mode (card before) before.fields with
      | Some old -> { m with leaving = Some (old, dissolve_frames) }
      | None -> m
  in
  (* live: this card's picture a few more rays along *)
  (if live_mode && (card m).place <> Nowhere then
     let p = progress (card m) m.fields in
     if not (Raytrace.finished p) then Raytrace.advance p ~rays:20_000);
  m

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view (computer : computer) (m : model) : shape list =
  let z = zoom computer in
  let w = float_of_int Myst_island.width *. z and h = float_of_int Myst_island.height *. z in
  let c = card m in
  (* a point of the picture, on the screen *)
  let at px py shape = move ((px -. (float_of_int Myst_island.width /. 2.)) *. z) (((float_of_int Myst_island.height /. 2.) -. py) *. z) shape in
  let still = match picture ~live_mode:(rendering_live computer) c m.fields with Some img -> [ bitmap w h img ] | None -> [] in
  let dissolving =
    match m.leaving with
    | Some (img, n) -> [ fade (float_of_int n /. float_of_int dissolve_frames) (bitmap w h img) ]
    | None -> []
  in
  (* the card's fields, drawn over its picture *)
  let labels = List.map (fun (f, px, py) -> at px py (scale z (words white (Myst_island.field m.fields f)))) c.labels in
  let ending =
    if c.place = Nowhere then
      [ move 0. 20. (words white "The green book takes you home.");
        move 0. (-20.) (words (rgb 150 150 150) "The End") ]
    else []
  in
  (* the cursor, as Myst's hand told you what a click would do *)
  let cursor =
    let x = computer.mouse.mx and y = computer.mouse.my in
    let glyph s = [ move (x +. 14.) (y -. 14.) (words (rgb 255 230 150) s) ] in
    match target m computer with
    | Turn_left -> glyph "<"
    | Turn_right -> glyph ">"
    | Forward -> glyph "^"
    | Button _ -> [ move x y (circle (rgb 255 230 150) 6.) ]
    | Nothing -> []
  in
  let dialog =
    match m.dialog with
    | None -> []
    | Some s ->
        [ move 0. ((-.h /. 2.) +. 40.) (rectangle (rgb 245 240 225) (w *. 0.9) 50.);
          move 0. ((-.h /. 2.) +. 40.) (words black s) ]
  in
  [ rectangle black computer.screen.width computer.screen.height ] @ still @ dissolving @ labels @ ending @ dialog @ cursor

let app = game view update initial
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
