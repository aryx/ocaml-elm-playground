(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Zork (Tim Anderson, Marc Blank, Bruce Daniels and
 * Dave Lebling, MIT, 1977; Infocom, 1980): a white house, a forest, a
 * trap door, and the Great Underground Empire below -- in words only.
 *
 *   type what to do, and enter: "open mailbox", "north", "take lamp",
 *   "turn on lamp", "kill troll with sword", "put egg in case"
 *
 * The text adventure started with Colossal Cave (Will Crowther, 1976, a
 * caver writing his cave for his daughters, and Don Woods, 1977, who
 * made it a game); the MIT students who played it wrote Zork on the
 * lab's PDP-10, and then founded Infocom to sell it. Its players
 * remember the parser -- it understood "put the jewel-encrusted egg in
 * the trophy case" where Colossal Cave understood two words -- and the
 * grue, the thing in the dark that eats you, which is only a sentence
 * and was never seen. (Names and dates from memory, to check.)
 *
 * The game that is nearly all model and hardly any view: the Elm
 * architecture with the picture taken away, and a line of text for the
 * input. What it is made of:
 *
 *   - The world ([rooms], [objects], [rules]), as data, on the
 *     adventure kit (gamekits/adventure's Adventure): where each object
 *     is, what has happened (the flags: "window open", "troll dead"),
 *     and a rule for each thing to try. A puzzle is a rule whose test
 *     reads a flag another one set: the window before the kitchen, the
 *     rug before the trap door, the lamp before the dark, the sword
 *     before the troll.
 *   - The parser ([parse]): words to a sentence. Lower case, the noise
 *     words dropped ("the", "brass", "jewel-encrusted"), the synonyms
 *     folded ("get" is "take", "lantern" is "lamp"), two-word verbs
 *     joined ("pick up", "turn on"); then a verb, an object, and after
 *     a preposition ("in", "with") a second one. A direction alone is a
 *     move, which is the map's business ([exits]), not a rule's. An
 *     unknown word is said back, the way Zork did.
 *   - The dark ([dark]): four rooms without a light of their own. With
 *     no lamp lit, nothing can be seen there, and moving on in the dark
 *     is to be eaten by a grue.
 *
 * What Zork brought beyond the parser was the Z-machine (Joel Berez and
 * Marc Blank, Infocom, 1979): the game compiled not for a computer but
 * for an imaginary one, a virtual machine, with a small interpreter
 * written once for each of the twenty computers of 1980 -- so every
 * Infocom game ran on all of them, twenty years before Java said
 * "write once, run anywhere". This toy is the other way round: one
 * program, and the playground's backends are the interpreters.
 *
 * What it uses: gamekits/adventure's Adventure (the world and the
 * rules, shared with TinyManiacMansion, which makes its sentences by
 * clicking instead of typing), Scene2d (enter and backspace, pressed
 * rather than held). The typing is computer.keyboard.typed (see
 * examples/Typing.ml).
 *
 * Left undone, exercises: "take all" and "it" (the parser remembering
 * the last object); the thief, who wanders and steals; a fight with
 * chance in it, as Zork's troll fought; save and restore; the rest of
 * the Great Underground Empire, whose map was drawn on graph paper by
 * every player; the Z-machine itself, a story file and an interpreter
 * (TinyCoreWar has a virtual machine to start from).
 *)
open Playground

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* a room: its name (the status line's), what it looks like, and
 * whether it is dark *)
let rooms : (string * (string * string * bool)) list =
  [ ("west", ("West of House",
      "You are standing in an open field west of a white house, with a boarded front door.", false));
    ("north", ("North of House",
      "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. \
       To the north a narrow path winds through the trees.", false));
    ("behind", ("Behind House",
      "You are behind the white house. A path leads into the forest to the east. In one corner of the house there \
       is a small window.", false));
    ("kitchen", ("Kitchen",
      "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of \
       food. A passage leads to the west, and a window to the east.", false));
    ("living", ("Living Room",
      "You are in the living room. There is a doorway to the east, and a wooden door with strange gothic \
       lettering to the west, which appears to be nailed shut.", false));
    ("forest", ("Forest Path",
      "This is a path winding through a dimly lit forest. One particularly large tree with some low branches \
       stands at the edge of the path. The house is to the south.", false));
    ("tree", ("Up a Tree",
      "You are about 10 feet above the ground nestled among some large branches.", false));
    ("cellar", ("Cellar",
      "You are in a dark and damp cellar with a narrow passageway leading north. A rickety staircase leads up.", true));
    ("troll", ("The Troll Room",
      "This is a small room with passages to the east and south. Bloodstains and deep scratches (perhaps made by \
       an axe) mar the walls.", true));
    ("passage", ("East-West Passage",
      "This is a narrow east-west passageway. There is a narrow stairway leading down at the north end of the \
       room.", true));
    ("gallery", ("Gallery",
      "This is an art gallery. Most of the paintings have been stolen by vandals with exceptional taste. The \
       vandals left through the west exit.", true)) ]

let room_name (r : string) : string = match List.assoc_opt r rooms with Some (n, _, _) -> n | None -> r
let dark (r : string) : bool = match List.assoc_opt r rooms with Some (_, _, d) -> d | None -> false

(* an object: its name, the line saying it is here (None: part of the
 * room, never listed), whether it can be taken, what examining it
 * shows *)
type thing = { name : string; lying : string option; portable : bool; looks : string }

let objects : (string * thing) list =
  [ ("mailbox", { name = "small mailbox"; lying = Some "There is a small mailbox here."; portable = false;
                  looks = "It's an ordinary mailbox." });
    ("leaflet", { name = "leaflet"; lying = Some "There is a leaflet here."; portable = true;
                  looks = "It says \"WELCOME TO ZORK!\" at the top." });
    ("window", { name = "window"; lying = None; portable = false; looks = "The window is slightly ajar." });
    ("lamp", { name = "brass lantern"; lying = Some "There is a brass lantern (battery-powered) here."; portable = true;
               looks = "A brass lantern, with a switch." });
    ("sword", { name = "elvish sword"; lying = Some "Above the trophy case hangs an elvish sword of great antiquity.";
                portable = true; looks = "The sword is of elvish workmanship." });
    ("rug", { name = "oriental rug"; lying = Some "A large oriental rug is in the center of the room."; portable = false;
              looks = "It is a beautiful rug, and very heavy." });
    ("door", { name = "trap door"; lying = Some "There is a trap door in the floor."; portable = false;
               looks = "The trap door is set into the floor." });
    ("case", { name = "trophy case"; lying = Some "There is a trophy case here."; portable = false;
               looks = "The trophy case has a glass front, for treasures to be seen." });
    ("egg", { name = "jewel-encrusted egg"; lying = Some "In a bird's nest here is a large egg encrusted with precious jewels.";
              portable = true; looks = "The egg is covered with fine gold inlay and lapis lazuli." });
    ("painting", { name = "painting"; lying = Some "Fortunately, there is still one chance for you to be a vandal, for on \
                                                   the far wall is a painting of unparalleled beauty."; portable = true;
                   looks = "A masterpiece by a neglected genius." });
    ("troll", { name = "troll"; lying = Some "A nasty-looking troll, brandishing a bloody axe, blocks all passages out \
                                              of the room."; portable = false; looks = "The troll is not pretty." }) ]

let thing (o : string) : thing = List.assoc o objects
let treasures = [ "egg"; "painting" ]

let start_places : (string * Adventure.place) list =
  Adventure.
    [ ("mailbox", Room "west"); ("leaflet", Inside "mailbox"); ("window", Room "behind"); ("lamp", Room "living");
      ("sword", Room "living"); ("rug", Room "living"); ("door", Nowhere); ("case", Room "living");
      ("egg", Room "tree"); ("painting", Room "gallery"); ("troll", Room "troll") ]

(* the trophy case is always open: its flag, set from the start *)
let start_world : Adventure.world = Adventure.start "west" start_places |> Adventure.set "case open"

(* where each way goes: the room, or why not *)
let exits (w : Adventure.world) (dir : string) : (string, string) result =
  let has = Adventure.has w in
  match (w.here, dir) with
  | "west", "north" | "behind", "north" | "forest", "south" -> Ok "north"
  | "west", "east" -> Error "The door is boarded and you can't remove the boards."
  | "north", "west" -> Ok "west"
  | "north", "east" -> Ok "behind"
  | "kitchen", "east" -> if has "window open" then Ok "behind" else Error "The window is closed."
  | "north", "north" | "tree", "down" -> Ok "forest"
  | "forest", "up" -> Ok "tree"
  | "behind", "west" | "behind", "in" -> if has "window open" then Ok "kitchen" else Error "The window is closed."
  | "kitchen", "west" -> Ok "living"
  | "living", "east" -> Ok "kitchen"
  | "living", "down" -> if has "door open" then Ok "cellar" else Error "You can't go that way."
  | "cellar", "up" -> Ok "living"
  | "cellar", "north" | "passage", "west" -> Ok "troll"
  | "troll", "south" -> Ok "cellar"
  | "troll", "east" -> if has "troll dead" then Ok "passage" else Error "The troll fends you off with a menacing gesture."
  | "passage", "east" -> Ok "gallery"
  | "gallery", "west" -> Ok "passage"
  | _ -> Error "You can't go that way."

let lit (w : Adventure.world) : bool = (not (dark w.here)) || (Adventure.visible w "lamp" && Adventure.has w "lamp on")

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let rule verb obj with_ test act : Adventure.rule = { verb; obj; with_; test; act }
let reply (msg : string) : Adventure.sentence -> Adventure.world -> Adventure.world * string = fun _ w -> (w, msg)
let flag_then (f : string) (msg : string) = fun _ w -> (Adventure.set f w, msg)
let is_carried o = fun _ (w : Adventure.world) -> Adventure.where w o = Adventure.Carried
let lacks f = fun _ (w : Adventure.world) -> not (Adventure.has w f)
let the_obj (s : Adventure.sentence) : string = Option.get s.obj

(* what the case holds when a treasure goes in: 50 points each, once *)
let score_treasure (s : Adventure.sentence) (w : Adventure.world) : Adventure.world * string =
  let o = the_obj s in
  let w = Adventure.put o (Inside "case") w in
  if List.mem o treasures && not (Adventure.has w (o ^ " scored")) then
    let w = { (Adventure.set (o ^ " scored") w) with score = w.score + 50 } in
    if w.score = 50 * List.length treasures then
      (Adventure.set "won" w,
       "Done. An almost inaudible voice whispers in your ear, \"Look to your treasures for the final secret.\"")
    else (w, "Done.")
  else (w, "Done.")

(* the specific ones first, the generic ones last *)
let rules : Adventure.rule list =
  let open Adventure in
  [ rule "open" (Some "mailbox") None (lacks "mailbox open")
      (flag_then "mailbox open" "Opening the small mailbox reveals a leaflet.");
    rule "read" (Some "leaflet") None always
      (reply "\"WELCOME TO ZORK! Zork is a game of adventure, danger, and low cunning. In it you will explore some of \
              the most amazing territory ever seen by mortals.\"");
    rule "open" (Some "window") None (lacks "window open")
      (flag_then "window open" "With great effort, you open the window far enough to allow entry.");
    rule "move" (Some "rug") None (lacks "rug moved")
      (fun _ w ->
        (set "rug moved" w |> put "door" (Room "living"),
         "With a great effort, the rug is moved to one side of the room, revealing the dusty cover of a closed trap door."));
    rule "move" (Some "rug") None always (reply "Having moved the carpet previously, you find it impossible to move it again.");
    rule "open" (Some "door") None (lacks "door open")
      (flag_then "door open" "The door reluctantly opens to reveal a rickety staircase descending into darkness.");
    rule "light" (Some "lamp") None (lacks "lamp on") (flag_then "lamp on" "The brass lantern is now on.");
    rule "extinguish" (Some "lamp") None (fun _ w -> has w "lamp on")
      (fun _ w -> (unset "lamp on" w, "The brass lantern is now off."));
    rule "attack" (Some "troll") (Some "sword") (is_carried "sword")
      (fun _ w ->
        (put "troll" Nowhere w |> set "troll dead",
         "Your sword finds the troll's heart. The troll, disarmed and dying, vanishes in a cloud of sinister black fog."));
    rule "attack" (Some "troll") None always (reply "Attacking the troll with your bare hands is suicidal.");
    rule "put" any (Some "case") (fun s w -> where w (the_obj s) = Carried) score_treasure;
    rule "take" (Some "troll") None always (reply "The troll spits in your face, grunting \"Better luck next time\".");
    rule "take" any None (fun s w -> where w (the_obj s) = Carried) (reply "You already have that!");
    rule "take" any None (fun s _ -> (thing (the_obj s)).portable) (fun s w -> (put (the_obj s) Carried w, "Taken."));
    rule "take" any None always (fun s w -> (w, "The " ^ (thing (the_obj s)).name ^ " is securely anchored."));
    rule "drop" any None (fun s w -> where w (the_obj s) = Carried) (fun s w -> (put (the_obj s) (Room w.here) w, "Dropped."));
    rule "open" any None (fun s w -> has w (the_obj s ^ " open")) (reply "It is already open.");
    rule "close" any None (fun s w -> has w (the_obj s ^ " open") && the_obj s <> "case")
      (fun s w -> (unset (the_obj s ^ " open") w, "Closed."));
    rule "examine" any None always (fun s w -> (w, (thing (the_obj s)).looks)) ]

(*****************************************************************************)
(* The parser *)
(*****************************************************************************)

let directions =
  [ ("n", "north"); ("north", "north"); ("s", "south"); ("south", "south"); ("e", "east"); ("east", "east");
    ("w", "west"); ("west", "west"); ("u", "up"); ("up", "up"); ("d", "down"); ("down", "down"); ("in", "in");
    ("enter", "in"); ("climb", "up") ]

let verbs =
  [ ("take", "take"); ("get", "take"); ("pick up", "take"); ("drop", "drop"); ("open", "open"); ("close", "close");
    ("read", "read"); ("move", "move"); ("push", "move"); ("pull", "move"); ("turn on", "light"); ("light", "light");
    ("turn off", "extinguish"); ("extinguish", "extinguish"); ("attack", "attack"); ("kill", "attack");
    ("hit", "attack"); ("put", "put"); ("place", "put"); ("examine", "examine"); ("x", "examine");
    ("look at", "examine") ]

let nouns =
  [ ("mailbox", "mailbox"); ("box", "mailbox"); ("leaflet", "leaflet"); ("mail", "leaflet"); ("window", "window");
    ("lamp", "lamp"); ("lantern", "lamp"); ("sword", "sword"); ("rug", "rug"); ("carpet", "rug"); ("door", "door");
    ("trapdoor", "door"); ("case", "case"); ("egg", "egg"); ("painting", "painting"); ("troll", "troll") ]

let noise =
  [ "the"; "a"; "an"; "small"; "brass"; "elvish"; "oriental"; "trophy"; "trap"; "jewel-encrusted"; "jeweled";
    "large"; "nasty"; "go"; "walk"; "tree"; "up" ]

let prepositions = [ "in"; "into"; "with"; "on"; "to"; "inside" ]

type command =
  | Move of string
  | Say of Adventure.sentence
  | Look
  | Inventory
  | Score
  | Unknown of string (* the word not understood *)
  | Nothing

(* Words to a command: the two-word verbs joined first ("pick up",
 * "turn on"), then the first word the verb, the nouns after it, the
 * second after a preposition. "climb tree" is "up", "go north" is
 * "north": "go" and "tree" are noise once the verb is known. *)
let parse (line : string) : command =
  let clean = String.map (fun c -> if c = ',' || c = '.' || c = '!' || c = '?' then ' ' else Char.lowercase_ascii c) line in
  let words = String.split_on_char ' ' clean |> List.filter (( <> ) "") in
  let words =
    match words with
    | a :: b :: rest when List.mem_assoc (a ^ " " ^ b) verbs -> (a ^ " " ^ b) :: rest
    | "go" :: rest -> rest
    | _ -> words
  in
  match words with
  | [] -> Nothing
  | [ ("l" | "look") ] -> Look
  | [ ("i" | "inventory") ] -> Inventory
  | [ "score" ] -> Score
  | [ "xyzzy" ] -> Unknown "xyzzy"
  | [ "climb"; "down" ] -> Move "down"
  | d :: rest when List.mem_assoc d directions && List.for_all (fun x -> List.mem x noise) rest -> Move (List.assoc d directions)
  | v :: rest when List.mem_assoc v verbs -> (
      let rest = List.filter (fun x -> not (List.mem x noise)) rest in
      let rec split before = function
        | p :: after when List.mem p prepositions -> (List.rev before, after)
        | x :: after -> split (x :: before) after
        | [] -> (List.rev before, [])
      in
      let first, second = split [] rest in
      let noun ws = match ws with [] -> Ok None | [ n ] when List.mem_assoc n nouns -> Ok (Some (List.assoc n nouns)) | n :: _ -> Error n in
      match (noun first, noun second) with
      | Ok obj, Ok with_ -> Say { verb = List.assoc v verbs; obj; with_ }
      | Error n, _ | _, Error n -> Unknown n)
  | w :: _ -> Unknown w

(*****************************************************************************)
(* The game: a command, and what is printed back *)
(*****************************************************************************)

type game = { world : Adventure.world; out : string list; (* what was printed, oldest first *) dead : bool }

let describe (w : Adventure.world) : string list =
  if not (lit w) then [ "It is pitch black. You are likely to be eaten by a grue." ]
  else
    let _, text, _ = List.assoc w.here rooms in
    let lines = List.filter_map (fun o -> (thing o).lying) (Adventure.in_room w) in
    let open_box = if w.here = "west" && Adventure.has w "mailbox open" && Adventure.where w "leaflet" = Inside "mailbox" then [ "The small mailbox contains a leaflet." ] else [] in
    let case = if w.here = "living" then List.filter_map (fun (o, p) -> if p = Adventure.Inside "case" then Some ("Your collection of treasures consists of: a " ^ (thing o).name ^ ".") else None) w.places else [] in
    room_name w.here :: text :: (lines @ open_box @ case)

let say (lines : string list) (g : game) : game = { g with out = g.out @ lines }

let walk (dir : string) (g : game) : game =
  let w = { g.world with turns = g.world.turns + 1 } in
  match exits w dir with
  | Error why -> say [ why ] { g with world = w }
  | Ok room ->
      (* in the dark, going on into more dark: the grue *)
      if (not (lit w)) && dark room then
        say [ "Oh, no! You have walked into the slavering fangs of a lurking grue!"; "****  You have died  ****" ]
          { g with world = w; dead = true }
      else
        let w = Adventure.go room w in
        say (describe w) { g with world = w }

let command (line : string) (g : game) : game =
  let g = say [ ""; "> " ^ line ] g in
  if g.dead || Adventure.has g.world "won" then g
  else
    match parse line with
    | Nothing -> say [ "I beg your pardon?" ] g
    | Unknown "xyzzy" -> say [ "A hollow voice says \"Fool.\"" ] g
    | Unknown word -> say [ Printf.sprintf "I don't know the word \"%s\"." word ] g
    | Look -> say (describe g.world) g
    | Score ->
        say [ Printf.sprintf "Your score is %d (total of %d points), in %d moves." g.world.score (50 * List.length treasures) g.world.turns ] g
    | Inventory -> (
        match Adventure.carried g.world with
        | [] -> say [ "You are empty-handed." ] g
        | l -> say ("You are carrying:" :: List.map (fun o -> "  A " ^ (thing o).name) l) g)
    | Move dir -> walk dir g
    | Say s when (not (lit g.world)) && not (s.verb = "light" && s.obj = Some "lamp") -> say [ "It's too dark to see!" ] g
    | Say s ->
        let w, reply = Adventure.run rules s g.world in
        let g = say [ reply ] { g with world = w } in
        if Adventure.has w "won" then say [ ""; "****  You have won  ****" ] g else g

let start () : game = { world = start_world; out = describe start_world; dead = false }

(*****************************************************************************)
(* Update: typing a line *)
(*****************************************************************************)

type model = { game : game; input : string; keys : unit Scene2d.t }

let initial_model : model = { game = start (); input = ""; keys = Scene2d.start () }

let update (computer : computer) (model : model) : model =
  let keys = Scene2d.update computer model.keys in
  let pressed f = Scene2d.pressed f keys in
  let over = model.game.dead || Adventure.has model.game.world "won" in
  if pressed (fun k -> k.kenter) then
    if over then { initial_model with keys } else { game = command model.input model.game; input = ""; keys }
  else if pressed (fun k -> k.kbackspace) then
    { model with input = (if model.input = "" then "" else String.sub model.input 0 (String.length model.input - 1)); keys }
  else
    let typed = String.concat "" (List.map (String.make 1) (List.filter (fun c -> c >= ' ' && c <= '~') (List.of_seq (String.to_seq computer.keyboard.typed)))) in
    { model with input = model.input ^ typed; keys }

(*****************************************************************************)
(* View: a terminal *)
(*****************************************************************************)

let columns = 72

(* a paragraph cut into lines of at most [columns] characters, at the
 * spaces *)
let wrap (s : string) : string list =
  let words = String.split_on_char ' ' s in
  let lines, last =
    List.fold_left
      (fun (lines, cur) w ->
        if cur = "" then (lines, w)
        else if String.length cur + 1 + String.length w > columns then (cur :: lines, w)
        else (lines, cur ^ " " ^ w))
      ([], "") words
  in
  List.rev (last :: lines)

let green = rgb 120 230 120
let line_height = 26.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let text color s = words color s |> scale 1.4 in
  let left = screen.left +. 40. in
  let g = model.game in
  let prompt =
    if g.dead || Adventure.has g.world "won" then "(press enter to play again)"
    else "> " ^ model.input ^ if Float.rem model.keys.elapsed 1. < 0.5 then "_" else " "
  in
  let lines = List.concat_map wrap g.out @ [ prompt ] in
  let fits = int_of_float ((screen.height -. 120.) /. line_height) in
  let shown = List.filteri (fun i _ -> i >= List.length lines - fits) lines in
  let n = List.length shown in
  (* a terminal is a grid of characters, each in its column, as on the
   * VT100s Zork was played on; the playground's font is proportional,
   * so each character is drawn centered in its own cell *)
  let column = words_font_size *. 0.6 *. 1.4 in
  let line color s y =
    List.init (String.length s) (fun i ->
        if s.[i] = ' ' then None else Some (text color (String.make 1 s.[i]) |> move (left +. ((float_of_int i +. 0.5) *. column)) y))
    |> List.filter_map Fun.id |> group
  in
  [ rectangle (rgb 10 14 10) screen.width screen.height;
    rectangle (rgb 60 90 60) screen.width 40. |> move_y (screen.top -. 20.);
    line black (room_name g.world.here) (screen.top -. 20.);
    text black (Printf.sprintf "Score: %d   Moves: %d" g.world.score g.world.turns) |> move (screen.right -. 200.) (screen.top -. 20.) ]
  @ List.mapi (fun i s -> line green s (screen.bottom +. 40. +. (float_of_int (n - 1 - i) *. line_height))) shown

let help = {|TinyZork
  type what to do, and enter: "open mailbox", "north", "take lamp", ...
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
