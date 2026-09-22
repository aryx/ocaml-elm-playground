(* Adventure: a world as data, and the player's sentences acted on it.

   An adventure game is nearly all model. Colossal Cave (Will Crowther,
   1976, and Don Woods, 1977) and Zork (Tim Anderson, Marc Blank, Bruce
   Daniels and Dave Lebling, MIT, 1977) are rooms, the objects in them,
   what has happened so far, and a rule for each thing the player may
   try: "open mailbox", "take lamp", "kill troll with sword". Ten years
   later Maniac Mansion (Ron Gilbert and Gary Winnick, Lucasfilm Games,
   1987) replaced the typing by clicking -- a verb from a menu, then an
   object on the screen -- and kept the same model underneath, in its
   engine SCUMM: a sentence, and a script per object answering it.

   So the model is shared, and only the way a sentence is made differs:

       "put the egg in the case"         click Use, the key, the door
                 |                                    |
          a parser (TinyZork)              a verb menu (TinyManiacMansion)
                  \                                  /
                   '---> { verb; obj; with_ } <-----'
                                   |
                     the rules: the first that matches
                     and whose [test] holds, [act]s
                                   |
                           a new world, and a reply

   The world is a value, like every model here: where each object is
   ([places]), and what has happened ([flags]: "window open", "troll
   dead"). A puzzle is a rule whose [test] reads a flag another rule
   set -- the whole design of these games, locks and keys again (see
   TinyZelda and TinyMetroid), written as data.

   Before any rule, [run] checks the objects named are here: in the
   room, carried, or in an open container that is (a container is open
   when the flag "<name> open" is set). Otherwise "You see no <name>
   here." -- which is what makes the parser's world feel solid: you
   cannot take the lamp from the other room.

   Part of the adventure kit (gamekits/adventure/); used by TinyZork and
   TinyManiacMansion. *)

(* {1 The world} *)

type place =
  | Room of string (* lying in that room *)
  | Carried
  | Inside of string (* in that container *)
  | Nowhere (* not yet in the game, or gone *)

type world = {
  here : string; (* the room the player is in *)
  places : (string * place) list; (* where each object is *)
  flags : string list; (* what has happened *)
  score : int;
  turns : int;
}

(* [start room places]: the player in [room], nothing happened yet *)
val start : string -> (string * place) list -> world

(* where an object is ([Nowhere] for one the world doesn't know) *)
val where : world -> string -> place

(* [put obj place w]: [obj] moved there *)
val put : string -> place -> world -> world

(* [go room w]: the player in [room] *)
val go : string -> world -> world

val has : world -> string -> bool
val set : string -> world -> world
val unset : string -> world -> world

(* [visible w obj]: here to be seen and used: in this room, carried, or
 * inside an open container that is itself visible *)
val visible : world -> string -> bool

(* the objects lying in the room (not those carried, not those inside
 * something), and those carried, in the order of [places] *)
val in_room : world -> string list
val carried : world -> string list

(* {1 Sentences and rules} *)

(* a verb, the object it is done to, and the one it is done with (or to,
 * or in): "take lamp" is { verb = "take"; obj = Some "lamp"; with_ =
 * None }, "put egg in case" { verb = "put"; obj = Some "egg"; with_ =
 * Some "case" } *)
type sentence = { verb : string; obj : string option; with_ : string option }

(* A rule answers a sentence: [verb], [obj] and [with_] must be the
 * sentence's, where [Some "*"] stands for any object at all (the
 * generic "take", "drop"); then [test] must hold of the sentence and
 * the world; then [act] makes the new world and the reply. Both are
 * given the sentence, for a generic rule to know its object. *)
type rule = {
  verb : string;
  obj : string option;
  with_ : string option;
  test : sentence -> world -> bool;
  act : sentence -> world -> world * string;
}

(* the object pattern standing for any object *)
val any : string option

(* [always]: a [test] that always holds *)
val always : sentence -> world -> bool

(* [run rules s w]: [s] acted on [w], one turn later: "You see no X
 * here." if an object named isn't [visible], else the first rule (in
 * the list's order: the specific ones first, the generic ones last)
 * that matches and whose test holds, else "You can't do that."
 *
 * Worked example: a lamp in the hall, a box there too, closed, with a
 * key in it; the rules
 *
 *   take *           -> carried, "Taken."
 *   open box         -> the flag "box open", "Opened."
 *
 * "take key" answers "You see no key here." (it is in the closed box),
 * "open box" then "take key" answers "Taken.", and the key is carried;
 * "take lamp" in the garden, where the lamp isn't, "You see no lamp
 * here."; "sing" "You can't do that."; five sentences, five turns. *)
val run : rule list -> sentence -> world -> world * string
