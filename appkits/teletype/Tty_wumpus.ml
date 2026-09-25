(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Teletype

(* See Tty_wumpus.mli *)

(*****************************************************************************)
(* The cave *)
(*****************************************************************************)

let tunnels =
  [| [||];
     [| 2; 5; 8 |]; [| 1; 3; 10 |]; [| 2; 4; 12 |]; [| 3; 5; 14 |]; [| 1; 4; 6 |];
     [| 5; 7; 15 |]; [| 6; 8; 17 |]; [| 1; 7; 9 |]; [| 8; 10; 18 |]; [| 2; 9; 11 |];
     [| 10; 12; 19 |]; [| 3; 11; 13 |]; [| 12; 14; 20 |]; [| 4; 13; 15 |]; [| 6; 14; 16 |];
     [| 15; 17; 20 |]; [| 7; 16; 18 |]; [| 9; 17; 19 |]; [| 11; 18; 20 |]; [| 13; 16; 19 |] |]

let joined (a : int) (b : int) : bool = Array.mem b tunnels.(a)

type cave = { you : int; wumpus : int; pits : int list; bats : int list; arrows : int }
type outcome = Won | Lost

(* a random neighbour of [room] *)
let somewhere_next (room : int) : int talk =
  let* i = random 3 in
  return tunnels.(room).(i)

(* a number from [lo] to [hi], asked again until it is one *)
let rec number (question : string) (lo : int) (hi : int) : int talk =
  let* answer = ask question in
  match int_of_string_opt (String.trim answer) with
  | Some n when n >= lo && n <= hi -> return n
  | _ ->
      let* () = print (Printf.sprintf "A NUMBER FROM %d TO %d, PLEASE.\n" lo hi) in
      number question lo hi

(*****************************************************************************)
(* The Wumpus wakes *)
(*****************************************************************************)

(* three times in four it moves to a neighbouring room; if that room
   is yours, or it stays in yours, it eats you *)
let wake (c : cave) : (cave, outcome) result talk =
  let* r = random 4 in
  let wumpus = if r = 3 then c.wumpus else tunnels.(c.wumpus).(r) in
  if wumpus = c.you then
    let* () = print "TSK TSK TSK -- THE WUMPUS GOT YOU!\n" in
    return (Error Lost)
  else return (Ok { c with wumpus })

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

(* what is in the room you just entered: the Wumpus (woken), a pit (the
   end), bats (carried to a random room, where it starts again) *)
let rec arrive (c : cave) : (cave, outcome) result talk =
  if c.you = c.wumpus then
    let* () = print "...OOPS! YOU BUMPED A WUMPUS!\n" in
    wake c
  else if List.mem c.you c.pits then
    let* () = print "YYYIIIIEEEE . . . YOU FELL IN A PIT.\n" in
    return (Error Lost)
  else if List.mem c.you c.bats then
    let* () = print "ZAP -- A SUPER BAT SNATCHES YOU! ELSEWHEREVILLE FOR YOU!\n" in
    let* r = random 20 in
    arrive { c with you = r + 1 }
  else return (Ok c)

let rec move (c : cave) : (cave, outcome) result talk =
  let* room = number "WHERE TO? " 1 20 in
  if joined c.you room then arrive { c with you = room }
  else
    let* () = print "NO TUNNEL GOES THERE.\n" in
    move c

(*****************************************************************************)
(* Shooting *)
(*****************************************************************************)

(* the rooms the arrow is told to fly through; not back where it just
   came from (A-B-A), which an arrow can't do *)
let rec path (n : int) (acc : int list) : int list talk =
  if List.length acc = n then return (List.rev acc)
  else
    let* room = number "ROOM #? " 1 20 in
    match acc with
    | _ :: before :: _ when room = before ->
        let* () = print "ARROWS AREN'T THAT CROOKED -- TRY ANOTHER ROOM.\n" in
        path n acc
    | _ -> path n (room :: acc)

type flight = Hit_wumpus | Hit_you | Missed

(* room by room: where the next room isn't joined to this one, a random
   tunnel instead *)
let rec fly (c : cave) (room : int) (rooms : int list) : flight talk =
  match rooms with
  | [] -> return Missed
  | next :: rest ->
      let* next = if joined room next then return next else somewhere_next room in
      if next = c.wumpus then return Hit_wumpus else if next = c.you then return Hit_you else fly c next rest

let shoot (c : cave) : (cave, outcome) result talk =
  let* n = number "NO. OF ROOMS (1-5)? " 1 5 in
  let* rooms = path n [] in
  let* flight = fly c c.you rooms in
  match flight with
  | Hit_wumpus ->
      let* () = print "AHA! YOU GOT THE WUMPUS!\n" in
      return (Error Won)
  | Hit_you ->
      let* () = print "OUCH! THE ARROW GOT YOU!\n" in
      return (Error Lost)
  | Missed -> (
      let* () = print "MISSED.\n" in
      let c = { c with arrows = c.arrows - 1 } in
      let* woken = wake c in
      match woken with
      | Ok c when c.arrows = 0 ->
          let* () = print "YOU ARE OUT OF ARROWS.\n" in
          return (Error Lost)
      | r -> return r)

(*****************************************************************************)
(* A turn *)
(*****************************************************************************)

(* what the next rooms give away, then where you are *)
let describe (c : cave) : string =
  let near = Array.to_list tunnels.(c.you) in
  let warn cond s = if cond then s else "" in
  warn (List.mem c.wumpus near) "I SMELL A WUMPUS!\n"
  ^ warn (List.exists (fun r -> List.mem r c.pits) near) "I FEEL A DRAFT.\n"
  ^ warn (List.exists (fun r -> List.mem r c.bats) near) "BATS NEARBY!\n"
  ^ Printf.sprintf "YOU ARE IN ROOM %d.\nTUNNELS LEAD TO %s.\n" c.you (String.concat " " (List.map string_of_int near))

let rec play (c : cave) : outcome talk =
  let* () = print ("\n" ^ describe c) in
  let* choice = ask "SHOOT OR MOVE (S-M)? " in
  let* result =
    match String.uppercase_ascii (String.trim choice) with
    | "S" -> shoot c
    | "M" -> move c
    | _ -> return (Ok c)
  in
  match result with Ok c -> play c | Error outcome -> return outcome

(*****************************************************************************)
(* Games *)
(*****************************************************************************)

(* six different rooms, drawn one at a time, a room already taken drawn
   again *)
let rec rooms (acc : int list) : int list talk =
  if List.length acc = 6 then return (List.rev acc)
  else
    let* r = random 20 in
    if List.mem (r + 1) acc then rooms acc else rooms ((r + 1) :: acc)

let setup : cave talk =
  let* taken = rooms [] in
  match taken with
  | [ you; wumpus; p1; p2; b1; b2 ] -> return { you; wumpus; pits = [ p1; p2 ]; bats = [ b1; b2 ]; arrows = 5 }
  | _ -> assert false

let instructions =
  "THE WUMPUS LIVES IN A CAVE OF 20 ROOMS, EACH WITH 3 TUNNELS TO\n\
   OTHER ROOMS (THE ROOMS ARE THE CORNERS OF A DODECAHEDRON).\n\
   HAZARDS: TWO ROOMS HAVE BOTTOMLESS PITS, TWO HAVE SUPER BATS THAT\n\
   CARRY YOU TO ANOTHER ROOM. THE WUMPUS SLEEPS UNTIL YOU SHOOT OR\n\
   ENTER ITS ROOM; AWAKE, IT MOVES ONE ROOM, OR EATS YOU.\n\
   EACH TURN, MOVE THROUGH A TUNNEL, OR SHOOT ONE OF YOUR 5 CROOKED\n\
   ARROWS THROUGH 1 TO 5 ROOMS, NAMED ONE BY ONE.\n\
   ONE ROOM AWAY, YOU SMELL THE WUMPUS, FEEL A PIT'S DRAFT, HEAR BATS.\n"

let yes (answer : string) : bool = String.length answer > 0 && Char.uppercase_ascii answer.[0] = 'Y'

let rec games (cave : cave) : unit talk =
  let* outcome = play cave in
  let* () = print (if outcome = Won then "HEE HEE HEE -- THE WUMPUS'LL GET YOU NEXT TIME!\n" else "HA HA HA -- YOU LOSE!\n") in
  let* again = ask "\nPLAY AGAIN (Y-N)? " in
  if not (yes again) then print "BYE!\n"
  else
    let* same = ask "SAME CAVE (Y-N)? " in
    if yes same then games cave
    else
      let* cave = setup in
      games cave

let program =
  let* () = print "HUNT THE WUMPUS\n\n" in
  let* help = ask "INSTRUCTIONS (Y-N)? " in
  let* () = if yes help then print instructions else return () in
  let* cave = setup in
  games cave
