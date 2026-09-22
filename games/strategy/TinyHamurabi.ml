(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Hamurabi (Doug Dyment, 1968, after The Sumer Game,
 * Mabel Addis and William McKay, IBM, 1964): rule ancient Sumer for ten
 * years, a harvest at a time.
 *
 *   type a number and enter, to each question: the acres to buy (or
 *   sell), the bushels to feed the people, the acres to sow
 *
 * The Sumer Game ran on an IBM 7090 for sixth-graders of Westchester
 * County, New York: Mabel Addis, a teacher, wrote its story, which
 * makes it the first game designed by a woman, and the first
 * simulation game -- a world of numbers the player steers. Doug
 * Dyment's rewrite in FOCAL, and then in BASIC (spelling the king
 * with one M), was printed in David Ahl's "101 BASIC Computer Games"
 * (1973), the book a generation typed its first programs from. SimCity
 * and Civilization, both in the catalogue, are its descendants.
 * (Names and dates from memory, to check.)
 *
 * The game is the model, entirely: a city is five numbers (the people,
 * the grain, the land, its price, the year), and a year is one
 * function ([harvest]) from the three numbers the player typed and
 * four throws of the dice to next year's five. The rules, as the BASIC
 * listing has them:
 *
 *   - a person eats 20 bushels a year; the unfed starve, and more than
 *     45% starved in one year ends the reign early (impeachment);
 *   - an acre needs half a bushel of seed, and a person can farm ten;
 *   - the harvest is 1 to 5 bushels an acre; rats eat half, a third,
 *     a quarter (...) of the store, one year in two;
 *   - newcomers come the more the city has land and grain for them;
 *   - one year in seven or so, the plague halves the people;
 *   - land trades at 17 to 26 bushels an acre.
 *
 * The dice are a separate record ([dice]), thrown before the year from
 * a generator kept in the model ([throw]): the rules are a pure
 * function the tests can give any dice to, and the game is the same
 * game from the same seed, as a BASIC game run twice on the same
 * machine was.
 *
 * The teletype: the answers were typed and the city's report printed,
 * in capitals, on a roll of paper -- the terminal of 1968, one line
 * after the other, nothing ever erased. TinyZork's terminal is the
 * screen that replaced it, ten years later; this one is paper.
 *
 * What it uses: Scene2d (enter and backspace pressed), the typing in
 * computer.keyboard.typed. Nothing else: the oldest kind of game
 * needs the least.
 *
 * Left undone, exercises: the harvest's graph, year by year (a first
 * view that is not text); the lands' own prices rising with demand
 * (what Hamurabi's successors added: supply and demand, TinyMULE's
 * subject); choosing the seed; Kingdom (1978) and Santa Paravia and
 * Fiumaccio (1978), the same game grown up with a castle and a
 * marketplace.
 *)
open Playground

(*****************************************************************************)
(* The city *)
(*****************************************************************************)

type city = {
  year : int;
  people : int;
  grain : int; (* bushels in store *)
  acres : int;
  price : int; (* bushels an acre, this year *)
  (* last year's report *)
  starved : int;
  arrived : int;
  plague : bool;
  yield : int; (* bushels an acre *)
  rats : int; (* bushels eaten *)
  (* the reign so far, for the verdict *)
  starved_pct_sum : int;
  starved_total : int;
}

(* Hamurabi's first year: what the BASIC listing starts from *)
let start_city : city =
  { year = 1; people = 100; grain = 2800; acres = 1000; price = 17; starved = 0; arrived = 5; plague = false; yield = 3;
    rats = 200; starved_pct_sum = 0; starved_total = 0 }

(* the year's four throws: the harvest (1-5 bushels an acre), the rats
 * (1-5: on an even throw they eat a 1/throw of the store), the
 * newcomers' die (1-5), the plague (true one year in seven), and next
 * year's price (17-26) *)
type dice = { d_yield : int; d_rats : int; d_come : int; d_plague : bool; d_price : int }

(* a linear congruential generator, the kind a 1968 BASIC's RND was *)
let next (seed : int) : int = ((seed * 1103515245) + 12345) land 0x3fffffff

let throw (seed : int) : dice * int =
  let s1 = next seed in
  let s2 = next s1 in
  let s3 = next s2 in
  let s4 = next s3 in
  let s5 = next s4 in
  let die n s = 1 + ((s lsr 8) mod n) in
  ({ d_yield = die 5 s1; d_rats = die 5 s2; d_come = die 5 s3; d_plague = die 100 s4 <= 15; d_price = 16 + die 10 s5 }, s5)

(* what the player decided, the year's three answers (the land as one
 * number: [bought] if positive, sold if negative) *)
type orders = { bought : int; food : int; sown : int }

(* the orders checked against the store, as the listing's questions do,
 * one at a time: an error names what is lacking *)
let check_land (c : city) (bought : int) : string option =
  if bought * c.price > c.grain then Some (Printf.sprintf "THINK AGAIN. YOU HAVE ONLY %d BUSHELS OF GRAIN." c.grain)
  else if -bought > c.acres then Some (Printf.sprintf "THINK AGAIN. YOU OWN ONLY %d ACRES." c.acres)
  else None

let check_food (c : city) (food : int) : string option =
  if food > c.grain then Some (Printf.sprintf "THINK AGAIN. YOU HAVE ONLY %d BUSHELS OF GRAIN." c.grain) else None

let check_sown (c : city) (sown : int) : string option =
  if sown > c.acres then Some (Printf.sprintf "THINK AGAIN. YOU OWN ONLY %d ACRES." c.acres)
  else if sown / 2 > c.grain then Some (Printf.sprintf "THINK AGAIN. YOU HAVE ONLY %d BUSHELS OF GRAIN." c.grain)
  else if sown > 10 * c.people then Some (Printf.sprintf "BUT YOU HAVE ONLY %d PEOPLE TO TEND THE FIELDS!" c.people)
  else None

(* the land traded: [c] after buying (or selling) at this year's price *)
let trade (c : city) (bought : int) : city = { c with acres = c.acres + bought; grain = c.grain - (bought * c.price) }

type verdict = Reigning of city | Impeached of city * int (* the starved *)

(* The year: the people fed, the land sown, then the dice -- the plague,
 * the harvest, the rats, the newcomers. [c] has already traded. *)
let harvest (d : dice) (o : orders) (c : city) : verdict =
  let grain = c.grain - o.food - (o.sown / 2) in
  let fed = min c.people (o.food / 20) in
  let starved = c.people - fed in
  if starved * 100 > 45 * c.people then Impeached (c, starved)
  else
    let grain = grain + (o.sown * d.d_yield) in
    let rats = if d.d_rats mod 2 = 0 then grain / d.d_rats else 0 in
    let grain = grain - rats in
    let arrived = (d.d_come * ((20 * c.acres) + grain) / max 1 fed / 100) + 1 in
    let people = fed + arrived in
    let people = if d.d_plague then people / 2 else people in
    Reigning
      { year = c.year + 1; people; grain; acres = c.acres; price = d.d_price; starved; arrived; plague = d.d_plague;
        yield = d.d_yield; rats; starved_pct_sum = c.starved_pct_sum + (100 * starved / c.people);
        starved_total = c.starved_total + starved }

(* After ten years, the listing's verdict: by the average share starved,
 * and the land per head. *)
let verdict (c : city) : string list =
  let pct = c.starved_pct_sum / 10 and per_head = c.acres / max 1 c.people in
  [ Printf.sprintf "IN YOUR 10-YEAR TERM OF OFFICE, %d PERCENT OF THE POPULATION STARVED PER YEAR ON AVERAGE," pct;
    Printf.sprintf "A TOTAL OF %d PEOPLE DIED! YOU STARTED WITH 10 ACRES PER PERSON AND ENDED WITH %d ACRES PER PERSON."
      c.starved_total per_head ]
  @
  if pct > 33 || per_head < 7 then
    [ "DUE TO THIS EXTREME MISMANAGEMENT YOU HAVE NOT ONLY BEEN IMPEACHED AND THROWN OUT OF OFFICE BUT YOU HAVE ALSO \
       BEEN DECLARED NATIONAL FINK!!!!" ]
  else if pct > 10 || per_head < 9 then
    [ "YOUR HEAVY-HANDED PERFORMANCE SMACKS OF NERO AND IVAN IV. THE PEOPLE (REMAINING) FIND YOU AN UNPLEASANT RULER, \
       AND, FRANKLY, HATE YOUR GUTS!!" ]
  else if pct > 3 || per_head < 10 then
    [ "YOUR PERFORMANCE COULD HAVE BEEN SOMEWHAT BETTER, BUT REALLY WASN'T TOO BAD AT ALL." ]
  else [ "A FANTASTIC PERFORMANCE!!! CHARLEMANGE, DISRAELI, AND JEFFERSON COMBINED COULD NOT HAVE DONE BETTER!" ]

(*****************************************************************************)
(* The dialogue: a report, three questions, a year *)
(*****************************************************************************)

type question = Land | Food | Sow | Over

type game = {
  city : city; (* traded already, once past the Land question *)
  asked : question;
  bought : int;
  food : int;
  seed : int;
  out : string list; (* the paper, oldest line first *)
}

let report (c : city) : string list =
  [ ""; "HAMURABI: I BEG TO REPORT TO YOU,";
    Printf.sprintf "IN YEAR %d, %d PEOPLE STARVED, %d CAME TO THE CITY." c.year c.starved c.arrived ]
  @ (if c.plague then [ "A HORRIBLE PLAGUE STRUCK! HALF THE PEOPLE DIED." ] else [])
  @ [ Printf.sprintf "POPULATION IS NOW %d." c.people; Printf.sprintf "THE CITY NOW OWNS %d ACRES." c.acres;
      Printf.sprintf "YOU HARVESTED %d BUSHELS PER ACRE." c.yield; Printf.sprintf "RATS ATE %d BUSHELS." c.rats;
      Printf.sprintf "YOU NOW HAVE %d BUSHELS IN STORE." c.grain; "";
      Printf.sprintf "LAND IS TRADING AT %d BUSHELS PER ACRE." c.price ]

let prompt (q : question) : string =
  match q with
  | Land -> "HOW MANY ACRES DO YOU WISH TO BUY (NEGATIVE TO SELL)?"
  | Food -> "HOW MANY BUSHELS DO YOU WISH TO FEED YOUR PEOPLE?"
  | Sow -> "HOW MANY ACRES DO YOU WISH TO PLANT WITH SEED?"
  | Over -> "SO LONG FOR NOW."

let start (seed : int) : game =
  { city = start_city; asked = Land; bought = 0; food = 0; seed;
    out = [ "HAMURABI"; "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"; "";
            "TRY YOUR HAND AT GOVERNING ANCIENT SUMERIA FOR A TEN-YEAR TERM OF OFFICE." ]
          @ report start_city @ [ prompt Land ] }

let say (lines : string list) (g : game) : game = { g with out = g.out @ lines }

(* an answer typed: checked, then the next question, or the year *)
let answer (line : string) (g : game) : game =
  let g = say [ "? " ^ line ] g in
  match (g.asked, int_of_string_opt (String.trim line)) with
  | Over, _ -> g
  | _, None -> say [ "HAMURABI: A NUMBER, PLEASE."; prompt g.asked ] g
  | _, Some n when n < 0 && g.asked <> Land -> say [ "HAMURABI: I CANNOT DO WHAT YOU WISH."; prompt g.asked ] g
  | Land, Some n -> (
      match check_land g.city n with
      | Some err -> say [ "HAMURABI: " ^ err; prompt Land ] g
      | None -> say [ prompt Food ] { g with city = trade g.city n; bought = n; asked = Food })
  | Food, Some n -> (
      match check_food g.city n with
      | Some err -> say [ "HAMURABI: " ^ err; prompt Food ] g
      | None -> say [ prompt Sow ] { g with food = n; asked = Sow })
  | Sow, Some n -> (
      (* the food is out of the store before the seed is *)
      match check_sown { g.city with grain = g.city.grain - g.food } n with
      | Some err -> say [ "HAMURABI: " ^ err; prompt Sow ] g
      | None -> (
          let dice, seed = throw g.seed in
          match harvest dice { bought = g.bought; food = g.food; sown = n } g.city with
          | Impeached (_, starved) ->
              say
                [ ""; Printf.sprintf "YOU STARVED %d PEOPLE IN ONE YEAR!!!" starved;
                  "DUE TO THIS EXTREME MISMANAGEMENT YOU HAVE NOT ONLY BEEN IMPEACHED AND THROWN OUT OF OFFICE BUT YOU \
                   HAVE ALSO BEEN DECLARED NATIONAL FINK!!!!"; prompt Over ]
                { g with asked = Over; seed }
          | Reigning c when c.year > 10 -> say ([ "" ] @ verdict c @ [ prompt Over ]) { g with city = c; asked = Over; seed }
          | Reigning c -> say (report c @ [ prompt Land ]) { g with city = c; asked = Land; seed }))

(*****************************************************************************)
(* Update: typing on the teletype *)
(*****************************************************************************)

type model = { game : game; input : string; keys : unit Scene2d.t }

let initial_model : model = { game = start 1968; input = ""; keys = Scene2d.start () }

let update (computer : computer) (model : model) : model =
  let keys = Scene2d.update computer model.keys in
  let pressed f = Scene2d.pressed f keys in
  if pressed (fun k -> k.kenter) then
    if model.game.asked = Over then { initial_model with keys }
    else { game = answer model.input model.game; input = ""; keys }
  else if pressed (fun k -> k.kbackspace) then
    { model with input = (if model.input = "" then "" else String.sub model.input 0 (String.length model.input - 1)); keys }
  else
    (* a teletype types capitals, and here digits and a minus *)
    let typed = String.to_seq computer.keyboard.typed |> Seq.filter (fun c -> (c >= '0' && c <= '9') || c = '-') |> String.of_seq in
    { model with input = model.input ^ typed; keys }

(*****************************************************************************)
(* View: the paper *)
(*****************************************************************************)

let columns = 64

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

let paper = rgb 245 238 215
let ink = rgb 40 36 60
let line_height = 26.

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let left = screen.left +. 110. in
  (* a character per column, as the teletype's type wheel struck them *)
  let column = words_font_size *. 0.6 *. 1.5 in
  let line s y =
    List.init (String.length s) (fun i ->
        if s.[i] = ' ' then None
        else Some (words ink (String.make 1 s.[i]) |> scale 1.5 |> move (left +. ((float_of_int i +. 0.5) *. column)) y))
    |> List.filter_map Fun.id |> group
  in
  let g = model.game in
  let typing = if g.asked = Over then "(PRESS ENTER FOR ANOTHER REIGN)" else "? " ^ model.input ^ if Float.rem model.keys.elapsed 1. < 0.5 then "_" else "" in
  let lines = List.concat_map wrap g.out @ [ typing ] in
  let fits = int_of_float ((screen.height -. 60.) /. line_height) in
  let shown = List.filteri (fun i _ -> i >= List.length lines - fits) lines in
  let n = List.length shown in
  (* the roll of paper, its sprocket holes down the sides *)
  [ rectangle (rgb 60 60 70) screen.width screen.height; rectangle paper (float_of_int columns *. column +. 160.) screen.height |> move_x (left +. (float_of_int columns *. column /. 2.)) ]
  @ List.init 40 (fun i ->
        let y = screen.top -. (float_of_int i *. 40.) -. 20. in
        group [ circle (rgb 60 60 70) 5. |> move (left -. 55.) y; circle (rgb 60 60 70) 5. |> move (left +. (float_of_int columns *. column) +. 55.) y ])
  @ List.mapi (fun i s -> line s (screen.bottom +. 30. +. (float_of_int (n - 1 - i) *. line_height))) shown

let help = {|TinyHamurabi
  type a number and enter, to each of Hamurabi's questions
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
