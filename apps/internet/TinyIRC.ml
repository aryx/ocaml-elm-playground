(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of ircII (Michael Sandrof, 1989), the first IRC
 * client, a year after IRC itself (Jarkko Oikarinen, Oulu, 1988): a
 * screen of lines scrolling up, one line to type at the bottom, and
 * commands starting with a slash. Talking is typing a line; everyone
 * in the channel sees it.
 *
 *   /join #tiny     go into a channel (a channel is born with its first user)
 *   /part           leave the current one
 *   /nick bob       change your name
 *   /msg bob hi     a line to bob only
 *   /quit           leave
 *
 * Run TinyIRC's server, then a TinyIRC per person, natively or in a
 * browser:
 *
 *   dune exec networking/ircd/tiny_ircd.exe
 *   dune exec apps/internet/TinyIRC.exe -- nick=alice
 *   http://localhost:8001/apps/internet/web/TinyIRC.html?nick=bob
 *
 * flags host= and port= (6667) for a server elsewhere, nick= (guest),
 * channel= (#tiny, joined once welcomed).
 *
 * What it teaches is networking/Irc.mli: a protocol a person can read,
 * every message a line of text ("PRIVMSG #tiny :hello"), which is what
 * this client sends when you type "hello"; and /raw sends a line of the
 * protocol itself, as the telnet sessions of 1988 typed it
 * ("/raw PRIVMSG #tiny :hello" says the same). The server (Irc_server.mli) keeps the nicks
 * and the channels and passes the lines on; the client only turns lines
 * into text on the screen, and typing into lines.
 *
 * Drawn as a terminal draws, a character per cell (IRC clients were
 * terminal programs, and still are: irssi, weechat): the playground's
 * words are centered, and a log needs its lines to start on the left.
 *
 * Uses: Irc (the messages), Transport (the connection: WebSocket
 * natively and in a browser); not the gui toolkit, nor
 * apps/office's Stroke_text (an appkit it isn't yet).
 *
 * Exercises: a window per channel, switched with the keys (ircII's
 * /window); the log scrolled back; nicks colored by a hash of the name;
 * plain TCP, to talk to irssi and the real IRC networks (Libera.Chat
 * still listens on port 6667).
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type connection = Unopened | Open of Transport.t | Refused of string
type line = { color : color; text : string }

type model = {
  connection : connection;
  nick : string;
  channel : string option; (* where a typed line goes *)
  log : line list; (* the newest first *)
  input : string;
  before : keyboard; (* the keys of the last frame: Enter and Backspace pressed, not held *)
  welcomed : bool; (* the server's 001 received: registered *)
}

let initial : model =
  { connection = Unopened; nick = "guest"; channel = None; log = []; input = ""; before = initial_computer.keyboard; welcomed = false }

let info = rgb 120 170 220
let error = rgb 230 90 80
let said = rgb 225 225 225
let mine = rgb 150 230 150
let add (color : color) (text : string) (m : model) : model = { m with log = List.filteri (fun i _ -> i < 500) ({ color; text } :: m.log) }

let send (m : model) (message : Irc.message) : unit =
  match m.connection with Open t -> t.send (Irc.print message) | _ -> ()

(*****************************************************************************)
(* What the server says *)
(*****************************************************************************)

let flag (flags : flags) (name : string) (default : string) : string = Option.value (List.assoc_opt name flags) ~default

let heard (flags : flags) (m : model) (message : Irc.message) : model =
  let who = match message.prefix with Some p -> Irc.nick_of p | None -> "?" in
  match (message.command, message.params) with
  | "001", [ _; welcome ] ->
      (* welcomed: the channel of the flags joined *)
      let channel = flag flags "channel" "#tiny" in
      send m (Irc.msg "JOIN" [ channel ]);
      add info welcome { m with welcomed = true }
  | "PRIVMSG", [ target; text ] when String.length target > 0 && target.[0] = '#' -> add said (Printf.sprintf "<%s> %s" who text) m
  | "PRIVMSG", [ _; text ] -> add mine (Printf.sprintf "*%s* %s" who text) m
  | "JOIN", channel :: _ ->
      let m = if who = m.nick then { m with channel = Some channel } else m in
      add info (Printf.sprintf "-> %s joined %s" who channel) m
  | "PART", channel :: _ ->
      let m = if who = m.nick && m.channel = Some channel then { m with channel = None } else m in
      add info (Printf.sprintf "<- %s left %s" who channel) m
  | "QUIT", reason -> add info (Printf.sprintf "<- %s quit (%s)" who (String.concat " " reason)) m
  | "NICK", [ nick ] ->
      let m = if who = m.nick then { m with nick } else m in
      add info (Printf.sprintf "%s is now %s" who nick) m
  | "353", [ _; _; channel; names ] -> add info (Printf.sprintf "in %s: %s" channel names) m
  | "366", _ -> m
  | "433", _ :: nick :: _ ->
      (* taken: the same name, a _ more *)
      let nick = nick ^ "_" in
      send m (Irc.msg "NICK" [ nick ]);
      add error (Printf.sprintf "that nick is taken; trying %s" nick) { m with nick }
  | "PING", token :: _ ->
      send m (Irc.msg "PONG" [ token ]);
      m
  (* another numeric: an error, its text after our nick *)
  | code, _ :: rest when String.length code = 3 -> add error (String.concat " " rest) m
  | _ -> add said (Irc.print message) m

(*****************************************************************************)
(* What you type *)
(*****************************************************************************)

let typed (m : model) (line : string) : model =
  let words = String.split_on_char ' ' line |> List.filter (( <> ) "") in
  match words with
  | [] -> m
  | "/join" :: channel :: _ ->
      send m (Irc.msg "JOIN" [ channel ]);
      m
  | [ "/part" ] -> (
      match m.channel with
      | Some c ->
          send m (Irc.msg "PART" [ c; "" ]);
          m
      | None -> add error "in no channel" m)
  | "/nick" :: nick :: _ ->
      send m (Irc.msg "NICK" [ nick ]);
      m
  | "/msg" :: nick :: _ :: _ ->
      let text = String.concat " " (List.tl (List.tl words)) in
      send m (Irc.msg "PRIVMSG" [ nick; text ]);
      add mine (Printf.sprintf "-> *%s* %s" nick text) m
  | "/raw" :: _ -> (
      (* the protocol itself, typed as in 1988 *)
      let raw = String.concat " " (List.tl words) in
      match m.connection with
      | Open t ->
          t.send raw;
          add info ("raw: " ^ raw) m
      | _ -> m)
  | "/quit" :: _ ->
      send m (Irc.msg "QUIT" [ "TinyIRC" ]);
      add info "you left" m
  | cmd :: _ when cmd.[0] = '/' -> add error ("no such command: " ^ cmd) m
  | _ -> (
      match m.channel with
      | Some c ->
          send m (Irc.msg "PRIVMSG" [ c; line ]);
          add said (Printf.sprintf "<%s> %s" m.nick line) m
      | None -> add error "join a channel first: /join #tiny" m)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (network : Cap.network) (computer : computer) (m : model) : model =
  (* the server, reached at the first frame (the flags known), NICK and
   * USER sent at once *)
  let m =
    match m.connection with
    | Unopened -> (
        let host = flag computer.flags "host" "localhost" and nick = flag computer.flags "nick" "guest" in
        let port = Option.value (Option.bind (List.assoc_opt "port" computer.flags) int_of_string_opt) ~default:6667 in
        match Transport.connect network (Relay { host; port }) with
        | Ok t ->
            let m = { m with connection = Open t; nick } in
            send m (Irc.msg "NICK" [ nick ]);
            send m (Irc.msg "USER" [ nick; "0"; "*"; nick ]);
            add info (Printf.sprintf "connecting to %s:%d as %s" host port nick) m
        | Error why -> add error why { m with connection = Refused why; nick })
    | _ -> m
  in
  let received = match m.connection with Open t -> t.receive () | _ -> [] in
  let m =
    List.fold_left
      (fun m l -> match Irc.parse l with Ok message -> heard computer.flags m message | Error _ -> m)
      m received
  in
  let k = computer.keyboard in
  let pressed down before = down && not before in
  let input = m.input ^ k.typed in
  let input =
    if pressed k.kbackspace m.before.kbackspace && String.length input > 0 then String.sub input 0 (String.length input - 1) else input
  in
  let entered = pressed k.kenter m.before.kenter in
  let m = { m with input; before = k } in
  if entered then typed { m with input = "" } (String.trim m.input) else m

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a character per cell, 11 wide, as a terminal *)
let cell = 11.

let terminal (x : number) (y : number) (color : color) (s : string) : shape list =
  List.filter_map Fun.id
    (List.init (min 88 (String.length s)) (fun i ->
         if s.[i] = ' ' then None
         else Some (words color (String.make 1 s.[i]) |> scale 1.4 |> move (x +. (cell *. float_of_int i) +. (cell /. 2.)) y)))

let view (_ : computer) (m : model) : shape list =
  let status =
    match m.connection with
    | Open _ when m.welcomed -> Printf.sprintf "%s on %s" m.nick (Option.value m.channel ~default:"no channel")
    | Open t -> t.status ()
    | Refused why -> why
    | Unopened -> "..."
  in
  let lines = List.filteri (fun i _ -> i < 38) m.log in
  [ rectangle (rgb 20 22 30) 1000. 1000.; rectangle (rgb 40 60 110) 1000. 40. |> move_y 480. ]
  @ terminal (-485.) 480. white ("TinyIRC  " ^ status)
  @ List.concat (List.mapi (fun i l -> terminal (-485.) (-400. +. (22. *. float_of_int i)) l.color l.text) lines)
  @ [ rectangle (rgb 40 40 50) 1000. 40. |> move_y (-460.) ]
  @ terminal (-485.) (-460.) white ("> " ^ m.input ^ "_")

(* the network granted: the connection is all it does with it
 * (plan_caps.md) *)
let app (network : < Cap.network ; .. >) =
  let network = (network :> Cap.network) in
  game view (update network) initial

let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
