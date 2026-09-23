(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Irc_server.mli *)

let name = "tiny"

type user = { mutable nick : string option; mutable registered : bool; mutable user_given : bool }

type t = {
  server : Server.t;
  users : (int, user) Hashtbl.t;
  (* the channels, each with its members, in the order they joined *)
  mutable channels : (string * int list) list;
}

let create (caps : < Cap.network ; .. >) ?(bind = "127.0.0.1") ?(port = 6667) () : t * int =
  let server, port = Server.listen caps ~bind ~port in
  ({ server; users = Hashtbl.create 16; channels = [] }, port)

let wait (t : t) (timeout : float) : unit = Server.wait t.server timeout

let nicks (t : t) : string list =
  Hashtbl.fold (fun _ u acc -> match u.nick with Some n when u.registered -> n :: acc | _ -> acc) t.users [] |> List.sort compare

let nick_of_id (t : t) (id : int) : string =
  match Hashtbl.find_opt t.users id with Some { nick = Some n; _ } -> n | _ -> "*"

let channels (t : t) : (string * string list) list = List.map (fun (c, ids) -> (c, List.map (nick_of_id t) ids)) t.channels

(*****************************************************************************)
(* Sending *)
(*****************************************************************************)

let send (t : t) (id : int) (m : Irc.message) : unit = Server.send t.server id (Irc.print m)

(* a numeric reply, from the server, to [id] *)
let reply (t : t) (id : int) (code : string) (params : string list) : unit =
  send t id (Irc.msg ~prefix:name code (nick_of_id t id :: params))

(* the prefix of a user's messages: nick!user@host *)
let prefix (t : t) (id : int) : string =
  let n = nick_of_id t id in
  Printf.sprintf "%s!%s@%s" n n name

let members (t : t) (channel : string) : int list = Option.value (List.assoc_opt channel t.channels) ~default:[]

(* everyone sharing a channel with [id], [id] too if [self] *)
let neighbours (t : t) ~(self : bool) (id : int) : int list =
  List.concat_map (fun (_, ids) -> if List.mem id ids then ids else []) t.channels
  |> List.sort_uniq compare
  |> List.filter (fun o -> self || o <> id)

let find_nick (t : t) (nick : string) : int option =
  Hashtbl.fold
    (fun id u acc -> if u.nick <> None && String.lowercase_ascii (Option.get u.nick) = String.lowercase_ascii nick then Some id else acc)
    t.users None

(*****************************************************************************)
(* The commands *)
(*****************************************************************************)

let names (t : t) (id : int) (channel : string) : unit =
  reply t id "353" [ "="; channel; String.concat " " (List.map (nick_of_id t) (members t channel)) ];
  reply t id "366" [ channel; "End of NAMES list" ]

let part (t : t) (id : int) (channel : string) (reason : string) : unit =
  if List.mem id (members t channel) then begin
    List.iter (fun o -> send t o (Irc.msg ~prefix:(prefix t id) "PART" [ channel; reason ])) (members t channel);
    t.channels <-
      List.filter_map
        (fun (c, ids) -> if c = channel then (match List.filter (( <> ) id) ids with [] -> None | ids -> Some (c, ids)) else Some (c, ids))
        t.channels
  end

(* gone: the ones sharing a channel told, its channels left *)
let quit (t : t) (id : int) (reason : string) : unit =
  List.iter (fun o -> send t o (Irc.msg ~prefix:(prefix t id) "QUIT" [ reason ])) (neighbours t ~self:false id);
  t.channels <-
    List.filter_map (fun (c, ids) -> match List.filter (( <> ) id) ids with [] -> None | ids -> Some (c, ids)) t.channels;
  Hashtbl.remove t.users id

let command (t : t) (id : int) (u : user) (m : Irc.message) : unit =
  match (m.command, m.params) with
  | "NICK", nick :: _ -> (
      match find_nick t nick with
      | Some other when other <> id -> reply t id "433" [ nick; "Nickname is already in use" ]
      | _ ->
          let before = prefix t id in
          if u.registered then
            List.iter (fun o -> send t o (Irc.msg ~prefix:before "NICK" [ nick ])) (neighbours t ~self:true id);
          u.nick <- Some nick;
          if u.user_given && not u.registered then begin
            u.registered <- true;
            reply t id "001" [ Printf.sprintf "Welcome to TinyIRC, %s" nick ]
          end)
  | "USER", _ :: _ ->
      u.user_given <- true;
      if u.nick <> None && not u.registered then begin
        u.registered <- true;
        reply t id "001" [ Printf.sprintf "Welcome to TinyIRC, %s" (nick_of_id t id) ]
      end
  | "PING", token :: _ -> send t id (Irc.msg ~prefix:name "PONG" [ name; token ])
  | "QUIT", params ->
      quit t id (match params with r :: _ -> r | [] -> "Quit");
      Server.close t.server id
  | _, _ when not u.registered -> reply t id "451" [ "You have not registered" ]
  | "JOIN", channels :: _ ->
      List.iter
        (fun channel ->
          if String.length channel > 1 && channel.[0] = '#' && not (List.mem id (members t channel)) then begin
            t.channels <-
              (if List.mem_assoc channel t.channels then List.map (fun (c, ids) -> if c = channel then (c, ids @ [ id ]) else (c, ids)) t.channels
               else t.channels @ [ (channel, [ id ]) ]);
            List.iter (fun o -> send t o (Irc.msg ~prefix:(prefix t id) "JOIN" [ channel ])) (members t channel);
            names t id channel
          end)
        (String.split_on_char ',' channels)
  | "PART", channels :: rest ->
      List.iter (fun c -> part t id c (match rest with r :: _ -> r | [] -> "")) (String.split_on_char ',' channels)
  | "NAMES", channel :: _ -> names t id channel
  | "PRIVMSG", [ target; text ] ->
      if String.length target > 0 && target.[0] = '#' then
        if List.mem id (members t target) then
          List.iter (fun o -> if o <> id then send t o (Irc.msg ~prefix:(prefix t id) "PRIVMSG" [ target; text ])) (members t target)
        else reply t id "404" [ target; "Cannot send to channel" ]
      else (
        match find_nick t target with
        | Some other -> send t other (Irc.msg ~prefix:(prefix t id) "PRIVMSG" [ target; text ])
        | None -> reply t id "401" [ target; "No such nick/channel" ])
  | command, _ -> reply t id "421" [ command; "Unknown command" ]

let step (t : t) : unit =
  List.iter
    (fun (e : Server.event) ->
      match e with
      | Joined id -> Hashtbl.replace t.users id { nick = None; registered = false; user_given = false }
      | Message (id, payload) -> (
          match Hashtbl.find_opt t.users id with
          | None -> ()
          | Some u ->
              (* a message a frame; lines, if a client sent several *)
              String.split_on_char '\n' payload
              |> List.filter (fun l -> String.trim l <> "")
              |> List.iter (fun line -> match Irc.parse line with Ok m -> command t id u m | Error _ -> ()))
      | Left id -> if Hashtbl.mem t.users id then quit t id "Connection closed")
    (Server.step t.server);
  Server.flush t.server
