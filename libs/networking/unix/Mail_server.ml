(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Mail_server.mli *)

(* a client of the SMTP side: what it said so far of the message *)
type smtp = {
  mutable hello : string option;
  mutable sender : string option;
  mutable recipients : string list; (* the users of this domain *)
  mutable data : string list option; (* the lines after DATA, the latest first *)
}

(* a client of the POP3 side *)
type pop = {
  mutable user : string option;
  mutable drop : Mbox.entry list option; (* the maildrop as it was at PASS: logged in *)
  mutable deleted : int list; (* the numbers marked, 1 for the first *)
}

(* where a side listens: WebSocket, for a browser, and plain TCP, for
   telnet and the mail clients of the world; a session is known by the
   server it came in on and its id there *)
type t = {
  smtp_servers : Server.t list;
  pop_servers : Server.t list;
  domain : string;
  passwords : (string * string) list option;
  changed : string -> Mbox.entry list -> unit;
  mutable drops : (string * Mbox.entry list) list;
  smtps : (int * int, smtp) Hashtbl.t;
  pops : (int * int, pop) Hashtbl.t;
  mutable queued : int; (* the messages taken, for "queued as" *)
}

type ports = { smtp : int; pop : int; smtp_plain : int; pop_plain : int }

let create (caps : < Cap.network ; .. >) ?(bind = "127.0.0.1") ?(ports = { smtp = 8025; pop = 8110; smtp_plain = 2525; pop_plain = 1100 }) ?(domain = "tiny")
    ?passwords ?(maildrops = []) ?(changed = fun _ _ -> ()) () : t * ports =
  let listen lines port = Server.listen caps ~lines ~bind ~port () in
  let smtp_ws, smtp = listen false ports.smtp and smtp_tcp, smtp_plain = listen true ports.smtp_plain in
  let pop_ws, pop = listen false ports.pop and pop_tcp, pop_plain = listen true ports.pop_plain in
  ( {
      smtp_servers = [ smtp_ws; smtp_tcp ];
      pop_servers = [ pop_ws; pop_tcp ];
      domain;
      passwords;
      changed;
      drops = maildrops;
      smtps = Hashtbl.create 8;
      pops = Hashtbl.create 8;
      queued = 0;
    },
    { smtp; pop; smtp_plain; pop_plain } )

let wait (t : t) (timeout : float) : unit =
  (* the four servers' sockets: a short sleep on each *)
  List.iter (fun s -> Server.wait s (timeout /. 4.)) (t.smtp_servers @ t.pop_servers)

let maildrop (t : t) (user : string) : Mbox.entry list = Option.value (List.assoc_opt user t.drops) ~default:[]

let set_maildrop (t : t) (user : string) (entries : Mbox.entry list) : unit =
  t.drops <- (user, entries) :: List.remove_assoc user t.drops;
  t.changed user entries

(* the date, as a Date: header writes it, in Greenwich *)
let date_of (now : float) : Mail.date =
  let day, time = Clock.local ~offset:0 now in
  { day; time = { time with second = Float.of_int (int_of_float time.second) }; offset = 0 }

(*****************************************************************************)
(* SMTP: the counter *)
(*****************************************************************************)

(* "bob@tiny" or "bob" is the user bob; mail for elsewhere is None *)
let local (t : t) (address : string) : string option =
  match String.index_opt address '@' with
  | None -> if address = "" then None else Some (String.lowercase_ascii address)
  | Some i ->
      let user = String.sub address 0 i and domain = String.sub address (i + 1) (String.length address - i - 1) in
      if String.lowercase_ascii domain = t.domain && user <> "" then Some (String.lowercase_ascii user) else None

(* the message taken: a Received: line on top, and into each maildrop *)
let deliver (t : t) (s : smtp) (lines : string list) (now : float) : string =
  let date = date_of now in
  let mail = Mail.parse (Smtp.unstuff lines) in
  let received =
    { Mail.name = "Received"; raw = Printf.sprintf " from %s by %s with SMTP;\n  %s" (Option.value s.hello ~default:"unknown") t.domain (Mail.date_to_string date) }
  in
  let entry = { Mbox.envelope = Mbox.envelope ~sender:(Option.value s.sender ~default:"MAILER-DAEMON") date; mail = { mail with fields = received :: mail.fields } } in
  List.iter (fun user -> set_maildrop t user (maildrop t user @ [ entry ])) (List.sort_uniq compare s.recipients);
  t.queued <- t.queued + 1;
  Printf.sprintf "OK: queued as %d" t.queued

let smtp_line (t : t) (server : Server.t) (id : int) (s : smtp) (line : string) (now : float) : unit =
  let answer code text = List.iter (Server.send server id) (Smtp.reply code [ text ]) in
  match s.data with
  | Some lines when line = "." ->
      answer 250 (deliver t s (List.rev lines) now);
      s.data <- None;
      s.sender <- None;
      s.recipients <- []
  | Some lines -> s.data <- Some (line :: lines)
  | None -> (
      match Smtp.parse_command line with
      | Helo d ->
          s.hello <- Some d;
          answer 250 t.domain
      | Ehlo d ->
          s.hello <- Some d;
          List.iter (Server.send server id) (Smtp.reply 250 [ Printf.sprintf "%s greets %s" t.domain d; "HELP" ])
      | Mail_from a ->
          s.sender <- Some a;
          s.recipients <- [];
          answer 250 "OK"
      | Rcpt_to a -> (
          if s.sender = None then answer 503 "MAIL FROM first"
          else
            match local t a with
            | Some user ->
                s.recipients <- s.recipients @ [ user ];
                answer 250 "OK"
            | None -> answer 550 (Printf.sprintf "not a relay: %s is not for %s" a t.domain))
      | Data -> if s.recipients = [] then answer 503 "RCPT TO first" else (s.data <- Some []; answer 354 "End data with <CR><LF>.<CR><LF>")
      | Rset ->
          s.sender <- None;
          s.recipients <- [];
          answer 250 "OK"
      | Noop -> answer 250 "OK"
      | Quit ->
          answer 221 "Bye";
          Server.close server id
      | Unknown _ -> answer 500 "Command not recognized")

(*****************************************************************************)
(* POP3: the letter box *)
(*****************************************************************************)

let text (e : Mbox.entry) : string = Mail.to_string e.mail

(* a message's unique id, the same in every session: a digest of it *)
let uid (e : Mbox.entry) : string = String.sub (Digest.to_hex (Digest.string (e.envelope ^ text e))) 0 16

let pop_line (t : t) (server : Server.t) (id : int) (p : pop) (line : string) : unit =
  let say s = Server.send server id s in
  let ok s = say ("+OK " ^ s) and err s = say ("-ERR " ^ s) in
  let words = String.split_on_char ' ' (String.trim line) in
  let verb = String.uppercase_ascii (List.hd words) and arg = List.nth_opt words 1 in
  (* the messages not marked, with their numbers *)
  let live drop = List.filteri (fun i _ -> not (List.mem (i + 1) p.deleted)) (List.mapi (fun i e -> (i + 1, e)) drop) in
  let message drop = Option.bind (Option.bind arg int_of_string_opt) (fun n -> List.assoc_opt n (live drop)) in
  match (verb, p.drop) with
  | "USER", None -> (
      match arg with
      | Some u ->
          p.user <- Some (String.lowercase_ascii u);
          ok ""
      | None -> err "USER who?")
  | "PASS", None -> (
      match p.user with
      | None -> err "USER first"
      | Some u ->
          let pass = String.trim (String.sub line 4 (String.length line - 4)) in
          let right = match t.passwords with None -> true | Some l -> List.assoc_opt u l = Some pass in
          if right then (
            let drop = maildrop t u in
            p.drop <- Some drop;
            ok (Printf.sprintf "%s's maildrop has %d messages" u (List.length drop)))
          else err "invalid password")
  | "QUIT", None ->
      ok "bye";
      Server.close server id
  | _, None -> err "log in first: USER and PASS"
  | "STAT", Some drop ->
      let l = live drop in
      ok (Printf.sprintf "%d %d" (List.length l) (List.fold_left (fun n (_, e) -> n + String.length (text e)) 0 l))
  | ("LIST" | "UIDL"), Some drop -> (
      let item (n, e) = Printf.sprintf "%d %s" n (if verb = "LIST" then string_of_int (String.length (text e)) else uid e) in
      match arg with
      | Some _ -> ( match message drop with Some e -> ok (item (Option.get (Option.bind arg int_of_string_opt), e)) | None -> err "no such message")
      | None ->
          ok (Printf.sprintf "%d messages" (List.length (live drop)));
          List.iter (fun x -> say (item x)) (live drop);
          say ".")
  | "RETR", Some drop -> (
      match message drop with
      | Some e ->
          ok (Printf.sprintf "%d octets" (String.length (text e)));
          List.iter say (Pop3.stuff (text e))
      | None -> err "no such message")
  | "DELE", Some drop -> (
      match (message drop, Option.bind arg int_of_string_opt) with
      | Some _, Some n ->
          p.deleted <- n :: p.deleted;
          ok (Printf.sprintf "message %d deleted" n)
      | _ -> err "no such message")
  | "RSET", Some _ ->
      p.deleted <- [];
      ok ""
  | "NOOP", Some _ -> ok ""
  | "QUIT", Some drop ->
      (* the update state: now, and only now, the marked ones go -- by
         their identity, since mail may have come in since PASS *)
      let gone = List.filteri (fun i _ -> List.mem (i + 1) p.deleted) drop in
      let u = Option.get p.user in
      if gone <> [] then set_maildrop t u (List.filter (fun e -> not (List.memq e gone)) (maildrop t u));
      ok "bye";
      Server.close server id
  | _ -> err "command not recognized"

(*****************************************************************************)
(* The loop *)
(*****************************************************************************)

let lines (payload : string) : string list =
  String.split_on_char '\n' payload |> List.map (fun l -> if l <> "" && l.[String.length l - 1] = '\r' then String.sub l 0 (String.length l - 1) else l)

let step (t : t) ~(now : float) : unit =
  List.iteri
    (fun k server ->
      List.iter
        (fun (e : Server.event) ->
          match e with
          | Joined id ->
              Hashtbl.replace t.smtps (k, id) { hello = None; sender = None; recipients = []; data = None };
              List.iter (Server.send server id) (Smtp.reply 220 [ t.domain ^ " ESMTP tiny_maild" ])
          | Message (id, payload) -> Option.iter (fun s -> List.iter (fun l -> smtp_line t server id s l now) (lines payload)) (Hashtbl.find_opt t.smtps (k, id))
          | Left id -> Hashtbl.remove t.smtps (k, id))
        (Server.step server);
      Server.flush server)
    t.smtp_servers;
  List.iteri
    (fun k server ->
      List.iter
        (fun (e : Server.event) ->
          match e with
          | Joined id ->
              Hashtbl.replace t.pops (k, id) { user = None; drop = None; deleted = [] };
              Server.send server id "+OK tiny_maild POP3 ready"
          | Message (id, payload) -> Option.iter (fun p -> List.iter (pop_line t server id p) (lines payload)) (Hashtbl.find_opt t.pops (k, id))
          (* dropped before QUIT: nothing deleted *)
          | Left id -> Hashtbl.remove t.pops (k, id))
        (Server.step server);
      Server.flush server)
    t.pop_servers
