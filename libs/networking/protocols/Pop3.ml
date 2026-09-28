(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pop3.mli *)

let status (line : string) : (string, string) result option =
  let after p = String.trim (String.sub line (String.length p) (String.length line - String.length p)) in
  let starts p = String.length line >= String.length p && String.sub line 0 (String.length p) = p in
  if starts "+OK" then Some (Ok (after "+OK")) else if starts "-ERR" then Some (Error (after "-ERR")) else None

(* the same transparency as SMTP's *)
let unstuff = Smtp.unstuff
let stuff = Smtp.stuff

(*****************************************************************************)
(* The client *)
(*****************************************************************************)

type phase =
  | Greeting
  | User
  | Pass
  | Stat
  | Listing of bool (* the status line read: the list's lines follow *)
  | Retr of int * bool (* the message, and whether its lines follow *)
  | Dele of int
  | Quitting
  | Closed of string option

type client = {
  user : string;
  pass : string;
  leave : bool;
  known : string list;
  limit : int option;
  phase : phase;
  lines : string list; (* a multi-line reply so far, the latest first *)
  wanted : (int * string) list; (* the messages to fetch, and their uid *)
  fetched : (string * string) list; (* the latest first *)
}

let client ~user ~pass ~leave ~known ?limit () = { user; pass; leave; known; limit; phase = Greeting; lines = []; wanted = []; fetched = [] }

let say c phase line = ({ c with phase; lines = [] }, [ line ])
let quit c = say c Quitting "QUIT"

(* the next message to fetch, or goodbye *)
let next c = match c.wanted with (n, _) :: _ -> say c (Retr (n, false)) (Printf.sprintf "RETR %d" n) | [] -> quit c

let step (c : client) (line : string) : client * string list =
  let failed why = say c (Closed (Some why)) "QUIT" in
  match c.phase with
  (* inside a multi-line reply: its lines, until the "." *)
  | Listing true | Retr (_, true) when line <> "." -> ({ c with lines = line :: c.lines }, [])
  | Listing true ->
      (* "1 120", or with UIDL "1 whqtswO00WBw418f9t5JxYwZ" *)
      let items = List.rev c.lines |> List.filter_map (fun l -> match String.split_on_char ' ' l with n :: rest -> Option.map (fun n -> (n, String.concat " " rest)) (int_of_string_opt n) | [] -> None) in
      let wanted = if c.leave then List.filter (fun (_, uid) -> not (List.mem uid c.known)) items else List.map (fun (n, _) -> (n, "")) items in
      let wanted = match c.limit with Some k -> List.filteri (fun i _ -> i >= List.length wanted - k) wanted | None -> wanted in
      next { c with wanted }
  | Retr (n, true) ->
      let uid = List.assoc n c.wanted in
      let c = { c with fetched = (uid, unstuff (List.rev c.lines)) :: c.fetched; wanted = List.remove_assoc n c.wanted } in
      if c.leave then next c else say c (Dele n) (Printf.sprintf "DELE %d" n)
  | _ -> (
      match (status line, c.phase) with
      | None, _ -> (c, [])
      | Some (Error why), Quitting -> ({ c with phase = Closed (Some why) }, [])
      | Some (Error why), _ -> failed why
      | Some (Ok _), Greeting -> say c User ("USER " ^ c.user)
      | Some (Ok _), User -> say c Pass ("PASS " ^ c.pass)
      | Some (Ok _), Pass -> say c Stat "STAT"
      | Some (Ok count), Stat ->
          if String.length count > 0 && count.[0] = '0' then quit c else say c (Listing false) (if c.leave then "UIDL" else "LIST")
      | Some (Ok _), Listing false -> ({ c with phase = Listing true; lines = [] }, [])
      | Some (Ok _), Retr (n, false) -> ({ c with phase = Retr (n, true); lines = [] }, [])
      | Some (Ok _), Dele _ -> next c
      | Some (Ok _), Quitting -> ({ c with phase = Closed None }, [])
      | Some (Ok _), _ -> (c, []))

let finished (c : client) : ((string * string) list, string) result option =
  match c.phase with Closed None -> Some (Ok (List.rev c.fetched)) | Closed (Some why) -> Some (Error why) | _ -> None
