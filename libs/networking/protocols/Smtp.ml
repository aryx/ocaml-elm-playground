(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Smtp.mli *)

(*****************************************************************************)
(* Replies and commands *)
(*****************************************************************************)

let reply_line (line : string) : (int * bool * string) option =
  let n = String.length line in
  if n >= 3 then
    match int_of_string_opt (String.sub line 0 3) with
    | Some code when n = 3 -> Some (code, false, "")
    | Some code when line.[3] = ' ' || line.[3] = '-' -> Some (code, line.[3] = '-', String.sub line 4 (n - 4))
    | _ -> None
  else None

let reply (code : int) (lines : string list) : string list =
  let k = List.length lines in
  List.mapi (fun i l -> Printf.sprintf "%d%c%s" code (if i < k - 1 then '-' else ' ') l) lines

type command = Helo of string | Ehlo of string | Mail_from of string | Rcpt_to of string | Data | Rset | Noop | Quit | Unknown of string

(* "FROM:<alice@tiny>" -> "alice@tiny" *)
let path (s : string) : string =
  let s = String.trim s in
  match (String.index_opt s '<', String.rindex_opt s '>') with Some i, Some j when j > i -> String.sub s (i + 1) (j - i - 1) | _ -> s

let parse_command (line : string) : command =
  let line = String.trim line in
  let verb, rest = match String.index_opt line ' ' with Some i -> (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1)) | None -> (line, "") in
  let after_colon s = match String.index_opt s ':' with Some i -> String.sub s (i + 1) (String.length s - i - 1) | None -> s in
  match String.uppercase_ascii verb with
  | "HELO" -> Helo (String.trim rest)
  | "EHLO" -> Ehlo (String.trim rest)
  | "MAIL" -> Mail_from (path (after_colon rest))
  | "RCPT" -> Rcpt_to (path (after_colon rest))
  | "DATA" -> Data
  | "RSET" -> Rset
  | "NOOP" -> Noop
  | "QUIT" -> Quit
  | _ -> Unknown line

let command_to_string = function
  | Helo d -> "HELO " ^ d
  | Ehlo d -> "EHLO " ^ d
  | Mail_from a -> "MAIL FROM:<" ^ a ^ ">"
  | Rcpt_to a -> "RCPT TO:<" ^ a ^ ">"
  | Data -> "DATA"
  | Rset -> "RSET"
  | Noop -> "NOOP"
  | Quit -> "QUIT"
  | Unknown s -> s

(*****************************************************************************)
(* The message *)
(*****************************************************************************)

type envelope = { sender : string; recipients : string list; text : string }

let envelope ~(sender : string) (m : Mail.t) : envelope =
  let recipients = List.concat_map (fun name -> List.concat_map (fun v -> List.map (fun (a : Mail.address) -> a.mailbox) (Mail.addresses v)) (Mail.get_all m name)) [ "to"; "cc"; "bcc" ] in
  { sender; recipients; text = Mail.to_string (Mail.remove "bcc" m) }

let stuff (text : string) : string list =
  let lines = String.split_on_char '\n' (Mail.lf text) in
  (* the text's last line break ends its last line, not a line of its own *)
  let lines = match List.rev lines with "" :: rest -> List.rev rest | _ -> lines in
  List.map (fun l -> if l <> "" && l.[0] = '.' then "." ^ l else l) lines @ [ "." ]

let unstuff (lines : string list) : string =
  let lines = match List.rev lines with "." :: rest -> List.rev rest | _ -> lines in
  String.concat "" (List.map (fun l -> (if l <> "" && l.[0] = '.' then String.sub l 1 (String.length l - 1) else l) ^ "\n") lines)

(*****************************************************************************)
(* The client *)
(*****************************************************************************)

type outcome = Sent of int * string list | Refused of string

type phase =
  | Greeting
  | Hello of bool (* EHLO sent; HELO after it was refused *)
  | Mail
  | Rcpt of string list (* the recipients still to name *)
  | Data_sent
  | Body
  | Reset
  | Quitting
  | Closed of string option (* why, if it went wrong *)

type client = {
  hello : string;
  todo : envelope list; (* the current one first *)
  phase : phase;
  accepted : int;
  refusals : string list; (* the current message's refused recipients *)
  outcomes : outcome list; (* the latest first *)
  more : string list; (* a reply's lines so far, before its last *)
}

let client ~(hello : string) (envelopes : envelope list) : client =
  { hello; todo = envelopes; phase = Greeting; accepted = 0; refusals = []; outcomes = []; more = [] }

let send c phase cmd = ({ c with phase }, [ command_to_string cmd ])

(* the current message is done with *)
let done_with (outcome : outcome) (c : client) : client = { c with todo = List.tl c.todo; outcomes = outcome :: c.outcomes }

(* the next message, or goodbye *)
let rec next (c : client) : client * string list =
  let c = { c with accepted = 0; refusals = [] } in
  match c.todo with
  | { recipients = []; _ } :: _ -> next (done_with (Refused "no recipient") c)
  | e :: _ -> send c Mail (Mail_from e.sender)
  | [] -> send c Quitting Quit

let refuse (why : string) (c : client) : client * string list = send (done_with (Refused why) c) Reset Rset

let step (c : client) (line : string) : client * string list =
  match reply_line line with
  | None -> (c, [])
  | Some (_, true, text) -> ({ c with more = c.more @ [ text ] }, [])
  | Some (code, false, text) -> (
      let text = String.concat " " (c.more @ [ text ]) and c = { c with more = [] } in
      let ok = code / 100 = 2 and said = Printf.sprintf "%d %s" code text in
      match c.phase with
      | Greeting -> if code = 220 then send c (Hello true) (Ehlo c.hello) else send c (Closed (Some said)) Quit
      | Hello ehlo ->
          if ok then next c
          else if ehlo then send c (Hello false) (Helo c.hello) (* a server of 1982: no EHLO *)
          else send c (Closed (Some said)) Quit
      | Mail -> if ok then send c (Rcpt (List.tl (List.hd c.todo).recipients)) (Rcpt_to (List.hd (List.hd c.todo).recipients)) else refuse said c
      | Rcpt rest -> (
          let c = if ok then { c with accepted = c.accepted + 1 } else { c with refusals = c.refusals @ [ said ] } in
          match rest with
          | r :: rest -> send c (Rcpt rest) (Rcpt_to r)
          | [] -> if c.accepted > 0 then send c Data_sent Data else refuse "no recipient accepted" c)
      | Data_sent -> if code = 354 then ({ c with phase = Body }, stuff (List.hd c.todo).text) else refuse said c
      | Body -> next (done_with (if ok then Sent (c.accepted, c.refusals) else Refused said) c)
      | Reset -> next c
      | Quitting -> ({ c with phase = Closed None }, [])
      | Closed _ -> (c, []))

let finished (c : client) : (outcome list, string) result option =
  match c.phase with
  | Closed None -> Some (Ok (List.rev c.outcomes))
  | Closed (Some why) -> Some (Error why)
  | _ -> None
