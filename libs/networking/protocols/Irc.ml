(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Irc.mli *)

type message = { prefix : string option; command : string; params : string list }

let msg ?prefix (command : string) (params : string list) : message = { prefix; command; params }

(* the word at the start of [s] and the rest, after the spaces *)
let word (s : string) : string * string =
  match String.index_opt s ' ' with
  | None -> (s, "")
  | Some i ->
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      let rec skip r = if String.length r > 0 && r.[0] = ' ' then skip (String.sub r 1 (String.length r - 1)) else r in
      (String.sub s 0 i, skip rest)

let parse (line : string) : (message, string) result =
  let line =
    let n = String.length line in
    if n >= 2 && String.sub line (n - 2) 2 = "\r\n" then String.sub line 0 (n - 2)
    else if n >= 1 && line.[n - 1] = '\n' then String.sub line 0 (n - 1)
    else line
  in
  if String.length line > 510 then Error "a line longer than 512 bytes"
  else
    let prefix, rest =
      if String.length line > 0 && line.[0] = ':' then
        let p, rest = word (String.sub line 1 (String.length line - 1)) in
        (Some p, rest)
      else (None, line)
    in
    let command, rest = word rest in
    if command = "" then Error (Printf.sprintf "no command in %S" line)
    else
      (* the parameters: words, until one starts with ':', the trailing *)
      let rec params rest acc =
        if rest = "" then List.rev acc
        else if rest.[0] = ':' || List.length acc = 14 then
          List.rev ((if rest.[0] = ':' then String.sub rest 1 (String.length rest - 1) else rest) :: acc)
        else
          let p, rest = word rest in
          params rest (p :: acc)
      in
      Ok { prefix; command = String.uppercase_ascii command; params = params rest [] }

let print (m : message) : string =
  let rec params = function
    | [] -> []
    | [ last ] when last = "" || String.contains last ' ' || last.[0] = ':' -> [ ":" ^ last ]
    | p :: rest -> p :: params rest
  in
  String.concat " " ((match m.prefix with Some p -> [ ":" ^ p ] | None -> []) @ (m.command :: params m.params))

let nick_of (prefix : string) : string = match String.index_opt prefix '!' with Some i -> String.sub prefix 0 i | None -> prefix
