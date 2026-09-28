(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Line_discipline.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type mode = Cooked | Raw
type event = Line of string | Key of string | Interrupt | End_of_file
type t = { mode : mode; echo : bool; line : string }

let create () = { mode = Cooked; echo = true; line = "" }
let mode (t : t) = t.mode
let set_mode (t : t) mode = { t with mode }
let set_echo (t : t) echo = { t with echo }
let pending (t : t) = t.line

(*****************************************************************************)
(* Keys *)
(*****************************************************************************)

let split_keys (s : string) : string list =
  let n = String.length s in
  (* where the key starting at [i] ends (excluded) *)
  let key_end i =
    let c = Char.code s.[i] in
    if s.[i] = '\x1b' && i + 1 < n then
      match s.[i + 1] with
      | '[' ->
          (* the parameters, then a final byte from @ to ~ *)
          let j = ref (i + 2) in
          while !j < n && not (s.[!j] >= '@' && s.[!j] <= '~') do
            incr j
          done;
          min n (!j + 1)
      | 'O' -> min n (i + 3)
      (* ESC and a character: Alt and that character *)
      | _ -> i + 2
    else if c land 0xE0 = 0xC0 then min n (i + 2)
    else if c land 0xF0 = 0xE0 then min n (i + 3)
    else if c land 0xF8 = 0xF0 then min n (i + 4)
    else i + 1
  in
  let rec go i acc = if i >= n then List.rev acc else let j = key_end i in go j (String.sub s i (j - i) :: acc) in
  go 0 []

(* the line without its last UTF-8 character (continuation bytes are
   10xxxxxx: drop them, then the lead) *)
let drop_last (s : string) : string =
  let n = ref (String.length s) in
  while !n > 0 && Char.code s.[!n - 1] land 0xC0 = 0x80 do
    decr n
  done;
  if !n > 0 then String.sub s 0 (!n - 1) else ""

(* how many characters (not bytes) the screen shows for [s] *)
let length (s : string) : int = String.fold_left (fun k c -> if Char.code c land 0xC0 = 0x80 then k else k + 1) 0 s

let rubout (k : int) : string = String.concat "" (List.init k (fun _ -> "\b \b"))

(*****************************************************************************)
(* Cooked mode *)
(*****************************************************************************)

(* one key: the tty after it, its echo, what the program reads *)
let cooked (t : t) (key : string) : t * string * event list =
  let echo s = if t.echo then s else "" in
  match key with
  | "\r" | "\n" -> ({ t with line = "" }, echo "\r\n", [ Line t.line ])
  | "\x7f" | "\b" ->
      if t.line = "" then (t, "", []) else ({ t with line = drop_last t.line }, echo (rubout 1), [])
  (* Control-U *)
  | "\x15" -> ({ t with line = "" }, echo (rubout (length t.line)), [])
  (* Control-W: the spaces before the word, then the word *)
  | "\x17" ->
      let rec strip pred s = if s <> "" && pred s.[String.length s - 1] then strip pred (drop_last s) else s in
      let line = strip (fun c -> c <> ' ') (strip (fun c -> c = ' ') t.line) in
      ({ t with line }, echo (rubout (length t.line - length line)), [])
  (* Control-C *)
  | "\x03" -> ({ t with line = "" }, echo "^C\r\n", [ Interrupt ])
  (* Control-D *)
  | "\x04" -> if t.line = "" then (t, "", [ End_of_file ]) else (t, "", [])
  (* other controls, and escape sequences (arrows...): dropped *)
  | _ when key.[0] < ' ' || key.[0] = '\x7f' -> (t, "", [])
  | _ -> ({ t with line = t.line ^ key }, echo key, [])

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let input (t : t) (bytes : string) : t * string * event list =
  List.fold_left
    (fun (t, echoed, events) key ->
      match t.mode with
      | Raw -> (t, echoed, events @ [ Key key ])
      | Cooked ->
          let t, e, evs = cooked t key in
          (t, echoed ^ e, events @ evs))
    (t, "", []) (split_keys bytes)

let output (bytes : string) : string =
  String.concat "\r\n" (String.split_on_char '\n' bytes)
