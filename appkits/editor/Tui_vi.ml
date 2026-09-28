(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tui_vi.mli *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* what y and d keep, and p puts back: whole lines, or characters *)
type register = Lines of string list | Chars of string

type mode = Normal | Insert | Command of string (* after ":" *) | Search of bool * string (* after "/" (true) or "?" *)

type snapshot = { s_lines : string array; s_row : int; s_col : int }

type model = {
  lines : string array; (* never changed in place: a change makes a new array *)
  row : int;
  col : int;
  want : int; (* the column j and k aim for *)
  top : int; (* the first line on the screen *)
  mode : mode;
  file : string;
  disk : (string * string) list;
  modified : bool;
  undo : snapshot option; (* before the last change *)
  register : register;
  pending : string list; (* the keys of a command not complete yet: "2"; "d" *)
  last_change : string list; (* the keys of the last change, for . *)
  recording : string list option; (* the keys of a change still being made: an insertion *)
  search : (bool * string) option; (* the last search, forward or not *)
  number : bool; (* :set number *)
  message : string;
  quit : bool;
}

(* the screen's lines of text: all but the status line *)
let height = 23

let line (m : model) (r : int) : string = m.lines.(r)
let nlines (m : model) : int = Array.length m.lines
let set_lines (m : model) (lines : string list) : model = { m with lines = Array.of_list (if lines = [] then [ "" ] else lines); modified = true }

(* the cursor inside the text: in normal mode on a character, never
   after the last one *)
let clamp (m : model) : model =
  let row = max 0 (min (nlines m - 1) m.row) in
  let len = String.length (line m row) in
  let last = if m.mode = Insert then len else max 0 (len - 1) in
  { m with row; col = max 0 (min last m.col) }

let first_nonblank (s : string) : int =
  let rec go i = if i < String.length s && (s.[i] = ' ' || s.[i] = '\t') then go (i + 1) else i in
  if go 0 >= String.length s then max 0 (String.length s - 1) else go 0

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(* A position is (row, col); (row, length) is the line's newline. The
   classes: blank (newlines too), word characters, and the rest --
   punctuation, a word of its own for w *)
let char_at (m : model) ((r, c) : int * int) : char = if c < String.length (line m r) then (line m r).[c] else '\n'

let cls (ch : char) : int =
  match ch with
  | ' ' | '\t' | '\n' -> 0
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> 1
  | _ -> 2

let next (m : model) ((r, c) : int * int) : (int * int) option =
  if c < String.length (line m r) then Some (r, c + 1) else if r + 1 < nlines m then Some (r + 1, 0) else None

let prev (m : model) ((r, c) : int * int) : (int * int) option =
  if c > 0 then Some (r, c - 1) else if r > 0 then Some (r - 1, String.length (line m (r - 1))) else None

(* an empty line is a word to w and b *)
let empty_line (m : model) ((r, c) : int * int) : bool = c = 0 && line m r = ""

(* w: past this word, then past blanks, stopping on an empty line *)
let word_forward (m : model) (p : int * int) : int * int =
  let rec skip_same p k = match next m p with Some q when cls (char_at m q) = k && k <> 0 -> skip_same q k | Some q -> Some q | None -> None in
  let rec skip_blank p = if cls (char_at m p) = 0 && not (empty_line m p) then match next m p with Some q -> skip_blank q | None -> p else p in
  let k = cls (char_at m p) in
  match (if k = 0 then next m p else skip_same p k) with
  | Some q -> skip_blank q
  | None -> (nlines m - 1, String.length (line m (nlines m - 1)))

(* b: back past blanks, then to the start of the word before *)
let word_backward (m : model) (p : int * int) : int * int =
  let rec skip_blank p = if cls (char_at m p) = 0 && not (empty_line m p) then match prev m p with Some q -> skip_blank q | None -> p else p in
  let rec start p = match prev m p with Some q when cls (char_at m q) = cls (char_at m p) && cls (char_at m p) <> 0 -> start q | _ -> p in
  match prev m p with Some q -> start (skip_blank q) | None -> p

(* e: to the last character of this word, or the next one *)
let word_end (m : model) (p : int * int) : int * int =
  let rec skip_blank p = if cls (char_at m p) = 0 then match next m p with Some q -> skip_blank q | None -> p else p in
  let rec finish p = match next m p with Some q when cls (char_at m q) = cls (char_at m p) -> finish q | _ -> p in
  match next m p with Some q -> finish (skip_blank q) | None -> p

(*****************************************************************************)
(* The grammar *)
(*****************************************************************************)

type parsed = Incomplete | Invalid | Complete of int option * string list

(* a motion's keys, read *)
type motion_keys = Motion_incomplete | Motion_invalid | Motion of string list

let is_digit (k : string) = String.length k = 1 && k.[0] >= '0' && k.[0] <= '9'

(* the motions of one key, and those that take a character after *)
let motions = [ "h"; "j"; "k"; "l"; "w"; "b"; "e"; "0"; "^"; "$"; "G"; "n"; "N"; " "; "\x1b[A"; "\x1b[B"; "\x1b[C"; "\x1b[D" ]
let char_motions = [ "f"; "F"; "t"; "T" ]

let simple =
  [ "x"; "X"; "D"; "C"; "s"; "S"; "Y"; "p"; "P"; "u"; "."; "i"; "a"; "I"; "A"; "o"; "O"; "J"; "~"; ":"; "/"; "?"; "\x06"; "\x02"; "\x04"; "\x15"; "\x07"; "\x0c" ]

(* count? ( motion | operator count? (motion | the operator again) | simple ) *)
let parse_command (keys : string list) : parsed =
  (* a count: digits, not starting with 0 (0 is a motion) *)
  let rec count keys acc =
    match keys with k :: rest when is_digit k && not (acc = None && k = "0") -> count rest (Some ((10 * Option.value acc ~default:0) + int_of_string k)) | _ -> (acc, keys)
  in
  let motion keys =
    match keys with
    | [] -> Motion_incomplete
    | [ k ] when List.mem k char_motions -> Motion_incomplete
    | [ k; c ] when List.mem k char_motions -> Motion [ k; c ]
    | [ "g" ] -> Motion_incomplete
    | [ "g"; "g" ] -> Motion [ "g"; "g" ]
    | [ k ] when List.mem k motions -> Motion [ k ]
    | _ -> Motion_invalid
  in
  let times a b = match (a, b) with None, None -> None | _ -> Some (Option.value a ~default:1 * Option.value b ~default:1) in
  let n1, rest = count keys None in
  match rest with
  | [] -> Incomplete
  | (("d" | "c" | "y") as op) :: rest -> (
      let n2, rest = count rest None in
      match rest with
      | [ k ] when k = op -> Complete (times n1 n2, [ op; op ])
      | _ -> ( match motion rest with Motion mo -> Complete (times n1 n2, op :: mo) | Motion_incomplete -> Incomplete | Motion_invalid -> Invalid))
  | [ ("r" | "Z") ] -> Incomplete
  | [ "r"; c ] -> Complete (n1, [ "r"; c ])
  | [ "Z"; "Z" ] -> Complete (n1, [ "Z"; "Z" ])
  | [ k ] when List.mem k simple -> Complete (n1, [ k ])
  | _ -> ( match motion rest with Motion mo -> Complete (n1, mo) | Motion_incomplete -> Incomplete | Motion_invalid -> Invalid)

(*****************************************************************************)
(* Motions *)
(*****************************************************************************)

(* a motion's reach: whole lines, or characters up to its end, the end
   itself included or not *)
type kind = Linewise | Exclusive | Inclusive

let rec repeat (n : int) (f : 'a -> 'a) (x : 'a) : 'a = if n <= 0 then x else repeat (n - 1) f (f x)

(* the text searched for, from after the cursor, wrapping at the end *)
let find_text (m : model) (forward : bool) (pat : string) : (int * int) option =
  if pat = "" then None
  else
    let n = nlines m in
    let matches_at r c = c + String.length pat <= String.length (line m r) && String.sub (line m r) c (String.length pat) = pat in
    let in_line r ~from ~upto =
      if forward then
        let rec go c = if c > upto then None else if matches_at r c then Some c else go (c + 1) in
        go from
      else
        let rec go c = if c < upto then None else if matches_at r c then Some c else go (c - 1) in
        go from
    in
    let rec scan k =
      if k > n then None
      else
        let r = ((if forward then m.row + k else m.row - k) mod n + n) mod n in
        let len = String.length (line m r) in
        let found =
          if k = 0 then (if forward then in_line r ~from:(m.col + 1) ~upto:len else in_line r ~from:(m.col - 1) ~upto:0)
          else if forward then in_line r ~from:0 ~upto:len
          else in_line r ~from:len ~upto:0
        in
        match found with Some c -> Some (r, c) | None -> scan (k + 1)
    in
    scan 0

(* where [motion] goes from the cursor, [n] times, and its kind *)
let motion (m : model) (mo : string list) (count : int option) : ((int * int) * kind) option =
  let n = Option.value count ~default:1 in
  let s = line m m.row in
  let here = (m.row, m.col) in
  let in_line f = match f with Some c -> Some ((m.row, c), Exclusive) | None -> None in
  let find_char c ~forward =
    let rec go i k = if i < 0 || i >= String.length s then None else if s.[i] = c then (if k = 1 then Some i else go (if forward then i + 1 else i - 1) (k - 1)) else go (if forward then i + 1 else i - 1) k in
    go (if forward then m.col + 1 else m.col - 1) n
  in
  match mo with
  | [ ("h" | "\x1b[D") ] -> Some ((m.row, max 0 (m.col - n)), Exclusive)
  | [ ("l" | " " | "\x1b[C") ] -> Some ((m.row, min (String.length s) (m.col + n)), Exclusive)
  | [ ("j" | "\x1b[B") ] -> if m.row + n < nlines m then Some ((m.row + n, m.want), Linewise) else None
  | [ ("k" | "\x1b[A") ] -> if m.row - n >= 0 then Some ((m.row - n, m.want), Linewise) else None
  | [ "w" ] -> Some (repeat n (word_forward m) here, Exclusive)
  | [ "b" ] -> Some (repeat n (word_backward m) here, Exclusive)
  | [ "e" ] -> Some (repeat n (word_end m) here, Inclusive)
  | [ "0" ] -> Some ((m.row, 0), Exclusive)
  | [ "^" ] -> Some ((m.row, first_nonblank s), Exclusive)
  | [ "$" ] ->
      let r = min (nlines m - 1) (m.row + n - 1) in
      Some ((r, max 0 (String.length (line m r) - 1)), Inclusive)
  | [ "G" ] ->
      let r = match count with Some k -> max 0 (min (nlines m - 1) (k - 1)) | None -> nlines m - 1 in
      Some ((r, first_nonblank (line m r)), Linewise)
  | [ "g"; "g" ] ->
      let r = match count with Some k -> max 0 (min (nlines m - 1) (k - 1)) | None -> 0 in
      Some ((r, first_nonblank (line m r)), Linewise)
  | [ "f"; c ] -> Option.map (fun (p, _) -> (p, Inclusive)) (in_line (find_char c.[0] ~forward:true))
  | [ "t"; c ] -> Option.map (fun ((r, col), _) -> ((r, col - 1), Inclusive)) (in_line (find_char c.[0] ~forward:true))
  | [ "F"; c ] -> in_line (find_char c.[0] ~forward:false)
  | [ "T"; c ] -> Option.map (fun ((r, col), k) -> ((r, col + 1), k)) (in_line (find_char c.[0] ~forward:false))
  | [ ("n" | "N") ] -> (
      match m.search with
      | Some (forward, pat) -> Option.map (fun p -> (p, Exclusive)) (find_text m (if mo = [ "n" ] then forward else not forward) pat)
      | None -> None)
  | _ -> None

(*****************************************************************************)
(* Changes *)
(*****************************************************************************)

let snapshot (m : model) : snapshot = { s_lines = m.lines; s_row = m.row; s_col = m.col }

(* the text between two positions, [a] before [b], [b] excluded *)
let text_between (m : model) ((r1, c1) : int * int) ((r2, c2) : int * int) : string =
  if r1 = r2 then String.sub (line m r1) c1 (c2 - c1)
  else
    let first = String.sub (line m r1) c1 (String.length (line m r1) - c1) in
    let middle = List.init (r2 - r1 - 1) (fun i -> line m (r1 + 1 + i)) in
    let last = String.sub (line m r2) 0 c2 in
    String.concat "\n" ((first :: middle) @ [ last ])

(* the text between two positions taken out, the cursor at the first *)
let delete_between (m : model) ((r1, c1) : int * int) ((r2, c2) : int * int) : model =
  let before = List.init r1 (line m) and after = List.init (nlines m - r2 - 1) (fun i -> line m (r2 + 1 + i)) in
  let joined = String.sub (line m r1) 0 c1 ^ String.sub (line m r2) c2 (String.length (line m r2) - c2) in
  { (set_lines m (before @ [ joined ] @ after)) with row = r1; col = c1 }

(* [text] put at a position, its newlines making lines; the cursor on
   its last character *)
let insert_text (m : model) ((r, c) : int * int) (text : string) : model =
  let s = line m r in
  let pieces = String.split_on_char '\n' text in
  let head = String.sub s 0 c and tail = String.sub s c (String.length s - c) in
  let k = List.length pieces in
  let new_lines = List.mapi (fun i p -> (if i = 0 then head else "") ^ p ^ if i = k - 1 then tail else "") pieces in
  let before = List.init r (line m) and after = List.init (nlines m - r - 1) (fun i -> line m (r + 1 + i)) in
  let last = List.nth pieces (k - 1) in
  let col = (if k = 1 then c else 0) + String.length last - 1 in
  { (set_lines m (before @ new_lines @ after)) with row = r + k - 1; col = max 0 col }

let insert_lines (m : model) (at : int) (ls : string list) : model =
  let before = List.init at (line m) and after = List.init (nlines m - at) (fun i -> line m (at + i)) in
  { (set_lines m (before @ ls @ after)) with row = at; col = first_nonblank (List.hd ls) }

let delete_lines (m : model) (r1 : int) (r2 : int) : model =
  let kept = List.filteri (fun i _ -> i < r1 || i > r2) (Array.to_list m.lines) in
  let m = set_lines m kept in
  let row = min r1 (nlines m - 1) in
  { m with row; col = first_nonblank (line m row) }

(* an operator on a motion's reach *)
let operate (m : model) (op : string) (target : int * int) (kind : kind) : model =
  let a = min (m.row, m.col) target and b = max (m.row, m.col) target in
  match kind with
  | Linewise -> (
      let r1 = fst a and r2 = fst b in
      let taken = List.init (r2 - r1 + 1) (fun i -> line m (r1 + i)) in
      let m = { m with register = Lines taken } in
      match op with
      | "y" -> { m with row = r1; message = (if r2 > r1 then Printf.sprintf "%d lines yanked" (r2 - r1 + 1) else "") }
      | "d" -> delete_lines m r1 r2
      | _ -> { (insert_lines (delete_lines m r1 r2) r1 [ "" ]) with mode = Insert; col = 0 })
  | Exclusive | Inclusive -> (
      (* inclusive: the character at the end is taken too; on a line's
         end, the newline isn't *)
      let b = if kind = Inclusive then (fst b, min (String.length (line m (fst b))) (snd b + 1)) else b in
      let b = if snd b > String.length (line m (fst b)) then (fst b, String.length (line m (fst b))) else b in
      let m = { m with register = Chars (text_between m a b) } in
      match op with
      | "y" -> { m with row = fst a; col = snd a }
      | "d" -> delete_between m a b
      | _ -> { (delete_between m a b) with mode = Insert })

(*****************************************************************************)
(* Normal mode *)
(*****************************************************************************)

let put (m : model) ~(after : bool) (n : int) : model =
  match m.register with
  | Lines ls ->
      let ls = List.concat (List.init n (fun _ -> ls)) in
      insert_lines m (if after then m.row + 1 else m.row) ls
  | Chars "" -> m
  | Chars s ->
      let s = String.concat "" (List.init n (fun _ -> s)) in
      let c = if after && line m m.row <> "" then m.col + 1 else m.col in
      insert_text m (m.row, c) s

let join (m : model) (n : int) : model =
  let rec go m k =
    if k = 0 || m.row + 1 >= nlines m then m
    else
      let a = line m m.row and b = String.trim (line m (m.row + 1)) in
      let joined = if b = "" then a else if a = "" then b else a ^ " " ^ b in
      let lines = List.filteri (fun i _ -> i <> m.row + 1) (Array.to_list m.lines) in
      let m = set_lines m (List.mapi (fun i l -> if i = m.row then joined else l) lines) in
      go { m with col = String.length a } (k - 1)
  in
  go m (max 1 (n - 1))

let replace_chars (m : model) (c : string) (n : int) : model =
  let s = line m m.row in
  if m.col + n > String.length s then m
  else
    let s = String.sub s 0 m.col ^ String.concat "" (List.init n (fun _ -> c)) ^ String.sub s (m.col + n) (String.length s - m.col - n) in
    { (set_lines m (List.mapi (fun i l -> if i = m.row then s else l) (Array.to_list m.lines))) with col = m.col + n - 1 }

let toggle_case (m : model) (n : int) : model =
  let s = Bytes.of_string (line m m.row) in
  let stop = min (Bytes.length s) (m.col + n) in
  for i = m.col to stop - 1 do
    let c = Bytes.get s i in
    Bytes.set s i (if Char.lowercase_ascii c = c then Char.uppercase_ascii c else Char.lowercase_ascii c)
  done;
  let s = Bytes.to_string s in
  { (set_lines m (List.mapi (fun i l -> if i = m.row then s else l) (Array.to_list m.lines))) with col = stop }

(* the commands that change the text: undo keeps what was before them,
   . repeats them *)
let is_change (cmd : string list) : bool =
  match cmd with
  | ("d" | "c") :: _ | [ ("x" | "X" | "D" | "C" | "s" | "S" | "p" | "P" | "J" | "~" | "i" | "a" | "I" | "A" | "o" | "O") ] | [ "r"; _ ] -> true
  | _ -> false

let info (m : model) : string =
  Printf.sprintf "\"%s\"%s %d lines --%d%%--" m.file (if m.modified then " [Modified]" else "") (nlines m) (100 * (m.row + 1) / nlines m)

(* the ex commands, after ":" (their definitions below, ex's) *)
let rec normal (m : model) (count : int option) (cmd : string list) : model =
  let n = Option.value count ~default:1 in
  let m = if is_change cmd then { m with undo = Some (snapshot m) } else m in
  let insert m = { m with mode = Insert } in
  match cmd with
  | [ op; op' ] when op = op' && List.mem op [ "d"; "c"; "y" ] ->
      if m.row + n - 1 >= nlines m then m else operate m op (m.row + n - 1, 0) Linewise
  | op :: mo when List.mem op [ "d"; "c"; "y" ] -> (
      (* cw is ce, and dw stops at the end of its line: vi's own rules *)
      let mo = if op = "c" && mo = [ "w" ] && cls (char_at m (m.row, m.col)) <> 0 then [ "e" ] else mo in
      match motion m mo count with
      | Some ((r, c), kind) ->
          let target = if mo = [ "w" ] && r > m.row then (m.row, String.length (line m m.row)) else (r, c) in
          operate m op target kind
      | None -> m)
  | [ "x" ] -> if line m m.row = "" then m else normal m count [ "d"; "l" ]
  | [ "X" ] -> if m.col = 0 then m else normal m count [ "d"; "h" ]
  | [ "D" ] -> normal m None [ "d"; "$" ]
  | [ "C" ] -> normal m None [ "c"; "$" ]
  | [ "s" ] -> normal m count [ "c"; "l" ]
  | [ "S" ] -> normal m count [ "c"; "c" ]
  | [ "Y" ] -> normal m count [ "y"; "y" ]
  | [ "p" ] -> put m ~after:true n
  | [ "P" ] -> put m ~after:false n
  | [ "J" ] -> join m n
  | [ "r"; c ] -> replace_chars m c n
  | [ "~" ] -> toggle_case m n
  | [ "u" ] -> (
      match m.undo with
      | Some s -> { m with lines = s.s_lines; row = s.s_row; col = s.s_col; undo = Some (snapshot m); modified = true }
      | None -> { m with message = "Nothing to undo" })
  | [ "i" ] -> insert m
  | [ "a" ] -> insert { m with col = (if line m m.row = "" then 0 else m.col + 1) }
  | [ "I" ] -> insert { m with col = first_nonblank (line m m.row) }
  | [ "A" ] -> insert { m with col = String.length (line m m.row) }
  | [ "o" ] -> insert (insert_lines m (m.row + 1) [ "" ])
  | [ "O" ] -> insert (insert_lines m m.row [ "" ])
  | [ ":" ] -> { m with mode = Command "" }
  | [ "/" ] -> { m with mode = Search (true, "") }
  | [ "?" ] -> { m with mode = Search (false, "") }
  | [ "Z"; "Z" ] -> ex m "x"
  | [ "\x06" ] -> page m (n * (height - 2))
  | [ "\x02" ] -> page m (-n * (height - 2))
  | [ "\x04" ] -> page m (height / 2)
  | [ "\x15" ] -> page m (-height / 2)
  | [ "\x07" ] -> { m with message = info m }
  | [ "\x0c" ] -> m
  | mo -> ( match motion m mo count with Some ((r, c), _) -> { m with row = r; col = c } | None -> m)

and page (m : model) (k : int) : model =
  let row = max 0 (min (nlines m - 1) (m.row + k)) in
  { m with row; top = max 0 (min (nlines m - 1) (m.top + k)); col = first_nonblank (line m row) }

(*****************************************************************************)
(* ex *)
(*****************************************************************************)

(* :s/old/new/ with its flags: the first on each line, or every one (g) *)
and substitute (m : model) (all_lines : bool) (arg : string) : model =
  if arg = "" then { m with message = "No previous substitute" }
  else
    match String.split_on_char arg.[0] (String.sub arg 1 (String.length arg - 1)) with
    | old :: rep :: flags when old <> "" ->
        let global = List.mem "g" flags in
        let count = ref 0 in
        let subst (s : string) : string =
          let b = Buffer.create (String.length s) in
          let rec go i done_one =
            if i > String.length s - String.length old then Buffer.add_string b (String.sub s i (String.length s - i))
            else if String.sub s i (String.length old) = old && (global || not done_one) then begin
              incr count;
              Buffer.add_string b rep;
              go (i + String.length old) true
            end
            else begin
              Buffer.add_char b s.[i];
              go (i + 1) done_one
            end
          in
          go 0 false;
          Buffer.contents b
        in
        let lines = List.mapi (fun i l -> if all_lines || i = m.row then subst l else l) (Array.to_list m.lines) in
        if !count = 0 then { m with message = "Substitute pattern match failed" }
        else { (set_lines { m with undo = Some (snapshot m) } lines) with message = (if !count > 1 then Printf.sprintf "%d substitutions" !count else "") }
    | _ -> { m with message = "Substitute needs /old/new/" }

and write (m : model) (file : string) : model =
  let text = String.concat "\n" (Array.to_list m.lines) ^ "\n" in
  { m with disk = (file, text) :: List.remove_assoc file m.disk; modified = false; file;
    message = Printf.sprintf "\"%s\" %d lines, %d characters" file (nlines m) (String.length text) }

and edit (m : model) (file : string) : model =
  let text, note = match List.assoc_opt file m.disk with Some t -> (t, "") | None -> ("", " [New file]") in
  let text = if String.length text > 0 && text.[String.length text - 1] = '\n' then String.sub text 0 (String.length text - 1) else text in
  let lines = Array.of_list (String.split_on_char '\n' text) in
  { m with lines; file; row = 0; col = 0; want = 0; top = 0; modified = false; undo = None;
    message = Printf.sprintf "\"%s\"%s %d lines, %d characters" file note (Array.length lines) (String.length text) }

and ex (m : model) (command : string) : model =
  let command = String.trim command in
  let word, arg =
    match String.index_opt command ' ' with Some i -> (String.sub command 0 i, String.trim (String.sub command i (String.length command - i))) | None -> (command, "")
  in
  let dirty = "No write since last change (add ! to override)" in
  match word with
  | "" -> m
  | "w" -> write m (if arg = "" then m.file else arg)
  | "q" -> if m.modified then { m with message = dirty } else { m with quit = true }
  | "q!" -> { m with quit = true }
  | "wq" | "x" -> { (write m m.file) with quit = true }
  | "e" -> if arg = "" then { m with message = "No file name" } else if m.modified then { m with message = dirty } else edit m arg
  | "e!" -> edit m (if arg = "" then m.file else arg)
  | "d" -> { (delete_lines { m with undo = Some (snapshot m) } m.row m.row) with register = Lines [ line m m.row ] }
  | "$" -> { m with row = nlines m - 1; col = first_nonblank (line m (nlines m - 1)) }
  | "set" -> (
      match arg with
      | "number" | "nu" -> { m with number = true }
      | "nonumber" | "nonu" -> { m with number = false }
      | _ -> { m with message = "Unknown option: " ^ arg })
  | _ when String.length word > 1 && word.[0] = 's' -> substitute m false (String.sub command 1 (String.length command - 1))
  | _ when String.length word > 2 && String.sub word 0 2 = "%s" -> substitute m true (String.sub command 2 (String.length command - 2))
  | _ -> (
      match int_of_string_opt word with
      | Some k -> let row = max 0 (min (nlines m - 1) (k - 1)) in { m with row; col = first_nonblank (line m row) }
      | None -> { m with message = "Not an editor command: " ^ command })

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let insert_key (m : model) (k : string) : model =
  let s = line m m.row in
  let set s m = { (set_lines m (List.mapi (fun i l -> if i = m.row then s else l) (Array.to_list m.lines))) with row = m.row } in
  match k with
  | "\x1b" -> { m with mode = Normal; col = max 0 (m.col - 1) }
  | "\r" ->
      let head = String.sub s 0 m.col and tail = String.sub s m.col (String.length s - m.col) in
      let before = List.init m.row (line m) and after = List.init (nlines m - m.row - 1) (fun i -> line m (m.row + 1 + i)) in
      { (set_lines m (before @ [ head; tail ] @ after)) with row = m.row + 1; col = 0 }
  | "\x7f" | "\b" ->
      (* within the line, as the original vi: the text before the
         insertion can't be backed over *)
      if m.col = 0 then m else { (set (String.sub s 0 (m.col - 1) ^ String.sub s m.col (String.length s - m.col)) m) with col = m.col - 1 }
  | "\x1b[D" -> { m with col = max 0 (m.col - 1) }
  | "\x1b[C" -> { m with col = min (String.length s) (m.col + 1) }
  | "\x1b[A" -> clamp { m with row = m.row - 1 }
  | "\x1b[B" -> clamp { m with row = m.row + 1 }
  | "\t" -> { (set (String.sub s 0 m.col ^ "\t" ^ String.sub s m.col (String.length s - m.col)) m) with col = m.col + 1 }
  | _ when String.length k = 1 && k.[0] >= ' ' && k.[0] < '\x7f' ->
      { (set (String.sub s 0 m.col ^ k ^ String.sub s m.col (String.length s - m.col)) m) with col = m.col + 1 }
  | _ -> m

let line_key (m : model) (text : string) (k : string) (make : string -> mode) (enter : string -> model) : model =
  match k with
  | "\x1b" | "\x07" -> { m with mode = Normal }
  | "\r" -> enter text
  | "\x7f" | "\b" -> if text = "" then { m with mode = Normal } else { m with mode = make (String.sub text 0 (String.length text - 1)) }
  | _ when String.length k = 1 && k.[0] >= ' ' -> { m with mode = make (text ^ k) }
  | _ -> m

(* the screen follows the cursor, a line at a time *)
let follow (m : model) : model =
  let m = clamp m in
  if m.row < m.top then { m with top = m.row } else if m.row >= m.top + height then { m with top = m.row - height + 1 } else m

let rec key (m : model) (k : string) : model =
  match m.mode with
  | Insert ->
      let m = { (insert_key m k) with recording = Option.map (fun r -> r @ [ k ]) m.recording } in
      (* back in normal mode: the change complete, for . *)
      if m.mode = Normal then { m with last_change = Option.value m.recording ~default:m.last_change; recording = None; want = m.col } else m
  | Command text -> line_key m text k (fun t -> Command t) (fun t -> ex { m with mode = Normal } t)
  | Search (forward, text) ->
      line_key m text k
        (fun t -> Search (forward, t))
        (fun t ->
          let pat = if t = "" then Option.fold ~none:"" ~some:snd m.search else t in
          let m = { m with mode = Normal; search = Some (forward, pat) } in
          match find_text m forward pat with Some (r, c) -> { m with row = r; col = c } | None -> { m with message = "Pattern not found: " ^ pat })
  | Normal -> (
      if k = "\x1b" then { m with pending = [] }
      else
        let keys = m.pending @ [ k ] in
        match parse_command keys with
        | Incomplete -> { m with pending = keys }
        | Invalid -> { m with pending = [] }
        | Complete (_, [ "." ]) ->
            (* the last change's keys, typed again *)
            List.fold_left key { m with pending = [] } m.last_change
        | Complete (count, cmd) ->
            let m = normal { m with pending = []; message = "" } count cmd in
            (* the column j and k aim for: where anything else left the
               cursor, and $ the end of every line *)
            let m =
              match cmd with
              | [ ("j" | "k" | "\x1b[A" | "\x1b[B") ] -> m
              | [ "$" ] -> { m with want = max_int }
              | _ -> { (clamp m) with want = (clamp m).col }
            in
            if not (is_change cmd) then m
            else if m.mode = Insert then { m with recording = Some keys }
            else { m with last_change = keys })

let update (ev : Tui.event) (m : model) : model =
  match ev with
  | Tick _ -> m
  | Key k ->
      (* Escape and a key typed within the same instant arrive as one
         (Alt and the key, to a terminal): vi takes them as two *)
      let keys = if String.length k = 2 && k.[0] = '\x1b' then [ "\x1b"; String.make 1 k.[1] ] else [ k ] in
      follow (List.fold_left key m keys)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let shown (s : string) : string =
  let s = String.concat "        " (String.split_on_char '\t' s) in
  String.map (fun c -> if c < ' ' then '?' else c) s

let view (m : model) : Curses.t =
  let gutter = if m.number then 8 else 0 in
  let screen = ref (Curses.create ~rows:24 ~cols:80) in
  for i = 0 to height - 1 do
    let r = m.top + i in
    if r < nlines m then begin
      if m.number then screen := Curses.put ~attrs:{ Vt.plain with fg = Vt.Yellow } i 0 (Printf.sprintf "%6d " (r + 1)) !screen;
      screen := Curses.put i gutter (shown (line m r)) !screen
    end
    else screen := Curses.put ~attrs:{ Vt.plain with fg = Vt.Blue; bold = true } i 0 "~" !screen
  done;
  let status, cursor =
    match m.mode with
    | Command t -> (":" ^ t, Some (height, 1 + String.length t))
    | Search (forward, t) -> ((if forward then "/" else "?") ^ t, Some (height, 1 + String.length t))
    | Insert -> ("-- INSERT --", None)
    | Normal -> (m.message, None)
  in
  let screen = Curses.put height 0 status !screen in
  let screen = Curses.put height 62 (Printf.sprintf "%d,%d" (m.row + 1) (m.col + 1)) screen in
  (* a tab is 8 columns on the screen: the cursor's column counts them *)
  let s = line m m.row in
  let col = String.length (shown (String.sub s 0 (min m.col (String.length s)))) in
  let cursor = match cursor with Some c -> c | None -> (m.row - m.top, min 79 (gutter + col)) in
  Curses.cursor (Some cursor) screen

(*****************************************************************************)
(* The start *)
(*****************************************************************************)

let readme =
  {|TinyVi. You are in normal mode: keys are commands.

  h j k l      left, down, up, right (the ADM-3A's arrows)
  w b e        a word forward, back, to its end
  0 ^ $        the line's start, first word, end
  gg G         the first line, the last; 5G the fifth
  i a o        insert before, after, on a new line; Escape to stop
  x dd         delete a character, a line
  dw d$ d2j    delete a word, to the end, three lines: an operator
               and a motion, each with a count if you like
  cw           change a word: delete it, then insert
  yy p P       yank a line, put it after, before
  u .          undo (u again: redo), repeat the last change
  /word n N    search forward, the next one, the previous one

After a colon, ex's commands, then Enter:

  :w           write the file       :q   quit (:q! without writing)
  :e poem.txt  edit another file    :12  go to line 12
  :%s/old/new/g                     substitute, on every line
  :set number                       line numbers

Try it on this file: 3dd, then u; cw then Escape then w then .
|}

let poem =
  {|Whose woods these are I think I know.
His house is in the village though;
He will not see me stopping here
To watch his woods fill up with snow.
|}

let disk = [ ("README", readme); ("poem.txt", poem) ]

let init : model =
  edit
    { lines = [| "" |]; row = 0; col = 0; want = 0; top = 0; mode = Normal; file = ""; disk; modified = false; undo = None; register = Chars "";
      pending = []; last_change = []; recording = None; search = None; number = false; message = ""; quit = false }
    "README"

let program : model Tui.program = { init; update; view; over = (fun m -> m.quit) }
let lines (m : model) = Array.to_list m.lines
let cursor (m : model) = (m.row, m.col)
let message (m : model) = m.message
let mode (m : model) = match m.mode with Normal -> "normal" | Insert -> "insert" | Command _ | Search _ -> "command"
let file (m : model) (name : string) = List.assoc_opt name m.disk
