(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_regexp.mli *)

type item = Range of char * char | Class of char (* d w s D W S *)

type node =
  | Char of char
  | Any
  | Set of bool * item list (* negated, its items *)
  | Start
  | End
  | Boundary of bool (* \b, or \B *)
  | Group of int option * node list list (* captured as n, or not; its alternatives *)
  | Repeat of node * int * int option * bool (* at least, at most, greedy *)

type t = { source : string; flags : string; alts : node list list; count : int; ignore_case : bool; multiline : bool }

exception Bad of string

(*****************************************************************************)
(* Reading a pattern *)
(*****************************************************************************)

let parse (p : string) : node list list * int =
  let n = String.length p in
  let pos = ref 0 and count = ref 0 in
  let peek () = if !pos < n then Some p.[!pos] else None in
  let next () = let c = p.[!pos] in incr pos; c in
  let number () =
    let start = !pos in
    while !pos < n && p.[!pos] >= '0' && p.[!pos] <= '9' do incr pos done;
    if !pos = start then None else Some (int_of_string (String.sub p start (!pos - start)))
  in
  (* \x: a class, or the character it stands for *)
  let escape () : [ `Class of char | `Char of char | `Chars of string | `Boundary of bool ] =
    if !pos >= n then raise (Bad "\\ at the end of the pattern");
    match next () with
    | ('d' | 'w' | 's' | 'D' | 'W' | 'S') as c -> `Class c
    | 'b' -> `Boundary true
    | 'B' -> `Boundary false
    | 'n' -> `Char '\n'
    | 't' -> `Char '\t'
    | 'r' -> `Char '\r'
    | 'f' -> `Char '\012'
    | 'v' -> `Char '\011'
    | '0' -> `Char '\000'
    | 'u' when !pos + 4 <= n -> (
        match int_of_string_opt ("0x" ^ String.sub p !pos 4) with
        | Some cp ->
            pos := !pos + 4;
            let b = Buffer.create 4 in
            Buffer.add_utf_8_uchar b (Uchar.of_int cp);
            `Chars (Buffer.contents b)
        | None -> `Char 'u')
    | 'x' when !pos + 2 <= n -> (
        match int_of_string_opt ("0x" ^ String.sub p !pos 2) with Some c -> pos := !pos + 2; `Char (Char.chr c) | None -> `Char 'x')
    | c when c >= '1' && c <= '9' -> raise (Bad "backreferences are not supported")
    | c -> `Char c
  in
  let rec alternatives () : node list list =
    let first = sequence [] in
    if peek () = Some '|' then (incr pos; first :: alternatives ()) else [ first ]
  and sequence acc : node list =
    match peek () with
    | None | Some '|' | Some ')' -> List.rev acc
    | Some _ -> sequence (List.rev_append (quantified ()) acc)
  (* an atom and its quantifier; a \u escape may be several bytes *)
  and quantified () : node list =
    let atoms = atom () in
    let quantifier =
      match peek () with
      | Some '*' -> incr pos; Some (0, None)
      | Some '+' -> incr pos; Some (1, None)
      | Some '?' -> incr pos; Some (0, Some 1)
      | Some '{' -> (
          let save = !pos in
          incr pos;
          match number () with
          | Some lo -> (
              match next () with
              | '}' -> Some (lo, Some lo)
              | ',' -> (
                  let hi = number () in
                  match next () with '}' -> Some (lo, hi) | _ -> pos := save + 1; None)
              | _ -> pos := save + 1; None)
          | None -> pos := save; None)
      | _ -> None
    in
    match quantifier with
    | None -> atoms
    | Some (lo, hi) ->
        let greedy = if peek () = Some '?' then (incr pos; false) else true in
        let last, before = match List.rev atoms with l :: b -> (l, List.rev b) | [] -> raise (Bad "nothing to repeat") in
        before @ [ Repeat (last, lo, hi, greedy) ]
  and atom () : node list =
    match next () with
    | '.' -> [ Any ]
    | '^' -> [ Start ]
    | '$' -> [ End ]
    | '(' ->
        let captured =
          if !pos + 1 < n && p.[!pos] = '?' then
            if p.[!pos + 1] = ':' then (pos := !pos + 2; None) else raise (Bad "lookarounds and named groups are not supported")
          else (incr count; Some !count)
        in
        let alts = alternatives () in
        if peek () <> Some ')' then raise (Bad "a ( never closed");
        incr pos;
        [ Group (captured, alts) ]
    | '[' -> [ set () ]
    | ('*' | '+' | '?') as c -> raise (Bad (Printf.sprintf "nothing to repeat before %c" c))
    | '\\' -> (
        match escape () with
        | `Class c -> [ Set (false, [ Class c ]) ]
        | `Char c -> [ Char c ]
        | `Chars s -> List.init (String.length s) (fun i -> Char s.[i])
        | `Boundary b -> [ Boundary b ])
    | c -> [ Char c ]
  and set () : node =
    let negated = if peek () = Some '^' then (incr pos; true) else false in
    let rec items acc first =
      match peek () with
      | None -> raise (Bad "a [ never closed")
      | Some ']' when not first -> incr pos; List.rev acc
      | Some _ ->
          let lo =
            match next () with
            | '\\' -> ( match escape () with `Class c -> `Item (Class c) | `Char c -> `C c | `Chars s -> `C s.[0] | `Boundary _ -> `C '\b')
            | c -> `C c
          in
          (match lo with
          | `Item it -> items (it :: acc) false
          | `C lo ->
              if peek () = Some '-' && !pos + 1 < n && p.[!pos + 1] <> ']' then (
                incr pos;
                let hi = match next () with '\\' -> ( match escape () with `Char c -> c | _ -> '-') | c -> c in
                items (Range (lo, hi) :: acc) false)
              else items (Range (lo, lo) :: acc) false)
    in
    Set (negated, items [] true)
  in
  let alts = alternatives () in
  if !pos < n then raise (Bad "an unmatched )");
  (alts, !count)

let compile (source : string) (flags : string) : (t, string) result =
  match parse source with
  | alts, count ->
      Ok { source; flags; alts; count; ignore_case = String.contains flags 'i'; multiline = String.contains flags 'm' }
  | exception Bad why -> Error why

let source (re : t) = re.source
let flags (re : t) = re.flags
let global (re : t) = String.contains re.flags 'g'
let groups (re : t) = re.count

(*****************************************************************************)
(* Matching *)
(*****************************************************************************)

let is_word (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
let is_space (c : char) : bool = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012' || c = '\011'

let class_has (c : char) (x : char) : bool =
  match c with
  | 'd' -> x >= '0' && x <= '9'
  | 'D' -> not (x >= '0' && x <= '9')
  | 'w' -> is_word x
  | 'W' -> not (is_word x)
  | 's' -> is_space x
  | _ -> not (is_space x)

(* at most this many steps per exec: {|(a*)*b|} on a long string stops *)
let budget = 1_000_000

let exec (re : t) (s : string) (from : int) : (int * int) option array option =
  let len = String.length s in
  let caps = Array.make (re.count + 1) None in
  let steps = ref 0 in
  let lower = Char.lowercase_ascii in
  let same a b = if re.ignore_case then lower a = lower b else a = b in
  let in_set items x =
    List.exists
      (fun it ->
        match it with
        | Class c -> class_has c x
        | Range (lo, hi) -> (x >= lo && x <= hi) || (re.ignore_case && ((lower x >= lower lo && lower x <= lower hi) || (Char.uppercase_ascii x >= lo && Char.uppercase_ascii x <= hi))))
      items
  in
  let rec seq (nodes : node list) (i : int) (k : int -> bool) : bool =
    match nodes with [] -> k i | nd :: rest -> one nd i (fun j -> seq rest j k)
  and alts (l : node list list) (i : int) (k : int -> bool) : bool = List.exists (fun sq -> seq sq i k) l
  and one (nd : node) (i : int) (k : int -> bool) : bool =
    incr steps;
    if !steps > budget then false
    else
      match nd with
      | Char c -> i < len && same s.[i] c && k (i + 1)
      | Any -> i < len && s.[i] <> '\n' && k (i + 1)
      | Set (negated, items) -> i < len && in_set items s.[i] <> negated && k (i + 1)
      | Start -> (i = 0 || (re.multiline && s.[i - 1] = '\n')) && k i
      | End -> (i = len || (re.multiline && s.[i] = '\n')) && k i
      | Boundary b ->
          let before = i > 0 && is_word s.[i - 1] and after = i < len && is_word s.[i] in
          (before <> after) = b && k i
      | Group (None, l) -> alts l i k
      | Group (Some g, l) ->
          let old = caps.(g) in
          alts l i (fun j ->
              let saved = caps.(g) in
              caps.(g) <- Some (i, j);
              k j || (caps.(g) <- saved; false))
          || (caps.(g) <- old; false)
      | Repeat (x, lo, hi, greedy) ->
          (* one more of x, unless at the maximum; an empty one ends it
           * (x* on an empty x would never) *)
          let rec rep count i =
            let more () = match hi with Some h when count >= h -> false | _ -> one x i (fun j -> j <> i && rep (count + 1) j) in
            if count < lo then one x i (fun j -> rep (count + 1) j) else if greedy then more () || k i else k i || more ()
          in
          rep 0 i
  in
  let rec from_ start =
    if start > len then None
    else (
      Array.fill caps 0 (Array.length caps) None;
      let found = ref None in
      if alts re.alts start (fun j -> found := Some j; true) then (
        caps.(0) <- Some (start, Option.get !found);
        Some (Array.copy caps))
      else if !steps > budget then None
      else from_ (start + 1))
  in
  from_ (max 0 from)
