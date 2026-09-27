(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

(*****************************************************************************)
(* Words *)
(*****************************************************************************)

(* A word (lowercased: the language is not case-sensitive), a number, a
   quoted string (as written), an operator, or the end of a line --
   which is a separator, since a statement is a line *)
type token = Word of string | Num of string | Str of string | Op of string | Nl

let tokens text =
  let n = String.length text in
  let rec go i acc =
    if i >= n then List.rev (Nl :: acc)
    else
      let c = text.[i] in
      let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
      let is_digit c = c >= '0' && c <= '9' in
      let span p j = if j < n && p text.[j] then true else false in
      let rec stop p j = if span p j then stop p (j + 1) else j in
      if c = '\n' then go (i + 1) (Nl :: acc)
      else if c = ' ' || c = '\t' || c = '\r' then go (i + 1) acc
      else if c = '-' && i + 1 < n && text.[i + 1] = '-' then
        (* a comment, to the end of the line *)
        go (stop (fun c -> c <> '\n') i) acc
      else if is_alpha c then
        let j = stop (fun c -> is_alpha c || is_digit c) i in
        go j (Word (String.lowercase_ascii (String.sub text i (j - i))) :: acc)
      else if is_digit c then
        let j = stop (fun c -> is_digit c || c = '.') i in
        go j (Num (String.sub text i (j - i)) :: acc)
      else if c = '"' then
        let j = stop (fun c -> c <> '"' && c <> '\n') (i + 1) in
        go (j + 1) (Str (String.sub text (i + 1) (j - i - 1)) :: acc)
      else
        let two = if i + 1 < n then String.sub text i 2 else "" in
        if List.mem two [ "&&"; "<>"; "<="; ">=" ] then go (i + 2) (Op two :: acc)
        else if String.contains "&+-*/=<>()," c then go (i + 1) (Op (String.make 1 c) :: acc)
        else error "a character I do not know: %C" c
  in
  go 0 []

(*****************************************************************************)
(* The tree *)
(*****************************************************************************)

type card_ref = Next | Prev | First | Last | Named of string | Numbered of int

type expr =
  | Lit of string
  | Var of string
  | Field of expr
  | Number_of_cards
  | Number_of_this_card
  | Name_of_this_card
  | Binop of string * expr * expr
  | Not of expr
  | Neg of expr

type dest = Into_field of expr | Into_var of string
type place = Into | After | Before
type go = Go_to of card_ref | Go_card of expr

type stmt =
  | Put of expr * place * dest
  | Add of expr * dest
  | Subtract of expr * dest
  | Go of go
  | Answer of expr
  | Beep
  | If of expr * stmt list * stmt list
  | Repeat of expr * stmt list
  | Repeat_with of string * expr * expr * stmt list
  | Pass of string
  | Message of string

type handler = { name : string; body : stmt list }
type script = handler list

let handlers script = List.map (fun h -> h.name) script

(*****************************************************************************)
(* Parsing: recursive descent over the tokens, a line at a time *)
(*****************************************************************************)

let parse text =
  let toks = Array.of_list (tokens text) in
  let pos = ref 0 in
  (* which line we are on, for the error messages *)
  let line () =
    let l = ref 1 in
    for i = 0 to min !pos (Array.length toks - 1) - 1 do
      if toks.(i) = Nl then incr l
    done;
    !l
  in
  let peek () = if !pos < Array.length toks then toks.(!pos) else Nl in
  let peek2 () = if !pos + 1 < Array.length toks then toks.(!pos + 1) else Nl in
  let advance () = incr pos in
  let fail what = error "line %d: %s" (line ()) what in
  let word w = peek () = Word w in
  let eat_word w = if word w then advance () else fail (Printf.sprintf "expected %S" w) in
  let skip_word w = if word w then advance () in
  let name () = match peek () with Word w -> advance (); w | _ -> fail "expected a name" in
  let at_end () = !pos >= Array.length toks in
  let rec skip_lines () = if (not (at_end ())) && peek () = Nl then (advance (); skip_lines ()) in
  let end_of_line () = match peek () with Nl -> advance () | _ -> fail "expected the end of the line" in
  (* expressions, loosest first *)
  let rec expr () = binary [ [ "or" ]; [ "and" ] ] comparison
  and binary levels next =
    match levels with
    | [] -> next ()
    | ops :: rest ->
        let sub () = binary rest next in
        let rec go l = match peek () with Word w when List.mem w ops -> advance (); go (Binop (w, l, sub ())) | _ -> l in
        go (sub ())
  and comparison () =
    let l = concat () in
    match peek () with
    | Op (("=" | "<>" | "<" | ">" | "<=" | ">=") as op) ->
        advance ();
        Binop (op, l, concat ())
    | Word "is" ->
        advance ();
        if word "not" then (advance (); Binop ("<>", l, concat ())) else Binop ("=", l, concat ())
    | Word "contains" ->
        advance ();
        Binop ("contains", l, concat ())
    | _ -> l
  and ops_level ops next () =
    let rec go l = match peek () with Op o when List.mem o ops -> advance (); go (Binop (o, l, next ())) | Word w when List.mem w ops -> advance (); go (Binop (w, l, next ())) | _ -> l in
    go (next ())
  and concat () = ops_level [ "&"; "&&" ] additive ()
  and additive () = ops_level [ "+"; "-" ] multiplicative ()
  and multiplicative () = ops_level [ "*"; "/"; "mod" ] unary ()
  and unary () =
    match peek () with
    | Op "-" -> advance (); Neg (unary ())
    | Word "not" -> advance (); Not (unary ())
    | _ -> primary ()
  and primary () =
    match peek () with
    | Num n -> advance (); Lit n
    | Str s -> advance (); Lit s
    | Op "(" ->
        advance ();
        let e = expr () in
        if peek () = Op ")" then (advance (); e) else fail "expected )"
    | Word ("card" | "bg" | "background") when peek2 () = Word "field" -> advance (); advance (); Field (primary ())
    | Word "field" -> advance (); Field (primary ())
    | Word "the" ->
        advance ();
        if word "number" then (
          advance ();
          eat_word "of";
          if word "cards" then (advance (); Number_of_cards)
          else (eat_word "this"; eat_word "card"; Number_of_this_card))
        else if word "name" then (advance (); eat_word "of"; eat_word "this"; eat_word "card"; Name_of_this_card)
        else fail "the what?"
    | Word "return" -> advance (); Lit "\n"
    | Word "empty" -> advance (); Lit ""
    | Word "space" -> advance (); Lit " "
    | Word "quote" -> advance (); Lit "\""
    | Word ("true" | "false" as b) -> advance (); Lit b
    | Word w -> advance (); Var w
    | _ -> fail "expected a value"
  in
  let dest () =
    match peek () with
    | Word ("card" | "bg" | "background") when peek2 () = Word "field" -> advance (); advance (); Into_field (primary ())
    | Word "field" -> advance (); Into_field (primary ())
    | Word w -> advance (); Into_var w
    | _ -> fail "expected a field or a variable"
  in
  (* statements; [block] reads lines up to one of [stops] *)
  let rec stmt () =
    match peek () with
    | Word "put" ->
        advance ();
        let e = expr () in
        let place = match peek () with Word "into" -> Into | Word "after" -> After | Word "before" -> Before | _ -> fail "put ... into what?" in
        advance ();
        Put (e, place, dest ())
    | Word "add" ->
        advance ();
        let e = expr () in
        eat_word "to";
        Add (e, dest ())
    | Word "subtract" ->
        advance ();
        let e = expr () in
        eat_word "from";
        Subtract (e, dest ())
    | Word "go" ->
        advance ();
        skip_word "to";
        let r = match peek () with Word "next" -> Some Next | Word ("prev" | "previous") -> Some Prev | Word "first" -> Some First | Word "last" -> Some Last | _ -> None in
        (match r with
        | Some r -> advance (); skip_word "card"; Go (Go_to r)
        | None -> eat_word "card"; Go (Go_card (expr ())))
    | Word "answer" -> advance (); Answer (expr ())
    | Word "beep" -> advance (); Beep
    | Word "pass" -> advance (); Pass (name ())
    | Word "if" ->
        advance ();
        let c = expr () in
        eat_word "then";
        if peek () = Nl then (
          (* the block form *)
          let yes = block [ "else"; "end" ] in
          let no =
            if word "else" then (
              advance ();
              if peek () = Nl then block [ "end" ] else [ stmt () ] @ (skip_lines (); if word "end" then [] else block [ "end" ]))
            else []
          in
          eat_word "end";
          eat_word "if";
          If (c, yes, no))
        else
          (* all on one line, perhaps with its else on the next *)
          let yes = stmt () in
          let save = !pos in
          skip_lines ();
          if word "else" then (advance (); If (c, [ yes ], [ stmt () ])) else (pos := save; If (c, [ yes ], []))
    | Word "repeat" ->
        advance ();
        if word "with" then (
          advance ();
          let v = name () in
          if peek () = Op "=" then advance () else fail "expected =";
          let a = expr () in
          eat_word "to";
          let b = expr () in
          let body = block [ "end" ] in
          eat_word "end";
          eat_word "repeat";
          Repeat_with (v, a, b, body))
        else
          let n = expr () in
          skip_word "times";
          let body = block [ "end" ] in
          eat_word "end";
          eat_word "repeat";
          Repeat (n, body)
    | Word w ->
        (* a message of our own; what follows on the line is ignored *)
        advance ();
        while (not (at_end ())) && peek () <> Nl do advance () done;
        Message w
    | _ -> fail "expected a command"
  and block stops =
    skip_lines ();
    let rec go acc =
      skip_lines ();
      match peek () with
      | _ when at_end () -> fail "missing end"
      | Word w when List.mem w stops -> List.rev acc
      | Nl -> go acc
      | _ ->
          let s = stmt () in
          end_of_line ();
          go (s :: acc)
    in
    go []
  in
  let rec handlers acc =
    skip_lines ();
    if at_end () then List.rev acc
    else (
      eat_word "on";
      let n = name () in
      let body = block [ "end" ] in
      eat_word "end";
      let closing = name () in
      if closing <> n then fail (Printf.sprintf "on %s ends with end %s" n closing);
      handlers ({ name = n; body } :: acc))
  in
  handlers []

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

type 'w world = {
  get_field : 'w -> string -> string;
  set_field : 'w -> string -> string -> 'w;
  go : 'w -> card_ref -> 'w;
  answer : 'w -> string -> 'w;
  beep : 'w -> 'w;
  number_of_cards : 'w -> int;
  card_number : 'w -> int;
  card_name : 'w -> string;
}

(* every value is a string; these are its readings *)
let number s =
  match float_of_string_opt (String.trim s) with
  | Some f -> f
  | None -> error "expected a number here, not %S" s

let show f = if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f

let truth s =
  match String.lowercase_ascii (String.trim s) with
  | "true" -> true
  | "false" -> false
  | _ -> error "expected true or false, not %S" s

let of_bool b = if b then "true" else "false"

(* numbers compared as numbers, anything else as text, ignoring case *)
let compare_values a b =
  match (float_of_string_opt (String.trim a), float_of_string_opt (String.trim b)) with
  | Some x, Some y -> compare x y
  | _ -> compare (String.lowercase_ascii a) (String.lowercase_ascii b)

let contains s sub =
  let s = String.lowercase_ascii s and sub = String.lowercase_ascii sub in
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

(* how a handler ended: normally, or passing its message on *)
type flow = Done | Passed

let rec send world path msg w = send_depth 0 world path msg w

and send_depth depth world path msg w =
  if depth > 64 then error "too many messages inside messages (a handler calling itself?)";
  match path with
  | [] -> w
  | script :: rest -> (
      match List.find_opt (fun h -> h.name = String.lowercase_ascii msg) script with
      | None -> send_depth depth world rest msg w
      | Some h -> (
          let vars = Hashtbl.create 8 in
          (* a message sent by a statement starts from this object *)
          let here = path in
          match run depth world here vars h.body w with
          | w, Done -> w
          | w, Passed -> send_depth depth world rest msg w))

and run depth world here vars stmts w =
  match stmts with
  | [] -> (w, Done)
  | s :: rest -> (
      match exec depth world here vars s w with
      | w, Done -> run depth world here vars rest w
      | w, Passed -> (w, Passed))

and exec depth world here vars s w =
  let ev e = eval world vars e w in
  let get d = match d with Into_field f -> world.get_field w (ev f) | Into_var v -> Option.value (Hashtbl.find_opt vars v) ~default:"" in
  let set d v w = match d with Into_field f -> world.set_field w (ev f) v | Into_var x -> Hashtbl.replace vars x v; w in
  match s with
  | Put (e, place, d) ->
      let v = ev e in
      let v = match place with Into -> v | After -> get d ^ v | Before -> v ^ get d in
      (set d v w, Done)
  | Add (e, d) -> (set d (show (number (get d) +. number (ev e))) w, Done)
  | Subtract (e, d) -> (set d (show (number (get d) -. number (ev e))) w, Done)
  | Go (Go_to r) -> (world.go w r, Done)
  | Go (Go_card e) ->
      let v = ev e in
      (world.go w (match int_of_string_opt (String.trim v) with Some n -> Numbered n | None -> Named v), Done)
  | Answer e -> (world.answer w (ev e), Done)
  | Beep -> (world.beep w, Done)
  | If (c, yes, no) -> run depth world here vars (if truth (ev c) then yes else no) w
  | Repeat (n, body) ->
      let n = int_of_float (number (ev n)) in
      let rec loop i w = if i >= n then (w, Done) else match run depth world here vars body w with w, Done -> loop (i + 1) w | r -> r in
      loop 0 w
  | Repeat_with (v, a, b, body) ->
      let a = int_of_float (number (ev a)) and b = int_of_float (number (ev b)) in
      let rec loop i w =
        if i > b then (w, Done)
        else (
          Hashtbl.replace vars v (string_of_int i);
          match run depth world here vars body w with w, Done -> loop (i + 1) w | r -> r)
      in
      loop a w
  | Pass _ -> (w, Passed)
  | Message m -> (send_depth (depth + 1) world here m w, Done)

and eval world vars e w =
  let ev e = eval world vars e w in
  match e with
  | Lit s -> s
  | Var v -> ( match Hashtbl.find_opt vars v with Some s -> s | None -> v)
  | Field f -> world.get_field w (ev f)
  | Number_of_cards -> string_of_int (world.number_of_cards w)
  | Number_of_this_card -> string_of_int (world.card_number w)
  | Name_of_this_card -> world.card_name w
  | Not e -> of_bool (not (truth (ev e)))
  | Neg e -> show (-.number (ev e))
  | Binop (op, a, b) -> (
      let a = ev a and b = ev b in
      let arith f = show (f (number a) (number b)) in
      match op with
      | "&" -> a ^ b
      | "&&" -> a ^ " " ^ b
      | "+" -> arith ( +. )
      | "-" -> arith ( -. )
      | "*" -> arith ( *. )
      | "/" -> if number b = 0. then error "division by zero" else arith ( /. )
      | "mod" -> arith Float.rem
      | "=" -> of_bool (compare_values a b = 0)
      | "<>" -> of_bool (compare_values a b <> 0)
      | "<" -> of_bool (compare_values a b < 0)
      | ">" -> of_bool (compare_values a b > 0)
      | "<=" -> of_bool (compare_values a b <= 0)
      | ">=" -> of_bool (compare_values a b >= 0)
      | "contains" -> of_bool (contains a b)
      | "and" -> of_bool (truth a && truth b)
      | "or" -> of_bool (truth a || truth b)
      | op -> error "an operator I do not know: %s" op)
