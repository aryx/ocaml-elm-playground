(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_lexer.mli *)

type kind =
  | Name of string
  | Keyword of string
  | Binary of string
  | Int of int
  | Large of bool * int list
  | Float of float
  | Char of char
  | String of string
  | Symbol of string
  | Array_start
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Caret
  | Assign
  | Period
  | Semicolon
  | Bar
  | Colon
  | Eof

type token = { kind : kind; start : int; stop : int }

exception Error of int * string

let min_small = -0x40000000
let max_small = 0x3FFFFFFF

(*****************************************************************************)
(* Numbers of any size *)
(*****************************************************************************)

(* a magnitude as its bytes, least significant first: [n * r + d], with
 * no int ever bigger than a few thousand, so that it works the same
 * with js_of_ocaml's 32-bit integers *)
let mul_add (bytes : int list) (r : int) (d : int) : int list =
  let rec go bytes carry =
    match bytes with
    | [] -> if carry = 0 then [] else (carry land 255) :: go [] (carry lsr 8)
    | b :: rest ->
        let v = (b * r) + carry in
        (v land 255) :: go rest (v lsr 8)
  in
  go bytes d

let small_of_bytes (neg : bool) (bytes : int list) : int option =
  let rec strip = function 0 :: rest -> strip rest | l -> l in
  let msb_first = strip (List.rev bytes) in
  if List.length msb_first > 4 then None
  else
    (* claude: 4 bytes can be 2^32 - 1, past a 32-bit int: compare
     * before building, byte by byte, against 2^30 *)
    let n = List.length msb_first in
    let top = match msb_first with [] -> 0 | b :: _ -> b in
    if n = 4 && top >= 0x40 && not (neg && top = 0x40 && List.for_all (( = ) 0) (List.tl msb_first)) then None
    else
      let v = List.fold_left (fun acc b -> (acc lsl 8) lor b) 0 msb_first in
      Some (if neg then -v else v)

(*****************************************************************************)
(* The characters *)
(*****************************************************************************)

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_binary c = String.contains "+-*/\\<>=~@%&?," c

let digit_value (c : char) : int =
  if is_digit c then Char.code c - Char.code '0'
  else if c >= 'A' && c <= 'Z' then Char.code c - Char.code 'A' + 10
  else 99

(*****************************************************************************)
(* The tokens *)
(*****************************************************************************)

let tokenize (s : string) : token array =
  let n = String.length s in
  let toks = ref [] in
  let at i = if i < n then s.[i] else '\000' in
  let emit kind start stop = toks := { kind; start; stop } :: !toks in
  (* an operand came last: then a minus is subtraction *)
  let after_operand () =
    match !toks with
    | { kind = Name _ | Int _ | Large _ | Float _ | Char _ | String _ | Symbol _ | Rparen | Rbracket; _ } :: _ -> true
    | _ -> false
  in
  (* a number from [i], its sign already read *)
  let number start neg i =
    let digits i radix =
      let rec go i bytes =
        if i < n && digit_value s.[i] < radix then go (i + 1) (mul_add bytes radix (digit_value s.[i])) else (i, bytes)
      in
      go i []
    in
    let i, bytes = digits i 10 in
    let i, bytes, radix =
      if at i = 'r' && digit_value (at (i + 1)) < 36 then
        let radix = match small_of_bytes false bytes with Some r when r >= 2 && r <= 36 -> r | _ -> raise (Error (i, "bad radix")) in
        let i, bytes = digits (i + 1) radix in
        (i, bytes, radix)
      else (i, bytes, 10)
    in
    let integer i bytes =
      match small_of_bytes neg bytes with Some v -> emit (Int v) start i | None -> emit (Large (neg, bytes)) start i
    in
    if radix = 10 && at i = '.' && is_digit (at (i + 1)) then begin
      (* a float: its text read by OCaml *)
      let j = ref (i + 1) in
      while is_digit (at !j) do incr j done;
      if at !j = 'e' && (is_digit (at (!j + 1)) || (at (!j + 1) = '-' && is_digit (at (!j + 2)))) then begin
        j := !j + 2;
        while is_digit (at !j) do incr j done
      end;
      let text = String.sub s start (!j - start) in
      emit (Float (float_of_string text)) start !j
    end
    else if at i = 'e' && is_digit (at (i + 1)) then begin
      (* 1e10: an integer times a power of ten *)
      let j = ref (i + 1) and e = ref 0 in
      while is_digit (at !j) do
        e := (!e * 10) + digit_value s.[!j];
        incr j
      done;
      let bytes = ref bytes in
      for _ = 1 to !e do
        bytes := mul_add !bytes radix 0
      done;
      integer !j !bytes
    end
    else integer i bytes
  in
  let rec skip i =
    if i >= n then i
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' | '\012' -> skip (i + 1)
      | '"' -> (
          match String.index_from_opt s (i + 1) '"' with
          | Some j -> skip (j + 1)
          | None -> raise (Error (i, "unmatched comment quote")))
      | _ -> i
  in
  let rec go i =
    let i = skip i in
    if i >= n then emit Eof n n
    else
      let c = s.[i] in
      if is_letter c then begin
        let j = ref i in
        while is_letter (at !j) || is_digit (at !j) do incr j done;
        let name = String.sub s i (!j - i) in
        if at !j = ':' && at (!j + 1) <> '=' then begin
          emit (Keyword (name ^ ":")) i (!j + 1);
          go (!j + 1)
        end
        else begin
          emit (Name name) i !j;
          go !j
        end
      end
      else if is_digit c then begin
        number i false i;
        go (List.hd !toks).stop
      end
      else if c = '-' && is_digit (at (i + 1)) && not (after_operand ()) then begin
        number i true (i + 1);
        go (List.hd !toks).stop
      end
      else if c = '$' then begin
        if i + 1 >= n then raise (Error (i, "a character expected after $"));
        emit (Char s.[i + 1]) i (i + 2);
        go (i + 2)
      end
      else if c = '\'' then begin
        let b = Buffer.create 16 in
        let rec str j =
          if j >= n then raise (Error (i, "unmatched string quote"))
          else if s.[j] = '\'' then
            if at (j + 1) = '\'' then begin
              Buffer.add_char b '\'';
              str (j + 2)
            end
            else j + 1
          else begin
            Buffer.add_char b s.[j];
            str (j + 1)
          end
        in
        let j = str (i + 1) in
        emit (String (Buffer.contents b)) i j;
        go j
      end
      else if c = '#' then begin
        let d = at (i + 1) in
        if d = '(' then begin
          emit Array_start i (i + 2);
          go (i + 2)
        end
        else if is_letter d then begin
          (* #foo, #at:put:, and #foo:bar (sic) as Smalltalk-80 read it *)
          let j = ref (i + 1) in
          while is_letter (at !j) || is_digit (at !j) || at !j = ':' do incr j done;
          emit (Symbol (String.sub s (i + 1) (!j - i - 1))) i !j;
          go !j
        end
        else if d = '\'' then
          (* #'two words': read as a string, made a symbol at the end *)
          go (i + 1)
        else if is_binary d || d = '|' then begin
          let j = ref (i + 1) in
          while is_binary (at !j) || at !j = '|' do incr j done;
          emit (Symbol (String.sub s (i + 1) (!j - i - 1))) i !j;
          go !j
        end
        else raise (Error (i, "a literal expected after #"))
      end
      else if c = ':' && at (i + 1) = '=' then begin
        emit Assign i (i + 2);
        go (i + 2)
      end
      else if c = '_' then begin
        emit Assign i (i + 1);
        go (i + 1)
      end
      else if c = '\xe2' && at (i + 1) = '\x86' && at (i + 2) = '\x90' then begin
        emit Assign i (i + 3);
        go (i + 3)
      end
      else if c = '^' || (c = '\xe2' && at (i + 1) = '\x86' && at (i + 2) = '\x91') then begin
        let len = if c = '^' then 1 else 3 in
        emit Caret i (i + len);
        go (i + len)
      end
      else if is_binary c then begin
        let j = ref (i + 1) in
        (* claude: "x--1" is x - -1; a minus never ends a run *)
        while is_binary (at !j) && not (at !j = '-' && is_digit (at (!j + 1))) do incr j done;
        emit (Binary (String.sub s i (!j - i))) i !j;
        go !j
      end
      else begin
        let k =
          match c with
          | '(' -> Lparen
          | ')' -> Rparen
          | '[' -> Lbracket
          | ']' -> Rbracket
          | '.' -> Period
          | ';' -> Semicolon
          | '|' -> Bar
          | ':' -> Colon
          | _ -> raise (Error (i, Printf.sprintf "unexpected character %C" c))
        in
        emit k i (i + 1);
        go (i + 1)
      end
  in
  go 0;
  (* the strings right after a # are symbols, spanning the # too *)
  Array.of_list (List.rev !toks)
  |> Array.map (fun t ->
         match t.kind with
         | String str when t.start > 0 && s.[t.start - 1] = '#' -> { t with kind = Symbol str; start = t.start - 1 }
         | _ -> t)

let to_string = function
  | Name s -> "Name " ^ s
  | Keyword s -> "Keyword " ^ s
  | Binary s -> "Binary " ^ s
  | Int i -> "Int " ^ string_of_int i
  | Large (neg, _) -> if neg then "Large -" else "Large"
  | Float f -> Printf.sprintf "Float %g" f
  | Char c -> Printf.sprintf "Char %c" c
  | String s -> Printf.sprintf "String %S" s
  | Symbol s -> "Symbol " ^ s
  | Array_start -> "Array_start"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Lbracket -> "Lbracket"
  | Rbracket -> "Rbracket"
  | Caret -> "Caret"
  | Assign -> "Assign"
  | Period -> "Period"
  | Semicolon -> "Semicolon"
  | Bar -> "Bar"
  | Colon -> "Colon"
  | Eof -> "Eof"
