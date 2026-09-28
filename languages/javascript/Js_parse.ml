(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_parse.mli *)
open Js_ast

type error = { line : int; message : string }

exception Error of error

(* the tokens, and where the parser is in them *)
type t = { tokens : Js_lexer.token array; mutable pos : int }

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

let peek (p : t) : Js_lexer.token = p.tokens.(min p.pos (Array.length p.tokens - 1))
let peek_at (p : t) (k : int) : Js_lexer.token = p.tokens.(min (p.pos + k) (Array.length p.tokens - 1))
let advance (p : t) : Js_lexer.token = let t = peek p in p.pos <- p.pos + 1; t

let describe (k : Js_lexer.kind) : string =
  match k with
  | Keyword w | Name w | Punct w -> Printf.sprintf "'%s'" w
  | Number f -> Js_ast.number_to_string f
  | String s -> Printf.sprintf "%S" s
  | Regex (r, f) -> Printf.sprintf "/%s/%s" r f
  | Eof -> "the end"

let fail (p : t) (message : string) = raise (Error { line = (peek p).line; message })
let unexpected (p : t) (what : string) = fail p (Printf.sprintf "expected %s, not %s" what (describe (peek p).kind))
let is_punct (p : t) (s : string) : bool = (peek p).kind = Punct s
let is_keyword (p : t) (s : string) : bool = (peek p).kind = Keyword s

let expect (p : t) (s : string) : unit = if is_punct p s then ignore (advance p) else unexpected p (Printf.sprintf "'%s'" s)

let name (p : t) : string =
  match (advance p).kind with
  | Name x -> x
  | _ ->
      p.pos <- p.pos - 1;
      unexpected p "a name"

(*****************************************************************************)
(* Expressions: Pratt *)
(*****************************************************************************)

(* an infix operator's binding power, and whether it is right-associative *)
let infix (op : string) : (int * bool) option =
  match op with
  | "=" | "+=" | "-=" | "*=" | "/=" | "%=" -> Some (1, true)
  | "?" -> Some (2, true)
  | "||" -> Some (3, false)
  | "&&" -> Some (4, false)
  | "===" | "!==" | "==" | "!=" -> Some (5, false)
  | "<" | ">" | "<=" | ">=" -> Some (6, false)
  | "+" | "-" -> Some (7, false)
  | "*" | "/" | "%" -> Some (8, false)
  | _ -> None

let prefix_power = 9
let postfix_power = 10

(* what an assignment or ++ may change *)
let target (p : t) (e : expr) : expr =
  match e with Name _ | Member _ | Index _ -> e | _ -> fail p "that cannot be assigned to"

(* the index of the ")" matching the "(" at [p.pos], if any *)
let closing (p : t) : int option =
  let rec go i depth =
    match p.tokens.(i).kind with
    | Eof -> None
    | Punct ("(" | "[" | "{") -> go (i + 1) (depth + 1)
    | Punct (")" | "]" | "}") -> if depth = 1 then Some i else go (i + 1) (depth - 1)
    | _ -> go (i + 1) depth
  in
  go p.pos 0

(* an arrow ahead: "x =>", or "( ... ) =>" *)
let arrow_ahead (p : t) : bool =
  match ((peek p).kind, (peek_at p 1).kind) with
  | Name _, Punct "=>" -> true
  | Punct "(", _ -> (
      match closing p with Some i -> p.tokens.(i + 1).kind = Punct "=>" | None -> false)
  | _ -> false

let rec expression (p : t) (min : int) : expr =
  let left = prefix p in
  loop p min left

(* the operators binding at least as tight as [min], each taking the
 * left side so far *)
and loop (p : t) (min : int) (left : expr) : expr =
  let t = peek p in
  match t.kind with
  | Punct "." when postfix_power >= min ->
      ignore (advance p);
      (* a keyword is a property name too: o.default, e.catch *)
      let x = match (advance p).kind with Name x | Keyword x -> x | _ -> p.pos <- p.pos - 1; unexpected p "a property name" in
      loop p min (Member (left, x))
  | Punct "[" when postfix_power >= min ->
      ignore (advance p);
      let i = expression p 0 in
      expect p "]";
      loop p min (Index (left, i))
  | Punct "(" when postfix_power >= min ->
      ignore (advance p);
      let args = arguments p in
      loop p min (Call (left, args))
  (* x++, but not x on one line and ++y on the next: [no LineTerminator here] *)
  (* x instanceof F: as tight as < *)
  | Keyword "instanceof" when 6 >= min ->
      ignore (advance p);
      loop p min (Binary ("instanceof", left, expression p 7))
  | Punct (("++" | "--") as op) when postfix_power >= min && not t.newline_before ->
      ignore (advance p);
      loop p min (Update (op, false, target p left))
  | Punct op -> (
      match infix op with
      | Some (power, right) when power >= min ->
          ignore (advance p);
          let next = if right then power else power + 1 in
          let e =
            match op with
            | "?" ->
                (* the middle as if in parentheses, then the rest *)
                let a = expression p 0 in
                expect p ":";
                Conditional (left, a, expression p next)
            | "=" | "+=" | "-=" | "*=" | "/=" | "%=" -> Assign (op, target p left, expression p next)
            | "&&" | "||" -> Logical (op, left, expression p next)
            | _ -> Binary (op, left, expression p next)
          in
          loop p min e
      | _ -> left)
  | _ -> left

(* what can start an expression *)
and prefix (p : t) : expr =
  if arrow_ahead p then arrow p
  else
    let t = advance p in
    match t.kind with
    | Number f -> Number f
    | String s -> String s
    | Name x -> Name x
    | Keyword "true" -> Bool true
    | Keyword "false" -> Bool false
    | Keyword "null" -> Null
    | Keyword "this" -> This
    | Keyword "typeof" -> Unary ("typeof", expression p prefix_power)
    | Keyword "function" -> Function (func p ~arrow:false)
    | Punct (("-" | "+" | "!") as op) -> Unary (op, expression p prefix_power)
    | Punct (("++" | "--") as op) -> Update (op, true, target p (expression p prefix_power))
    | Punct "(" ->
        let e = expression p 0 in
        expect p ")";
        e
    | Punct "[" ->
        let rec elements acc =
          if is_punct p "]" then List.rev acc
          else
            let e = expression p 1 in
            if is_punct p "," then (ignore (advance p); elements (e :: acc))
            else List.rev (e :: acc)
        in
        let es = elements [] in
        expect p "]";
        Array es
    | Punct "{" ->
        let rec members acc =
          if is_punct p "}" then List.rev acc
          else
            let key =
              match (advance p).kind with
              | Name k | Keyword k | String k -> k
              | Number f -> Js_ast.number_to_string f
              | _ -> p.pos <- p.pos - 1; unexpected p "a property name"
            in
            expect p ":";
            let v = expression p 1 in
            if is_punct p "," then (ignore (advance p); members ((key, v) :: acc))
            else List.rev ((key, v) :: acc)
        in
        let kvs = members [] in
        expect p "}";
        Object kvs
    | Regex (r, f) -> Regex (r, f)
    (* new F(a), new F: F a name and its members, not a call *)
    | Keyword "new" ->
        let rec members e =
          match (peek p).kind with
          | Punct "." -> (
              ignore (advance p);
              match (advance p).kind with Name x | Keyword x -> members (Member (e, x)) | _ -> p.pos <- p.pos - 1; unexpected p "a property name")
          | Punct "[" ->
              ignore (advance p);
              let i = expression p 0 in
              expect p "]";
              members (Index (e, i))
          | _ -> e
        in
        let callee = members (prefix p) in
        let args = if is_punct p "(" then (ignore (advance p); arguments p) else [] in
        New (callee, args)
    | Keyword ("class" as k) ->
        p.pos <- p.pos - 1;
        fail p (Printf.sprintf "%s is not supported here (an exercise: prototypes and new are)" k)
    | _ ->
        p.pos <- p.pos - 1;
        unexpected p "an expression"

(* f(a, b): what is after the "(" *)
and arguments (p : t) : expr list =
  let rec go acc =
    if is_punct p ")" then (ignore (advance p); List.rev acc)
    else
      let e = expression p 1 in
      if is_punct p "," then (ignore (advance p); go (e :: acc))
      else (expect p ")"; List.rev (e :: acc))
  in
  go []

(* "(a, b)": the parameters *)
and params (p : t) : string list =
  expect p "(";
  let rec go acc =
    if is_punct p ")" then (ignore (advance p); List.rev acc)
    else
      let x = name p in
      if is_punct p "," then (ignore (advance p); go (x :: acc))
      else (expect p ")"; List.rev (x :: acc))
  in
  go []

(* x => ..., (a, b) => ...: a body in braces, or an expression returned *)
and arrow (p : t) : expr =
  let ps = if is_punct p "(" then params p else [ name p ] in
  let line = (peek p).line in
  expect p "=>";
  let body =
    if is_punct p "{" then block_body p else [ { line; stmt = Return (Some (expression p 1)) } ]
  in
  Function { name = None; params = ps; body; arrow = true }

(* function name? (params) { body }: what is after the keyword *)
and func (p : t) ~(arrow : bool) : func =
  let name = match (peek p).kind with Name x -> ignore (advance p); Some x | _ -> None in
  let ps = params p in
  { name; params = ps; body = block_body p; arrow }

(*****************************************************************************)
(* Statements: recursive descent *)
(*****************************************************************************)

(* "{ statements }" *)
and block_body (p : t) : stmt list =
  expect p "{";
  let rec go acc = if is_punct p "}" then (ignore (advance p); List.rev acc) else go (statement p :: acc) in
  go []

(* a statement's end: ";", or before "}", the end, or a new line *)
and end_statement (p : t) : unit =
  let t = peek p in
  if is_punct p ";" then ignore (advance p)
  else if is_punct p "}" || t.kind = Eof || t.newline_before then ()
  else unexpected p "';' or a new line"

and let_kind (k : string) : let_kind = match k with "const" -> Const_kind | "var" -> Var_kind | _ -> Let_kind

(* let a = 1, b: after the keyword *)
and declarations (p : t) : (string * expr option) list =
  let rec go acc =
    let x = name p in
    let init = if is_punct p "=" then (ignore (advance p); Some (expression p 1)) else None in
    if is_punct p "," then (ignore (advance p); go ((x, init) :: acc)) else List.rev ((x, init) :: acc)
  in
  go []

and statement (p : t) : stmt =
  let t = peek p in
  let line = t.line in
  let s stmt = { line; stmt } in
  match t.kind with
  | Punct ";" -> ignore (advance p); s Empty
  | Punct "{" -> s (Block (block_body p))
  | Keyword (("let" | "const" | "var") as k) ->
      ignore (advance p);
      let ds = declarations p in
      end_statement p;
      s (Let (let_kind k, ds))
  | Keyword "function" ->
      ignore (advance p);
      let f = func p ~arrow:false in
      if f.name = None then fail p "a function declaration needs a name";
      s (Function_decl f)
  | Keyword "return" ->
      ignore (advance p);
      (* return, then a new line: returns nothing *)
      let next = peek p in
      let value = if is_punct p ";" || is_punct p "}" || next.kind = Eof || next.newline_before then None else Some (expression p 0) in
      end_statement p;
      s (Return value)
  | Keyword "if" ->
      ignore (advance p);
      expect p "(";
      let c = expression p 0 in
      expect p ")";
      let a = statement p in
      let b = if is_keyword p "else" then (ignore (advance p); Some (statement p)) else None in
      s (If (c, a, b))
  | Keyword "while" ->
      ignore (advance p);
      expect p "(";
      let c = expression p 0 in
      expect p ")";
      s (While (c, statement p))
  | Keyword "for" ->
      ignore (advance p);
      expect p "(";
      s (for_rest p)
  | Keyword "break" -> ignore (advance p); end_statement p; s Break
  | Keyword "continue" -> ignore (advance p); end_statement p; s Continue
  | Keyword "throw" ->
      ignore (advance p);
      let e = expression p 0 in
      end_statement p;
      s (Throw e)
  | Keyword "try" ->
      ignore (advance p);
      let body = block_body p in
      if not (is_keyword p "catch") then unexpected p "catch (finally: an exercise)";
      ignore (advance p);
      expect p "(";
      let x = name p in
      expect p ")";
      let handler = block_body p in
      if is_keyword p "finally" then fail p "finally is not supported here (an exercise)";
      s (Try (body, x, handler))
  | Keyword (("class" | "switch" | "do" | "delete" | "in" | "instanceof" | "void") as k) ->
      fail p (Printf.sprintf "'%s' is not supported here (plan_tiny_firefox.md: what is left out)" k)
  | _ ->
      let e = expression p 0 in
      end_statement p;
      s (Expr e)

(* for (let x of xs) body, or for (init; test; update) body: what is
 * after the "(" *)
and for_rest (p : t) : statement =
  match ((peek p).kind, (peek_at p 1).kind, (peek_at p 2).kind) with
  | Keyword (("let" | "const" | "var") as k), Name x, Name "of" ->
      p.pos <- p.pos + 3;
      let xs = expression p 0 in
      expect p ")";
      For_of (let_kind k, x, xs, statement p)
  | _ ->
      let line = (peek p).line in
      let init =
        match (peek p).kind with
        | Punct ";" -> None
        | Keyword (("let" | "const" | "var") as k) ->
            ignore (advance p);
            Some { line; stmt = Let (let_kind k, declarations p) }
        | _ -> Some { line; stmt = Expr (expression p 0) }
      in
      expect p ";";
      let test = if is_punct p ";" then None else Some (expression p 0) in
      expect p ";";
      let update = if is_punct p ")" then None else Some (expression p 0) in
      expect p ")";
      For (init, test, update, statement p)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let with_tokens (text : string) (f : t -> 'a) : ('a, error) result =
  match Js_lexer.tokenize text with
  | exception Js_lexer.Error (line, message) -> Error { line; message }
  | tokens -> (
      let p = { tokens = Array.of_list tokens; pos = 0 } in
      match f p with x -> Ok x | exception Error e -> Error e)

let parse (text : string) : (program, error) result =
  with_tokens text (fun p ->
      let rec go acc = if (peek p).kind = Eof then List.rev acc else go (statement p :: acc) in
      go [])

let parse_expression (text : string) : (expr, error) result =
  with_tokens text (fun p ->
      let e = expression p 0 in
      if (peek p).kind <> Eof then unexpected p "the end";
      e)
