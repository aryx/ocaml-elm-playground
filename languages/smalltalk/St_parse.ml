(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_parse.mli *)

open St_ast
module L = St_lexer

exception Error of int * string

(*****************************************************************************)
(* The tokens, one at a time *)
(*****************************************************************************)

type st = { toks : L.token array; mutable i : int }

let peek (p : st) : L.kind = p.toks.(p.i).kind
let peek2 (p : st) : L.kind = if p.i + 1 < Array.length p.toks then p.toks.(p.i + 1).kind else L.Eof
let tok (p : st) : L.token = p.toks.(p.i)
let advance (p : st) : unit = if p.i < Array.length p.toks - 1 then p.i <- p.i + 1
let fail (p : st) (msg : string) = raise (Error ((tok p).start, msg))
let last_stop (p : st) : int = if p.i = 0 then 0 else p.toks.(p.i - 1).stop

let expect (p : st) (k : L.kind) (msg : string) : unit = if peek p = k then advance p else fail p msg

let name (p : st) (msg : string) : string =
  match peek p with
  | L.Name n ->
      advance p;
      n
  | _ -> fail p msg

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

let rec literal_array (p : st) : literal =
  (* after "#(" or a nested "(" *)
  let items = ref [] in
  let rec go () =
    match peek p with
    | L.Rparen -> advance p
    | L.Eof -> fail p ") expected"
    | _ ->
        items := array_item p :: !items;
        go ()
  in
  go ();
  L_array (List.rev !items)

and array_item (p : st) : literal =
  let t = tok p in
  match t.kind with
  | L.Int i -> advance p; L_int i
  | L.Large (n, b) -> advance p; L_large (n, b)
  | L.Float f -> advance p; L_float f
  | L.Char c -> advance p; L_char c
  | L.String s -> advance p; L_string s
  | L.Symbol s -> advance p; L_symbol s
  | L.Name "nil" -> advance p; L_nil
  | L.Name "true" -> advance p; L_true
  | L.Name "false" -> advance p; L_false
  | L.Name n -> advance p; L_symbol n
  | L.Binary b -> advance p; L_symbol b
  | L.Bar -> advance p; L_symbol "|"
  | L.Keyword k ->
      (* at:put: is two keyword tokens, touching *)
      advance p;
      let rec more acc stop =
        match (tok p).kind with
        | L.Keyword k when (tok p).start = stop ->
            let stop = (tok p).stop in
            advance p;
            more (acc ^ k) stop
        | _ -> acc
      in
      L_symbol (more k t.stop)
  | L.Lparen | L.Array_start -> advance p; literal_array p
  | _ -> fail p "literal expected"

let literal_of_token (p : st) : literal option =
  match peek p with
  | L.Int i -> advance p; Some (L_int i)
  | L.Large (n, b) -> advance p; Some (L_large (n, b))
  | L.Float f -> advance p; Some (L_float f)
  | L.Char c -> advance p; Some (L_char c)
  | L.String s -> advance p; Some (L_string s)
  | L.Symbol s -> advance p; Some (L_symbol s)
  | L.Array_start -> advance p; Some (literal_array p)
  | _ -> None

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

let mk e start stop = { e; pos = (start, stop) }

let rec expression (p : st) : expr =
  match (peek p, peek2 p) with
  | L.Name v, L.Assign ->
      let start = (tok p).start in
      advance p;
      advance p;
      let x = expression p in
      mk (Assign (v, x)) start (snd x.pos)
  | _ -> cascade p

and cascade (p : st) : expr =
  let x = keyword_expr p in
  if peek p <> L.Semicolon then x
  else
    match x.e with
    | Send (r, sel, args) ->
        let first = (sel, args, x.pos) in
        let msgs = ref [ first ] in
        while peek p = L.Semicolon do
          advance p;
          msgs := message p :: !msgs
        done;
        mk (Cascade (r, List.rev !msgs)) (fst x.pos) (last_stop p)
    | _ -> fail p "Cascading not expected"

(* one message of a cascade, after its ";" *)
and message (p : st) : string * expr list * pos =
  let start = (tok p).start in
  match peek p with
  | L.Name sel ->
      advance p;
      (sel, [], (start, last_stop p))
  | L.Binary sel ->
      advance p;
      let arg = unary_expr p in
      (sel, [ arg ], (start, last_stop p))
  | L.Bar ->
      advance p;
      let arg = unary_expr p in
      ("|", [ arg ], (start, last_stop p))
  | L.Keyword _ ->
      let sel = Buffer.create 16 and args = ref [] in
      while match peek p with L.Keyword _ -> true | _ -> false do
        (match peek p with L.Keyword k -> Buffer.add_string sel k | _ -> ());
        advance p;
        args := binary_expr p :: !args
      done;
      (Buffer.contents sel, List.rev !args, (start, last_stop p))
  | _ -> fail p "Message expected"

and keyword_expr (p : st) : expr =
  let r = binary_expr p in
  match peek p with
  | L.Keyword _ ->
      let sel = Buffer.create 16 and args = ref [] in
      while match peek p with L.Keyword _ -> true | _ -> false do
        (match peek p with L.Keyword k -> Buffer.add_string sel k | _ -> ());
        advance p;
        args := binary_expr p :: !args
      done;
      mk (Send (r, Buffer.contents sel, List.rev !args)) (fst r.pos) (last_stop p)
  | _ -> r

and binary_expr (p : st) : expr =
  let rec go (r : expr) =
    match peek p with
    | L.Binary sel ->
        advance p;
        let arg = unary_expr p in
        go (mk (Send (r, sel, [ arg ])) (fst r.pos) (snd arg.pos))
    | L.Bar ->
        advance p;
        let arg = unary_expr p in
        go (mk (Send (r, "|", [ arg ])) (fst r.pos) (snd arg.pos))
    | _ -> r
  in
  go (unary_expr p)

and unary_expr (p : st) : expr =
  let rec go (r : expr) =
    match peek p with
    | L.Name sel ->
        let stop = (tok p).stop in
        advance p;
        go (mk (Send (r, sel, [])) (fst r.pos) stop)
    | _ -> r
  in
  go (primary p)

and primary (p : st) : expr =
  let start = (tok p).start in
  match literal_of_token p with
  | Some l -> mk (Lit l) start (last_stop p)
  | None -> (
      match peek p with
      | L.Name v ->
          advance p;
          mk (Var v) start (last_stop p)
      | L.Lparen ->
          advance p;
          let x = expression p in
          expect p L.Rparen ") expected";
          (* the parentheses count in the position: the debugger
           * highlights what the person wrote *)
          { x with pos = (start, last_stop p) }
      | L.Lbracket -> block p
      | _ -> fail p "Argument expected")

and block (p : st) : expr =
  let start = (tok p).start in
  advance p;
  let args = ref [] in
  while peek p = L.Colon do
    advance p;
    args := name p "Argument name expected" :: !args
  done;
  if !args <> [] then (match peek p with L.Bar -> advance p | L.Rbracket -> () | _ -> fail p "Vertical bar expected");
  let temps = temporaries p in
  let body = statements p in
  expect p L.Rbracket "Period or right bracket expected";
  mk (Block (List.rev !args, temps, body)) start (last_stop p)

and temporaries (p : st) : string list =
  if peek p <> L.Bar then []
  else begin
    advance p;
    let temps = ref [] in
    while match peek p with L.Name _ -> true | _ -> false do
      temps := name p "" :: !temps
    done;
    expect p L.Bar "Vertical bar expected";
    List.rev !temps
  end

and statements (p : st) : stmt list =
  let rec go acc =
    match peek p with
    | L.Rbracket | L.Eof -> List.rev acc
    | L.Caret ->
        let start = (tok p).start in
        advance p;
        let x = expression p in
        let s = Return (x, (start, snd x.pos)) in
        if peek p = L.Period then advance p;
        (match peek p with L.Rbracket | L.Eof -> () | _ -> fail p "Nothing more expected");
        List.rev (s :: acc)
    | _ -> (
        let x = expression p in
        match peek p with
        | L.Period ->
            advance p;
            go (Expr x :: acc)
        | L.Rbracket | L.Eof -> List.rev (Expr x :: acc)
        | _ -> fail p "Nothing more expected")
  in
  go []

(*****************************************************************************)
(* Methods *)
(*****************************************************************************)

let pattern (p : st) : string * string list =
  match peek p with
  | L.Name sel ->
      advance p;
      (sel, [])
  | L.Binary sel ->
      advance p;
      (sel, [ name p "Argument name expected" ])
  | L.Bar ->
      advance p;
      ("|", [ name p "Argument name expected" ])
  | L.Keyword _ ->
      let sel = Buffer.create 16 and args = ref [] in
      while match peek p with L.Keyword _ -> true | _ -> false do
        (match peek p with L.Keyword k -> Buffer.add_string sel k | _ -> ());
        advance p;
        args := name p "Argument name expected" :: !args
      done;
      (Buffer.contents sel, List.rev !args)
  | _ -> fail p "Message pattern expected"

(* <primitive: 60> *)
let primitive (p : st) : int option =
  match (peek p, peek2 p) with
  | L.Binary "<", L.Keyword "primitive:" -> (
      advance p;
      advance p;
      match peek p with
      | L.Int n ->
          advance p;
          expect p (L.Binary ">") "> expected";
          Some n
      | _ -> fail p "Integer expected")
  | _ -> None

let tokens (text : string) : st =
  try { toks = L.tokenize text; i = 0 } with L.Error (pos, msg) -> raise (Error (pos, msg))

let parse_body (p : st) (selector : string) (args : string list) : method_ =
  (* the primitive may come before or after the temporaries *)
  let prim1 = primitive p in
  let temps = temporaries p in
  let prim2 = primitive p in
  let body = statements p in
  if peek p <> L.Eof then fail p "Nothing more expected";
  let primitive = match prim1 with Some _ -> prim1 | None -> prim2 in
  { selector; args; temps; primitive; body }

let parse_method (text : string) : method_ =
  let p = tokens text in
  let selector, args = pattern p in
  parse_body p selector args

let parse_doit (text : string) : method_ =
  let p = tokens text in
  parse_body p "DoIt" []

let parse_literal (text : string) : literal option =
  match tokens text with
  | p -> ( match literal_of_token p with Some l when peek p = L.Eof -> Some l | _ -> None | exception Error _ -> None)
  | exception Error _ -> None

let selector_of (text : string) : string option =
  match tokens text with
  | p -> ( try Some (fst (pattern p)) with Error _ -> None)
  | exception Error _ -> None
