(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Scheme

(* See Scheme_syntax.mli *)

exception Error of string * Sexpr.span

let fail (x : Sexpr.t) fmt = Printf.ksprintf (fun msg -> raise (Error (msg, x.span))) fmt

(*****************************************************************************)
(* Quote *)
(*****************************************************************************)

let rec datum (x : Sexpr.t) : Scheme.t =
  match x.datum with
  | Int n -> Int n
  | Float f -> Real f
  | Str s -> Str s
  | Sym s -> Sym s
  | Char c -> Char c
  | Bool b -> Bool b
  | List (xs, tail) -> List.fold_right (fun x rest -> Pair (datum x, rest)) xs (match tail with Some t -> datum t | None -> Nil)
  | Vector xs -> Vector (Array.of_list (List.map datum xs))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk (desc : desc) (span : Sexpr.span) : expr = { desc; span }
let quote v span = mk (Quote v) span

(* a built-in called by the rewriting: the procedure itself, not its
   name, so a program's own list or cons can't change what `(...)
   means *)
let prim (name : string) (span : Sexpr.span) : expr = quote (Proc (Prim name)) span

let name_of (x : Sexpr.t) (what : string) : string = match x.datum with Sym s -> s | _ -> fail x "%s: expected a name, but found %s" what (Sexpr.to_string x)

(* the parts of a list form, or an error on (a . b) *)
let parts (x : Sexpr.t) : Sexpr.t list = match x.datum with List (xs, None) -> xs | _ -> fail x "bad syntax: %s" (Sexpr.to_string x)

let is_define (x : Sexpr.t) : bool = match x.datum with List ({ datum = Sym "define"; _ } :: _, None) -> true | _ -> false

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

let rec expr (x : Sexpr.t) : expr =
  let sp = x.span in
  match x.datum with
  | Int _ | Float _ | Str _ | Char _ | Bool _ | Vector _ -> quote (datum x) sp
  | Sym s -> mk (Var s) sp
  | List ([], None) -> fail x "(): expected a function after the open parenthesis, but nothing's there"
  | List (_, Some _) -> fail x "bad syntax: a dotted list is not an expression"
  | List (({ datum = Sym head; _ } as h) :: args, None) -> form x h head args
  | List (f :: args, None) -> mk (App (expr f, List.map expr args)) sp

(* a list whose head is a symbol: a special form, or a call *)
and form (x : Sexpr.t) (h : Sexpr.t) (head : string) (args : Sexpr.t list) : expr =
  let sp = x.span in
  match (head, args) with
  | "quote", [ d ] -> quote (datum d) sp
  | "quote", _ -> fail x "quote: expected one thing after quote"
  | "quasiquote", [ d ] -> quasi d
  | ("lambda" | "λ"), formals :: body -> mk (Lambda (lambda x "" formals body)) sp
  | ("lambda" | "λ"), [] -> fail x "lambda: expected the variables, then the body"
  | "if", [ c; a; b ] -> mk (If (expr c, expr a, expr b)) sp
  | "if", [ c; a ] -> mk (If (expr c, expr a, quote Void sp)) sp
  | "if", _ -> fail x "if: expected a question and two answers, but found %d parts" (List.length args)
  | "set!", [ v; e ] -> mk (Set (name_of v "set!", expr e)) sp
  | "set!", _ -> fail x "set!: expected a variable and an expression"
  | "begin", [] -> quote Void sp
  | "begin", es -> mk (Seq (List.map expr es)) sp
  | "let", { datum = Sym name; _ } :: bindings :: body ->
      (* the named let: a loop *)
      let vars, inits = bind_list bindings in
      let loop = { params = vars; rest = None; locals = []; body = []; name } in
      let loop = mk (Lambda (body_of x { loop with body = [] } body)) sp in
      let f = mk (Lambda { params = []; rest = None; locals = [ name ]; body = [ mk (Set (name, loop)) sp; mk (Var name) sp ]; name = "" }) sp in
      mk (App (mk (App (f, [])) sp, inits)) sp
  | "let", bindings :: body ->
      let vars, inits = bind_list bindings in
      mk (App (mk (Lambda (body_of x { params = vars; rest = None; locals = []; body = []; name = "" } body)) sp, inits)) sp
  | "let*", bindings :: body -> (
      match parts bindings with
      | [] | [ _ ] -> form x h "let" args
      | b :: more -> form x h "let" [ Sexpr.make (List ([ b ], None)) bindings.span; Sexpr.make (List (Sexpr.sym "let*" h.span :: Sexpr.make (List (more, None)) bindings.span :: body, None)) sp ])
  | "letrec", bindings :: body | "letrec*", bindings :: body ->
      let defs = List.map (fun b -> match parts b with [ v; e ] -> Sexpr.make (List ([ Sexpr.sym "define" b.span; v; e ], None)) b.span | _ -> fail b "letrec: expected [name expression]") (parts bindings) in
      mk (App (mk (Lambda (body_of x { params = []; rest = None; locals = []; body = []; name = "" } (defs @ body))) sp, [])) sp
  | ("let" | "let*" | "letrec"), [] -> fail x "%s: expected the bindings, then the body" head
  | "and", [] -> quote (Bool true) sp
  | "and", [ a ] -> expr a
  | "and", a :: rest -> mk (If (expr a, form x h "and" rest, quote (Bool false) sp)) sp
  | "or", [] -> quote (Bool false) sp
  | "or", [ a ] -> expr a
  | "or", a :: rest ->
      let t = mk (Var " t") sp in
      mk (App (mk (Lambda { params = [ " t" ]; rest = None; locals = []; body = [ mk (If (t, t, form x h "or" rest)) sp ]; name = "" }) sp, [ expr a ])) sp
  | "cond", clauses -> cond x clauses
  | "when", c :: body -> mk (If (expr c, mk (Seq (List.map expr body)) sp, quote Void sp)) sp
  | "unless", c :: body -> mk (If (expr c, quote Void sp, mk (Seq (List.map expr body)) sp)) sp
  | "case", key :: clauses ->
      (* (case k [(d ...) a] [else b]): k once, then memv down the clauses *)
      let t = mk (Var " t") sp in
      let rec go cs =
        match cs with
        | [] -> quote Void sp
        | c :: rest -> (
            match parts c with
            | { datum = Sym "else"; _ } :: body -> mk (Seq (List.map expr body)) c.span
            | ds :: body -> mk (If (mk (App (prim "memv" c.span, [ t; quote (datum ds) ds.span ])) c.span, mk (Seq (List.map expr body)) c.span, go rest)) c.span
            | [] -> fail c "case: expected a clause")
      in
      mk (App (mk (Lambda { params = [ " t" ]; rest = None; locals = []; body = [ go clauses ]; name = "" }) sp, [ expr key ])) sp
  | "big-bang", w :: clauses ->
      let clause c = match parts c with [ { datum = Sym name; _ }; e ] -> (name, expr e) | _ -> fail c "big-bang: expected a clause such as [on-tick f]" in
      mk (Big_bang (expr w, List.map clause clauses)) sp
  | ("define" | "define-struct"), _ -> fail x "%s: found a definition that is not at the top level" head
  | _ -> mk (App (expr h, List.map expr args)) sp

(* [(x e) ...]: the variables and their expressions *)
and bind_list (bindings : Sexpr.t) : string list * expr list =
  List.split (List.map (fun b -> match parts b with [ v; e ] -> (name_of v "let", expr e) | _ -> fail b "let: expected a variable and an expression") (parts bindings))

(* (lambda formals body ...): (a b), (a . rest), or a single name for all *)
and lambda (x : Sexpr.t) (name : string) (formals : Sexpr.t) (body : Sexpr.t list) : lambda =
  let params, rest =
    match formals.datum with
    | Sym r -> ([], Some r)
    | List (ps, tail) -> (List.map (fun p -> name_of p "lambda") ps, Option.map (fun t -> name_of t "lambda") tail)
    | _ -> fail formals "lambda: expected the variables, but found %s" (Sexpr.to_string formals)
  in
  body_of x { params; rest; locals = []; body = []; name } body

(* a body: its defines become its own variables, set in turn *)
and body_of (x : Sexpr.t) (l : lambda) (forms : Sexpr.t list) : lambda =
  if List.for_all is_define forms then fail x "expected an expression for the body, but nothing's there";
  let one (f : Sexpr.t) : string list * expr =
    if is_define f then
      match definition f with
      | { desc = Define (name, e); span } -> ([ name ], mk (Set (name, e)) span)
      | _ -> fail f "define: bad syntax"
    else ([], expr f)
  in
  let locals, body = List.split (List.map one forms) in
  { l with locals = List.concat locals; body }

and cond (x : Sexpr.t) (clauses : Sexpr.t list) : expr =
  match clauses with
  | [] ->
      (* no question was true: the teaching languages' error, R5RS's
         unspecified value *)
      mk (App (prim "cond-fell-through" x.span, [])) x.span
  | c :: rest -> (
      match parts c with
      | [ { datum = Sym "else"; _ } ] -> fail c "cond: expected an answer after else"
      | { datum = Sym "else"; _ } :: body -> mk (Seq (List.map expr body)) c.span
      | [ q ] ->
          let t = mk (Var " t") c.span in
          mk (App (mk (Lambda { params = [ " t" ]; rest = None; locals = []; body = [ mk (If (t, t, cond x rest)) c.span ]; name = "" }) c.span, [ expr q ])) c.span
      | q :: body -> mk (If (expr q, mk (Seq (List.map expr body)) c.span, cond x rest)) c.span
      | [] -> fail c "cond: expected a clause with a question and an answer, but found an empty part")

(* `d: data, and the ,e inside it computed *)
and quasi (d : Sexpr.t) : expr =
  let sp = d.span in
  match d.datum with
  | List ([ { datum = Sym "unquote"; _ }; e ], None) -> expr e
  | List (xs, tail) ->
      let last = match tail with Some t -> quasi t | None -> quote Nil sp in
      List.fold_right
        (fun (x : Sexpr.t) rest ->
          match x.datum with
          | List ([ { datum = Sym "unquote-splicing"; _ }; e ], None) -> mk (App (prim "append" x.span, [ expr e; rest ])) x.span
          | _ -> mk (App (prim "cons" x.span, [ quasi x; rest ])) x.span)
        xs last
  | _ -> quote (datum d) sp

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

and definition (x : Sexpr.t) : expr =
  match parts x with
  | [ _; { datum = Sym name; _ }; e ] -> mk (Define (name, match expr e with { desc = Lambda l; span } -> { desc = Lambda { l with name }; span } | e -> e)) x.span
  | _ :: ({ datum = List (({ datum = Sym name; _ }) :: params, tail); _ } as header) :: body ->
      if body = [] then fail x "define: expected an expression for the function's body, but nothing's there";
      let formals = Sexpr.make (List (params, tail)) header.span in
      mk (Define (name, mk (Lambda (lambda x name formals body)) x.span)) x.span
  | [ _; v ] -> fail x "define: expected an expression after the name %s, but nothing's there" (Sexpr.to_string v)
  | _ -> fail x "define: expected a name, or a name and its arguments in parentheses"

let top (x : Sexpr.t) : expr =
  match x.datum with
  | List ({ datum = Sym "define"; _ } :: _, None) -> definition x
  | List ([ { datum = Sym "define-struct"; _ }; name; fields ], None) ->
      mk (Define_struct (name_of name "define-struct", List.map (fun f -> name_of f "define-struct") (parts fields))) x.span
  | List ({ datum = Sym "define-struct"; _ } :: _, None) -> fail x "define-struct: expected a name and the fields in parentheses"
  | _ -> expr x
