(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Scheme_step.mli *)

type step = { before : string; redex : int * int; after : string; contractum : int * int }

exception Error of string

let fail fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

(*****************************************************************************)
(* Terms *)
(*****************************************************************************)

(* a Beginning Student expression, as text is: no environment, values
   written into it by substitution *)
type term =
  | V of Scheme.t
  | Id of string
  | Call of string * term list
  | If of term * term * term
  | Cond of (term option * term) list (* None: else *)
  | And of term list
  | Or of term list
  | Mark of term (* the redex, or what replaced it: highlighted *)

type form = Def of string * term | Expr of term

(* what the definitions made: constants' values, functions, structures *)
type def = Const of Scheme.t | Fun of string list * term | Struct_op of Scheme.proc

let rec term (x : Sexpr.t) : term =
  match x.datum with
  | Sym "true" -> V (Bool true)
  | Sym "false" -> V (Bool false)
  | Sym "empty" -> V Nil
  | Sym s -> Id s
  | Int _ | Float _ | Str _ | Char _ | Bool _ | Vector _ -> V (Scheme_syntax.datum x)
  | List ([ { datum = Sym "quote"; _ }; d ], None) -> V (Scheme_syntax.datum d)
  | List ([ { datum = Sym "if"; _ }; c; a; b ], None) -> If (term c, term a, term b)
  | List ({ datum = Sym "cond"; _ } :: clauses, None) ->
      Cond
        (List.map
           (fun (c : Sexpr.t) ->
             match c.datum with
             | List ([ { datum = Sym "else"; _ }; a ], None) -> (None, term a)
             | List ([ q; a ], None) -> (Some (term q), term a)
             | _ -> fail "cond: expected a question and an answer in %s" (Sexpr.to_string c))
           clauses)
  | List ({ datum = Sym "and"; _ } :: xs, None) -> And (List.map term xs)
  | List ({ datum = Sym "or"; _ } :: xs, None) -> Or (List.map term xs)
  | List ({ datum = Sym (("lambda" | "λ" | "local" | "let" | "let*" | "letrec" | "set!" | "begin" | "define" | "define-struct" | "big-bang") as f); _ } :: _, None) ->
      fail "the stepper knows Beginning Student only, and %s is not in it" f
  | List ({ datum = Sym f; _ } :: args, None) -> Call (f, List.map term args)
  | _ -> fail "not a Beginning Student expression: %s" (Sexpr.to_string x)

(*****************************************************************************)
(* Printing, and where the mark is *)
(*****************************************************************************)

let to_text (f : form) : string * (int * int) =
  let b = Buffer.create 64 and mark = ref (0, 0) in
  let add = Buffer.add_string b in
  let rec go t =
    match t with
    | V v -> add (Scheme.print Constructor v)
    | Id x -> add x
    | Call (f, args) -> add "("; add f; List.iter (fun a -> add " "; go a) args; add ")"
    | If (c, a, e) -> add "(if "; go c; add " "; go a; add " "; go e; add ")"
    | Cond clauses ->
        add "(cond";
        List.iter (fun (q, a) -> add " ["; (match q with Some q -> go q | None -> add "else"); add " "; go a; add "]") clauses;
        add ")"
    | And xs -> add "(and"; List.iter (fun x -> add " "; go x) xs; add ")"
    | Or xs -> add "(or"; List.iter (fun x -> add " "; go x) xs; add ")"
    | Mark t ->
        let start = Buffer.length b in
        go t;
        mark := (start, Buffer.length b)
  in
  (match f with Def (x, t) -> add ("(define " ^ x ^ " "); go t; add ")" | Expr t -> go t);
  (Buffer.contents b, !mark)

(*****************************************************************************)
(* A step *)
(*****************************************************************************)

let rec subst (x : string) (v : Scheme.t) (t : term) : term =
  let s = subst x v in
  match t with
  | Id y when y = x -> V v
  | V _ | Id _ -> t
  | Call (f, args) -> Call (f, List.map s args)
  | If (c, a, b) -> If (s c, s a, s b)
  | Cond cl -> Cond (List.map (fun (q, a) -> (Option.map s q, s a)) cl)
  | And xs -> And (List.map s xs)
  | Or xs -> Or (List.map s xs)
  | Mark t -> Mark (s t)

(* a value, and in Beginning Student a constructor called on values is
   one too: (make-posn 1 2) is not reduced, it is the posn *)
let rec value (defs : (string * def) list) (t : term) : Scheme.t option =
  match t with
  | V v -> Some v
  | Call (f, args) -> (
      match List.assoc_opt f defs with
      | Some (Struct_op (Make (name, n))) when List.length args = n ->
          let vs = List.filter_map (value defs) args in
          if List.length vs = n then Some (Struct (name, vs)) else None
      | _ -> None)
  | _ -> None

let boolean (what : string) (v : Scheme.t) : bool =
  match v with Bool b -> b | _ -> fail "%s: question result is not true or false: %s" what (Scheme.print Constructor v)

(* a call whose arguments are all values *)
let call (defs : (string * def) list) (f : string) (args : Scheme.t list) : term =
  match List.assoc_opt f defs with
  | Some (Fun (params, body)) ->
      if List.length params <> List.length args then fail "%s: expects %d arguments, given %d" f (List.length params) (List.length args);
      List.fold_left2 (fun body p a -> subst p a body) body params args
  | Some (Struct_op (Make (name, n))) -> if List.length args <> n then fail "make-%s: expects %d arguments, given %d" name n (List.length args) else V (Struct (name, args))
  | Some (Struct_op (Get (name, i, field))) -> (
      match args with [ Struct (s, fields) ] when s = name -> V (List.nth fields i) | _ -> fail "%s-%s: expects argument of type <struct:%s>" name field name)
  | Some (Struct_op (Is name)) -> V (Bool (match args with [ Struct (s, _) ] -> s = name | _ -> false))
  | Some _ -> fail "%s: this is a value, not a function" f
  | None -> ( match Scheme_prims.apply f args with v -> V v | exception Scheme_prims.Error msg -> fail "%s" msg | exception Not_found -> fail "%s: this function is not defined" f)

(* [reduce defs t]: the term with its redex marked, and with what
   replaced it marked; None when [t] is a value *)
let rec reduce (defs : (string * def) list) (t : term) : (term * term) option =
  let contract t' = Some (Mark t, Mark t') in
  match t with
  | V _ | Mark _ -> None
  | Id x -> (
      match List.assoc_opt x defs with
      | Some (Const v) -> contract (V v)
      | _ -> if x = "pi" then contract (V (Real Float.pi)) else fail "%s: this variable is not defined" x)
  | Call (f, args) -> (
      match inside defs args with
      | Some (b, a) -> Some (Call (f, b), Call (f, a))
      | None -> if value defs t <> None then None else contract (call defs f (List.filter_map (value defs) args)))
  | If (c, a, e) -> (
      match reduce defs c with
      | Some (b, a') -> Some (If (b, a, e), If (a', a, e))
      | None -> contract (if boolean "if" (Option.get (value defs c)) then a else e))
  | Cond [] -> fail "cond: all question results were false"
  | Cond ((None, a) :: _) -> contract a
  | Cond ((Some q, a) :: rest) -> (
      match reduce defs q with
      | Some (b, a') -> Some (Cond ((Some b, a) :: rest), Cond ((Some a', a) :: rest))
      | None -> contract (if boolean "cond" (Option.get (value defs q)) then a else Cond rest))
  | And [] -> contract (V (Bool true))
  | Or [] -> contract (V (Bool false))
  | And (x :: rest) -> logic defs "and" x rest (fun b -> if b then And rest else V (Bool false)) (fun xs -> And xs)
  | Or (x :: rest) -> logic defs "or" x rest (fun b -> if b then V (Bool true) else Or rest) (fun xs -> Or xs)

(* the first argument not a value, reduced *)
and inside defs (args : term list) : (term list * term list) option =
  match args with
  | [] -> None
  | x :: rest -> (
      match reduce defs x with
      | Some (b, a) -> Some (b :: rest, a :: rest)
      | None -> Option.map (fun (b, a) -> (x :: b, x :: a)) (inside defs rest))

(* and, or: the first operand to a value, then the form shortened or
   done *)
and logic defs (what : string) x rest (next : bool -> term) (rebuild : term list -> term) =
  match reduce defs x with
  | Some (b, a) -> Some (rebuild (b :: rest), rebuild (a :: rest))
  | None -> Some (Mark (rebuild (x :: rest)), Mark (next (boolean what (Option.get (value defs x)))))

let rec unmark (t : term) : term =
  match t with
  | Mark t -> unmark t
  | V _ | Id _ -> t
  | Call (f, args) -> Call (f, List.map unmark args)
  | If (c, a, b) -> If (unmark c, unmark a, unmark b)
  | Cond cl -> Cond (List.map (fun (q, a) -> (Option.map unmark q, unmark a)) cl)
  | And xs -> And (List.map unmark xs)
  | Or xs -> Or (List.map unmark xs)

(*****************************************************************************)
(* The program *)
(*****************************************************************************)

let steps ?(max = 1000) (text : string) : step list * string option =
  let acc = ref [] in
  (* a form reduced to a value, each step recorded *)
  let rec run defs (make : term -> form) (t : term) : Scheme.t =
    if List.length !acc >= max then fail "the stepper stopped after %d steps" max;
    match reduce defs t with
    | None -> Option.get (value defs t)
    | Some (b, a) ->
        let before, redex = to_text (make b) and after, contractum = to_text (make a) in
        acc := { before; redex; after; contractum } :: !acc;
        run defs make (unmark a)
  in
  let form defs (x : Sexpr.t) : (string * def) list =
    match x.datum with
    | List ([ { datum = Sym "define"; _ }; { datum = List (({ datum = Sym f; _ }) :: params, None); _ }; body ], None) ->
        let param (p : Sexpr.t) = match p.datum with Sym s -> s | _ -> fail "define: expected a variable, found %s" (Sexpr.to_string p) in
        (f, Fun (List.map param params, term body)) :: defs
    | List ([ { datum = Sym "define"; _ }; { datum = Sym c; _ }; e ], None) -> (c, Const (run defs (fun t -> Def (c, t)) (term e))) :: defs
    | List ([ { datum = Sym "define-struct"; _ }; { datum = Sym name; _ }; { datum = List (fields, None); _ } ], None) ->
        let fields = List.map (fun (f : Sexpr.t) -> match f.datum with Sym s -> s | _ -> fail "define-struct: expected a field name") fields in
        (("make-" ^ name, Struct_op (Make (name, List.length fields))) :: (name ^ "?", Struct_op (Is name)) :: List.mapi (fun i f -> (name ^ "-" ^ f, Struct_op (Get (name, i, f)))) fields) @ defs
    | List ({ datum = Sym ("define" | "define-struct"); _ } :: _, None) -> fail "define: not a Beginning Student definition: %s" (Sexpr.to_string x)
    (* a world runs in time, not in steps: left out *)
    | List ({ datum = Sym "big-bang"; _ } :: _, None) -> defs
    | _ -> ignore (run defs (fun t -> Expr t) (term x)); defs
  in
  match List.fold_left form [] (Sexpr_read.read_all Scheme text) with
  | _ -> (List.rev !acc, None)
  | exception Error msg -> (List.rev !acc, Some msg)
  | exception Sexpr_read.Error (msg, _) -> (List.rev !acc, Some ("read: " ^ msg))
