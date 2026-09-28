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

(* See Scheme_eval.mli *)

module Smap = Map.Make (String)
module Imap = Map.Make (Int)

type world = { init : Scheme.t; handlers : (string * Scheme.t) list; span : Sexpr.span }
type error = { message : string; at : Sexpr.span option }
type outcome = Done of Scheme.t | Running | Failed of error | World of world

type control =
  | Eval of expr * env
  | Return of Scheme.t
  | Wait of world (* the host runs the world, then resume *)

type state = {
  control : control;
  kont : kont;
  store : Scheme.t Imap.t;
  next : loc;
  globals : loc Smap.t;
  builtins : int; (* the globals made by create: the rest are the program's *)
  output : string list; (* display's text, newest first *)
  seed : int; (* random's: Lehmer's generator, 48271 *)
  steps : int;
}

(* a step's error, and the text at fault *)
exception Fail of string * Sexpr.span

(*****************************************************************************)
(* The store *)
(*****************************************************************************)

let alloc (st : state) (v : Scheme.t) : state * loc = ({ st with store = Imap.add st.next v st.store; next = st.next + 1 }, st.next)

let locate (st : state) (env : env) (x : string) (span : Sexpr.span) : loc =
  match List.assoc_opt x env with
  | Some l -> l
  | None -> ( match Smap.find_opt x st.globals with Some l -> l | None -> raise (Fail ("reference to an undefined identifier: " ^ x, span)))

let define (st : state) (x : string) (v : Scheme.t) : state =
  match Smap.find_opt x st.globals with
  | Some l -> { st with store = Imap.add l v st.store }
  | None ->
      let st, l = alloc st v in
      { st with globals = Smap.add x l st.globals }

(*****************************************************************************)
(* Apply *)
(*****************************************************************************)

let eval_body (st : state) (body : expr list) (env : env) (k : kont) : state =
  match body with
  | [] -> { st with control = Return Void; kont = k }
  | [ e ] -> { st with control = Eval (e, env); kont = k }
  | e :: rest -> { st with control = Eval (e, env); kont = K_seq (rest, env, k) }

let arity_error (name : string) (want : string) (args : Scheme.t list) (span : Sexpr.span) =
  let name = if name = "" then "#<procedure>" else name in
  raise (Fail (Printf.sprintf "%s: expects %s, given %d%s" name want (List.length args) (if args = [] then "" else ": " ^ String.concat " " (List.map (print Write) args)), span))

let rec apply (st : state) (f : Scheme.t) (args : Scheme.t list) (span : Sexpr.span) (k : kont) : state =
  let return v = { st with control = Return v; kont = k } in
  match f with
  | Proc (Closure (l, env)) ->
      let n = List.length l.params in
      if (l.rest = None && List.length args <> n) || List.length args < n then
        arity_error l.name (Printf.sprintf "%s%d argument%s" (if l.rest = None then "" else "at least ") n (if n = 1 then "" else "s")) args span;
      let rec bind st env params args =
        match (params, args) with
        | p :: ps, a :: rest ->
            let st, loc = alloc st a in
            bind st ((p, loc) :: env) ps rest
        | _, rest -> (
            match l.rest with
            | Some r ->
                let st, loc = alloc st (list rest) in
                (st, (r, loc) :: env)
            | None -> (st, env))
      in
      let st, env = bind st env l.params args in
      (* the body's own defines, unset until their turn *)
      let st, env = List.fold_left (fun (st, env) x -> let st, loc = alloc st Void in (st, (x, loc) :: env)) (st, env) l.locals in
      eval_body st l.body env k
  | Proc (Cont k') -> ( match args with [ v ] -> { st with control = Return v; kont = k' } | _ -> arity_error "continuation" "1 argument" args span)
  | Proc (Make (name, n)) -> if List.length args <> n then arity_error ("make-" ^ name) (Printf.sprintf "%d arguments" n) args span else return (Struct (name, args))
  | Proc (Get (name, i, field)) -> (
      match args with
      | [ Struct (s, fields) ] when s = name -> return (List.nth fields i)
      | [ v ] -> raise (Fail (Printf.sprintf "%s-%s: expects argument of type <struct:%s>; given %s" name field name (print Write v), span))
      | _ -> arity_error (name ^ "-" ^ field) "1 argument" args span)
  | Proc (Is name) -> ( match args with [ v ] -> return (Bool (match v with Struct (s, _) -> s = name | _ -> false)) | _ -> arity_error (name ^ "?") "1 argument" args span)
  | Proc (Prim name) -> prim st name args span k
  | _ ->
      raise
        (Fail
           ( Printf.sprintf "procedure application: expected procedure, given: %s%s" (print Write f)
               (if args = [] then " (no arguments)" else "; arguments were: " ^ String.concat " " (List.map (print Write) args)),
             span ))

(* the built-ins that need the machine, then the others *)
and prim (st : state) (name : string) (args : Scheme.t list) (span : Sexpr.span) (k : kont) : state =
  let return v = { st with control = Return v; kont = k } in
  match (name, args) with
  | ("call/cc" | "call-with-current-continuation"), [ f ] -> apply st f [ Proc (Cont k) ] span k
  | "apply", f :: rest when rest <> [] -> (
      let firsts = List.rev (List.tl (List.rev rest)) and last = List.hd (List.rev rest) in
      match to_list last with Some xs -> apply st f (firsts @ xs) span k | None -> raise (Fail ("apply: expects a list as its last argument; given " ^ print Write last, span)))
  | "display", [ v ] -> { (return Void) with output = display v :: st.output }
  | "write", [ v ] -> { (return Void) with output = print Write v :: st.output }
  | "newline", [] -> { (return Void) with output = "\n" :: st.output }
  | "random", _ -> (
      let seed = st.seed * 48271 mod 2147483647 in
      let st = { st with seed } in
      match args with
      | [ Int n ] when n > 0 -> { st with control = Return (Int (seed mod n)); kont = k }
      | [] -> { st with control = Return (Real (float_of_int seed /. 2147483647.)); kont = k }
      | _ -> raise (Fail ("random: expects argument of type <positive integer>", span)))
  | ("call/cc" | "call-with-current-continuation" | "apply" | "display" | "write" | "newline"), _ -> arity_error name "other arguments" args span
  | _ -> ( match Scheme_prims.apply name args with v -> return v | exception Scheme_prims.Error msg -> raise (Fail (msg, span)))

let specials = [ "call/cc"; "call-with-current-continuation"; "apply"; "display"; "write"; "newline"; "random" ]

(*****************************************************************************)
(* A step *)
(*****************************************************************************)

let step (st : state) : state =
  let st = { st with steps = st.steps + 1 } in
  match st.control with
  | Wait _ -> st
  | Eval (e, env) -> (
      let return v = { st with control = Return v } in
      match e.desc with
      | Quote v -> return v
      | Var x -> return (Imap.find (locate st env x e.span) st.store)
      | Lambda l -> return (Proc (Closure (l, env)))
      | If (c, a, b) -> { st with control = Eval (c, env); kont = K_if (a, b, env, st.kont) }
      | Set (x, e1) -> { st with control = Eval (e1, env); kont = K_set (locate st env x e.span, st.kont) }
      | App (f, args) -> { st with control = Eval (f, env); kont = K_app ([], args, env, e.span, st.kont) }
      | Seq body -> eval_body st body env st.kont
      | Define (x, e1) -> { st with control = Eval (e1, env); kont = K_define (x, st.kont) }
      | Define_struct (name, fields) ->
          let st = define st ("make-" ^ name) (Proc (Make (name, List.length fields))) in
          let st = define st (name ^ "?") (Proc (Is name)) in
          let st = List.fold_left (fun st (i, f) -> define st (name ^ "-" ^ f) (Proc (Get (name, i, f)))) st (List.mapi (fun i f -> (i, f)) fields) in
          { st with control = Return Void }
      | Big_bang (w, clauses) -> { st with control = Eval (w, env); kont = K_big_bang ([], clauses, [], env, e.span, st.kont) })
  | Return v -> (
      match st.kont with
      | Halt -> st
      | K_if (a, b, env, k) -> { st with control = Eval ((if truthy v then a else b), env); kont = k }
      | K_app (vals, e :: rest, env, span, k) -> { st with control = Eval (e, env); kont = K_app (v :: vals, rest, env, span, k) }
      | K_app (vals, [], _, span, k) -> (
          match List.rev (v :: vals) with f :: args -> apply st f args span k | [] -> assert false)
      | K_set (loc, k) -> { st with store = Imap.add loc v st.store; control = Return Void; kont = k }
      | K_seq (body, env, k) -> eval_body st body env k
      | K_define (x, k) ->
          (* a lambda defined is named by its definition *)
          let v = match v with Proc (Closure (l, env)) when l.name = "" -> Proc (Closure ({ l with name = x }, env)) | _ -> v in
          { (define st x v) with control = Return Void; kont = k }
      | K_big_bang (vals, (name, e) :: rest, names, env, span, k) -> { st with control = Eval (e, env); kont = K_big_bang (v :: vals, rest, name :: names, env, span, k) }
      | K_big_bang (vals, [], names, _, span, k) -> (
          match List.rev (v :: vals) with
          | init :: handlers -> { st with control = Wait { init; handlers = List.combine (List.rev names) handlers; span }; kont = k }
          | [] -> assert false))

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

let start (st : state) (e : expr) : state = { st with control = Eval (e, []); kont = Halt }

let run ?(fuel = 100_000) (st : state) : outcome * state =
  let rec go st n =
    match (st.control, st.kont) with
    | Return v, Halt -> (Done v, st)
    | Wait w, _ -> (World w, st)
    | _ when n = 0 -> (Running, st)
    | _ -> (
        match step st with
        | st' -> go st' (n - 1)
        | exception Fail (message, span) -> (Failed { message; at = Some span }, { st with control = Return Void; kont = Halt }))
  in
  go st fuel

let resume (st : state) (v : Scheme.t) : state = { st with control = Return v }

let call ?(fuel = 1_000_000) (st : state) (f : Scheme.t) (args : Scheme.t list) : (Scheme.t, error) result * state =
  let control, kont = (st.control, st.kont) in
  let e = { desc = App ({ desc = Quote f; span = Sexpr.nowhere }, List.map (fun a -> { desc = Quote a; span = Sexpr.nowhere }) args); span = Sexpr.nowhere } in
  let result, st = match run ~fuel (start st e) with
    | Done v, st -> (Ok v, st)
    | Failed err, st -> (Error err, st)
    | Running, st -> (Error { message = "the program ran too long"; at = None }, st)
    | World w, st -> (Error { message = "big-bang: a world can't start another"; at = Some w.span }, st)
  in
  (result, { st with control; kont })

let take_output (st : state) : string * state = (String.concat "" (List.rev st.output), { st with output = [] })

let eval_all ?(fuel = 10_000_000) (st : state) (text : string) : (Scheme.t, error) result * state =
  let rec go st last forms =
    match forms with
    | [] -> (Ok last, st)
    | x :: rest -> (
        match Scheme_syntax.top x with
        | exception Scheme_syntax.Error (message, span) -> (Error { message; at = Some span }, st)
        | e -> (
            match run ~fuel (start st e) with
            | Done v, st -> go st v rest
            | Failed err, st -> (Error err, st)
            | Running, st -> (Error { message = "the program ran too long"; at = None }, st)
            | World w, st -> (Error { message = "big-bang: no world can run here"; at = Some w.span }, st)))
  in
  match Sexpr_read.read_all Scheme text with
  | forms -> go st Void forms
  | exception Sexpr_read.Error (message, pos) -> (Error { message = "read: " ^ message; at = Some { start = pos; stop = pos + 1 } }, st)

let steps (st : state) : int = st.steps

let create () : state =
  let st = { control = Return Void; kont = Halt; store = Imap.empty; next = 0; globals = Smap.empty; builtins = 0; output = []; seed = 1; steps = 0 } in
  let st = List.fold_left (fun st name -> define st name (Proc (Prim name))) st (Scheme_prims.names @ specials) in
  match eval_all st Scheme_prelude.text with
  | Ok _, st -> { st with builtins = st.next; steps = 0 }
  | Error err, _ -> failwith ("Scheme_prelude: " ^ err.message)

let defined (st : state) : string list =
  (* the globals' locations are allocated in order: those past the
     built-ins' are the program's *)
  Smap.bindings st.globals |> List.filter (fun (_, l) -> l >= st.builtins) |> List.map fst
