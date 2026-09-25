(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_eval.mli *)
open Js_value
module A = Js_ast

type t = {
  globals : scope;
  mutable protos : Js_builtins.protos option; (* set once the built-ins are *)
  mutable line : int; (* of the statement running: an error's *)
  mutable steps : int; (* left in this run's budget *)
  mutable budget : int;
  mutable depth : int; (* of the calls *)
}

type error = { line : int; message : string }

(* how a statement ended *)
type outcome = Normal | Return of value | Break | Continue

let max_depth = 2_000

(*****************************************************************************)
(* Scopes *)
(*****************************************************************************)

let new_scope (parent : scope) : scope = { vars = Hashtbl.create 8; parent = Some parent }

let rec lookup (s : scope) (x : string) : binding option =
  match Hashtbl.find_opt s.vars x with Some b -> Some b | None -> Option.bind s.parent (fun p -> lookup p x)

let declare (s : scope) (x : string) ~(constant : bool) (v : value) : unit = Hashtbl.replace s.vars x { value = v; constant }

(* a for's next iteration: the same names, in new bindings holding the
 * same values (Hashtbl.copy would share the bindings, and every
 * iteration's closures would see the last i) *)
let copy_scope (s : scope) : scope =
  let vars = Hashtbl.create 8 in
  Hashtbl.iter (fun x (b : binding) -> Hashtbl.replace vars x { b with value = b.value }) s.vars;
  { s with vars }

(*****************************************************************************)
(* Properties *)
(*****************************************************************************)

let protos (t : t) : Js_builtins.protos = Option.get t.protos

(* the array index a key names, if it is one: "3", not "03" nor "-1" *)
let index_of_key (k : string) : int option =
  match int_of_string_opt k with Some i when i >= 0 && string_of_int i = k -> Some i | _ -> None

let key_of (v : value) : string = match v with Number f when Float.is_integer f && f >= 0. -> Printf.sprintf "%.0f" f | v -> to_string v

let get (t : t) (target : value) (k : string) : value =
  match target with
  | Undefined | Null -> throw "TypeError" (Printf.sprintf "Cannot read properties of %s (reading '%s')" (to_string target) k)
  | String s -> (
      match (k, index_of_key k) with
      | "length", _ -> Number (float_of_int (String.length s))
      | _, Some i -> if i < String.length s then String (String.make 1 s.[i]) else Undefined
      | _ -> Option.value (get_own (protos t).strings k) ~default:Undefined)
  | Object ({ kind = Array a; _ } as o) -> (
      match (k, index_of_key k) with
      | "length", _ -> Number (float_of_int a.length)
      | _, Some i -> if i < a.length then a.elements.(i) else Undefined
      | _ -> (
          match get_own o k with Some v -> v | None -> Option.value (get_own (protos t).arrays k) ~default:Undefined))
  | Object o -> Option.value (get_own o k) ~default:Undefined
  | Bool _ | Number _ -> Undefined

let set (target : value) (k : string) (v : value) : unit =
  match target with
  | Undefined | Null -> throw "TypeError" (Printf.sprintf "Cannot set properties of %s (setting '%s')" (to_string target) k)
  | Object ({ kind = Array a; _ } as o) -> (
      let grow n =
        if n > Array.length a.elements then (
          let bigger = Array.make (max n (2 * Array.length a.elements)) Undefined in
          Array.blit a.elements 0 bigger 0 a.length;
          a.elements <- bigger)
      in
      match (k, index_of_key k) with
      | "length", _ ->
          let n = int_of_float (to_number v) in
          grow n;
          for i = a.length to n - 1 do a.elements.(i) <- Undefined done;
          a.length <- n
      | _, Some i ->
          grow (i + 1);
          for j = a.length to i - 1 do a.elements.(j) <- Undefined done;
          a.elements.(i) <- v;
          a.length <- max a.length (i + 1)
      | _ -> set_own o k v)
  | Object o -> set_own o k v
  (* a property of a primitive: lost, as JavaScript loses it *)
  | Bool _ | Number _ | String _ -> ()

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

let arithmetic (op : string) (a : value) (b : value) : value =
  match op with
  | "+" -> (
      match (to_primitive a, to_primitive b) with
      | (String _ as x), y | x, (String _ as y) -> String (to_string x ^ to_string y)
      | x, y -> Number (to_number x +. to_number y))
  | "-" -> Number (to_number a -. to_number b)
  | "*" -> Number (to_number a *. to_number b)
  | "/" -> Number (to_number a /. to_number b)
  | "%" -> Number (Float.rem (to_number a) (to_number b))
  | "<" | ">" | "<=" | ">=" -> (
      let cmp =
        match (to_primitive a, to_primitive b) with
        | String x, String y -> Some (compare x y)
        | x, y ->
            let x = to_number x and y = to_number y in
            if Float.is_nan x || Float.is_nan y then None else Some (compare x y)
      in
      match cmp with
      | None -> Bool false
      | Some c -> Bool (match op with "<" -> c < 0 | ">" -> c > 0 | "<=" -> c <= 0 | _ -> c >= 0))
  | "===" | "==" -> Bool (strict_equal a b)
  | "!==" | "!=" -> Bool (not (strict_equal a b))
  | _ -> throw "SyntaxError" ("unknown operator " ^ op)

(* "x", "o.m": what a TypeError names *)
let rec describe (e : A.expr) : string =
  match e with
  | Name x -> x
  | This -> "this"
  | Member (o, x) -> describe o ^ "." ^ x
  | Index (o, _) -> describe o ^ "[...]"
  | Call (f, _) -> describe f ^ "(...)"
  | _ -> "expression"

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

let tick (t : t) : unit =
  t.steps <- t.steps - 1;
  if t.steps < 0 then throw "RangeError" "the script ran too long (a loop that never ends?)"

let rec eval_expr (t : t) (s : scope) (this : value) (e : A.expr) : value =
  match e with
  | Number f -> Number f
  | String x -> String x
  | Bool b -> Bool b
  | Null -> Null
  | This -> this
  | Name x -> (
      match lookup s x with Some b -> b.value | None -> throw "ReferenceError" (x ^ " is not defined"))
  | Array es -> Object (new_array (List.map (eval_expr t s this) es))
  | Object kvs ->
      let o = new_object () in
      List.iter (fun (k, v) -> set_own o k (eval_expr t s this v)) kvs;
      Object o
  | Function f -> closure s this f
  | Unary (op, x) -> (
      match op with
      (* typeof of an undeclared name is "undefined", not an error *)
      | "typeof" -> (
          match x with
          | Name n when lookup s n = None -> String "undefined"
          | _ -> String (typeof (eval_expr t s this x)))
      | "!" -> Bool (not (truthy (eval_expr t s this x)))
      | "-" -> Number (-.to_number (eval_expr t s this x))
      | _ -> Number (to_number (eval_expr t s this x)))
  | Update (op, prefix, target) ->
      let old = to_number (eval_expr t s this target) in
      let now = if op = "++" then old +. 1. else old -. 1. in
      assign t s this target (Number now);
      Number (if prefix then now else old)
  | Binary (op, a, b) ->
      let a = eval_expr t s this a in
      arithmetic op a (eval_expr t s this b)
  | Logical ("&&", a, b) -> let v = eval_expr t s this a in if truthy v then eval_expr t s this b else v
  | Logical (_, a, b) -> let v = eval_expr t s this a in if truthy v then v else eval_expr t s this b
  | Assign ("=", target, v) ->
      let v = eval_expr t s this v in
      assign t s this target v;
      v
  | Assign (op, target, v) ->
      let old = eval_expr t s this target in
      let v = arithmetic (String.sub op 0 1) old (eval_expr t s this v) in
      assign t s this target v;
      v
  | Conditional (c, a, b) -> if truthy (eval_expr t s this c) then eval_expr t s this a else eval_expr t s this b
  | Member (o, k) -> get t (eval_expr t s this o) k
  | Index (o, k) ->
      let o = eval_expr t s this o in
      get t o (key_of (eval_expr t s this k))
  | Call (f, args) ->
      (* a method call: this is the object the function was read from *)
      let fn, self =
        match f with
        | Member (o, k) -> let o = eval_expr t s this o in (get t o k, o)
        | Index (o, k) -> let o = eval_expr t s this o in (get t o (key_of (eval_expr t s this k)), o)
        | _ -> (eval_expr t s this f, Undefined)
      in
      let args = List.map (eval_expr t s this) args in
      (match fn with Object { kind = Closure _ | Host_function _; _ } -> () | _ -> throw "TypeError" (describe f ^ " is not a function"));
      call_value t fn ~this:self args

and closure (s : scope) (this : value) (f : A.func) : value =
  let c = { func = f; scope = s; this = (if f.arrow then Some this else None) } in
  Object { (new_object ()) with kind = Closure c }

and assign (t : t) (s : scope) (this : value) (target : A.expr) (v : value) : unit =
  match target with
  | Name x -> (
      match lookup s x with
      | Some { constant = true; _ } -> throw "TypeError" "Assignment to constant variable."
      | Some b -> b.value <- v
      | None -> throw "ReferenceError" (x ^ " is not defined"))
  | Member (o, k) -> set (eval_expr t s this o) k v
  | Index (o, k) ->
      let o = eval_expr t s this o in
      set o (key_of (eval_expr t s this k)) v
  | _ -> throw "SyntaxError" "Invalid assignment target"

and call_value (t : t) (fn : value) ~(this : value) (args : value list) : value =
  match fn with
  | Object { kind = Host_function (_, f); _ } -> f ~this args
  | Object { kind = Closure c; _ } ->
      if t.depth >= max_depth then throw "RangeError" "Maximum call stack size exceeded";
      t.depth <- t.depth + 1;
      let frame = new_scope c.scope in
      List.iteri (fun i x -> declare frame x ~constant:false (Option.value (List.nth_opt args i) ~default:Undefined)) c.func.params;
      let this = match c.this with Some captured -> captured | None -> this in
      let line = t.line in
      (* the caller's line back when the call returns; not when it
       * throws, so that the error keeps the line it was thrown on *)
      (match exec_block t frame this c.func.body with
      | result ->
          t.depth <- t.depth - 1;
          t.line <- line;
          (match result with Return v -> v | _ -> Undefined)
      | exception e ->
          t.depth <- t.depth - 1;
          raise e)
  | _ -> throw "TypeError" (display fn ^ " is not a function")

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

(* a block's statements in [s]: its function declarations first *)
and exec_block (t : t) (s : scope) (this : value) (body : A.stmt list) : outcome =
  List.iter
    (fun (st : A.stmt) -> match st.stmt with Function_decl f -> declare s (Option.get f.name) ~constant:false (closure s this f) | _ -> ())
    body;
  let rec go (body : A.stmt list) =
    match body with
    | [] -> Normal
    | st :: rest -> ( match exec t s this st with Normal -> go rest | leave -> leave)
  in
  go body

and exec (t : t) (s : scope) (this : value) (st : A.stmt) : outcome =
  t.line <- st.line;
  tick t;
  let eval = eval_expr t s this in
  match st.stmt with
  | Expr e -> ignore (eval e); Normal
  | Let (kind, decls) ->
      List.iter (fun (x, init) -> declare s x ~constant:(kind = Const_kind) (match init with Some e -> eval e | None -> Undefined)) decls;
      Normal
  | Function_decl _ -> Normal (* defined by its block, first *)
  | Return e -> Return (match e with Some e -> eval e | None -> Undefined)
  | If (c, a, b) -> (
      if truthy (eval c) then exec t (new_scope s) this a
      else match b with Some b -> exec t (new_scope s) this b | None -> Normal)
  | While (c, body) ->
      let rec loop () =
        if truthy (eval c) then
          match exec t (new_scope s) this body with Break -> Normal | Return v -> Return v | Normal | Continue -> loop ()
        else Normal
      in
      loop ()
  | For (init, test, update, body) ->
      let first = new_scope s in
      Option.iter (fun i -> ignore (exec t first this i)) init;
      (* each iteration in a copy of the last: its own i *)
      let rec loop (it : scope) =
        let ok = match test with Some c -> truthy (eval_expr t it this c) | None -> true in
        if not ok then Normal
        else
          match exec t (new_scope it) this body with
          | Break -> Normal
          | Return v -> Return v
          | Normal | Continue ->
              let next = copy_scope it in
              Option.iter (fun u -> ignore (eval_expr t next this u)) update;
              loop next
      in
      loop first
  | For_of (kind, x, xs, body) ->
      let items =
        match eval xs with
        | Object ({ kind = Array _; _ } as o) -> array_items o
        | String str -> List.init (String.length str) (fun i -> String (String.make 1 str.[i]))
        | v -> throw "TypeError" (display v ^ " is not iterable")
      in
      let rec loop items =
        match items with
        | [] -> Normal
        | v :: rest -> (
            let it = new_scope s in
            declare it x ~constant:(kind = Const_kind) v;
            match exec t it this body with Break -> Normal | Return v -> Return v | Normal | Continue -> loop rest)
      in
      loop items
  | Break -> Break
  | Continue -> Continue
  | Throw e -> raise (Throw (eval e))
  | Try (body, x, handler) -> (
      match exec_block t (new_scope s) this body with
      | outcome -> outcome
      | exception Throw v ->
          let h = new_scope s in
          declare h x ~constant:false v;
          exec_block t h this handler)
  | Block body -> exec_block t (new_scope s) this body
  | Empty -> Normal

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let default_budget = 10_000_000

let create ?(log = fun _ -> ()) ?(seed = 1) () : t =
  let globals = { vars = Hashtbl.create 64; parent = None } in
  let t = { globals; protos = None; line = 0; steps = default_budget; budget = default_budget; depth = 0 } in
  let call f ~this args = call_value t f ~this args in
  t.protos <- Some (Js_builtins.install ~call ~log ~seed (fun x v -> declare globals x ~constant:false v));
  t

(* the error a console shows: an error object as "Name: message", any
 * other value thrown as "Uncaught " and it *)
let error_of (t : t) (v : value) : error =
  let message =
    match v with
    | Object o -> (
        match (get_own o "name", get_own o "message") with
        | Some (String n), Some (String m) -> n ^ ": " ^ m
        | _ -> "Uncaught " ^ display v)
    | v -> "Uncaught " ^ display v
  in
  { line = t.line; message }

(* a run or a call, its budget renewed, its throws caught *)
let guarded (t : t) (f : unit -> value) : (value, error) result =
  t.steps <- t.budget;
  t.depth <- 0;
  match f () with
  | v -> Ok v
  | exception Throw v -> Error (error_of t v)
  | exception Stack_overflow -> Error { line = t.line; message = "RangeError: Maximum call stack size exceeded" }

let run (t : t) (program : A.program) : (value, error) result =
  guarded t (fun () ->
      (* the value of the last expression statement: a console's echo *)
      let last = ref Undefined in
      List.iter
        (fun (st : A.stmt) -> match st.stmt with Function_decl f -> declare t.globals (Option.get f.name) ~constant:false (closure t.globals Undefined f) | _ -> ())
        program;
      List.iter
        (fun (st : A.stmt) ->
          match st.stmt with
          | Expr e ->
              t.line <- st.line;
              tick t;
              last := eval_expr t t.globals Undefined e
          | _ -> (
              match exec t t.globals Undefined st with
              | Normal -> ()
              | Return _ -> throw "SyntaxError" "Illegal return statement"
              | Break | Continue -> throw "SyntaxError" "Illegal break or continue statement"))
        program;
      !last)

let eval (t : t) (text : string) : (value, error) result =
  match Js_parse.parse text with
  | Ok program -> run t program
  | Error e -> Error { line = e.line; message = "SyntaxError: " ^ e.message }

let call (t : t) (f : value) ~(this : value) (args : value list) : (value, error) result =
  guarded t (fun () -> call_value t f ~this args)

let global (t : t) (x : string) : value option = Option.map (fun b -> b.value) (Hashtbl.find_opt t.globals.vars x)
let define (t : t) (x : string) (v : value) : unit = declare t.globals x ~constant:false v
let set_budget (t : t) (steps : int) : unit = t.budget <- steps
