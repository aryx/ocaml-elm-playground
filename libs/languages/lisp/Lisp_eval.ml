(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Lisp

(* See Lisp_eval.mli *)

(*****************************************************************************)
(* Signals *)
(*****************************************************************************)

exception Signal of Lisp.t

let signal (sym : string) (data : Lisp.t list) : 'a = raise (Signal (Cons (Sym sym, list data)))
let error (msg : string) : 'a = signal "error" [ Str msg ]
let wrong_type (pred : string) (v : Lisp.t) : 'a = signal "wrong-type-argument" [ Sym pred; v ]

let error_message (e : Lisp.t) : string =
  match e with
  | Cons (Sym "error", Cons (Str msg, Sym "nil")) -> msg
  | Cons (Sym sym, data) -> (
      let data = Option.value (to_list data) ~default:[ data ] in
      let say what = if data = [] then what else what ^ ": " ^ String.concat ", " (List.map Lisp.print data) in
      match sym with
      | "void-variable" -> say "Symbol's value as variable is void"
      | "void-function" -> say "Symbol's function definition is void"
      | "wrong-type-argument" -> say "Wrong type argument"
      | "wrong-number-of-arguments" -> say "Wrong number of arguments"
      | "invalid-function" -> say "Invalid function"
      | "arith-error" -> say "Arithmetic error"
      | "invalid-read-syntax" -> say "Invalid read syntax"
      | "end-of-file" -> "End of file during parsing"
      | "beginning-of-buffer" -> "Beginning of buffer"
      | "end-of-buffer" -> "End of buffer"
      | "search-failed" -> say "Search failed"
      | _ -> say sym)
  | v -> Lisp.print v

(*****************************************************************************)
(* The built-in functions that don't touch the state *)
(*****************************************************************************)

let int_of (v : Lisp.t) : int = match v with Int n -> n | _ -> wrong_type "integerp" v
let string_of (v : Lisp.t) : string = match v with Str s -> s | _ -> wrong_type "stringp" v
let list_of (v : Lisp.t) : Lisp.t list = match to_list v with Some xs -> xs | None -> wrong_type "listp" v

(* eq: the same object; numbers and symbols by value, as Emacs's
   small integers and interned symbols are *)
let eq (a : Lisp.t) (b : Lisp.t) : bool = match (a, b) with (Int _ | Sym _), _ -> a = b | _ -> a == b

let arity (name : string) (n : int) (args : Lisp.t list) : unit =
  if List.length args <> n then signal "wrong-number-of-arguments" [ Sym name; Int (List.length args) ]

let one name f args = arity name 1 args; f (List.hd args)
let two name f args = arity name 2 args; f (List.nth args 0) (List.nth args 1)

let arith (name : string) (op : int -> int -> int) (unit_ : int) (args : Lisp.t list) : Lisp.t =
  match List.map int_of args with
  | [] -> Int unit_
  | [ x ] when name = "-" -> Int (-x)
  | x :: xs -> Int (List.fold_left (fun a b -> if (name = "/" || name = "%") && b = 0 then signal "arith-error" [] else op a b) x xs)

let compare_all (op : int -> int -> bool) (args : Lisp.t list) : Lisp.t =
  let rec ok = function a :: (b :: _ as rest) -> op a b && ok rest | _ -> true in
  of_bool (ok (List.map int_of args))

let find_cell (test : Lisp.t -> bool) (l : Lisp.t) : Lisp.t =
  let rec go = function Cons (y, _) as c when test y -> c | Cons (_, rest) -> go rest | _ -> nil in
  go l

let substring (args : Lisp.t list) : Lisp.t =
  match args with
  | s :: from :: rest ->
      let s = string_of s in
      let n = String.length s in
      let index v = let i = int_of v in if i < 0 then n + i else i in
      let i = index from and j = match rest with e :: _ when e <> nil -> index e | _ -> n in
      if i < 0 || j > n || i > j then signal "args-out-of-range" [ Str s; from ] else Str (String.sub s i (j - i))
  | _ -> signal "wrong-number-of-arguments" [ Sym "substring"; Int (List.length args) ]

(* format's directives: %s as princ writes, %S as prin1, %d, %c, %% *)
let format (args : Lisp.t list) : Lisp.t =
  match args with
  | [] -> signal "wrong-number-of-arguments" [ Sym "format"; Int 0 ]
  | fmt :: args ->
      let fmt = string_of fmt in
      let b = Buffer.create 32 in
      let rec go i args =
        if i >= String.length fmt then ()
        else if fmt.[i] = '%' && i + 1 < String.length fmt then begin
          let next () = match args with a :: rest -> (a, rest) | [] -> error "Not enough arguments for format string" in
          match fmt.[i + 1] with
          | '%' ->
              Buffer.add_char b '%';
              go (i + 2) args
          | ('s' | 'S' | 'd' | 'c') as d ->
              let a, rest = next () in
              Buffer.add_string b
                (match d with
                | 's' -> Lisp.princ a
                | 'S' -> Lisp.print a
                | 'd' -> string_of_int (int_of a)
                | _ -> String.make 1 (Char.chr (int_of a land 255)));
              go (i + 2) rest
          | c -> error (Printf.sprintf "Invalid format operation %%%c" c)
        end
        else begin
          Buffer.add_char b fmt.[i];
          go (i + 1) args
        end
      in
      go 0 args;
      Str (Buffer.contents b)

let case (f : char -> char) (v : Lisp.t) : Lisp.t =
  match v with Int c -> Int (Char.code (f (Char.chr (c land 255)))) | v -> Str (String.map f (string_of v))

let pure_builtins : (string * (Lisp.t list -> Lisp.t)) list =
  [ ("+", arith "+" ( + ) 0);
    ("-", arith "-" ( - ) 0);
    ("*", arith "*" ( * ) 1);
    ("/", arith "/" ( / ) 1);
    ("%", arith "%" ( mod ) 0);
    ("max", arith "max" max 0);
    ("min", arith "min" min 0);
    ("1+", one "1+" (fun v -> Int (int_of v + 1)));
    ("1-", one "1-" (fun v -> Int (int_of v - 1)));
    ("abs", one "abs" (fun v -> Int (abs (int_of v))));
    ("=", compare_all ( = ));
    ("/=", compare_all ( <> ));
    ("<", compare_all ( < ));
    (">", compare_all ( > ));
    ("<=", compare_all ( <= ));
    (">=", compare_all ( >= ));
    ("eq", two "eq" (fun a b -> of_bool (eq a b)));
    ("equal", two "equal" (fun a b -> of_bool (a = b)));
    ("null", one "null" (fun v -> of_bool (v = nil)));
    ("not", one "not" (fun v -> of_bool (v = nil)));
    ("cons", two "cons" (fun a b -> Cons (a, b)));
    ("car", one "car" (function Cons (a, _) -> a | Sym "nil" -> nil | v -> wrong_type "listp" v));
    ("cdr", one "cdr" (function Cons (_, d) -> d | Sym "nil" -> nil | v -> wrong_type "listp" v));
    ("car-safe", one "car-safe" (function Cons (a, _) -> a | _ -> nil));
    ("cdr-safe", one "cdr-safe" (function Cons (_, d) -> d | _ -> nil));
    ("list", list);
    ("length", one "length" (function Str s -> Int (String.length s) | v -> Int (List.length (list_of v))));
    ("nth", two "nth" (fun n l -> Option.value (List.nth_opt (list_of l) (int_of n)) ~default:nil));
    ("nthcdr", two "nthcdr" (fun n l -> list (List.filteri (fun i _ -> i >= int_of n) (list_of l))));
    ( "append",
      fun args ->
        match List.rev args with
        | [] -> nil
        | last :: firsts -> List.fold_left (fun acc l -> List.fold_right (fun x acc -> Cons (x, acc)) (list_of l) acc) last firsts );
    ("reverse", one "reverse" (fun l -> list (List.rev (list_of l))));
    ("memq", two "memq" (fun x l -> find_cell (eq x) l));
    ("member", two "member" (fun x l -> find_cell (( = ) x) l));
    ("assq", two "assq" (fun k l -> Option.value (List.find_opt (function Cons (y, _) -> eq k y | _ -> false) (list_of l)) ~default:nil));
    ("assoc", two "assoc" (fun k l -> Option.value (List.find_opt (function Cons (y, _) -> k = y | _ -> false) (list_of l)) ~default:nil));
    ("symbolp", one "symbolp" (function Sym _ -> t | _ -> nil));
    ("stringp", one "stringp" (function Str _ -> t | _ -> nil));
    ("integerp", one "integerp" (function Int _ -> t | _ -> nil));
    ("numberp", one "numberp" (function Int _ -> t | _ -> nil));
    ("consp", one "consp" (function Cons _ -> t | _ -> nil));
    ("atom", one "atom" (function Cons _ -> nil | _ -> t));
    ("listp", one "listp" (function Cons _ | Sym "nil" -> t | _ -> nil));
    ("concat", fun args -> Str (String.concat "" (List.map string_of args)));
    ("substring", substring);
    ("string=", two "string=" (fun a b -> of_bool (string_of a = string_of b)));
    ("string<", two "string<" (fun a b -> of_bool (string_of a < string_of b)));
    ("upcase", one "upcase" (case Char.uppercase_ascii));
    ("downcase", one "downcase" (case Char.lowercase_ascii));
    ("string-to-number", one "string-to-number" (fun s -> Int (Option.value (int_of_string_opt (String.trim (string_of s))) ~default:0)));
    ("number-to-string", one "number-to-string" (fun n -> Str (string_of_int (int_of n))));
    ("char-to-string", one "char-to-string" (fun c -> Str (String.make 1 (Char.chr (int_of c land 255)))));
    ("string-to-char", one "string-to-char" (fun s -> let s = string_of s in Int (if s = "" then 0 else Char.code s.[0])));
    ("make-string", two "make-string" (fun n c -> Str (String.make (max 0 (int_of n)) (Char.chr (int_of c land 255)))));
    ("symbol-name", one "symbol-name" (function Sym s -> Str s | v -> wrong_type "symbolp" v));
    ("intern", one "intern" (fun s -> Sym (string_of s)));
    ("format", format);
    ("prin1-to-string", one "prin1-to-string" (fun v -> Str (Lisp.print v)));
    ("identity", one "identity" Fun.id);
    ("error", fun args -> error (Lisp.princ (format args)));
    ("signal", two "signal" (fun sym data -> raise (Signal (Cons (sym, data)))));
    ( "read-from-string",
      (* (value . where the reading stopped), what a loop reading a
         file of definitions needs *)
      one "read-from-string" (fun s ->
          let s = string_of s in
          match Lisp_read.read s 0 with
          | v, pos -> Cons (v, Int pos)
          | exception Lisp_read.Error msg -> if Lisp_read.only_blank s 0 then signal "end-of-file" [] else signal "invalid-read-syntax" [ Str msg ]) ) ]

(* how deep the calls are: the OCaml stack's, not the Lisp state's, so
   a counter of our own (Emacs's max-lisp-eval-depth, 1600) *)
let depth = ref 0
let max_depth = 1000

(* nil, t and :keywords are constants, their own values *)
let constant (name : string) : bool = name = "nil" || name = "t" || (name <> "" && name.[0] = ':')

type 'h state = {
  vars : (string * Lisp.t) list;
  funs : (string * Lisp.t) list;
  subrs : (string * 'h subr) list;
  specials : (string * 'h subr) list;
  commands : (string * Lisp.t) list;
  docs : (string * string) list;
  specpdl : (string * Lisp.t option) list;
  host : 'h;
  fuel : int;
  (* the state when the last signal was raised: the one cell that is
     changed in place, since an OCaml exception can't carry a value of
     a type variable ('h) -- read by [protect] *)
  failed : 'h state option ref;
}

and 'h subr = 'h state -> Lisp.t list -> Lisp.t * 'h state

exception Error of Lisp.t

(* an association list's binding replaced *)
let add (k : string) (v : 'a) (l : (string * 'a) list) : (string * 'a) list = (k, v) :: List.remove_assoc k l

(* the signal, and the state it leaves: [st] and its changes up to it *)
let raise_error (st : 'h state) (data : Lisp.t) : 'a =
  st.failed := Some st;
  raise (Error data)

let protect (st : 'h state) (f : 'h state -> 'a * 'h state) : ('a, Lisp.t) result * 'h state =
  match f st with
  | v, st -> (Ok v, st)
  | exception Error data -> (Error data, Option.value !(st.failed) ~default:st)

(***************************************************************************)
(* Variables and functions *)
(***************************************************************************)

let get_var (st : 'h state) (name : string) : Lisp.t option = List.assoc_opt name st.vars
let set_var (name : string) (v : Lisp.t) (st : 'h state) : 'h state = { st with vars = add name v st.vars }

let rec function_of (st : 'h state) (name : string) : Lisp.t option =
  match List.assoc_opt name st.funs with Some (Sym alias) when alias <> "nil" -> function_of st alias | f -> f

let define_subr ?interactive ?doc name f st =
  let st = { st with subrs = add name f st.subrs; funs = add name (Subr name) st.funs } in
  let st = match interactive with Some spec -> { st with commands = add name (Str spec) st.commands } | None -> st in
  match doc with Some d -> { st with docs = add name d st.docs } | None -> st

let define_special name f st = { st with specials = add name f st.specials }

(* the dynamic binding of a let or a call: the old value saved on the
   specpdl, to be put back by [unbind] -- Emacs's specbind *)
let bind (name : string) (v : Lisp.t) (st : 'h state) : 'h state =
  if constant name then error ("Attempt to set a constant symbol: " ^ name)
  else { (set_var name v st) with specpdl = (name, get_var st name) :: st.specpdl }

(* ... and unbind_to: the specpdl popped down to [depth] entries,
   each saved value put back; after a body, and after an error that
   a condition-case catches *)
let rec unbind_to (depth : int) (st : 'h state) : 'h state =
  if List.length st.specpdl <= depth then st
  else
    match st.specpdl with
    | (name, old) :: rest ->
        let vars = match old with Some v -> add name v st.vars | None -> List.remove_assoc name st.vars in
        unbind_to depth { st with vars; specpdl = rest }
    | [] -> st

let rec body_of (f : Lisp.t) : Lisp.t list =
  match f with
  | Cons (Sym "lambda", Cons (_, body)) -> Option.value (to_list body) ~default:[]
  | Cons (Sym "macro", f) -> body_of f
  | _ -> []

let resolve (st : 'h state) (f : Lisp.t) : Lisp.t option = match f with Sym s -> function_of st s | f -> Some f

(* the docstring is skipped, then (interactive ...) if first *)
let interactive_spec (st : 'h state) (f : Lisp.t) : Lisp.t option =
  match resolve st f with
  | Some (Subr name) -> List.assoc_opt name st.commands
  | Some f ->
      let rec find = function
        | Str _ :: (_ :: _ as rest) -> find rest
        | Cons (Sym "interactive", args) :: _ -> Some (match args with Cons (spec, _) -> spec | _ -> nil)
        | _ -> None
      in
      find (body_of f)
  | None -> None

let documentation (st : 'h state) (f : Lisp.t) : string option =
  match resolve st f with
  | Some (Subr name) -> List.assoc_opt name st.docs
  | Some f -> ( match body_of f with Str doc :: _ :: _ -> Some doc | _ -> None)
  | None -> None

(* a built-in's signal becomes an Error with the state it was given *)
let guard (st : 'h state) (f : unit -> Lisp.t * 'h state) : Lisp.t * 'h state = try f () with Signal data -> raise_error st data

(***************************************************************************)
(* Eval *)
(***************************************************************************)

let symbols (v : Lisp.t) : string list =
  match to_list v with Some xs -> List.map (function Sym s -> s | x -> wrong_type "symbolp" x) xs | None -> wrong_type "listp" v

let rec eval (st : 'h state) (e : Lisp.t) : Lisp.t * 'h state =
  guard st @@ fun () ->
  if st.fuel <= 0 then error "Lisp ran too long (a real Emacs would wait for C-g)";
  let st = { st with fuel = st.fuel - 1 } in
  match e with
  | Int _ | Str _ | Subr _ -> (e, st)
  | Sym name when constant name -> (e, st)
  | Sym name -> ( match get_var st name with Some v -> (v, st) | None -> signal "void-variable" [ e ])
  | Cons (Sym name, args) -> (
      let args = match to_list args with Some a -> a | None -> wrong_type "listp" args in
      match special name with
      | Some form -> form st args
      | None -> (
          match List.assoc_opt name st.specials with
          | Some form -> form st args
          | None -> (
              match function_of st name with
              | Some (Cons (Sym "macro", f)) ->
                  let expansion, st = apply st f args in
                  eval st expansion
              | Some f ->
                  let values, st = eval_list st args in
                  apply st f values
              | None -> signal "void-function" [ Sym name ])))
  | Cons (f, args) ->
      (* ((lambda (x) ...) 3): a function written in place *)
      let values, st = eval_list st (Option.value (to_list args) ~default:[]) in
      apply st f values

and eval_list (st : 'h state) (es : Lisp.t list) : Lisp.t list * 'h state =
  let vs, st = List.fold_left (fun (acc, st) e -> let v, st = eval st e in (v :: acc, st)) ([], st) es in
  (List.rev vs, st)

and progn (st : 'h state) (es : Lisp.t list) : Lisp.t * 'h state = List.fold_left (fun (_, st) e -> eval st e) (nil, st) es

and apply (st : 'h state) (f : Lisp.t) (args : Lisp.t list) : Lisp.t * 'h state =
  guard st @@ fun () ->
  match f with
  | Sym name -> ( match function_of st name with Some f -> apply st f args | None -> signal "void-function" [ f ])
  | Subr name -> (List.assoc name st.subrs) st args
  | Cons (Sym "lambda", Cons (params, body)) ->
      (* the parameters bound to the arguments: &optional ones nil
         when missing, &rest the list of those left *)
      let wrong () = signal "wrong-number-of-arguments" [ f; Int (List.length args) ] in
      let rec pair params args optional =
        match (params, args) with
        | [], [] -> []
        | [], _ -> wrong ()
        | "&optional" :: ps, _ -> pair ps args true
        | "&rest" :: p :: _, _ -> [ (p, list args) ]
        | p :: ps, a :: rest -> (p, a) :: pair ps rest optional
        | p :: ps, [] -> if optional then (p, nil) :: pair ps [] optional else wrong ()
      in
      let bindings = pair (symbols params) args false in
      if !depth >= max_depth then error "Lisp nesting exceeds max-lisp-eval-depth";
      incr depth;
      Fun.protect ~finally:(fun () -> decr depth) @@ fun () ->
      let outer = List.length st.specpdl in
      let st = List.fold_left (fun st (p, v) -> bind p v st) st bindings in
      let v, st = progn st (Option.value (to_list body) ~default:[]) in
      (v, unbind_to outer st)
  | _ -> signal "invalid-function" [ f ]

(***************************************************************************)
(* Special forms *)
(***************************************************************************)

(* the forms whose arguments eval doesn't evaluate first *)
and special (name : string) : 'h subr option =
  match name with
  | "quote" | "function" -> Some (fun st args -> match args with [ x ] -> (x, st) | _ -> error (name ^ " takes one argument"))
  | "lambda" -> Some (fun st args -> (Cons (Sym "lambda", list args), st))
  | "if" ->
      Some
        (fun st args ->
          match args with
          | c :: yes :: no ->
              let v, st = eval st c in
              if truthy v then eval st yes else progn st no
          | _ -> error "if needs a condition and a then")
  | "cond" ->
      Some
        (fun st clauses ->
          let rec go st = function
            | [] -> (nil, st)
            | clause :: rest -> (
                match to_list clause with
                | Some (c :: body) ->
                    let v, st = eval st c in
                    if not (truthy v) then go st rest else if body = [] then (v, st) else progn st body
                | _ -> error "a cond clause is (condition body...)")
          in
          go st clauses)
  | "and" -> Some (fun st args -> List.fold_left (fun (v, st) e -> if truthy v then eval st e else (v, st)) (t, st) args)
  | "or" -> Some (fun st args -> List.fold_left (fun (v, st) e -> if truthy v then (v, st) else eval st e) (nil, st) args)
  | "progn" -> Some progn
  | "prog1" ->
      Some
        (fun st args ->
          match args with
          | first :: rest ->
              let v, st = eval st first in
              (v, snd (progn st rest))
          | [] -> error "prog1 needs a form")
  | "while" ->
      Some
        (fun st args ->
          match args with
          | c :: body ->
              let rec loop st =
                let v, st = eval st c in
                if truthy v then loop (snd (progn st body)) else (nil, st)
              in
              loop st
          | [] -> error "while needs a condition")
  | "setq" ->
      Some
        (fun st args ->
          let rec go v st = function
            | Sym name :: e :: rest ->
                if constant name then error ("Attempt to set a constant symbol: " ^ name);
                let v, st = eval st e in
                go v (set_var name v st) rest
            | [] -> (v, st)
            | _ -> error "setq takes symbols and values, in pairs"
          in
          go nil st args)
  | "let" | "let*" ->
      Some
        (fun st args ->
          match args with
          | specs :: body ->
              let specs =
                List.map
                  (function
                    | Sym s | Cons (Sym s, Sym "nil") -> (s, nil) | Cons (Sym s, Cons (e, _)) -> (s, e) | x -> wrong_type "symbolp" x)
                  (list_of specs)
              in
              let outer = List.length st.specpdl in
              (* let evaluates every value first, then binds; let* binds
                 each before evaluating the next *)
              let st =
                if name = "let" then
                  let values, st = eval_list st (List.map snd specs) in
                  List.fold_left2 (fun st (s, _) v -> bind s v st) st specs values
                else List.fold_left (fun st (s, e) -> let v, st = eval st e in bind s v st) st specs
              in
              let v, st = progn st body in
              (v, unbind_to outer st)
          | [] -> error "let needs its bindings")
  | "defun" | "defmacro" ->
      Some
        (fun st args ->
          match args with
          | Sym fname :: params :: body ->
              let f = Cons (Sym "lambda", Cons (params, list body)) in
              let f = if name = "defmacro" then Cons (Sym "macro", f) else f in
              (Sym fname, { st with funs = add fname f st.funs })
          | _ -> error (name ^ " needs a name and parameters"))
  | "defvar" | "defconst" ->
      Some
        (fun st args ->
          match args with
          | Sym v :: rest -> (
              (* defvar doesn't change a variable already set: a user's
                 setting in .emacs survives the package's loading *)
              match rest with
              | e :: _ when name = "defconst" || get_var st v = None ->
                  let value, st = eval st e in
                  (Sym v, set_var v value st)
              | _ -> (Sym v, st))
          | _ -> error (name ^ " needs a symbol"))
  | "interactive" -> Some (fun st _ -> (nil, st))
  | "condition-case" ->
      Some
        (fun st args ->
          match args with
          | var :: body :: handlers -> (
              let outer = List.length st.specpdl in
              try eval st body
              with Error data as exn -> (
                let st_err = Option.value !(st.failed) ~default:st in
                let sym = match data with Cons (Sym s, _) -> s | _ -> "error" in
                (* a handler catches its symbol, and error catches them all *)
                let catches = function Cons (Sym ("error" | "t"), _) -> true | Cons (Sym s, _) -> s = sym | _ -> false in
                match List.find_opt catches handlers with
                | None -> raise exn
                | Some handler -> (
                    (* the changes before the error kept, the lets
                       inside the body undone *)
                    let st = unbind_to outer st_err in
                    let body = match handler with Cons (_, b) -> list_of b | _ -> [] in
                    match var with
                    | Sym v when v <> "nil" ->
                        let r, st = progn (bind v data st) body in
                        (r, unbind_to outer st)
                    | _ -> progn st body)))
          | _ -> error "condition-case needs a variable and a body")
  | _ -> None

(***************************************************************************)
(* The built-in functions over the state *)
(***************************************************************************)

let with_state : (string * 'h subr) list =
  [ ("boundp", fun st args -> (one "boundp" (function Sym s -> of_bool (constant s || get_var st s <> None) | _ -> nil) args, st));
    ("fboundp", fun st args -> (one "fboundp" (function Sym s -> of_bool (function_of st s <> None) | _ -> nil) args, st));
    ( "symbol-value",
      fun st args ->
        ( one "symbol-value"
            (function
              | Sym s as v -> ( match get_var st s with Some x -> x | None -> if constant s then v else signal "void-variable" [ v ])
              | v -> wrong_type "symbolp" v)
            args,
          st ) );
    ("symbol-function", fun st args -> (one "symbol-function" (function Sym s -> Option.value (List.assoc_opt s st.funs) ~default:nil | v -> wrong_type "symbolp" v) args, st));
    ( "set",
      fun st args ->
        match args with
        | [ Sym s; v ] -> if constant s then error ("Attempt to set a constant symbol: " ^ s) else (v, set_var s v st)
        | _ -> wrong_type "symbolp" (list args) );
    ("fset", fun st args -> match args with [ Sym s; f ] -> (f, { st with funs = add s f st.funs }) | _ -> wrong_type "symbolp" (list args));
    ("eval", fun st args -> arity "eval" 1 args; eval st (List.hd args));
    ("funcall", fun st args -> match args with f :: rest -> apply st f rest | [] -> signal "wrong-number-of-arguments" [ Sym "funcall"; Int 0 ]);
    ( "apply",
      fun st args ->
        match List.rev args with
        | last :: firsts -> ( match List.rev firsts with f :: rest -> apply st f (rest @ list_of last) | [] -> apply st last [])
        | [] -> signal "wrong-number-of-arguments" [ Sym "apply"; Int 0 ] );
    ( "mapcar",
      fun st args ->
        arity "mapcar" 2 args;
        let f = List.nth args 0 in
        let vs, st = List.fold_left (fun (acc, st) x -> let v, st = apply st f [ x ] in (v :: acc, st)) ([], st) (list_of (List.nth args 1)) in
        (list (List.rev vs), st) );
    ("commandp", fun st args -> arity "commandp" 1 args; (of_bool (interactive_spec st (List.hd args) <> None), st));
    ("documentation", fun st args -> arity "documentation" 1 args; ((match documentation st (List.hd args) with Some d -> Str d | None -> nil), st)) ]

let load (st : 'h state) (text : string) : 'h state =
  let forms = try Lisp_read.read_all text with Lisp_read.Error msg -> raise_error st (Cons (Sym "invalid-read-syntax", list [ Str msg ])) in
  List.fold_left (fun st form -> snd (eval st form)) st forms

let create ?(fuel = 1_000_000) (host : 'h) : 'h state =
  let st =
    { vars = []; funs = []; subrs = []; specials = []; commands = []; docs = []; specpdl = []; host; fuel; failed = ref None }
  in
  let st = List.fold_left (fun st (name, f) -> define_subr name (fun st args -> (f args, st)) st) st pure_builtins in
  let st = List.fold_left (fun st (name, f) -> define_subr name f st) st with_state in
  load st Lisp_prelude.text
