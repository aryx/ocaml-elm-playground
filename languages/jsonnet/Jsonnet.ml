(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jsonnet.mli *)

open Jsonnet_ast
module Smap = Map.Make (String)

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

type value =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | Arr of value Lazy.t array
  | Obj of obj
  | Fun of func

and func =
  | Closure of env * param list * expr
  (* its name, its parameters and their defaults, what it does *)
  | Builtin of string * (string * value option) list * (value Lazy.t array -> value)

(* an object: its layers, the lowest first; its fields computed, kept *)
and obj = { layers : layer array; cache : (string, value) Hashtbl.t; mutable checked : bool }

(* a layer: its fields, its asserts, and whether it is an outermost
 * object's (its $ the whole object then) *)
and layer = { fields : (string * fdef) list; asserts : (env * (string * expr) list * expr * expr option) list; outer : bool }

(* a field: its visibility, its +:, and its code with the object's locals,
 * or a value (std's) *)
and fdef = { vis : visibility; plus : bool; body : body }
and body = Code of env * (string * expr) list * expr | Const of value

(* what a piece of code sees: the variables, and self, the layers under
 * [upto] being super, and $ *)
and env = { vars : value Lazy.t Smap.t; self : obj option; upto : int; dollar : obj option }

exception Runtime of string

let fail fmt = Printf.ksprintf (fun s -> raise (Runtime s)) fmt

(* where the evaluation is, for the errors *)
let file = ref ""
let line = ref 0

let type_name = function
  | Null -> "null"
  | Bool _ -> "boolean"
  | Num _ -> "number"
  | Str _ -> "string"
  | Arr _ -> "array"
  | Obj _ -> "object"
  | Fun _ -> "function"

let new_obj layers = { layers; cache = Hashtbl.create 8; checked = false }

(* all the fields' names, visible or not, sorted *)
let field_names (o : obj) : string list = List.sort_uniq compare (List.concat_map (fun l -> List.map fst l.fields) (Array.to_list o.layers))

(* a field shown in the output: the highest layer's :: or ::: that has
 * it, else shown *)
let visible (o : obj) (name : string) : bool =
  let rec go i =
    if i < 0 then true
    else match List.assoc_opt name o.layers.(i).fields with Some { vis = Hidden; _ } -> false | Some { vis = Visible; _ } -> true | _ -> go (i - 1)
  in
  go (Array.length o.layers - 1)

let has (o : obj) ~(upto : int) (name : string) : bool =
  let rec go i = i >= 0 && (List.mem_assoc name o.layers.(i).fields || go (i - 1)) in
  go (upto - 1)

(* numbers as jsonnet prints them: integers without a point *)
let show_num (n : float) : string =
  if Float.is_integer n && Float.abs n < 1e15 then Printf.sprintf "%.0f" n
  else
    let s = Printf.sprintf "%.17g" n in
    if float_of_string (Printf.sprintf "%.15g" n) = n then Printf.sprintf "%.15g" n else s

let to_int (what : string) (v : value) : int =
  match v with Num n when Float.is_integer n -> int_of_float n | Num n -> fail "%s: %g is not an integer" what n | v -> fail "%s: a number expected, not a %s" what (type_name v)

(*****************************************************************************)
(* Evaluation *)
(*****************************************************************************)

(* the imports read, and how: set by [eval] *)
let reader : (string -> string option) ref = ref (fun _ -> None)
let imported : (string, value) Hashtbl.t = Hashtbl.create 8
let root : env ref = ref { vars = Smap.empty; self = None; upto = 0; dollar = None }
let depth = ref 0

(* the locals of an object, each seeing the others and self *)
let with_locals (env : env) (locals : (string * expr) list) (eval : env -> expr -> value) : env =
  if locals = [] then env
  else
    let r = ref env in
    let vars = List.fold_left (fun m (x, e) -> Smap.add x (lazy (eval !r e)) m) env.vars locals in
    r := { env with vars };
    !r

let rec eval (env : env) (e : expr) : value =
  match e with
  | Null -> Null
  | Bool b -> Bool b
  | Num n -> Num n
  | Str s -> Str s
  | At (l, e) ->
      line := l;
      eval env e
  | Self -> ( match env.self with Some o -> Obj o | None -> fail "self outside an object")
  | Dollar -> ( match env.dollar with Some o -> Obj o | None -> fail "$ outside an object")
  | Var x -> ( match Smap.find_opt x env.vars with Some v -> Lazy.force v | None -> fail "unknown variable %s" x)
  | Array es -> Arr (Array.of_list (List.map (fun e -> lazy (eval env e)) es))
  | Array_comp (body, comps) -> Arr (Array.of_list (List.map (fun env -> lazy (eval env body)) (comprehend env comps)))
  | Object members -> Obj (new_obj [| layer_of env members |])
  | Object_comp (locals, k, v, comps) ->
      let fields =
        List.fold_left
          (fun acc env' ->
            match eval env' k with
            | Null -> acc
            | Str name ->
                if List.mem_assoc name acc then fail "duplicate field %s" name;
                (name, { vis = Default; plus = false; body = Code (env', locals, v) }) :: acc
            | x -> fail "a field's name must be a string, not a %s" (type_name x))
          [] (comprehend env comps)
      in
      Obj (new_obj [| { fields = List.rev fields; asserts = []; outer = env.dollar = None } |])
  | Field (e, f) -> (
      match eval env e with
      | Obj o -> field o f
      | v -> fail "a field %s of a %s" f (type_name v))
  | Index (e, i) -> index (eval env e) (eval env i)
  | Slice (e, a, b, c) -> slice env (eval env e) a b c
  | Super_field f -> super env f
  | Super_index i -> ( match eval env i with Str f -> super env f | v -> fail "super[...]: a string expected, not a %s" (type_name v))
  | In_super e -> (
      match (eval env e, env.self) with
      | Str f, Some o -> Bool (has o ~upto:env.upto f)
      | Str _, None -> Bool false
      | v, _ -> fail "in super: a string expected, not a %s" (type_name v))
  | Call (f, pos, named) -> (
      match eval env f with
      | Fun fn -> apply fn (List.map (fun e -> lazy (eval env e)) pos) (List.map (fun (x, e) -> (x, lazy (eval env e))) named)
      | v -> fail "a %s called, not a function" (type_name v))
  | Local (binds, body) ->
      let r = ref env in
      let vars = List.fold_left (fun m (x, e) -> Smap.add x (lazy (eval !r e)) m) env.vars binds in
      r := { env with vars };
      eval !r body
  | If (c, a, b) -> (
      match eval env c with
      | Bool true -> eval env a
      | Bool false -> ( match b with Some b -> eval env b | None -> Null)
      | v -> fail "if: a boolean expected, not a %s" (type_name v))
  | Binary ("&&", a, b) -> ( match eval env a with Bool false -> Bool false | Bool true -> boolean "&&" (eval env b) | v -> fail "&&: a %s" (type_name v))
  | Binary ("||", a, b) -> ( match eval env a with Bool true -> Bool true | Bool false -> boolean "||" (eval env b) | v -> fail "||: a %s" (type_name v))
  | Binary (op, a, b) -> binary op (eval env a) (eval env b)
  | Unary (op, a) -> (
      match (op, eval env a) with
      | "-", Num n -> Num (-.n)
      | "+", Num n -> Num n
      | "!", Bool b -> Bool (not b)
      | "~", Num n -> Num (Int64.to_float (Int64.lognot (Int64.of_float n)))
      | _, v -> fail "%s on a %s" op (type_name v))
  | Function (ps, body) -> Fun (Closure (env, ps, body))
  | Assert (c, msg, body) -> (
      match eval env c with
      | Bool true -> eval env body
      | Bool false -> fail "assertion failed%s" (match msg with Some m -> ": " ^ string_of (eval env m) | None -> "")
      | v -> fail "assert: a boolean expected, not a %s" (type_name v))
  | Error e -> raise (Runtime (string_of (eval env e)))
  | Import path -> (
      match Hashtbl.find_opt imported path with
      | Some v -> v
      | None -> (
          match !reader path with
          | None -> fail "cannot import %s" path
          | Some text -> (
              match Jsonnet_parse.parse ~path text with
              | Error (l, msg) -> fail "%s:%d: %s" path l msg
              | Ok e ->
                  let saved_file = !file and saved_line = !line in
                  file := path;
                  let v = eval !root e in
                  file := saved_file;
                  line := saved_line;
                  Hashtbl.replace imported path v;
                  v)))
  | Importstr path -> ( match !reader path with Some text -> Str text | None -> fail "cannot import %s" path)

and boolean op = function Bool b -> Bool b | v -> fail "%s: a boolean expected, not a %s" op (type_name v)

(* an object literal's layer: its fields' names computed, its locals and
 * asserts kept for each field's code *)
and layer_of (env : env) (members : member list) : layer =
  let locals = List.filter_map (function Local_m (x, e) -> Some (x, e) | _ -> None) members in
  let fields =
    List.fold_left
      (fun acc m ->
        match m with
        | Field_m (name, plus, vis, e) -> (
            let name = match name with Fixed s -> Some s | Computed k -> ( match eval env k with Str s -> Some s | Null -> None | v -> fail "a field's name must be a string, not a %s" (type_name v)) in
            match name with
            | None -> acc
            | Some s ->
                if List.mem_assoc s acc then fail "duplicate field %s" s;
                (s, { vis; plus; body = Code (env, locals, e) }) :: acc)
        | _ -> acc)
      [] members
  in
  let asserts = List.filter_map (function Assert_m (c, m) -> Some (env, locals, c, m) | _ -> None) members in
  { fields = List.rev fields; asserts; outer = env.dollar = None }

(* what a field's code sees: self the whole object, super the layers
 * under its own, $, and the object's locals *)
and field_env (o : obj) (i : int) (env : env) (locals : (string * expr) list) : env =
  let env = { env with self = Some o; upto = i; dollar = (if o.layers.(i).outer then Some o else env.dollar) } in
  with_locals env locals eval

(* a field of [o], the highest layer's under [upto]; +: adds it to the
 * one under *)
and get (o : obj) ~(upto : int) (name : string) : value option =
  let rec find i = if i < 0 then None else match List.assoc_opt name o.layers.(i).fields with Some f -> Some (i, f) | None -> find (i - 1) in
  match find (upto - 1) with
  | None -> None
  | Some (i, f) ->
      let v = match f.body with Const v -> v | Code (env, locals, e) -> eval (field_env o i env locals) e in
      if f.plus then Some (match get o ~upto:i name with Some below -> binary "+" below v | None -> v) else Some v

and field (o : obj) (name : string) : value =
  match Hashtbl.find_opt o.cache name with
  | Some v -> v
  | None -> (
      match get o ~upto:(Array.length o.layers) name with
      | Some v ->
          Hashtbl.replace o.cache name v;
          v
      | None -> fail "field does not exist: %s" name)

and super (env : env) (name : string) : value =
  match env.self with
  | None -> fail "super outside an object"
  | Some o -> ( match get o ~upto:env.upto name with Some v -> v | None -> fail "field does not exist in super: %s" name)

(* the environments a comprehension's fors and ifs give *)
and comprehend (env : env) (comps : comp list) : env list =
  match comps with
  | [] -> [ env ]
  | For (x, e) :: rest -> (
      match eval env e with
      | Arr a -> List.concat_map (fun v -> comprehend { env with vars = Smap.add x v env.vars } rest) (Array.to_list a)
      | v -> fail "for %s in: an array expected, not a %s" x (type_name v))
  | If_comp c :: rest -> ( match eval env c with Bool true -> comprehend env rest | Bool false -> [] | v -> fail "if: a %s" (type_name v))

and index (v : value) (i : value) : value =
  match (v, i) with
  | Arr a, Num _ ->
      let k = to_int "an index" i in
      if k < 0 || k >= Array.length a then fail "index %d out of bounds (%d elements)" k (Array.length a);
      Lazy.force a.(k)
  | Str s, Num _ ->
      let k = to_int "an index" i in
      if k < 0 || k >= String.length s then fail "index %d out of bounds (%d characters)" k (String.length s);
      Str (String.make 1 s.[k])
  | Obj o, Str f -> field o f
  | _ -> fail "a %s indexed by a %s" (type_name v) (type_name i)

and slice env v a b c =
  let num what = function Some e -> Some (to_int what (eval env e)) | None -> None in
  let a = num "a slice's start" a and b = num "a slice's end" b and c = num "a slice's step" c in
  let pick len = let a = Option.value a ~default:0 and b = Option.value b ~default:len and c = Option.value c ~default:1 in
    let a = max 0 (min len a) and b = max 0 (min len b) in
    if c <= 0 then fail "a slice's step must be positive";
    List.filter (fun k -> (k - a) mod c = 0) (List.init (max 0 (b - a)) (fun k -> a + k))
  in
  match v with
  | Arr arr -> Arr (Array.of_list (List.map (fun k -> arr.(k)) (pick (Array.length arr))))
  | Str s -> Str (String.concat "" (List.map (fun k -> String.make 1 s.[k]) (pick (String.length s))))
  | v -> fail "a slice of a %s" (type_name v)

and binary (op : string) (a : value) (b : value) : value =
  let int x = Int64.of_float x and flt x = Int64.to_float x in
  match (op, a, b) with
  | "+", Num x, Num y -> Num (x +. y)
  | "+", Str x, Str y -> Str (x ^ y)
  | "+", Str x, y -> Str (x ^ string_of y)
  | "+", x, Str y -> Str (string_of x ^ y)
  | "+", Arr x, Arr y -> Arr (Array.append x y)
  | "+", Obj x, Obj y -> Obj (new_obj (Array.append x.layers y.layers))
  | "-", Num x, Num y -> Num (x -. y)
  | "*", Num x, Num y -> Num (x *. y)
  | "/", Num x, Num y -> if y = 0. then fail "division by zero" else Num (x /. y)
  | "%", Num x, Num y -> if y = 0. then fail "division by zero" else Num (Float.rem x y)
  | "%", Str fmt, v -> Str (format fmt v)
  | ("<" | "<=" | ">" | ">="), _, _ ->
      let c = compare_values a b in
      Bool (match op with "<" -> c < 0 | "<=" -> c <= 0 | ">" -> c > 0 | _ -> c >= 0)
  | "==", _, _ -> Bool (equal a b)
  | "!=", _, _ -> Bool (not (equal a b))
  | "in", Str f, Obj o -> Bool (has o ~upto:(Array.length o.layers) f)
  | "&", Num x, Num y -> Num (flt (Int64.logand (int x) (int y)))
  | "|", Num x, Num y -> Num (flt (Int64.logor (int x) (int y)))
  | "^", Num x, Num y -> Num (flt (Int64.logxor (int x) (int y)))
  | "<<", Num x, Num y -> Num (flt (Int64.shift_left (int x) (int_of_float y)))
  | ">>", Num x, Num y -> Num (flt (Int64.shift_right (int x) (int_of_float y)))
  | _ -> fail "%s between a %s and a %s" op (type_name a) (type_name b)

and compare_values (a : value) (b : value) : int =
  match (a, b) with
  | Num x, Num y -> compare x y
  | Str x, Str y -> compare x y
  | Arr x, Arr y ->
      let n = min (Array.length x) (Array.length y) in
      let rec go k = if k = n then compare (Array.length x) (Array.length y) else let c = compare_values (Lazy.force x.(k)) (Lazy.force y.(k)) in if c <> 0 then c else go (k + 1) in
      go 0
  | _ -> fail "comparing a %s and a %s" (type_name a) (type_name b)

and equal (a : value) (b : value) : bool =
  match (a, b) with
  | Null, Null -> true
  | Bool x, Bool y -> x = y
  | Num x, Num y -> x = y
  | Str x, Str y -> x = y
  | Arr x, Arr y -> Array.length x = Array.length y && Array.for_all2 (fun p q -> equal (Lazy.force p) (Lazy.force q)) x y
  | Obj x, Obj y ->
      let shown o = List.filter (visible o) (field_names o) in
      let fx = shown x and fy = shown y in
      fx = fy && List.for_all (fun f -> equal (field x f) (field y f)) fx
  | Fun _, _ | _, Fun _ -> fail "comparing functions"
  | _ -> false

and apply (fn : func) (pos : value Lazy.t list) (named : (string * value Lazy.t) list) : value =
  incr depth;
  if !depth > 2000 then (depth := 0; fail "too deep: a recursion that does not end?");
  let v =
    match fn with
    | Closure (env, ps, body) ->
        if List.length pos > List.length ps then fail "too many arguments: %d for %d" (List.length pos) (List.length ps);
        let r = ref env in
        let vars =
          List.fold_left
            (fun (m, k) (x, default) ->
              let v =
                match List.nth_opt pos k with
                | Some v -> v
                | None -> (
                    match (List.assoc_opt x named, default) with
                    | Some v, _ -> v
                    | None, Some d -> lazy (eval !r d)
                    | None, None -> fail "missing argument %s" x)
              in
              (Smap.add x v m, k + 1))
            (env.vars, 0) ps
          |> fst
        in
        List.iter (fun (x, _) -> if not (List.mem_assoc x ps) then fail "no parameter %s" x) named;
        r := { env with vars };
        eval !r body
    | Builtin (name, ps, f) ->
        let args =
          Array.of_list
            (List.mapi
               (fun k (x, default) ->
                 match List.nth_opt pos k with
                 | Some v -> v
                 | None -> (
                     match (List.assoc_opt x named, default) with
                     | Some v, _ -> v
                     | None, Some d -> Lazy.from_val d
                     | None, None -> fail "std.%s: missing argument %s" name x))
               ps)
        in
        if List.length pos > List.length ps then fail "std.%s: too many arguments" name;
        f args
  in
  decr depth;
  v

(*****************************************************************************)
(* Output *)
(*****************************************************************************)

(* an object's asserts, once, before it is shown *)
and check (o : obj) : unit =
  if not o.checked then begin
    o.checked <- true;
    Array.iteri
      (fun i l ->
        List.iter
          (fun (env, locals, c, m) ->
            match eval (field_env o i env locals) c with
            | Bool true -> ()
            | Bool false -> fail "object assertion failed%s" (match m with Some m -> ": " ^ string_of (eval (field_env o i env locals) m) | None -> "")
            | v -> fail "assert: a boolean expected, not a %s" (type_name v))
          l.asserts)
      o.layers
  end

and manifest (v : value) : Json.t =
  match v with
  | Null -> Json.Null
  | Bool b -> Json.Bool b
  | Num n -> Json.Number n
  | Str s -> Json.String s
  | Arr a -> Json.Array (List.map (fun v -> manifest (Lazy.force v)) (Array.to_list a))
  | Obj o ->
      check o;
      Json.Object (List.filter_map (fun f -> if visible o f then Some (f, manifest (field o f)) else None) (field_names o))
  | Fun _ -> fail "a function cannot be manifested"

(* a value as text: a string itself, anything else its JSON *)
and string_of (v : value) : string = match v with Str s -> s | v -> json_text (manifest v)

and json_text (j : Json.t) : string =
  match j with
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> show_num n
  | String s -> quote s
  | Array [] -> "[ ]"
  | Array js -> "[" ^ String.concat ", " (List.map json_text js) ^ "]"
  | Object [] -> "{ }"
  | Object fs -> "{" ^ String.concat ", " (List.map (fun (k, v) -> quote k ^ ": " ^ json_text v) fs) ^ "}"

and quote (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\t' -> Buffer.add_string b "\\t"
      | '\r' -> Buffer.add_string b "\\r"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* Python's %, as std.format: %s %d %i %f %e %g %x %o %c %%, flags - 0 +
 * and space, a width, a precision; %(name)s takes an object's field *)
and format (fmt : string) (vals : value) : string =
  let args = ref (match vals with Arr a -> List.map Lazy.force (Array.to_list a) | Obj _ -> [] | v -> [ v ]) in
  let next_arg () = match !args with v :: rest -> args := rest; v | [] -> fail "format: not enough values" in
  let b = Buffer.create (String.length fmt) in
  let n = String.length fmt in
  let i = ref 0 in
  while !i < n do
    if fmt.[!i] <> '%' then (Buffer.add_char b fmt.[!i]; incr i)
    else begin
      incr i;
      if !i >= n then fail "format: a %% at the end";
      if fmt.[!i] = '%' then (Buffer.add_char b '%'; incr i)
      else begin
        let keyed = fmt.[!i] = '(' in
        let arg =
          if keyed then begin
            let j = try String.index_from fmt !i ')' with Not_found -> fail "format: %%( not closed" in
            let key = String.sub fmt (!i + 1) (j - !i - 1) in
            i := j + 1;
            match vals with Obj o -> field o key | _ -> fail "format: %%(%s) needs an object" key
          end
          else Null
        in
        let flags = Buffer.create 4 in
        while !i < n && String.contains "-0+ #" fmt.[!i] do Buffer.add_char flags fmt.[!i]; incr i done;
        let digits () = let st = !i in while !i < n && fmt.[!i] >= '0' && fmt.[!i] <= '9' do incr i done; if !i > st then Some (int_of_string (String.sub fmt st (!i - st))) else None in
        let width = digits () in
        let prec = if !i < n && fmt.[!i] = '.' then (incr i; Some (Option.value (digits ()) ~default:0)) else None in
        if !i >= n then fail "format: a conversion expected";
        let conv = fmt.[!i] in
        incr i;
        let arg = if keyed then arg else match vals with Obj _ -> fail "format: an object needs %%(name)" | _ -> next_arg () in
        let flags = Buffer.contents flags in
        let left = String.contains flags '-' and zero = String.contains flags '0' in
        let sign x s = if x >= 0. && String.contains flags '+' then "+" ^ s else if x >= 0. && String.contains flags ' ' then " " ^ s else s in
        let num () = match arg with Num x -> x | v -> fail "format: %%%c needs a number, not a %s" conv (type_name v) in
        let body =
          match conv with
          | 's' -> string_of arg
          | 'd' | 'i' | 'u' -> let x = Float.trunc (num ()) in sign x (Printf.sprintf "%.0f" x)
          | 'f' | 'F' -> let x = num () in sign x (Printf.sprintf "%.*f" (Option.value prec ~default:6) x)
          | 'e' | 'E' -> let x = num () in sign x (Printf.sprintf (if conv = 'e' then "%.*e" else "%.*E") (Option.value prec ~default:6) x)
          | 'g' | 'G' -> let x = num () in sign x (Printf.sprintf (if conv = 'g' then "%.*g" else "%.*G") (Option.value prec ~default:6) x)
          | 'x' -> Printf.sprintf "%Lx" (Int64.of_float (num ()))
          | 'X' -> Printf.sprintf "%LX" (Int64.of_float (num ()))
          | 'o' -> Printf.sprintf "%Lo" (Int64.of_float (num ()))
          | 'c' -> ( match arg with Num x -> String.make 1 (Char.chr (int_of_float x land 255)) | Str s -> s | v -> fail "format: %%c of a %s" (type_name v))
          | c -> fail "format: an unknown conversion %%%c" c
        in
        let body = match (conv, prec) with 's', Some p when String.length body > p -> String.sub body 0 p | _ -> body in
        let pad = match width with Some w when w > String.length body -> w - String.length body | _ -> 0 in
        if left then (Buffer.add_string b body; Buffer.add_string b (String.make pad ' '))
        else if zero && conv <> 's' then begin
          (* the zeros after the sign *)
          if String.length body > 0 && (body.[0] = '-' || body.[0] = '+' || body.[0] = ' ') then (Buffer.add_char b body.[0]; Buffer.add_string b (String.make pad '0'); Buffer.add_string b (String.sub body 1 (String.length body - 1)))
          else (Buffer.add_string b (String.make pad '0'); Buffer.add_string b body)
        end
        else (Buffer.add_string b (String.make pad ' '); Buffer.add_string b body)
      end
    end
  done;
  if !args <> [] && (match vals with Arr _ -> true | _ -> false) then fail "format: too many values";
  Buffer.contents b

(*****************************************************************************)
(* std *)
(*****************************************************************************)

let call (f : value) (args : value list) : value =
  match f with Fun fn -> apply fn (List.map Lazy.from_val args) [] | v -> fail "a function expected, not a %s" (type_name v)

let arr_of what = function Arr a -> Array.to_list (Array.map Lazy.force a) | v -> fail "std.%s: an array expected, not a %s" what (type_name v)
let str_of what = function Str s -> s | v -> fail "std.%s: a string expected, not a %s" what (type_name v)
let obj_of what = function Obj o -> o | v -> fail "std.%s: an object expected, not a %s" what (type_name v)
let num_of what = function Num n -> n | v -> fail "std.%s: a number expected, not a %s" what (type_name v)
let arr (vs : value list) : value = Arr (Array.of_list (List.map Lazy.from_val vs))

let starts_with ~prefix s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

(* [s] cut at each [sep] *)
let split_on (s : string) (sep : string) : string list =
  if sep = "" then fail "std.split: an empty separator";
  let n = String.length s and m = String.length sep in
  let rec go st i acc =
    if i > n - m then List.rev (String.sub s st (n - st) :: acc)
    else if String.sub s i m = sep then go (i + m) (i + m) (String.sub s st (i - st) :: acc)
    else go st (i + 1) acc
  in
  go 0 0 []

let rec merge_patch (target : value) (patch : value) : value =
  match patch with
  | Obj p ->
      let t = match target with Obj t -> Some t | _ -> None in
      let tfields = match t with Some t -> List.filter (visible t) (field_names t) | None -> [] in
      let pfields = List.filter (visible p) (field_names p) in
      let keep = List.filter (fun f -> not (List.mem f pfields)) tfields in
      let merged =
        List.map (fun f -> (f, field (Option.get t) f)) keep
        @ List.filter_map
            (fun f ->
              match field p f with
              | Null -> None
              | v -> Some (f, merge_patch (match t with Some t when List.mem f tfields -> field t f | _ -> Null) v))
            pfields
      in
      Obj (new_obj [| { fields = List.map (fun (f, v) -> (f, { vis = Default; plus = false; body = Const v })) merged; asserts = []; outer = true } |])
  | v -> v

let std () : value =
  let b name ps f = (name, { vis = Hidden; plus = false; body = Const (Fun (Builtin (name, ps, f))) }) in
  let p x = (x, None) and opt x v = (x, Some v) in
  let fields =
    [
      b "length" [ p "x" ] (fun a ->
          match Lazy.force a.(0) with
          | Str s -> Num (float_of_int (String.length s))
          | Arr x -> Num (float_of_int (Array.length x))
          | Obj o -> Num (float_of_int (List.length (List.filter (visible o) (field_names o))))
          | Fun (Closure (_, ps, _)) -> Num (float_of_int (List.length ps))
          | v -> fail "std.length of a %s" (type_name v));
      b "type" [ p "x" ] (fun a -> Str (type_name (Lazy.force a.(0))));
      b "toString" [ p "a" ] (fun a -> Str (string_of (Lazy.force a.(0))));
      b "join" [ p "sep"; p "arr" ] (fun a ->
          match Lazy.force a.(0) with
          | Str sep -> Str (String.concat sep (List.filter_map (function Null -> None | v -> Some (str_of "join" v)) (arr_of "join" (Lazy.force a.(1)))))
          | Arr sep ->
              let parts = List.filter (fun v -> v <> Null) (arr_of "join" (Lazy.force a.(1))) in
              Arr (Array.concat (List.mapi (fun k v -> match v with Arr x -> if k = 0 then x else Array.append sep x | v -> fail "std.join: an array expected, not a %s" (type_name v)) parts))
          | v -> fail "std.join: a separator string or array, not a %s" (type_name v));
      b "map" [ p "func"; p "arr" ] (fun a ->
          let f = Lazy.force a.(0) in
          match Lazy.force a.(1) with
          | Arr x -> Arr (Array.map (fun v -> lazy (call f [ Lazy.force v ])) x)
          | Str s -> Arr (Array.init (String.length s) (fun k -> lazy (call f [ Str (String.make 1 s.[k]) ])))
          | v -> fail "std.map: an array expected, not a %s" (type_name v));
      b "filter" [ p "func"; p "arr" ] (fun a ->
          let f = Lazy.force a.(0) in
          arr (List.filter (fun v -> match call f [ v ] with Bool b -> b | r -> fail "std.filter: a boolean expected, not a %s" (type_name r)) (arr_of "filter" (Lazy.force a.(1)))));
      b "foldl" [ p "func"; p "arr"; p "init" ] (fun a ->
          let f = Lazy.force a.(0) in
          List.fold_left (fun acc v -> call f [ acc; v ]) (Lazy.force a.(2)) (arr_of "foldl" (Lazy.force a.(1))));
      b "foldr" [ p "func"; p "arr"; p "init" ] (fun a ->
          let f = Lazy.force a.(0) in
          List.fold_right (fun v acc -> call f [ v; acc ]) (arr_of "foldr" (Lazy.force a.(1))) (Lazy.force a.(2)));
      b "range" [ p "from"; p "to" ] (fun a ->
          let x = to_int "std.range" (Lazy.force a.(0)) and y = to_int "std.range" (Lazy.force a.(1)) in
          arr (List.init (max 0 (y - x + 1)) (fun k -> Num (float_of_int (x + k)))));
      b "makeArray" [ p "sz"; p "func" ] (fun a ->
          let f = Lazy.force a.(1) in
          Arr (Array.init (to_int "std.makeArray" (Lazy.force a.(0))) (fun k -> lazy (call f [ Num (float_of_int k) ]))));
      b "objectFields" [ p "o" ] (fun a -> let o = obj_of "objectFields" (Lazy.force a.(0)) in arr (List.map (fun f -> Str f) (List.filter (visible o) (field_names o))));
      b "objectFieldsAll" [ p "o" ] (fun a -> arr (List.map (fun f -> Str f) (field_names (obj_of "objectFieldsAll" (Lazy.force a.(0))))));
      b "objectHas" [ p "o"; p "f" ] (fun a ->
          let o = obj_of "objectHas" (Lazy.force a.(0)) and f = str_of "objectHas" (Lazy.force a.(1)) in
          Bool (has o ~upto:(Array.length o.layers) f && visible o f));
      b "objectHasAll" [ p "o"; p "f" ] (fun a ->
          let o = obj_of "objectHasAll" (Lazy.force a.(0)) in
          Bool (has o ~upto:(Array.length o.layers) (str_of "objectHasAll" (Lazy.force a.(1)))));
      b "objectValues" [ p "o" ] (fun a -> let o = obj_of "objectValues" (Lazy.force a.(0)) in arr (List.map (field o) (List.filter (visible o) (field_names o))));
      b "mapWithKey" [ p "func"; p "obj" ] (fun a ->
          let f = Lazy.force a.(0) and o = obj_of "mapWithKey" (Lazy.force a.(1)) in
          let fields = List.map (fun k -> (k, { vis = Default; plus = false; body = Const (call f [ Str k; field o k ]) })) (List.filter (visible o) (field_names o)) in
          Obj (new_obj [| { fields; asserts = []; outer = true } |]));
      b "get" [ p "o"; p "f"; opt "default" Null; opt "inc_hidden" (Bool true) ] (fun a ->
          let o = obj_of "get" (Lazy.force a.(0)) and f = str_of "get" (Lazy.force a.(1)) in
          let hidden_ok = Lazy.force a.(3) = Bool true in
          if has o ~upto:(Array.length o.layers) f && (hidden_ok || visible o f) then field o f else Lazy.force a.(2));
      b "format" [ p "str"; p "vals" ] (fun a -> Str (format (str_of "format" (Lazy.force a.(0))) (Lazy.force a.(1))));
      b "startsWith" [ p "a"; p "b" ] (fun a -> Bool (starts_with ~prefix:(str_of "startsWith" (Lazy.force a.(1))) (str_of "startsWith" (Lazy.force a.(0)))));
      b "endsWith" [ p "a"; p "b" ] (fun a ->
          let s = str_of "endsWith" (Lazy.force a.(0)) and e = str_of "endsWith" (Lazy.force a.(1)) in
          Bool (String.length s >= String.length e && String.sub s (String.length s - String.length e) (String.length e) = e));
      b "split" [ p "str"; p "c" ] (fun a -> arr (List.map (fun s -> Str s) (split_on (str_of "split" (Lazy.force a.(0))) (str_of "split" (Lazy.force a.(1))))));
      b "strReplace" [ p "str"; p "from"; p "to" ] (fun a ->
          Str (String.concat (str_of "strReplace" (Lazy.force a.(2))) (split_on (str_of "strReplace" (Lazy.force a.(0))) (str_of "strReplace" (Lazy.force a.(1))))));
      b "substr" [ p "str"; p "from"; p "len" ] (fun a ->
          let s = str_of "substr" (Lazy.force a.(0)) in
          let from = max 0 (to_int "std.substr" (Lazy.force a.(1))) and len = max 0 (to_int "std.substr" (Lazy.force a.(2))) in
          let from = min from (String.length s) in
          Str (String.sub s from (min len (String.length s - from))));
      b "stringChars" [ p "str" ] (fun a -> let s = str_of "stringChars" (Lazy.force a.(0)) in arr (List.init (String.length s) (fun k -> Str (String.make 1 s.[k]))));
      b "asciiUpper" [ p "str" ] (fun a -> Str (String.uppercase_ascii (str_of "asciiUpper" (Lazy.force a.(0)))));
      b "asciiLower" [ p "str" ] (fun a -> Str (String.lowercase_ascii (str_of "asciiLower" (Lazy.force a.(0)))));
      b "member" [ p "arr"; p "x" ] (fun a ->
          let x = Lazy.force a.(1) in
          match Lazy.force a.(0) with
          | Arr v -> Bool (Array.exists (fun y -> equal (Lazy.force y) x) v)
          | Str s -> let sub = str_of "member" x in Bool (List.length (split_on s sub) > 1)
          | v -> fail "std.member of a %s" (type_name v));
      b "count" [ p "arr"; p "x" ] (fun a -> let x = Lazy.force a.(1) in Num (float_of_int (List.length (List.filter (equal x) (arr_of "count" (Lazy.force a.(0)))))));
      b "flattenArrays" [ p "arrs" ] (fun a -> arr (List.concat_map (arr_of "flattenArrays") (arr_of "flattenArrays" (Lazy.force a.(0)))));
      b "sort" [ p "arr"; opt "keyF" Null ] (fun a ->
          let key = match Lazy.force a.(1) with Null -> Fun.id | f -> fun v -> call f [ v ] in
          arr (List.stable_sort (fun x y -> compare_values (key x) (key y)) (arr_of "sort" (Lazy.force a.(0)))));
      b "uniq" [ p "arr"; opt "keyF" Null ] (fun a ->
          let key = match Lazy.force a.(1) with Null -> Fun.id | f -> fun v -> call f [ v ] in
          let rec go = function x :: (y :: _ as rest) -> if equal (key x) (key y) then go (x :: List.tl rest) else x :: go rest | l -> l in
          arr (go (arr_of "uniq" (Lazy.force a.(0)))));
      b "reverse" [ p "arr" ] (fun a -> arr (List.rev (arr_of "reverse" (Lazy.force a.(0)))));
      b "find" [ p "value"; p "arr" ] (fun a ->
          let x = Lazy.force a.(0) in
          arr (List.concat (List.mapi (fun k v -> if equal v x then [ Num (float_of_int k) ] else []) (arr_of "find" (Lazy.force a.(1))))));
      b "abs" [ p "n" ] (fun a -> Num (Float.abs (num_of "abs" (Lazy.force a.(0)))));
      b "min" [ p "a"; p "b" ] (fun a -> Num (Float.min (num_of "min" (Lazy.force a.(0))) (num_of "min" (Lazy.force a.(1)))));
      b "max" [ p "a"; p "b" ] (fun a -> Num (Float.max (num_of "max" (Lazy.force a.(0))) (num_of "max" (Lazy.force a.(1)))));
      b "floor" [ p "x" ] (fun a -> Num (Float.floor (num_of "floor" (Lazy.force a.(0)))));
      b "ceil" [ p "x" ] (fun a -> Num (Float.ceil (num_of "ceil" (Lazy.force a.(0)))));
      b "pow" [ p "x"; p "n" ] (fun a -> Num (Float.pow (num_of "pow" (Lazy.force a.(0))) (num_of "pow" (Lazy.force a.(1)))));
      b "sqrt" [ p "x" ] (fun a -> Num (Float.sqrt (num_of "sqrt" (Lazy.force a.(0)))));
      b "isString" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Str _ -> true | _ -> false));
      b "isNumber" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Num _ -> true | _ -> false));
      b "isBoolean" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Bool _ -> true | _ -> false));
      b "isArray" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Arr _ -> true | _ -> false));
      b "isObject" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Obj _ -> true | _ -> false));
      b "isFunction" [ p "v" ] (fun a -> Bool (match Lazy.force a.(0) with Fun _ -> true | _ -> false));
      b "parseInt" [ p "str" ] (fun a ->
          let s = str_of "parseInt" (Lazy.force a.(0)) in
          match int_of_string_opt s with Some n when s <> "" && s.[0] <> '+' -> Num (float_of_int n) | _ -> fail "std.parseInt: not an integer: %S" s);
      b "mergePatch" [ p "target"; p "patch" ] (fun a -> merge_patch (Lazy.force a.(0)) (Lazy.force a.(1)));
      b "assertEqual" [ p "a"; p "b" ] (fun a ->
          let x = Lazy.force a.(0) and y = Lazy.force a.(1) in
          if equal x y then Bool true else fail "assertion failed: %s != %s" (string_of x) (string_of y));
      b "trace" [ p "str"; p "rest" ] (fun a -> Lazy.force a.(1));
    ]
  in
  Obj (new_obj [| { fields; asserts = []; outer = true } |])

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval ?(read = fun _ -> None) ~(path : string) (text : string) : (Json.t, string) result =
  match Jsonnet_parse.parse ~path text with
  | Error (l, msg) -> Error (Printf.sprintf "%s:%d: %s" path l msg)
  | Ok e -> (
      reader := read;
      Hashtbl.reset imported;
      depth := 0;
      file := path;
      line := 0;
      root := { vars = Smap.singleton "std" (Lazy.from_val (std ())); self = None; upto = 0; dollar = None };
      match manifest (eval !root e) with
      | j -> Ok j
      | exception Runtime msg -> Error (Printf.sprintf "%s:%d: %s" !file !line msg)
      | exception Stack_overflow -> Error (Printf.sprintf "%s:%d: too deep: a recursion that does not end?" !file !line))
