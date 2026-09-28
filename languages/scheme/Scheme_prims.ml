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

(* See Scheme_prims.mli *)

exception Error of string

let fail fmt = Printf.ksprintf (fun msg -> raise (Error msg)) fmt

(* DrScheme v20x's message: "car: expects argument of type <pair>;
   given 5" *)
let expects (name : string) (what : string) (v : Scheme.t) = fail "%s: expects argument of type <%s>; given %s" name what (print Write v)

let arity (name : string) (n : int) (args : Scheme.t list) =
  if List.length args <> n then fail "%s: expects %d argument%s, given %d" name n (if n = 1 then "" else "s") (List.length args)

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

let num name v = match v with Int n -> float_of_int n | Real f -> f | _ -> expects name "number" v
let int name v = match v with Int n -> n | Real f when Float.is_integer f -> int_of_float f | _ -> expects name "integer" v
let is_num v = match v with Int _ | Real _ -> true | _ -> false

(* an operation on two numbers: on integers if both are, else on reals *)
let arith name (fi : int -> int -> int) (ff : float -> float -> float) (a : Scheme.t) (b : Scheme.t) : Scheme.t =
  match (a, b) with Int x, Int y -> Int (fi x y) | _ -> Real (ff (num name a) (num name b))

let fold name fi ff unit args =
  match args with
  | [] -> unit
  | [ a ] -> if is_num a then a else expects name "number" a
  | a :: rest -> List.fold_left (arith name fi ff) a rest

let divide (a : Scheme.t) (b : Scheme.t) : Scheme.t =
  match (a, b) with
  | _, (Int 0 | Real 0.) -> fail "/: division by zero"
  | Int x, Int y when x mod y = 0 -> Int (x / y)
  | _ -> Real (num "/" a /. num "/" b)

(* = < ...: each pair in turn, (< 1 2 3) *)
let compare_all name (ok : float -> float -> bool) args =
  let rec go = function a :: (b :: _ as rest) -> ok (num name a) (num name b) && go rest | [ a ] -> ignore (num name a); true | [] -> true in
  if List.length args < 2 then fail "%s: expects at least 2 arguments, given %d" name (List.length args);
  Bool (go args)

let real_to_num (f : float) : Scheme.t = if Float.is_integer f && Float.abs f < 1e15 then Int (int_of_float f) else Real f

let rounding name (f : float -> float) args =
  arity name 1 args;
  match args with [ Int n ] -> Int n | [ v ] -> Real (f (num name v)) | _ -> assert false

let integer_op name (op : int -> int -> int) args =
  arity name 2 args;
  match args with [ a; b ] -> if int name b = 0 then fail "%s: undefined for 0" name else Int (op (int name a) (int name b)) | _ -> assert false

(* Scheme's modulo takes the divisor's sign, OCaml's mod the dividend's *)
let modulo a b = let r = a mod b in if r <> 0 && (r < 0) <> (b < 0) then r + b else r

let number_to_string (v : Scheme.t) : string = match v with Real f -> Scheme.print Write (Real f) | _ -> Scheme.print Write v

(*****************************************************************************)
(* Lists, strings *)
(*****************************************************************************)

let pair name v = match v with Pair (a, d) -> (a, d) | _ -> expects name "pair" v
let str name v = match v with Str s -> s | _ -> expects name "string" v
let chr name v = match v with Char c -> c | _ -> expects name "character" v
let lst name v = match to_list v with Some xs -> xs | None -> expects name "list" v

(* car, cadr, caddr...: the a's and d's read right to left *)
let cxr (name : string) (path : string) args =
  arity name 1 args;
  let v = ref (List.hd args) in
  for i = String.length path - 1 downto 0 do
    let a, d = pair name !v in
    v := if path.[i] = 'a' then a else d
  done;
  !v

let nth name (i : int) args =
  arity name 1 args;
  match List.nth_opt (lst name (List.hd args)) i with Some v -> v | None -> fail "%s: list contains too few elements" name

(* member and its kin: the tail starting with [x], or #f *)
let rec member (eq : Scheme.t -> Scheme.t -> bool) (x : Scheme.t) (l : Scheme.t) : Scheme.t =
  match l with Pair (a, d) -> if eq x a then l else member eq x d | _ -> Bool false

let rec assoc (eq : Scheme.t -> Scheme.t -> bool) (x : Scheme.t) (l : Scheme.t) : Scheme.t =
  match l with Pair ((Pair (k, _) as entry), d) -> if eq x k then entry else assoc eq x d | Pair (_, d) -> assoc eq x d | _ -> Bool false

let eqv a b = match (a, b) with Proc p, Proc q -> p == q | (Pair _ | Vector _ | Struct _ | Str _), _ -> a == b | _ -> a = b

(* format's ~a (display) ~s (write) ~n and ~~ *)
let format (fmt : string) (args : Scheme.t list) : string =
  let b = Buffer.create 32 and args = ref args in
  let next () = match !args with v :: rest -> args := rest; v | [] -> fail "format: not enough arguments for the format string" in
  let i = ref 0 in
  while !i < String.length fmt do
    (if fmt.[!i] = '~' && !i + 1 < String.length fmt then begin
       (match fmt.[!i + 1] with
       | 'a' | 'A' -> Buffer.add_string b (display (next ()))
       | 's' | 'S' | 'v' -> Buffer.add_string b (print Write (next ()))
       | 'n' | '%' -> Buffer.add_char b '\n'
       | c -> Buffer.add_char b c);
       incr i
     end
     else Buffer.add_char b fmt.[!i]);
    incr i
  done;
  Buffer.contents b

(*****************************************************************************)
(* Images *)
(*****************************************************************************)

let length name v = let f = num name v in if f < 0. then expects name "non-negative number" v else f
let color name v = match v with Str s | Sym s -> String.lowercase_ascii s | _ -> expects name "color" v

let mode name v : Scheme_image.mode =
  match v with Str ("solid" | "Solid") | Sym "solid" -> Solid | Str ("outline" | "Outline") | Sym "outline" -> Outline | _ -> expects name "mode (\"solid\" or \"outline\")" v

let img name v = match v with Image i -> i | _ -> expects name "image" v

(* beside, above, overlay: two images or more, folded *)
let combine name (f : Scheme_image.t -> Scheme_image.t -> Scheme_image.t) args =
  if List.length args < 2 then fail "%s: expects at least 2 arguments, given %d" name (List.length args);
  let imgs = List.map (img name) args in
  Image (List.fold_left f (List.hd imgs) (List.tl imgs))

let image name args : Scheme.t =
  let open Scheme_image in
  match (name, args) with
  | "circle", [ r; m; c ] -> Image (Circle (length name r, mode name m, color name c))
  | "ellipse", [ w; h; m; c ] -> Image (Ellipse (length name w, length name h, mode name m, color name c))
  | "rectangle", [ w; h; m; c ] -> Image (Rectangle (length name w, length name h, mode name m, color name c))
  | "square", [ s; m; c ] -> Image (Rectangle (length name s, length name s, mode name m, color name c))
  | "triangle", [ s; m; c ] -> Image (Triangle (length name s, mode name m, color name c))
  | "text", [ s; size; c ] -> Image (Text (str name s, length name size, color name c))
  | "empty-scene", [ w; h ] -> Image (Scene (length name w, length name h))
  | "beside", _ -> combine name (fun a b -> Beside (a, b)) args
  | "above", _ -> combine name (fun a b -> Above (a, b)) args
  | "overlay", _ -> combine name (fun a b -> Overlay (a, b)) args
  | "place-image", [ i; x; y; scene ] -> Image (Place (img name i, num name x, num name y, img name scene))
  | "image-width", [ i ] -> real_to_num (Float.round (width (img name i)))
  | "image-height", [ i ] -> real_to_num (Float.round (height (img name i)))
  | "image?", [ v ] -> Bool (match v with Image _ -> true | _ -> false)
  | _ -> fail "%s: wrong number of arguments (%d)" name (List.length args)

let image_names =
  [ "circle"; "ellipse"; "rectangle"; "square"; "triangle"; "text"; "empty-scene"; "beside"; "above"; "overlay"; "place-image"; "image-width"; "image-height"; "image?" ]

(*****************************************************************************)
(* The table *)
(*****************************************************************************)

let one name args = arity name 1 args; List.hd args
let two name args = arity name 2 args; match args with [ a; b ] -> (a, b) | _ -> assert false
let pred name (p : Scheme.t -> bool) = (name, fun args -> Bool (p (one name args)))
(* sqrt of 4 is 2, of 2 is a real *)
let float1 name (f : float -> float) = (name, fun args -> let r = f (num name (one name args)) in if Float.is_integer r then Int (int_of_float r) else Real r)

let table : (string * (Scheme.t list -> Scheme.t)) list =
  [ ("+", fold "+" ( + ) ( +. ) (Int 0));
    ("*", fold "*" ( * ) ( *. ) (Int 1));
    ("-", fun args -> match args with [ a ] -> arith "-" ( - ) ( -. ) (Int 0) a | [] -> fail "-: expects at least 1 argument" | _ -> fold "-" ( - ) ( -. ) (Int 0) args);
    ("/", fun args -> match args with [ a ] -> divide (Int 1) a | a :: rest when rest <> [] -> List.fold_left divide a rest | _ -> fail "/: expects at least 1 argument");
    ("=", compare_all "=" ( = ));
    ("<", compare_all "<" ( < ));
    (">", compare_all ">" ( > ));
    ("<=", compare_all "<=" ( <= ));
    (">=", compare_all ">=" ( >= ));
    ("quotient", integer_op "quotient" ( / ));
    ("remainder", integer_op "remainder" ( mod ));
    ("modulo", integer_op "modulo" modulo);
    ("abs", fun args -> match one "abs" args with Int n -> Int (abs n) | v -> Real (Float.abs (num "abs" v)));
    ("min", fun args -> fold "min" min Float.min (Int 0) args);
    ("max", fun args -> fold "max" max Float.max (Int 0) args);
    ("add1", fun args -> arith "add1" ( + ) ( +. ) (one "add1" args) (Int 1));
    ("sub1", fun args -> arith "sub1" ( - ) ( -. ) (one "sub1" args) (Int 1));
    ("sqr", fun args -> let v = one "sqr" args in arith "sqr" ( * ) ( *. ) v v);
    pred "zero?" (fun v -> num "zero?" v = 0.);
    pred "positive?" (fun v -> num "positive?" v > 0.);
    pred "negative?" (fun v -> num "negative?" v < 0.);
    pred "even?" (fun v -> int "even?" v mod 2 = 0);
    pred "odd?" (fun v -> int "odd?" v mod 2 <> 0);
    pred "number?" is_num;
    pred "integer?" (fun v -> match v with Int _ -> true | Real f -> Float.is_integer f | _ -> false);
    pred "real?" is_num;
    pred "rational?" is_num;
    pred "exact?" (fun v -> match v with Int _ -> true | Real _ -> false | _ -> expects "exact?" "number" v);
    pred "inexact?" (fun v -> match v with Real _ -> true | Int _ -> false | _ -> expects "inexact?" "number" v);
    ("exact->inexact", fun args -> Real (num "exact->inexact" (one "exact->inexact" args)));
    ("inexact->exact", fun args -> match one "inexact->exact" args with Real f -> Int (int_of_float (Float.round f)) | v -> ignore (num "inexact->exact" v); v);
    ("floor", rounding "floor" Float.floor);
    ("ceiling", rounding "ceiling" Float.ceil);
    ("round", rounding "round" Float.round);
    ("truncate", rounding "truncate" Float.trunc);
    float1 "sqrt" Float.sqrt;
    float1 "exp" Float.exp;
    float1 "log" Float.log;
    ("sin", fun args -> Real (sin (num "sin" (one "sin" args))));
    ("cos", fun args -> Real (cos (num "cos" (one "cos" args))));
    ("tan", fun args -> Real (tan (num "tan" (one "tan" args))));
    ("atan", fun args -> match args with [ y; x ] -> Real (Float.atan2 (num "atan" y) (num "atan" x)) | _ -> Real (atan (num "atan" (one "atan" args))));
    ("expt", fun args ->
      match two "expt" args with
      | Int b, Int e when e >= 0 -> let rec pow b e = if e = 0 then 1 else b * pow b (e - 1) in Int (pow b e)
      | b, e -> Real (Float.pow (num "expt" b) (num "expt" e)));
    ("gcd", fun args -> let rec gcd a b = if b = 0 then abs a else gcd b (a mod b) in Int (List.fold_left (fun g v -> gcd g (int "gcd" v)) 0 args));
    ("number->string", fun args -> Str (number_to_string (let v = one "number->string" args in ignore (num "number->string" v); v)));
    ("string->number", fun args ->
      let s = str "string->number" (one "string->number" args) in
      match int_of_string_opt s with Some n -> Int n | None -> ( match float_of_string_opt s with Some f when s <> "" -> Real f | _ -> Bool false));
    (* booleans, equality *)
    pred "not" (fun v -> v = Bool false);
    pred "boolean?" (fun v -> match v with Bool _ -> true | _ -> false);
    ("eq?", fun args -> let a, b = two "eq?" args in Bool (eqv a b));
    ("eqv?", fun args -> let a, b = two "eqv?" args in Bool (eqv a b));
    ("equal?", fun args -> let a, b = two "equal?" args in Bool (equal a b));
    ("boolean=?", fun args -> let a, b = two "boolean=?" args in Bool (a = b));
    (* symbols *)
    pred "symbol?" (fun v -> match v with Sym _ -> true | _ -> false);
    ("symbol->string", fun args -> match one "symbol->string" args with Sym s -> Str s | v -> expects "symbol->string" "symbol" v);
    ("string->symbol", fun args -> Sym (str "string->symbol" (one "string->symbol" args)));
    ("symbol=?", fun args -> let a, b = two "symbol=?" args in Bool (a = b));
    (* strings *)
    pred "string?" (fun v -> match v with Str _ -> true | _ -> false);
    ("string-length", fun args -> Int (String.length (str "string-length" (one "string-length" args))));
    ("string-append", fun args -> Str (String.concat "" (List.map (str "string-append") args)));
    ("substring", fun args ->
      match args with
      | s :: i :: rest ->
          let s = str "substring" s and i = int "substring" i in
          let j = match rest with [ j ] -> int "substring" j | _ -> String.length s in
          if i < 0 || j > String.length s || i > j then fail "substring: ending index %d out of range [%d, %d] for string %S" j i (String.length s) s
          else Str (String.sub s i (j - i))
      | _ -> fail "substring: expects 2 or 3 arguments");
    ("string-ref", fun args ->
      let s, i = two "string-ref" args in
      let s = str "string-ref" s and i = int "string-ref" i in
      if i < 0 || i >= String.length s then fail "string-ref: index %d out of range for %S" i s else Char (Char.code s.[i]));
    ("string=?", fun args -> let a, b = two "string=?" args in Bool (str "string=?" a = str "string=?" b));
    ("string<?", fun args -> let a, b = two "string<?" args in Bool (str "string<?" a < str "string<?" b));
    ("string>?", fun args -> let a, b = two "string>?" args in Bool (str "string>?" a > str "string>?" b));
    ("string-upcase", fun args -> Str (String.uppercase_ascii (str "string-upcase" (one "string-upcase" args))));
    ("string-downcase", fun args -> Str (String.lowercase_ascii (str "string-downcase" (one "string-downcase" args))));
    ("string->list", fun args -> list (List.map (fun c -> Char (Char.code c)) (List.of_seq (String.to_seq (str "string->list" (one "string->list" args))))));
    ("list->string", fun args -> Str (String.of_seq (List.to_seq (List.map (fun c -> Char.chr (chr "list->string" c land 255)) (lst "list->string" (one "list->string" args))))));
    ("string", fun args -> Str (String.of_seq (List.to_seq (List.map (fun c -> Char.chr (chr "string" c land 255)) args))));
    ("format", fun args -> match args with f :: rest -> Str (format (str "format" f) rest) | [] -> fail "format: expects a format string");
    (* characters *)
    pred "char?" (fun v -> match v with Char _ -> true | _ -> false);
    ("char->integer", fun args -> Int (chr "char->integer" (one "char->integer" args)));
    ("integer->char", fun args -> Char (int "integer->char" (one "integer->char" args)));
    ("char=?", fun args -> let a, b = two "char=?" args in Bool (chr "char=?" a = chr "char=?" b));
    ("char<?", fun args -> let a, b = two "char<?" args in Bool (chr "char<?" a < chr "char<?" b));
    ("char-upcase", fun args -> Char (Char.code (Char.uppercase_ascii (Char.chr (chr "char-upcase" (one "char-upcase" args) land 255)))));
    pred "char-alphabetic?" (fun v -> match Char.chr (chr "char-alphabetic?" v land 255) with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false);
    pred "char-numeric?" (fun v -> match Char.chr (chr "char-numeric?" v land 255) with '0' .. '9' -> true | _ -> false);
    pred "char-whitespace?" (fun v -> List.mem (chr "char-whitespace?" v) [ 32; 9; 10; 13 ]);
    (* pairs and lists *)
    ("cons", fun args -> let a, d = two "cons" args in Pair (a, d));
    ("car", cxr "car" "a");
    ("cdr", cxr "cdr" "d");
    ("caar", cxr "caar" "aa");
    ("cadr", cxr "cadr" "ad");
    ("cdar", cxr "cdar" "da");
    ("cddr", cxr "cddr" "dd");
    ("caddr", cxr "caddr" "add");
    ("cdddr", cxr "cdddr" "ddd");
    ("first", fun args -> match one "first" args with Pair (a, _) -> a | v -> expects "first" "non-empty list" v);
    ("rest", fun args -> match one "rest" args with Pair (_, d) -> d | v -> expects "rest" "non-empty list" v);
    ("second", nth "second" 1);
    ("third", nth "third" 2);
    ("fourth", nth "fourth" 3);
    ("last", fun args -> match List.rev (lst "last" (one "last" args)) with v :: _ -> v | [] -> expects "last" "non-empty list" Nil);
    pred "empty?" (fun v -> v = Nil);
    pred "null?" (fun v -> v = Nil);
    pred "cons?" (fun v -> match v with Pair _ -> true | _ -> false);
    pred "pair?" (fun v -> match v with Pair _ -> true | _ -> false);
    pred "list?" (fun v -> to_list v <> None);
    ("list", fun args -> list args);
    ("length", fun args -> Int (List.length (lst "length" (one "length" args))));
    ("append", fun args ->
      match List.rev args with
      | [] -> Nil
      | last :: firsts -> List.fold_left (fun acc l -> List.fold_right (fun x rest -> Pair (x, rest)) (lst "append" l) acc) last firsts);
    ("reverse", fun args -> list (List.rev (lst "reverse" (one "reverse" args))));
    ("list-ref", fun args ->
      let l, i = two "list-ref" args in
      match List.nth_opt (lst "list-ref" l) (int "list-ref" i) with Some v -> v | None -> fail "list-ref: index %s too large for list" (print Write i));
    ("list-tail", fun args ->
      let l, k = two "list-tail" args in
      let rec go l k = if k = 0 then l else go (snd (pair "list-tail" l)) (k - 1) in
      go l (int "list-tail" k));
    ("member", fun args -> let x, l = two "member" args in member equal x l);
    ("memv", fun args -> let x, l = two "memv" args in member eqv x l);
    ("memq", fun args -> let x, l = two "memq" args in member eqv x l);
    ("assoc", fun args -> let x, l = two "assoc" args in assoc equal x l);
    ("assv", fun args -> let x, l = two "assv" args in assoc eqv x l);
    ("assq", fun args -> let x, l = two "assq" args in assoc eqv x l);
    ("remove", fun args ->
      let x, l = two "remove" args in
      let rec go = function [] -> [] | y :: ys -> if equal x y then ys else y :: go ys in
      list (go (lst "remove" l)));
    (* vectors *)
    pred "vector?" (fun v -> match v with Vector _ -> true | _ -> false);
    ("vector", fun args -> Vector (Array.of_list args));
    ("make-vector", fun args -> match args with [ n ] -> Vector (Array.make (int "make-vector" n) (Int 0)) | [ n; v ] -> Vector (Array.make (int "make-vector" n) v) | _ -> fail "make-vector: expects 1 or 2 arguments");
    ("vector-length", fun args -> match one "vector-length" args with Vector xs -> Int (Array.length xs) | v -> expects "vector-length" "vector" v);
    ("vector-ref", fun args ->
      match two "vector-ref" args with
      | Vector xs, i -> let i = int "vector-ref" i in if i < 0 || i >= Array.length xs then fail "vector-ref: index %d out of range" i else xs.(i)
      | v, _ -> expects "vector-ref" "vector" v);
    ("vector->list", fun args -> match one "vector->list" args with Vector xs -> list (Array.to_list xs) | v -> expects "vector->list" "vector" v);
    ("list->vector", fun args -> Vector (Array.of_list (lst "list->vector" (one "list->vector" args))));
    (* the rest *)
    pred "procedure?" (fun v -> match v with Proc _ -> true | _ -> false);
    ("void", fun _ -> Void);
    ("identity", fun args -> one "identity" args);
    ("error", fun args ->
      match args with
      | Sym who :: msg :: rest -> fail "%s: %s" who (String.concat " " (display msg :: List.map (print Write) rest))
      | msg :: rest -> fail "%s" (String.concat " " (display msg :: List.map (print Write) rest))
      | [] -> fail "error");
    ("cond-fell-through", fun _ -> fail "cond: all question results were false") ]
  @ List.map (fun name -> (name, image name)) image_names

let names = List.map fst table

let by_name : (string, Scheme.t list -> Scheme.t) Hashtbl.t =
  let h = Hashtbl.create 200 in
  List.iter (fun (name, f) -> Hashtbl.replace h name f) table;
  h

let apply (name : string) (args : Scheme.t list) : Scheme.t = (Hashtbl.find by_name name) args
