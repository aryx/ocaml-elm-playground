(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_builtins.mli *)
open Js_value

type protos = { strings : obj; arrays : obj }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let arg (args : value list) (i : int) : value = Option.value (List.nth_opt args i) ~default:Undefined
let num (args : value list) (i : int) : float = to_number (arg args i)

(* an integer argument, [default] when absent *)
let int_arg (args : value list) (i : int) ~(default : int) : int =
  match arg args i with Undefined -> default | v -> let f = to_number v in if Float.is_nan f then 0 else int_of_float f

(* a start or end index: negative ones from the end, then clamped *)
let relative (i : int) (n : int) : int = if i < 0 then max 0 (n + i) else min i n

let obj_of (fields : (string * value) list) : value =
  let o = new_object () in
  List.iter (fun (k, v) -> set_own o k v) fields;
  Object o

let fn = host_function
let array (vs : value list) : value = Object (new_array vs)

(* a string's this, as the method sees it *)
let this_string (this : value) : string = to_string this

let this_array (name : string) (this : value) : obj * items =
  match this with
  | Object ({ kind = Array items; _ } as o) -> (o, items)
  | _ -> throw "TypeError" (Printf.sprintf "Array.prototype.%s called on something not an array" name)

let set_items (items : items) (vs : value list) : unit =
  items.elements <- Array.of_list vs;
  items.length <- List.length vs

(* the string's index of [needle] from [from], or -1 *)
let index_of (s : string) (needle : string) (from : int) : int =
  let n = String.length s and m = String.length needle in
  let rec go i = if i + m > n then -1 else if String.sub s i m = needle then i else go (i + 1) in
  go (max 0 from)

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let string_methods () : obj =
  let o = new_object () in
  let def name f = set_own o name (fn name (fun ~this args -> f (this_string this) args)) in
  def "toUpperCase" (fun s _ -> String (String.uppercase_ascii s));
  def "toLowerCase" (fun s _ -> String (String.lowercase_ascii s));
  def "slice" (fun s args ->
      let n = String.length s in
      let a = relative (int_arg args 0 ~default:0) n and b = relative (int_arg args 1 ~default:n) n in
      String (if b > a then String.sub s a (b - a) else ""));
  def "substring" (fun s args ->
      let n = String.length s in
      let clamp i = max 0 (min i n) in
      let a = clamp (int_arg args 0 ~default:0) and b = clamp (int_arg args 1 ~default:n) in
      let a, b = (min a b, max a b) in
      String (String.sub s a (b - a)));
  def "charAt" (fun s args ->
      let i = int_arg args 0 ~default:0 in
      String (if i >= 0 && i < String.length s then String.make 1 s.[i] else ""));
  def "indexOf" (fun s args -> Number (float_of_int (index_of s (to_string (arg args 0)) (int_arg args 1 ~default:0))));
  def "includes" (fun s args -> Bool (index_of s (to_string (arg args 0)) 0 >= 0));
  def "startsWith" (fun s args -> Bool (String.starts_with ~prefix:(to_string (arg args 0)) s));
  def "endsWith" (fun s args -> Bool (String.ends_with ~suffix:(to_string (arg args 0)) s));
  def "split" (fun s args ->
      match arg args 0 with
      | Undefined -> array [ String s ]
      | sep ->
          let sep = to_string sep in
          if sep = "" then array (List.init (String.length s) (fun i -> String (String.make 1 s.[i])))
          else
            let rec go from acc =
              match index_of s sep from with
              | -1 -> List.rev (String (String.sub s from (String.length s - from)) :: acc)
              | i -> go (i + String.length sep) (String (String.sub s from (i - from)) :: acc)
            in
            array (go 0 []));
  def "trim" (fun s _ -> String (String.trim s));
  def "repeat" (fun s args ->
      let n = int_arg args 0 ~default:0 in
      if n < 0 then throw "RangeError" "Invalid count value" else String (String.concat "" (List.init n (fun _ -> s))));
  def "padStart" (fun s args ->
      let n = int_arg args 0 ~default:0 and pad = match arg args 1 with Undefined -> " " | v -> to_string v in
      let missing = n - String.length s in
      if missing <= 0 || pad = "" then String s
      else String (String.sub (String.concat "" (List.init missing (fun _ -> pad))) 0 missing ^ s));
  def "concat" (fun s args -> String (String.concat "" (s :: List.map to_string args)));
  o

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

let array_methods ~(call : value -> this:value -> value list -> value) : obj =
  let o = new_object () in
  let def name f =
    set_own o name
      (fn name (fun ~this args ->
           let arr, items = this_array name this in
           f arr items args))
  in
  (* f(item, index, array) for each item, as the callbacks are called *)
  let each (arr : obj) (f : value) (k : value -> int -> unit) : unit =
    List.iteri (fun i v -> k (call f ~this:Undefined [ v; Number (float_of_int i); Object arr ]) i) (array_items arr)
  in
  def "push" (fun arr items args ->
      set_items items (array_items arr @ args);
      Number (float_of_int items.length));
  def "pop" (fun arr items _ ->
      match List.rev (array_items arr) with
      | [] -> Undefined
      | last :: rest -> set_items items (List.rev rest); last);
  def "shift" (fun arr items _ -> match array_items arr with [] -> Undefined | first :: rest -> set_items items rest; first);
  def "unshift" (fun arr items args ->
      set_items items (args @ array_items arr);
      Number (float_of_int items.length));
  def "join" (fun arr _ args ->
      let sep = match arg args 0 with Undefined -> "," | v -> to_string v in
      String (String.concat sep (List.map (fun v -> match v with Undefined | Null -> "" | v -> to_string v) (array_items arr))));
  let find_index arr v = let rec go i l = match l with [] -> -1 | x :: r -> if strict_equal x v then i else go (i + 1) r in go 0 (array_items arr) in
  def "indexOf" (fun arr _ args -> Number (float_of_int (find_index arr (arg args 0))));
  def "includes" (fun arr _ args -> Bool (find_index arr (arg args 0) >= 0));
  def "slice" (fun arr items args ->
      let n = items.length in
      let a = relative (int_arg args 0 ~default:0) n and b = relative (int_arg args 1 ~default:n) n in
      array (List.filteri (fun i _ -> i >= a && i < b) (array_items arr)));
  def "concat" (fun arr _ args ->
      array (array_items arr @ List.concat_map (fun v -> match v with Object ({ kind = Array _; _ } as o) -> array_items o | v -> [ v ]) args));
  def "reverse" (fun arr items _ -> set_items items (List.rev (array_items arr)); Object arr);
  def "sort" (fun arr items args ->
      let compare =
        match arg args 0 with
        | Undefined -> fun a b -> compare (to_string a) (to_string b)
        | f -> fun a b -> let r = to_number (call f ~this:Undefined [ a; b ]) in if r < 0. then -1 else if r > 0. then 1 else 0
      in
      (* stable, as JavaScript's has been since 2019; undefined last *)
      let defined, undefined = List.partition (fun v -> v <> Undefined) (array_items arr) in
      set_items items (List.stable_sort compare defined @ undefined);
      Object arr);
  def "forEach" (fun arr _ args -> each arr (arg args 0) (fun _ _ -> ()); Undefined);
  def "map" (fun arr _ args ->
      let out = ref [] in
      each arr (arg args 0) (fun r _ -> out := r :: !out);
      array (List.rev !out));
  def "filter" (fun arr _ args ->
      let items = Array.of_list (array_items arr) and out = ref [] in
      each arr (arg args 0) (fun r i -> if truthy r then out := items.(i) :: !out);
      array (List.rev !out));
  def "reduce" (fun arr _ args ->
      let f = arg args 0 in
      let start, rest =
        match (args, array_items arr) with
        | [ _ ], [] -> throw "TypeError" "Reduce of empty array with no initial value"
        | [ _ ], x :: rest -> (x, List.mapi (fun i v -> (i + 1, v)) rest)
        | _, xs -> (arg args 1, List.mapi (fun i v -> (i, v)) xs)
      in
      List.fold_left (fun acc (i, v) -> call f ~this:Undefined [ acc; v; Number (float_of_int i); Object arr ]) start rest);
  let first arr f =
    let rec go i l = match l with [] -> None | v :: r -> if truthy (call f ~this:Undefined [ v; Number (float_of_int i); Object arr ]) then Some (i, v) else go (i + 1) r in
    go 0 (array_items arr)
  in
  def "find" (fun arr _ args -> match first arr (arg args 0) with Some (_, v) -> v | None -> Undefined);
  def "findIndex" (fun arr _ args -> match first arr (arg args 0) with Some (i, _) -> Number (float_of_int i) | None -> Number (-1.));
  def "some" (fun arr _ args -> Bool (first arr (arg args 0) <> None));
  def "every" (fun arr _ args ->
      let f = arg args 0 in
      Bool (first arr (host_function "not" (fun ~this:_ a -> Bool (not (truthy (call f ~this:Undefined a))))) = None));
  o

(*****************************************************************************)
(* The globals *)
(*****************************************************************************)

(* the longest prefix of [s] (after spaces) that reads as an integer in
 * [radix], else NaN: parseInt("42px") is 42 *)
let parse_int (s : string) (radix : int) : float =
  let s = String.trim s in
  let sign, s = if s <> "" && (s.[0] = '-' || s.[0] = '+') then ((if s.[0] = '-' then -1. else 1.), String.sub s 1 (String.length s - 1)) else (1., s) in
  let radix, s =
    if (radix = 16 || radix = 0) && String.length s > 1 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then (16, String.sub s 2 (String.length s - 2))
    else ((if radix = 0 then 10 else radix), s)
  in
  let digit c =
    let d = match c with '0' .. '9' -> Char.code c - 48 | 'a' .. 'z' -> Char.code c - 87 | 'A' .. 'Z' -> Char.code c - 55 | _ -> 99 in
    if d < radix then Some d else None
  in
  let rec go i acc = if i < String.length s then match digit s.[i] with Some d -> go (i + 1) ((acc *. float_of_int radix) +. float_of_int d) | None -> (i, acc) else (i, acc) in
  match go 0 0. with 0, _ -> Float.nan | _, v -> sign *. v

(* the longest prefix that reads as a decimal number *)
let parse_float (s : string) : float =
  let s = String.trim s in
  let rec longest n = if n = 0 then Float.nan else match float_of_string_opt (String.sub s 0 n) with Some f when not (String.contains (String.sub s 0 n) '_') -> f | _ -> longest (n - 1) in
  if String.starts_with ~prefix:"Infinity" s then Float.infinity else longest (String.length s)

let install ~(call : value -> this:value -> value list -> value) ~(log : string -> unit) ~(seed : int) (define : string -> value -> unit) : protos =
  let shown args = String.concat " " (List.map display args) in
  define "console"
    (obj_of
       [ ("log", fn "log" (fun ~this:_ args -> log (shown args); Undefined));
         ("error", fn "error" (fun ~this:_ args -> log (shown args); Undefined));
         ("warn", fn "warn" (fun ~this:_ args -> log (shown args); Undefined)) ]);
  let seed = ref (Lehmer.scramble seed) in
  let math1 name f = (name, fn name (fun ~this:_ args -> Number (f (num args 0)))) in
  let fold name init pick = (name, fn name (fun ~this:_ args -> Number (List.fold_left (fun m v -> let x = to_number v in if Float.is_nan m || Float.is_nan x then Float.nan else pick m x) init args))) in
  define "Math"
    (obj_of
       [ math1 "floor" Float.floor; math1 "ceil" Float.ceil;
         (* half up, not half to even: Math.round(-2.5) is -2 *)
         math1 "round" (fun x -> Float.floor (x +. 0.5));
         math1 "trunc" Float.trunc; math1 "abs" Float.abs; math1 "sqrt" Float.sqrt;
         math1 "sign" (fun x -> if Float.is_nan x then x else if x > 0. then 1. else if x < 0. then -1. else x);
         ("pow", fn "pow" (fun ~this:_ args -> Number (Float.pow (num args 0) (num args 1))));
         fold "min" Float.infinity Float.min; fold "max" Float.neg_infinity Float.max;
         ("random", fn "random" (fun ~this:_ _ -> seed := Lehmer.next !seed; Number (Lehmer.to_unit !seed)));
         ("PI", Number Float.pi) ]);
  define "String" (fn "String" (fun ~this:_ args -> String (match args with [] -> "" | v :: _ -> to_string v)));
  define "Number" (fn "Number" (fun ~this:_ args -> Number (match args with [] -> 0. | v :: _ -> to_number v)));
  define "Boolean" (fn "Boolean" (fun ~this:_ args -> Bool (truthy (arg args 0))));
  define "parseInt" (fn "parseInt" (fun ~this:_ args -> Number (parse_int (to_string (arg args 0)) (int_arg args 1 ~default:0))));
  define "parseFloat" (fn "parseFloat" (fun ~this:_ args -> Number (parse_float (to_string (arg args 0)))));
  define "isNaN" (fn "isNaN" (fun ~this:_ args -> Bool (Float.is_nan (num args 0))));
  define "NaN" (Number Float.nan);
  define "Infinity" (Number Float.infinity);
  define "undefined" Undefined;
  define "JSON"
    (obj_of
       [ ("stringify",
          fn "stringify" (fun ~this:_ args ->
              match to_json (arg args 0) with
              | Some s -> String s
              | None -> if arg args 0 = Undefined then Undefined else throw "TypeError" "Converting circular structure to JSON")) ]);
  define "Object"
    (obj_of
       [ ("keys",
          fn "keys" (fun ~this:_ args ->
              match arg args 0 with
              | Object ({ kind = Array a; _ }) -> array (List.init a.length (fun i -> String (string_of_int i)))
              | Object o -> array (List.map (fun k -> String k) (keys o))
              | _ -> array [])) ]);
  define "Array" (obj_of [ ("isArray", fn "isArray" (fun ~this:_ args -> Bool (match arg args 0 with Object { kind = Array _; _ } -> true | _ -> false))) ]);
  { strings = string_methods (); arrays = array_methods ~call }
