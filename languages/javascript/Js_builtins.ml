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

type protos = { strings : obj; arrays : obj; objects : obj; functions : obj; regexps : obj; numbers : obj }

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
(* Regular expressions *)
(*****************************************************************************)

(* a RegExp object, its prototype [proto] *)
let regexp_value (proto : obj) (re : Js_regexp.t) : value =
  let o = { (new_object ()) with kind = Regexp re } in
  o.proto <- Some proto;
  set_own o "lastIndex" (Number 0.);
  Object o

let compile (source : string) (flags : string) : Js_regexp.t =
  match Js_regexp.compile source flags with Ok re -> re | Error why -> throw "SyntaxError" ("Invalid regular expression: /" ^ source ^ "/: " ^ why)

(* a match as exec gives it: the matched text, each group's (undefined
 * if it took no part), its index and input *)
let match_array (s : string) (spans : (int * int) option array) : value =
  let texts = Array.to_list (Array.map (function Some (a, b) -> String (String.sub s a (b - a)) | None -> Undefined) spans) in
  let a = new_array texts in
  set_own a "index" (Number (float_of_int (match spans.(0) with Some (i, _) -> i | None -> 0)));
  set_own a "input" (String s);
  Object a

(* every match of [re] in [s], left to right (an empty one moving on
 * by one) *)
let all_matches (re : Js_regexp.t) (s : string) : (int * int) option array list =
  let rec go from acc =
    if from > String.length s then List.rev acc
    else
      match Js_regexp.exec re s from with
      | Some spans -> (
          match spans.(0) with Some (a, b) -> go (if b = a then b + 1 else b) (spans :: acc) | None -> List.rev acc)
      | None -> List.rev acc
  in
  go 0 []

(* a replacement's text: $& the match, $1..$9 its groups, $$ a dollar *)
let expand (template : string) (s : string) (spans : (int * int) option array) : string =
  let b = Buffer.create (String.length template) in
  let n = String.length template in
  let group g = match if g < Array.length spans then spans.(g) else None with Some (x, y) -> String.sub s x (y - x) | None -> "" in
  let rec go i =
    if i < n then
      if template.[i] = '$' && i + 1 < n then (
        match template.[i + 1] with
        | '$' -> Buffer.add_char b '$'; go (i + 2)
        | '&' -> Buffer.add_string b (group 0); go (i + 2)
        | '1' .. '9' as c -> Buffer.add_string b (group (Char.code c - 48)); go (i + 2)
        | _ -> Buffer.add_char b '$'; go (i + 1))
      else (Buffer.add_char b template.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

let string_methods ~(call : value -> this:value -> value list -> value) ~(regexps : obj) : obj =
  let o = new_object () in
  (* a pattern: a RegExp's, or a string's (its characters as they are) *)
  let pattern (v : value) : Js_regexp.t =
    match v with
    | Object { kind = Regexp re; _ } -> re
    | v ->
        let s = to_string v in
        let b = Buffer.create (String.length s) in
        String.iter (fun c -> if String.contains "\\^$.|?*+()[]{}/" c then Buffer.add_char b '\\'; Buffer.add_char b c) s;
        compile (Buffer.contents b) ""
  in
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
  def "lastIndexOf" (fun s args ->
      let needle = to_string (arg args 0) in
      let rec go i = if i < 0 then -1 else if i + String.length needle <= String.length s && String.sub s i (String.length needle) = needle then i else go (i - 1) in
      Number (float_of_int (go (String.length s - String.length needle))));
  def "charCodeAt" (fun s args ->
      let i = int_arg args 0 ~default:0 in
      Number (if i >= 0 && i < String.length s then float_of_int (Char.code s.[i]) else Float.nan));
  def "substr" (fun s args ->
      let n = String.length s in
      let a = relative (int_arg args 0 ~default:0) n in
      let len = max 0 (min (int_arg args 1 ~default:(n - a)) (n - a)) in
      String (String.sub s a len));
  def "trimStart" (fun s _ -> let t = String.trim s in if t = "" then String "" else String (String.sub s (index_of s t 0) (String.length s - index_of s t 0)));
  def "toString" (fun s _ -> String s);
  (* with a regular expression, or a string as one *)
  def "match" (fun s args ->
      let re = pattern (arg args 0) in
      if Js_regexp.global re then
        match all_matches re s with
        | [] -> Null
        | ms -> array (List.map (fun spans -> match spans.(0) with Some (a, b) -> String (String.sub s a (b - a)) | None -> Undefined) ms)
      else match Js_regexp.exec re s 0 with Some spans -> match_array s spans | None -> Null);
  def "search" (fun s args ->
      match Js_regexp.exec (pattern (arg args 0)) s 0 with Some spans -> ( match spans.(0) with Some (a, _) -> Number (float_of_int a) | None -> Number (-1.)) | None -> Number (-1.));
  def "replace" (fun s args ->
      let re = pattern (arg args 0) in
      let ms = if Js_regexp.global re then all_matches re s else Option.to_list (Js_regexp.exec re s 0) in
      let b = Buffer.create (String.length s) in
      let last =
        List.fold_left
          (fun from spans ->
            match spans.(0) with
            | Some (a, e) ->
                Buffer.add_string b (String.sub s from (a - from));
                (match arg args 1 with
                | Object { kind = Closure _ | Host_function _; _ } as f ->
                    let groups = List.tl (Array.to_list (Array.map (function Some (x, y) -> String (String.sub s x (y - x)) | None -> Undefined) spans)) in
                    Buffer.add_string b (to_string (call f ~this:Undefined ((String (String.sub s a (e - a)) :: groups) @ [ Number (float_of_int a); String s ])))
                | v -> Buffer.add_string b (expand (to_string v) s spans));
                e
            | None -> from)
          0 ms
      in
      Buffer.add_string b (String.sub s last (String.length s - last));
      String (Buffer.contents b));
  (* split by a regular expression *)
  let split = Option.get (get_own o "split") in
  set_own o "split"
    (fn "split" (fun ~this args ->
         match arg args 0 with
         | Object { kind = Regexp re; _ } ->
             let s = this_string this in
             let pieces, last =
               List.fold_left
                 (fun (acc, from) spans -> match spans.(0) with Some (a, b) when b > a -> (String (String.sub s from (a - from)) :: acc, b) | _ -> (acc, from))
                 ([], 0) (all_matches re s)
             in
             array (List.rev (String (String.sub s last (String.length s - last)) :: pieces))
         | _ -> ( match split with Object { kind = Host_function (_, f); _ } -> f ~this args | _ -> Undefined)));
  ignore regexps;
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
  def "lastIndexOf" (fun arr _ args ->
      let v = arg args 0 in
      let rec go i l = match l with [] -> -1 | x :: r -> if strict_equal x v then i else go (i - 1) r in
      Number (float_of_int (go (List.length (array_items arr) - 1) (List.rev (array_items arr)))));
  (* splice(start, count, items...): the removed, the items put in their place *)
  def "splice" (fun arr items args ->
      let all = array_items arr in
      let n = items.length in
      let start = relative (int_arg args 0 ~default:0) n in
      let count = max 0 (min (int_arg args 1 ~default:(n - start)) (n - start)) in
      let inserted = match args with _ :: _ :: rest -> rest | _ -> [] in
      let before = List.filteri (fun i _ -> i < start) all and removed = List.filteri (fun i _ -> i >= start && i < start + count) all in
      let after = List.filteri (fun i _ -> i >= start + count) all in
      set_items items (before @ inserted @ after);
      array removed);
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

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(* a day number since 1970-01-01 as its year, month (1-12), day: Howard
 * Hinnant's civil_from_days *)
let civil (days : int) : int * int * int =
  let z = days + 719468 in
  let era = (if z >= 0 then z else z - 146096) / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((yoe + (era * 400)) + (if m <= 2 then 1 else 0), m, d)

(* a Date: its milliseconds since 1970 (UTC, the only zone here), its
 * getters *)
let date (ms : float) : value =
  let days = int_of_float (Float.floor (ms /. 86_400_000.)) in
  let in_day = ms -. (float_of_int days *. 86_400_000.) in
  let y, m, d = civil days in
  let field name v = (name, fn name (fun ~this:_ _ -> Number v)) in
  let hours = Float.floor (in_day /. 3_600_000.) and minutes = Float.floor (Float.rem in_day 3_600_000. /. 60_000.) in
  let seconds = Float.floor (Float.rem in_day 60_000. /. 1000.) in
  let iso = Printf.sprintf "%04d-%02d-%02dT%02.0f:%02.0f:%02.0f.%03.0fZ" y m d hours minutes seconds (Float.rem in_day 1000.) in
  let methods =
    [ field "getTime" ms; field "valueOf" ms; field "getFullYear" (float_of_int y); field "getMonth" (float_of_int (m - 1));
      field "getDate" (float_of_int d); field "getDay" (float_of_int (((days mod 7) + 11) mod 7)); field "getHours" hours;
      field "getMinutes" minutes; field "getSeconds" seconds; field "getMilliseconds" (Float.rem in_day 1000.);
      field "getTimezoneOffset" 0.;
      ("toISOString", fn "toISOString" (fun ~this:_ _ -> String iso)); ("toString", fn "toString" (fun ~this:_ _ -> String iso)) ]
    |> List.concat_map (fun (k, v) -> if String.starts_with ~prefix:"get" k then [ (k, v); ("getUTC" ^ String.sub k 3 (String.length k - 3), v) ] else [ (k, v) ])
  in
  host_object
    { class_name = "Date"; get = (fun k -> Option.value (List.assoc_opt k methods) ~default:Undefined); set = (fun _ _ -> ()); show = (fun () -> iso) }

(*****************************************************************************)
(* URIs *)
(*****************************************************************************)

(* encodeURIComponent: every byte but letters, digits and -_.!~*'() as
 * %XX; encodeURI keeps a URI's punctuation too *)
let percent_encode ~(keep : string) (s : string) : string =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || String.contains keep c then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let percent_decode (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '%' && i + 2 < n + 0 && i + 2 <= n - 1 then
        match int_of_string_opt ("0x" ^ String.sub s (i + 1) 2) with Some c -> Buffer.add_char b (Char.chr c); go (i + 3) | None -> Buffer.add_char b '%'; go (i + 1)
      else (Buffer.add_char b s.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

let install ~(call : value -> this:value -> value list -> value) ~(log : string -> unit) ~(seed : int) ?(now = fun () -> 0.)
    (define : string -> value -> unit) : protos =
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
  (* the prototypes: an object's, a function's, a regular expression's,
   * a number's; a string's and an array's below *)
  let objects = new_object () and functions = new_object () and regexps = new_object () and numbers = new_object () in
  let method_ (o : obj) name f = set_own o name (fn name f) in
  method_ objects "hasOwnProperty" (fun ~this args -> match this with Object o -> Bool (get_own o (to_string (arg args 0)) <> None) | _ -> Bool false);
  method_ objects "toString" (fun ~this _ -> to_primitive this);
  method_ functions "call" (fun ~this args -> match args with [] -> call this ~this:Undefined [] | self :: rest -> call this ~this:self rest);
  method_ functions "apply" (fun ~this args ->
      call this ~this:(arg args 0) (match arg args 1 with Object ({ kind = Array _; _ } as a) -> array_items a | _ -> []));
  method_ functions "bind" (fun ~this args ->
      let f = this and self = arg args 0 and bound = match args with _ :: rest -> rest | [] -> [] in
      fn "bound" (fun ~this:_ more -> call f ~this:self (bound @ more)));
  let regexp_of this = match this with Object { kind = Regexp re; _ } -> re | _ -> throw "TypeError" "not a RegExp" in
  (* exec and test: from lastIndex, and moving it, when global *)
  let exec this s =
    let re = regexp_of this in
    let o = match this with Object o -> o | _ -> assert false in
    let from = if Js_regexp.global re then int_of_float (to_number (Option.value (get_own o "lastIndex") ~default:(Number 0.))) else 0 in
    match Js_regexp.exec re s from with
    | Some spans ->
        (if Js_regexp.global re then match spans.(0) with Some (_, e) -> set_own o "lastIndex" (Number (float_of_int e)) | None -> ());
        Some spans
    | None ->
        set_own o "lastIndex" (Number 0.);
        None
  in
  method_ regexps "exec" (fun ~this args -> let s = to_string (arg args 0) in match exec this s with Some spans -> match_array s spans | None -> Null);
  method_ regexps "test" (fun ~this args -> Bool (exec this (to_string (arg args 0)) <> None));
  method_ regexps "toString" (fun ~this _ -> to_primitive this);
  method_ numbers "toFixed" (fun ~this args -> String (Printf.sprintf "%.*f" (int_arg args 0 ~default:0) (to_number this)));
  method_ numbers "toString" (fun ~this _ -> String (to_string this));
  let strings = string_methods ~call ~regexps and arrays = array_methods ~call in
  (* a constructor with its prototype and its own functions *)
  let constructor name f (proto : obj) (statics : (string * value) list) =
    let c = fn name f in
    (match c with
    | Object o ->
        set_own o "prototype" (Object proto);
        set_own proto "constructor" c;
        List.iter (fun (k, v) -> set_own o k v) statics
    | _ -> ());
    define name c
  in
  constructor "String" (fun ~this:_ args -> String (match args with [] -> "" | v :: _ -> to_string v)) strings
    [ ("fromCharCode", fn "fromCharCode" (fun ~this:_ args -> String (String.concat "" (List.map (fun v -> String.make 1 (Char.chr (int_of_float (to_number v) land 255))) args)))) ];
  constructor "Number" (fun ~this:_ args -> Number (match args with [] -> 0. | v :: _ -> to_number v)) numbers [];
  constructor "Function" (fun ~this:_ _ -> throw "EvalError" "new Function is not supported") functions [];
  constructor "RegExp"
    (fun ~this:_ args ->
      match arg args 0 with
      | Object { kind = Regexp re; _ } -> regexp_value regexps re
      | v -> regexp_value regexps (compile (to_string v) (match arg args 1 with Undefined -> "" | f -> to_string f)))
    regexps [];
  constructor "Date"
    (fun ~this:_ args -> date (match args with [] -> now () | v :: _ -> to_number v))
    (new_object ())
    [ ("now", fn "now" (fun ~this:_ _ -> Number (now ()))) ];
  (* Error and its kinds: an error object, with new or without *)
  List.iter
    (fun name ->
      let proto = new_object () in
      set_own proto "name" (String name);
      constructor name (fun ~this:_ args -> error name (match arg args 0 with Undefined -> "" | v -> to_string v)) proto [])
    [ "Error"; "TypeError"; "RangeError"; "SyntaxError"; "ReferenceError" ];
  define "encodeURIComponent" (fn "encodeURIComponent" (fun ~this:_ args -> String (percent_encode ~keep:"-_.!~*'()" (to_string (arg args 0)))));
  define "encodeURI" (fn "encodeURI" (fun ~this:_ args -> String (percent_encode ~keep:"-_.!~*'();/?:@&=+$,#" (to_string (arg args 0)))));
  define "decodeURIComponent" (fn "decodeURIComponent" (fun ~this:_ args -> String (percent_decode (to_string (arg args 0)))));
  define "decodeURI" (fn "decodeURI" (fun ~this:_ args -> String (percent_decode (to_string (arg args 0)))));
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
  constructor "Object"
    (fun ~this:_ args -> match arg args 0 with Object _ as o -> o | _ -> Object (new_object ()))
    objects
    [ ("create",
       fn "create" (fun ~this:_ args ->
           let o = new_object () in
           (match arg args 0 with Object p -> o.proto <- Some p | _ -> ());
           Object o));
      ("getPrototypeOf", fn "getPrototypeOf" (fun ~this:_ args -> match arg args 0 with Object { proto = Some p; _ } -> Object p | _ -> Null));
      ("assign",
       fn "assign" (fun ~this:_ args ->
           match args with
           | (Object target as t) :: sources ->
               List.iter (fun v -> match v with Object o -> List.iter (fun k -> set_own target k (Option.get (get_own o k))) (keys o) | _ -> ()) sources;
               t
           | v :: _ -> v
           | [] -> Undefined));
      ("keys",
          fn "keys" (fun ~this:_ args ->
              match arg args 0 with
              | Object ({ kind = Array a; _ }) -> array (List.init a.length (fun i -> String (string_of_int i)))
              | Object o -> array (List.map (fun k -> String k) (keys o))
              | _ -> array [])) ];
  constructor "Array"
    (fun ~this:_ args -> match args with [ Number n ] -> array (List.init (int_of_float n) (fun _ -> Undefined)) | _ -> array args)
    arrays
    [ ("isArray", fn "isArray" (fun ~this:_ args -> Bool (match arg args 0 with Object { kind = Array _; _ } -> true | _ -> false)));
      ("from", fn "from" (fun ~this:_ args -> match arg args 0 with Object ({ kind = Array _; _ } as a) -> array (array_items a) | _ -> array [])) ];
  { strings; arrays; objects; functions; regexps; numbers }
