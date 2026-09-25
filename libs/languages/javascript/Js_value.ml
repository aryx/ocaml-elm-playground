(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_value.mli *)

type value = Undefined | Null | Bool of bool | Number of float | String of string | Object of obj
and obj = { id : int; mutable props : (string * value ref) list; kind : kind }

and kind =
  | Plain
  | Array of items
  | Closure of closure
  | Host_function of string * (this:value -> value list -> value)

and items = { mutable elements : value array; mutable length : int }
and closure = { func : Js_ast.func; scope : scope; this : value option }
and scope = { vars : (string, binding) Hashtbl.t; parent : scope option }
and binding = { mutable value : value; constant : bool }

exception Throw of value

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)

let counter = ref 0

let make (kind : kind) : obj =
  incr counter;
  { id = !counter; props = []; kind }

let new_object () : obj = make Plain

let new_array (vs : value list) : obj =
  let elements = Array.of_list vs in
  make (Array { elements; length = Array.length elements })

let host_function (name : string) (f : this:value -> value list -> value) : value = Object (make (Host_function (name, f)))
let get_own (o : obj) (k : string) : value option = Option.map ( ! ) (List.assoc_opt k o.props)

let set_own (o : obj) (k : string) (v : value) : unit =
  match List.assoc_opt k o.props with Some r -> r := v | None -> o.props <- (k, ref v) :: o.props

let keys (o : obj) : string list = List.rev_map fst o.props
let array_items (o : obj) : value list = match o.kind with Array a -> Array.to_list (Array.sub a.elements 0 a.length) | _ -> []

let error (name : string) (message : string) : value =
  let o = new_object () in
  set_own o "name" (String name);
  set_own o "message" (String message);
  Object o

let throw (name : string) (message : string) : 'a = raise (Throw (error name message))

(*****************************************************************************)
(* Conversions *)
(*****************************************************************************)

let typeof (v : value) : string =
  match v with
  | Undefined -> "undefined"
  (* the mistake of 1995, kept since: pages relied on it *)
  | Null -> "object"
  | Bool _ -> "boolean"
  | Number _ -> "number"
  | String _ -> "string"
  | Object { kind = Closure _ | Host_function _; _ } -> "function"
  | Object _ -> "object"

let truthy (v : value) : bool =
  match v with
  | Undefined | Null -> false
  | Bool b -> b
  | Number f -> not (f = 0. || Float.is_nan f)
  | String s -> s <> ""
  | Object _ -> true

(* -0 is printed 0, as JavaScript does *)
let number_to_string (f : float) : string = if f = 0. then "0" else Js_ast.number_to_string f

let rec to_string (v : value) : string =
  match v with
  | Undefined -> "undefined"
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number f -> number_to_string f
  | String s -> s
  | Object _ -> to_string (to_primitive v)

(* an object as a primitive: an array its items joined with commas
 * (undefined and null as ""), a function its source's stand-in, an
 * object "[object Object]" *)
and to_primitive (v : value) : value =
  match v with
  | Object ({ kind = Array _; _ } as o) ->
      String (String.concat "," (List.map (fun v -> match v with Undefined | Null -> "" | v -> to_string v) (array_items o)))
  | Object { kind = Closure { func = { name; _ }; _ }; _ } ->
      String (Printf.sprintf "function %s() { ... }" (Option.value name ~default:""))
  | Object { kind = Host_function (name, _); _ } -> String (Printf.sprintf "function %s() { [native code] }" name)
  (* an error, as Error.prototype.toString says it: "TypeError: ..."
   * (with no prototypes, told by its name and message) *)
  | Object ({ kind = Plain; _ } as o) -> (
      match (get_own o "name", get_own o "message") with
      | Some (String name), Some (String message) -> String (name ^ ": " ^ message)
      | _ -> String "[object Object]")
  | v -> v

let to_number (v : value) : float =
  match to_primitive v with
  | Undefined -> Float.nan
  | Null -> 0.
  | Bool b -> if b then 1. else 0.
  | Number f -> f
  | String s -> (
      let s = String.trim s in
      if s = "" then 0.
      else
        match s with
        | "Infinity" | "+Infinity" -> Float.infinity
        | "-Infinity" -> Float.neg_infinity
        | _ ->
            (* decimal digits, a point, an exponent, or 0x: nothing else
             * (OCaml's float_of_string also reads "nan", "1_000", "0b1") *)
            let ok = String.for_all (fun c -> (c >= '0' && c <= '9') || String.contains ".eE+-xXabcdefABCDEF" c) s in
            let hex = String.length s > 2 && (String.sub s 0 2 = "0x" || String.sub s 0 2 = "0X") in
            let decimal = not (String.exists (fun c -> String.contains "xXabcdfABCDF" c) s) in
            if ok && (hex || decimal) then Option.value (float_of_string_opt s) ~default:Float.nan else Float.nan)
  | Object _ -> Float.nan

let strict_equal (a : value) (b : value) : bool =
  match (a, b) with
  | Undefined, Undefined | Null, Null -> true
  | Bool x, Bool y -> x = y
  | Number x, Number y -> x = y (* NaN is not equal to itself; +0 is -0 *)
  | String x, String y -> String.equal x y
  | Object x, Object y -> x == y
  | _ -> false

(*****************************************************************************)
(* Showing values *)
(*****************************************************************************)

let display (v : value) : string =
  let rec go ~top (seen : obj list) (v : value) =
    match v with
    | String s -> if top then s else Printf.sprintf "%S" s
    | Object o when List.memq o seen -> "[Circular]"
    | Object ({ kind = Array _; _ } as o) -> "[" ^ String.concat ", " (List.map (go ~top:false (o :: seen)) (array_items o)) ^ "]"
    | Object ({ kind = Plain; _ } as o) ->
        "{" ^ String.concat ", " (List.map (fun k -> k ^ ": " ^ go ~top:false (o :: seen) (Option.get (get_own o k))) (keys o)) ^ "}"
    | Object { kind = Closure { func = { name; _ }; _ }; _ } -> "function " ^ Option.value name ~default:"(anonymous)"
    | Object { kind = Host_function (name, _); _ } -> "function " ^ name
    | v -> to_string v
  in
  go ~top:true [] v

let to_json (v : value) : string option =
  let quote s =
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
  in
  let exception Cycle in
  (* None: left out (undefined, a function) *)
  let rec go (seen : obj list) (v : value) : string option =
    match v with
    | Undefined -> None
    | Null -> Some "null"
    | Bool b -> Some (string_of_bool b)
    (* JSON has no NaN and no Infinity *)
    | Number f -> Some (if Float.is_finite f then number_to_string f else "null")
    | String s -> Some (quote s)
    | Object o when List.memq o seen -> raise Cycle
    | Object ({ kind = Array _; _ } as o) ->
        Some ("[" ^ String.concat "," (List.map (fun v -> Option.value (go (o :: seen) v) ~default:"null") (array_items o)) ^ "]")
    | Object ({ kind = Plain; _ } as o) ->
        Some
          ("{"
          ^ String.concat ","
              (List.filter_map (fun k -> Option.map (fun s -> quote k ^ ":" ^ s) (go (o :: seen) (Option.get (get_own o k)))) (keys o))
          ^ "}")
    | Object _ -> None
  in
  match go [] v with s -> s | exception Cycle -> None
