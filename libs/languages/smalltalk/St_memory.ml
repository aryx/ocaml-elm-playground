(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_memory.mli *)

type oop = int
type body = Pointers of oop array | Bytes of Bytes.t | Float of float | Method of oop array * Bytes.t | Free

type known = {
  mutable small_integer : oop;
  mutable string : oop;
  mutable symbol : oop;
  mutable array : oop;
  mutable float : oop;
  mutable character : oop;
  mutable compiled_method : oop;
  mutable method_context : oop;
  mutable block_context : oop;
  mutable message : oop;
  mutable association : oop;
  mutable point : oop;
  mutable large_positive : oop;
  mutable large_negative : oop;
  mutable metaclass : oop;
  mutable method_dictionary : oop;
  mutable true_ : oop;
  mutable false_ : oop;
  mutable smalltalk : oop;
  mutable characters : oop array;
  mutable special_selectors : oop array;
}

type t = {
  mutable classes : oop array; (* entry i's class *)
  mutable bodies : body array;
  mutable free : int list; (* entries freed, used first *)
  mutable top : int; (* the entries from here on were never used *)
  mutable in_use : int;
  mutable allocated : int;
  symbol_table : (string, oop) Hashtbl.t;
  known : known;
}

let nil = 0

let empty_known () =
  {
    small_integer = 0;
    string = 0;
    symbol = 0;
    array = 0;
    float = 0;
    character = 0;
    compiled_method = 0;
    method_context = 0;
    block_context = 0;
    message = 0;
    association = 0;
    point = 0;
    large_positive = 0;
    large_negative = 0;
    metaclass = 0;
    method_dictionary = 0;
    true_ = 0;
    false_ = 0;
    smalltalk = 0;
    characters = [||];
    special_selectors = [||];
  }

let create () : t =
  let n = 4096 in
  let m =
    {
      classes = Array.make n 0;
      bodies = Array.make n Free;
      free = [];
      top = 1;
      in_use = 1;
      allocated = 0;
      symbol_table = Hashtbl.create 1024;
      known = empty_known ();
    }
  in
  (* nil, the entry 0, an object with no fields *)
  m.bodies.(0) <- Pointers [||];
  m

let known (m : t) : known = m.known

(*****************************************************************************)
(* SmallIntegers *)
(*****************************************************************************)

let is_int (o : oop) : bool = o land 1 = 1
let int_of (o : oop) : int = o asr 1
let of_int (i : int) : oop = (i lsl 1) lor 1
let fits (i : int) : bool = i >= St_lexer.min_small && i <= St_lexer.max_small

(*****************************************************************************)
(* Objects *)
(*****************************************************************************)

let index (o : oop) : int = o lsr 1

let grow (m : t) : unit =
  let n = Array.length m.classes in
  let classes = Array.make (2 * n) 0 and bodies = Array.make (2 * n) Free in
  Array.blit m.classes 0 classes 0 n;
  Array.blit m.bodies 0 bodies 0 n;
  m.classes <- classes;
  m.bodies <- bodies

let alloc (m : t) ~(cls : oop) (b : body) : oop =
  let i =
    match m.free with
    | i :: rest ->
        m.free <- rest;
        i
    | [] ->
        if m.top >= Array.length m.classes then grow m;
        let i = m.top in
        m.top <- m.top + 1;
        i
  in
  m.classes.(i) <- cls;
  m.bodies.(i) <- b;
  m.in_use <- m.in_use + 1;
  m.allocated <- m.allocated + 1;
  i lsl 1

let class_of (m : t) (o : oop) : oop = if is_int o then m.known.small_integer else m.classes.(index o)
let set_class (m : t) (o : oop) (c : oop) : unit = m.classes.(index o) <- c
let body (m : t) (o : oop) : body = if is_int o then Free else m.bodies.(index o)
let set_body (m : t) (o : oop) (b : body) : unit = m.bodies.(index o) <- b

let fields (m : t) (o : oop) : oop array =
  match body m o with Pointers a | Method (a, _) -> a | Bytes _ | Float _ | Free -> [||]

let fetch (m : t) (o : oop) (i : int) : oop = (fields m o).(i)
let store (m : t) (o : oop) (i : int) (v : oop) : unit = (fields m o).(i) <- v

let size (m : t) (o : oop) : int =
  match body m o with Pointers a | Method (a, _) -> Array.length a | Bytes b -> Bytes.length b | Float _ | Free -> 0

let string_of (m : t) (o : oop) : string = match body m o with Bytes b -> Bytes.to_string b | _ -> ""
let new_string (m : t) (s : string) : oop = alloc m ~cls:m.known.string (Bytes (Bytes.of_string s))
let new_array (m : t) (a : oop array) : oop = alloc m ~cls:m.known.array (Pointers a)
let new_float (m : t) (f : float) : oop = alloc m ~cls:m.known.float (Float f)
let float_of (m : t) (o : oop) : float = match body m o with Float f -> f | _ -> nan

let symbol (m : t) (s : string) : oop =
  match Hashtbl.find_opt m.symbol_table s with
  | Some o -> o
  | None ->
      let o = alloc m ~cls:m.known.symbol (Bytes (Bytes.of_string s)) in
      Hashtbl.replace m.symbol_table s o;
      o

let symbols (m : t) : (string * oop) list = Hashtbl.fold (fun s o acc -> (s, o) :: acc) m.symbol_table []

let become (m : t) (a : oop) (b : oop) : unit =
  let i = index a and j = index b in
  let c = m.classes.(i) and bd = m.bodies.(i) in
  m.classes.(i) <- m.classes.(j);
  m.bodies.(i) <- m.bodies.(j);
  m.classes.(j) <- c;
  m.bodies.(j) <- bd

(*****************************************************************************)
(* The whole memory *)
(*****************************************************************************)

let instances (m : t) (cls : oop) : oop list =
  let acc = ref [] in
  for i = m.top - 1 downto 0 do
    if m.classes.(i) = cls && m.bodies.(i) <> Free then acc := (i lsl 1) :: !acc
  done;
  !acc

let live (m : t) : int = m.in_use
let allocated (m : t) : int = m.allocated

let gc (m : t) ~(roots : oop list) : int =
  let marks = Bytes.make m.top '\000' in
  let k = m.known in
  (* an explicit stack rather than recursion: a long linked list of
   * contexts would overflow OCaml's *)
  let stack = ref [] in
  let push o =
    if (not (is_int o)) && index o < m.top && Bytes.get marks (index o) = '\000' then begin
      Bytes.set marks (index o) '\001';
      stack := o :: !stack
    end
  in
  let scan o =
    let i = index o in
    push m.classes.(i);
    match m.bodies.(i) with
    | Pointers a ->
        let cls = m.classes.(i) in
        let last =
          if (cls = k.method_context || cls = k.block_context) && Array.length a > 2 && is_int a.(2) then
            min (Array.length a - 1) (int_of a.(2))
          else Array.length a - 1
        in
        for f = 0 to last do
          push a.(f)
        done
    | Method (a, _) -> Array.iter push a
    | Bytes _ | Float _ | Free -> ()
  in
  push nil;
  List.iter push [ k.true_; k.false_; k.smalltalk ];
  Array.iter push k.characters;
  Array.iter push k.special_selectors;
  Hashtbl.iter (fun _ o -> push o) m.symbol_table;
  List.iter push roots;
  let rec drain () =
    match !stack with
    | [] -> ()
    | o :: rest ->
        stack := rest;
        scan o;
        drain ()
  in
  drain ();
  let freed = ref 0 in
  for i = 1 to m.top - 1 do
    if Bytes.get marks i = '\000' && m.bodies.(i) <> Free then begin
      m.bodies.(i) <- Free;
      m.classes.(i) <- 0;
      m.free <- i :: m.free;
      incr freed
    end
  done;
  m.in_use <- m.in_use - !freed;
  m.allocated <- 0;
  !freed

let entries (m : t) : (int * oop * body) list =
  let acc = ref [] in
  for i = m.top - 1 downto 0 do
    if m.bodies.(i) <> Free then acc := (i, m.classes.(i), m.bodies.(i)) :: !acc
  done;
  !acc

let restore (known : known) (es : (int * oop * body) list) : t =
  let top = List.fold_left (fun acc (i, _, _) -> max acc (i + 1)) 1 es in
  let n = ref 4096 in
  while !n < top + 1 do
    n := 2 * !n
  done;
  let m =
    {
      classes = Array.make !n 0;
      bodies = Array.make !n Free;
      free = [];
      top;
      in_use = 0;
      allocated = 0;
      symbol_table = Hashtbl.create 1024;
      known;
    }
  in
  List.iter
    (fun (i, c, b) ->
      m.classes.(i) <- c;
      m.bodies.(i) <- b;
      m.in_use <- m.in_use + 1)
    es;
  for i = top - 1 downto 1 do
    if m.bodies.(i) = Free then m.free <- i :: m.free
  done;
  List.iter
    (fun (i, c, b) -> match b with Bytes s when c = known.symbol -> Hashtbl.replace m.symbol_table (Bytes.to_string s) (i lsl 1) | _ -> ())
    es;
  m
