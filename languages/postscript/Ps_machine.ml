(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ps_machine.mli *)

module G = Ps_graphics

type value =
  | Int of int
  | Real of float
  | Bool of bool
  | Name of string
  | Literal of string
  | String of string
  | Array of array_
  | Dict of (string, value) Hashtbl.t
  | Operator of string
  | Font of float
  | Mark
  | Null

and array_ = { items : value array; spans : (int * int) array; exec : bool }

type paint = { lines : ((float * float) list * bool) list; rgb : float * float * float; how : how }
and how = Fill | Stroke of float

type host = { glyph : char -> (float * float) list list * float }
type status = Running | Done | Failed of string

(*****************************************************************************)
(* The machine *)
(*****************************************************************************)

(* the graphics state: what gsave saves and grestore brings back *)
type gstate = {
  ctm : G.matrix;
  path : G.segment list; (* reversed *)
  current : (float * float) option; (* in device space *)
  start : (float * float) option; (* the subpath's first point *)
  rgb : float * float * float;
  line_width : float;
  font : float;
}

(* what is being run: the program's text from a place, a procedure
   from an item, or a loop and what is left of it *)
type frame =
  | Source of int
  | Run of array_ * int
  | Repeat of int * array_
  | For of float * float * float * bool * array_ (* the value, the step, the limit, integers? *)
  | Loop of array_
  | Forall of value list list * array_ (* what each turn pushes *)

type t = {
  text : string;
  host : host;
  operands : value list; (* top first *)
  exec : frame list;
  dicts : (string, value) Hashtbl.t list; (* userdict, then systemdict, at the bottom *)
  gs : gstate;
  saved : gstate list;
  painted : paint list; (* reversed *)
  finished : paint list list; (* reversed *)
  printed : string list; (* reversed *)
  state : status;
  last : (int * int) option;
  count : int;
}

(* an error's name, as the Red Book names them; [Offending] adds the
   command that raised it *)
exception Error of string
exception Offending of string * string

let initial_gs = { ctm = G.identity; path = []; current = None; start = None; rgb = (0., 0., 0.); line_width = 1.; font = 0. }

(*****************************************************************************)
(* Values *)
(*****************************************************************************)

let number_text f =
  let s = Printf.sprintf "%g" f in
  if String.contains s '.' || String.contains s 'e' || String.contains s 'n' || String.contains s 'i' then s else s ^ ".0"

let rec show v =
  match v with
  | Int n -> string_of_int n
  | Real f -> number_text f
  | Bool b -> string_of_bool b
  | Name n -> n
  | Literal n -> "/" ^ n
  | String s -> "(" ^ s ^ ")"
  | Array a ->
      let inside = String.concat " " (Array.to_list (Array.map show a.items)) in
      if a.exec then "{" ^ inside ^ "}" else "[" ^ inside ^ "]"
  | Dict _ -> "-dict-"
  | Operator n -> "--" ^ n ^ "--"
  | Font _ -> "-font-"
  | Mark -> "-mark-"
  | Null -> "null"

(* what = prints: a string or a name as its characters *)
let text_of v = match v with String s -> s | Name n | Literal n -> n | v -> show v

let push v m = { m with operands = v :: m.operands }
let pop m = match m.operands with v :: rest -> (v, { m with operands = rest }) | [] -> raise (Error "stackunderflow")

let num = function Int n -> float_of_int n | Real f -> f | _ -> raise (Error "typecheck")
let pop_num m = let v, m = pop m in (num v, m)
let pop_int m = match pop m with Int n, m -> (n, m) | _ -> raise (Error "typecheck")
let pop_bool m = match pop m with Bool b, m -> (b, m) | _ -> raise (Error "typecheck")
let pop_proc m = match pop m with Array a, m -> (a, m) | _ -> raise (Error "typecheck")
let pop_key m = match pop m with (Literal k | String k | Name k), m -> (k, m) | _ -> raise (Error "typecheck")

let rec lookup dicts key = match dicts with [] -> None | d :: rest -> ( match Hashtbl.find_opt d key with Some v -> Some v | None -> lookup rest key)
let run_frame a m = { m with exec = Run (a, 0) :: m.exec }
let output s m = { m with printed = s :: m.printed }

(*****************************************************************************)
(* Operators *)
(*****************************************************************************)

(* integers stay integers when both are, as the Red Book's add does *)
let arith fi ff m =
  let b, m = pop m in
  let a, m = pop m in
  match (a, b) with Int x, Int y -> push (Int (fi x y)) m | _ -> push (Real (ff (num a) (num b))) m

let real1 f m = let x, m = pop_num m in push (Real (f x)) m
let deg r = r *. 180. /. Float.pi
let rad d = d *. Float.pi /. 180.

let rec equal a b =
  match (a, b) with
  | (Int _ | Real _), (Int _ | Real _) -> num a = num b
  | (Name x | Literal x | String x), (Name y | Literal y | String y) -> x = y
  | Array x, Array y -> x.items == y.items
  | Dict x, Dict y -> x == y
  | _ -> a = b && equal_simple a

and equal_simple = function Bool _ | Mark | Null | Operator _ | Font _ -> true | _ -> false

let compare_op test m =
  let b, m = pop m in
  let a, m = pop m in
  let c = match (a, b) with String x, String y -> compare x y | _ -> compare (num a) (num b) in
  push (Bool (test c)) m

let logic fb fi m =
  let b, m = pop m in
  let a, m = pop m in
  match (a, b) with Bool x, Bool y -> push (Bool (fb x y)) m | Int x, Int y -> push (Int (fi x y)) m | _ -> raise (Error "typecheck")

(* the top [n] operands, top first, and the rest *)
let rec take n l = if n = 0 then ([], l) else match l with x :: rest -> let a, b = take (n - 1) rest in (x :: a, b) | [] -> raise (Error "stackunderflow")

(* what forall pushes at each turn: an item, a character's code, or a
   key and its value *)
let turns_of v = match v with
  | Array a -> List.map (fun x -> [ x ]) (Array.to_list a.items)
  | String s -> List.init (String.length s) (fun i -> [ Int (Char.code s.[i]) ])
  | Dict d -> Hashtbl.fold (fun k v acc -> [ Literal k; v ] :: acc) d []
  | _ -> raise (Error "typecheck")

let gs f m = { m with gs = f m.gs }
let current m = match m.gs.current with Some p -> p | None -> raise (Error "nocurrentpoint")
let add_segment s m = gs (fun g -> { g with path = s :: g.path }) m

let move_to p m = gs (fun g -> { g with path = G.Move p :: g.path; current = Some p; start = Some p }) m
let line_to p m = ignore (current m); gs (fun g -> { g with path = G.Line p :: g.path; current = Some p }) m

let paint how m =
  let lines = G.flatten (List.rev m.gs.path) in
  let m = if lines = [] then m else { m with painted = { lines; rgb = m.gs.rgb; how } :: m.painted } in
  gs (fun g -> { g with path = []; current = None; start = None }) m

let hsb h s b =
  let i = Float.to_int (Float.floor (h *. 6.)) mod 6 and f = (h *. 6.) -. Float.floor (h *. 6.) in
  let p = b *. (1. -. s) and q = b *. (1. -. (s *. f)) and t = b *. (1. -. (s *. (1. -. f))) in
  match i with 0 -> (b, t, p) | 1 -> (q, b, p) | 2 -> (p, b, t) | 3 -> (p, q, b) | 4 -> (t, p, b) | _ -> (b, p, q)

(* a string's letters, stroked from the current point *)
let show_text s m =
  let size = m.gs.font in
  if size = 0. then raise (Error "invalidfont");
  let ux, uy = G.transform (G.invert m.gs.ctm) (current m) in
  let strokes, x =
    String.fold_left
      (fun (acc, x) ch ->
        let lines, advance = m.host.glyph ch in
        let placed = List.map (fun l -> (List.map (fun (gx, gy) -> G.transform m.gs.ctm (x +. (size *. gx), uy +. (size *. gy))) l, false)) lines in
        (placed @ acc, x +. (size *. advance)))
      ([], ux) s
  in
  let m = if strokes = [] then m else { m with painted = { lines = strokes; rgb = m.gs.rgb; how = Stroke (size *. 0.07 *. G.scale_of m.gs.ctm) } :: m.painted } in
  gs (fun g -> { g with current = Some (G.transform g.ctm (x, uy)) }) m

let width_of s m = String.fold_left (fun w ch -> w +. (m.gs.font *. snd (m.host.glyph ch))) 0. s

let apply (op : string) (m : t) : t =
  match op with
  (* the stack *)
  | "pop" -> snd (pop m)
  | "exch" -> let b, m = pop m in let a, m = pop m in push a (push b m)
  | "dup" -> let a, m = pop m in push a (push a m)
  | "copy" -> let n, m = pop_int m in let top, _ = take n m.operands in { m with operands = top @ m.operands }
  | "index" -> let n, m = pop_int m in ( match List.nth_opt m.operands n with Some v when n >= 0 -> push v m | _ -> raise (Error "rangecheck"))
  | "roll" ->
      let j, m = pop_int m in
      let n, m = pop_int m in
      if n = 0 then m
      else
        let top, rest = take n m.operands in
        (* bottom first, rotated j places toward the top *)
        let bottom_first = List.rev top in
        let j = ((j mod n) + n) mod n in
        let a, b = take (n - j) bottom_first in
        { m with operands = List.rev (b @ a) @ rest }
  | "clear" -> { m with operands = [] }
  | "count" -> push (Int (List.length m.operands)) m
  | "mark" | "[" -> push Mark m
  | "cleartomark" -> let rec drop = function Mark :: rest -> rest | _ :: rest -> drop rest | [] -> raise (Error "unmatchedmark") in { m with operands = drop m.operands }
  | "counttomark" -> let rec n i = function Mark :: _ -> i | _ :: rest -> n (i + 1) rest | [] -> raise (Error "unmatchedmark") in push (Int (n 0 m.operands)) m
  | "]" ->
      let rec gather acc = function Mark :: rest -> (acc, rest) | v :: rest -> gather (v :: acc) rest | [] -> raise (Error "unmatchedmark") in
      let items, rest = gather [] m.operands in
      push (Array { items = Array.of_list items; spans = [||]; exec = false }) { m with operands = rest }
  (* arithmetic *)
  | "add" -> arith ( + ) ( +. ) m
  | "sub" -> arith ( - ) ( -. ) m
  | "mul" -> arith ( * ) ( *. ) m
  | "div" -> let b, m = pop_num m in let a, m = pop_num m in if b = 0. then raise (Error "undefinedresult") else push (Real (a /. b)) m
  | "idiv" -> let b, m = pop_int m in let a, m = pop_int m in if b = 0 then raise (Error "undefinedresult") else push (Int (a / b)) m
  | "mod" -> let b, m = pop_int m in let a, m = pop_int m in if b = 0 then raise (Error "undefinedresult") else push (Int (a mod b)) m
  | "neg" -> ( match pop m with Int n, m -> push (Int (-n)) m | v, m -> push (Real (-.num v)) m)
  | "abs" -> ( match pop m with Int n, m -> push (Int (abs n)) m | v, m -> push (Real (Float.abs (num v))) m)
  | "sqrt" -> real1 Float.sqrt m
  | "sin" -> real1 (fun d -> Float.sin (rad d)) m
  | "cos" -> real1 (fun d -> Float.cos (rad d)) m
  | "atan" -> let den, m = pop_num m in let n, m = pop_num m in let a = deg (Float.atan2 n den) in push (Real (if a < 0. then a +. 360. else a)) m
  | "exp" -> let e, m = pop_num m in let b, m = pop_num m in push (Real (b ** e)) m
  | "ln" -> real1 Float.log m
  | "log" -> real1 Float.log10 m
  | "round" | "floor" | "ceiling" | "truncate" -> (
      let f = match op with "round" -> Float.round | "floor" -> Float.floor | "ceiling" -> Float.ceil | _ -> Float.trunc in
      match pop m with Int n, m -> push (Int n) m | v, m -> push (Real (f (num v))) m)
  | "cvi" -> let x, m = pop_num m in push (Int (Float.to_int x)) m
  | "cvr" -> let x, m = pop_num m in push (Real x) m
  (* comparison and logic *)
  | "eq" -> let b, m = pop m in let a, m = pop m in push (Bool (equal a b)) m
  | "ne" -> let b, m = pop m in let a, m = pop m in push (Bool (not (equal a b))) m
  | "gt" -> compare_op (fun c -> c > 0) m
  | "ge" -> compare_op (fun c -> c >= 0) m
  | "lt" -> compare_op (fun c -> c < 0) m
  | "le" -> compare_op (fun c -> c <= 0) m
  | "and" -> logic ( && ) ( land ) m
  | "or" -> logic ( || ) ( lor ) m
  | "xor" -> logic ( <> ) ( lxor ) m
  | "not" -> ( match pop m with Bool b, m -> push (Bool (not b)) m | Int n, m -> push (Int (lnot n)) m | _ -> raise (Error "typecheck"))
  (* control: operators that take procedures *)
  | "exec" -> ( match pop m with Array a, m when a.exec -> run_frame a m | v, m -> push v m)
  | "if" -> let p, m = pop_proc m in let b, m = pop_bool m in if b then run_frame p m else m
  | "ifelse" -> let q, m = pop_proc m in let p, m = pop_proc m in let b, m = pop_bool m in run_frame (if b then p else q) m
  | "repeat" -> let p, m = pop_proc m in let n, m = pop_int m in { m with exec = Repeat (n, p) :: m.exec }
  | "for" ->
      let p, m = pop_proc m in
      let limit, m = pop_num m in
      let inc, m = pop m in
      let init, m = pop m in
      let ints = match (init, inc) with Int _, Int _ -> true | _ -> false in
      { m with exec = For (num init, num inc, limit, ints, p) :: m.exec }
  | "loop" -> let p, m = pop_proc m in { m with exec = Loop p :: m.exec }
  | "forall" -> let p, m = pop_proc m in let v, m = pop m in { m with exec = Forall (turns_of v, p) :: m.exec }
  | "exit" ->
      let rec out = function
        | (Repeat _ | For _ | Loop _ | Forall _) :: rest -> rest
        | Run _ :: rest -> out rest
        | _ -> raise (Error "invalidexit")
      in
      { m with exec = out m.exec }
  | "quit" -> { m with exec = []; state = Done }
  (* dictionaries *)
  | "def" -> let v, m = pop m in let k, m = pop_key m in Hashtbl.replace (List.hd m.dicts) k v; m
  | "dict" -> let _, m = pop_int m in push (Dict (Hashtbl.create 16)) m
  | "begin" -> ( match pop m with Dict d, m -> { m with dicts = d :: m.dicts } | _ -> raise (Error "typecheck"))
  | "end" -> if List.length m.dicts <= 2 then raise (Error "dictstackunderflow") else { m with dicts = List.tl m.dicts }
  | "currentdict" -> push (Dict (List.hd m.dicts)) m
  | "load" -> let k, m = pop_key m in ( match lookup m.dicts k with Some v -> push v m | None -> raise (Error "undefined"))
  | "store" ->
      let v, m = pop m in
      let k, m = pop_key m in
      let d = match List.find_opt (fun d -> Hashtbl.mem d k) m.dicts with Some d -> d | None -> List.hd m.dicts in
      Hashtbl.replace d k v; m
  | "known" -> let k, m = pop_key m in ( match pop m with Dict d, m -> push (Bool (Hashtbl.mem d k)) m | _ -> raise (Error "typecheck"))
  (* arrays and strings *)
  | "array" -> let n, m = pop_int m in push (Array { items = Array.make (max 0 n) Null; spans = [||]; exec = false }) m
  | "string" -> let n, m = pop_int m in push (String (String.make (max 0 n) ' ')) m
  | "length" -> (
      match pop m with
      | Array a, m -> push (Int (Array.length a.items)) m
      | String s, m -> push (Int (String.length s)) m
      | Dict d, m -> push (Int (Hashtbl.length d)) m
      | (Name s | Literal s), m -> push (Int (String.length s)) m
      | _ -> raise (Error "typecheck"))
  | "get" -> (
      let key, m = pop m in
      match (pop m, key) with
      | (Array a, m), Int i -> if i < 0 || i >= Array.length a.items then raise (Error "rangecheck") else push a.items.(i) m
      | (String s, m), Int i -> if i < 0 || i >= String.length s then raise (Error "rangecheck") else push (Int (Char.code s.[i])) m
      | (Dict d, m), (Literal k | Name k | String k) -> ( match Hashtbl.find_opt d k with Some v -> push v m | None -> raise (Error "undefined"))
      | _ -> raise (Error "typecheck"))
  | "put" -> (
      let v, m = pop m in
      let key, m = pop m in
      match (pop m, key) with
      | (Array a, m), Int i -> if i < 0 || i >= Array.length a.items then raise (Error "rangecheck") else (a.items.(i) <- v; m)
      | (Dict d, m), (Literal k | Name k | String k) -> Hashtbl.replace d k v; m
      | _ -> raise (Error "typecheck"))
  | "aload" -> ( match pop m with Array a, m -> push (Array a) (Array.fold_left (fun m v -> push v m) m a.items) | _ -> raise (Error "typecheck"))
  | "cvx" -> ( match pop m with Literal n, m -> push (Name n) m | Array a, m -> push (Array { a with exec = true }) m | v, m -> push v m)
  | "cvlit" -> ( match pop m with Name n, m -> push (Literal n) m | Array a, m -> push (Array { a with exec = false }) m | v, m -> push v m)
  | "cvn" -> let k, m = pop_key m in push (Literal k) m
  | "cvs" -> let _, m = pop m in let v, m = pop m in push (String (text_of v)) m
  (* printing, to the transcript *)
  | "=" -> let v, m = pop m in output (text_of v) m
  | "==" -> let v, m = pop m in output (show v) m
  | "print" -> ( match pop m with String s, m -> output s m | _ -> raise (Error "typecheck"))
  | "pstack" -> List.fold_left (fun m v -> output (show v) m) m m.operands
  | "stack" -> List.fold_left (fun m v -> output (text_of v) m) m m.operands
  (* the path *)
  | "newpath" -> gs (fun g -> { g with path = []; current = None; start = None }) m
  | "moveto" -> let y, m = pop_num m in let x, m = pop_num m in move_to (G.transform m.gs.ctm (x, y)) m
  | "rmoveto" ->
      let dy, m = pop_num m in
      let dx, m = pop_num m in
      let cx, cy = current m and ddx, ddy = G.dtransform m.gs.ctm (dx, dy) in
      move_to (cx +. ddx, cy +. ddy) m
  | "lineto" -> let y, m = pop_num m in let x, m = pop_num m in line_to (G.transform m.gs.ctm (x, y)) m
  | "rlineto" ->
      let dy, m = pop_num m in
      let dx, m = pop_num m in
      let cx, cy = current m and ddx, ddy = G.dtransform m.gs.ctm (dx, dy) in
      line_to (cx +. ddx, cy +. ddy) m
  | "curveto" | "rcurveto" ->
      let ys, m = List.fold_left (fun (acc, m) _ -> let v, m = pop_num m in (v :: acc, m)) ([], m) [ 1; 2; 3; 4; 5; 6 ] in
      let cx, cy = current m in
      let point i =
        let x = List.nth ys (2 * i) and y = List.nth ys ((2 * i) + 1) in
        if op = "curveto" then G.transform m.gs.ctm (x, y) else let dx, dy = G.dtransform m.gs.ctm (x, y) in (cx +. dx, cy +. dy)
      in
      let p3 = point 2 in
      gs (fun g -> { g with path = G.Curve (point 0, point 1, p3) :: g.path; current = Some p3 }) m
  | "arc" | "arcn" ->
      let a2, m = pop_num m in
      let a1, m = pop_num m in
      let r, m = pop_num m in
      let y, m = pop_num m in
      let x, m = pop_num m in
      let p0, curves = G.arc (x, y) r a1 a2 ~clockwise:(op = "arcn") in
      let t = G.transform m.gs.ctm in
      let m = match m.gs.current with Some _ -> line_to (t p0) m | None -> move_to (t p0) m in
      List.fold_left (fun m (c1, c2, p) -> gs (fun g -> { g with path = G.Curve (t c1, t c2, t p) :: g.path; current = Some (t p) }) m) m curves
  | "closepath" -> ( match m.gs.current with None -> m | Some _ -> gs (fun g -> { g with current = g.start }) (add_segment G.Close m))
  | "currentpoint" -> let x, y = G.transform (G.invert m.gs.ctm) (current m) in push (Real y) (push (Real x) m)
  (* painting *)
  | "fill" | "eofill" -> paint Fill m
  | "stroke" -> paint (Stroke (m.gs.line_width *. G.scale_of m.gs.ctm)) m
  | "setlinewidth" -> let w, m = pop_num m in gs (fun g -> { g with line_width = w }) m
  | "setgray" -> let v, m = pop_num m in gs (fun g -> { g with rgb = (v, v, v) }) m
  | "setrgbcolor" -> let b, m = pop_num m in let g', m = pop_num m in let r, m = pop_num m in gs (fun g -> { g with rgb = (r, g', b) }) m
  | "sethsbcolor" -> let b, m = pop_num m in let s, m = pop_num m in let h, m = pop_num m in gs (fun g -> { g with rgb = hsb h s b }) m
  | "gsave" -> { m with saved = m.gs :: m.saved }
  | "grestore" -> ( match m.saved with g :: rest -> { m with gs = g; saved = rest } | [] -> m)
  (* the coordinates *)
  | "translate" -> let y, m = pop_num m in let x, m = pop_num m in gs (fun g -> { g with ctm = G.concat (G.translation x y) g.ctm }) m
  | "scale" -> let y, m = pop_num m in let x, m = pop_num m in gs (fun g -> { g with ctm = G.concat (G.scaling x y) g.ctm }) m
  | "rotate" -> let a, m = pop_num m in gs (fun g -> { g with ctm = G.concat (G.rotation a) g.ctm }) m
  | "showpage" -> { m with finished = List.rev m.painted :: m.finished; painted = []; gs = initial_gs; saved = [] }
  (* text *)
  | "findfont" -> let _, m = pop_key m in push (Font 1.) m
  | "scalefont" -> let n, m = pop_num m in ( match pop m with Font s, m -> push (Font (s *. n)) m | _ -> raise (Error "typecheck"))
  | "setfont" -> ( match pop m with Font s, m -> gs (fun g -> { g with font = s }) m | _ -> raise (Error "typecheck"))
  | "show" -> ( match pop m with String s, m -> show_text s m | _ -> raise (Error "typecheck"))
  | "stringwidth" -> ( match pop m with String s, m -> push (Real 0.) (push (Real (width_of s m)) m) | _ -> raise (Error "typecheck"))
  | _ -> raise (Error "undefined")

let operators =
  [ "pop"; "exch"; "dup"; "copy"; "index"; "roll"; "clear"; "count"; "mark"; "["; "]"; "cleartomark"; "counttomark";
    "add"; "sub"; "mul"; "div"; "idiv"; "mod"; "neg"; "abs"; "sqrt"; "sin"; "cos"; "atan"; "exp"; "ln"; "log";
    "round"; "floor"; "ceiling"; "truncate"; "cvi"; "cvr"; "eq"; "ne"; "gt"; "ge"; "lt"; "le"; "and"; "or"; "xor"; "not";
    "exec"; "if"; "ifelse"; "repeat"; "for"; "loop"; "forall"; "exit"; "quit";
    "def"; "dict"; "begin"; "end"; "currentdict"; "load"; "store"; "known";
    "array"; "string"; "length"; "get"; "put"; "aload"; "cvx"; "cvlit"; "cvn"; "cvs";
    "="; "=="; "print"; "pstack"; "stack";
    "newpath"; "moveto"; "rmoveto"; "lineto"; "rlineto"; "curveto"; "rcurveto"; "arc"; "arcn"; "closepath"; "currentpoint";
    "fill"; "eofill"; "stroke"; "setlinewidth"; "setgray"; "setrgbcolor"; "sethsbcolor"; "gsave"; "grestore";
    "translate"; "scale"; "rotate"; "showpage"; "findfont"; "scalefont"; "setfont"; "show"; "stringwidth" ]

(*****************************************************************************)
(* Running *)
(*****************************************************************************)

(* a box for each letter, when no host gives the letters *)
let boxes = { glyph = (fun _ -> ([ [ (0.1, 0.); (0.5, 0.); (0.5, 0.7); (0.1, 0.7); (0.1, 0.) ] ], 0.6)) }

let start ?(host = boxes) text =
  let systemdict = Hashtbl.create 128 in
  List.iter (fun op -> Hashtbl.replace systemdict op (Operator op)) operators;
  Hashtbl.replace systemdict "true" (Bool true);
  Hashtbl.replace systemdict "false" (Bool false);
  Hashtbl.replace systemdict "null" Null;
  {
    text; host; operands = []; exec = [ Source 0 ]; dicts = [ Hashtbl.create 64; systemdict ]; gs = initial_gs; saved = [];
    painted = []; finished = []; printed = []; state = Running; last = None; count = 0;
  }

let value_of (k : Ps_lexer.kind) : value =
  match k with
  | Int n -> Int n
  | Real f -> Real f
  | Name n -> Name n
  | Literal n -> Literal n
  | String s -> String s
  | Open_brace | Close_brace -> Null

(* a procedure's body, from after its { to its }: kept, not run *)
let rec read_proc text pos : array_ * int =
  let rec go pos items =
    match Ps_lexer.next text pos with
    | None -> raise (Offending ("syntaxerror", "{"))
    | Some (Error _) -> raise (Offending ("syntaxerror", "("))
    | Some (Ok { kind = Close_brace; stop; _ }) ->
        let items = List.rev items in
        ({ items = Array.of_list (List.map fst items); spans = Array.of_list (List.map snd items); exec = true }, stop)
    | Some (Ok { kind = Open_brace; start; _ }) ->
        let inner, stop = read_proc text (start + 1) in
        go stop ((Array inner, (start, stop)) :: items)
    | Some (Ok t) -> go t.stop ((value_of t.kind, (t.start, t.stop)) :: items)
  in
  go pos []

(* an executable name: an operator runs, a procedure is entered, any
   other value is pushed *)
let rec execute_name n m =
  match lookup m.dicts n with
  | None -> raise (Offending ("undefined", n))
  | Some (Operator op) -> ( try apply op m with Error e -> raise (Offending (e, op)))
  | Some (Array a) when a.exec -> run_frame a m
  | Some (Name other) when other <> n -> execute_name other m
  | Some v -> push v m

(* an object met in the text or in a procedure's body: a name is
   executed, anything else -- a procedure too -- is data *)
let execute_direct v m = match v with Name n -> execute_name n m | v -> push v m

let step m =
  if m.state <> Running then m
  else
    let m = { m with count = m.count + 1 } in
    try
      match m.exec with
      | [] -> { m with state = Done }
      | frame :: rest -> (
          match frame with
          | Source pos -> (
              match Ps_lexer.next m.text pos with
              | None -> { m with exec = rest }
              | Some (Error _) -> raise (Offending ("syntaxerror", "("))
              | Some (Ok { kind = Close_brace; _ }) -> raise (Offending ("syntaxerror", "}"))
              | Some (Ok { kind = Open_brace; start; _ }) ->
                  let proc, stop = read_proc m.text (start + 1) in
                  push (Array proc) { m with exec = Source stop :: rest; last = Some (start, stop) }
              | Some (Ok t) -> execute_direct (value_of t.kind) { m with exec = Source t.stop :: rest; last = Some (t.start, t.stop) })
          | Run (a, i) ->
              let n = Array.length a.items in
              if i >= n then { m with exec = rest }
              else
                (* the last item leaves the frame first: a procedure's
                   last call is a jump, and recursion keeps no frame *)
                let exec = if i = n - 1 then rest else Run (a, i + 1) :: rest in
                let last = if i < Array.length a.spans then Some a.spans.(i) else m.last in
                execute_direct a.items.(i) { m with exec; last }
          | Repeat (k, p) -> if k <= 0 then { m with exec = rest } else run_frame p { m with exec = Repeat (k - 1, p) :: rest }
          | For (v, inc, limit, ints, p) ->
              if (inc >= 0. && v > limit) || (inc < 0. && v < limit) then { m with exec = rest }
              else
                let m = push (if ints then Int (Float.to_int v) else Real v) { m with exec = For (v +. inc, inc, limit, ints, p) :: rest } in
                run_frame p m
          | Loop p -> run_frame p m
          | Forall ([], _) -> { m with exec = rest }
          | Forall (turn :: more, p) -> run_frame p (List.fold_left (fun m v -> push v m) { m with exec = Forall (more, p) :: rest } turn))
    with
    | Offending (e, cmd) -> { m with state = Failed (Printf.sprintf "%%%%[ Error: %s; OffendingCommand: %s ]%%%%" e cmd) }
    | Error e -> { m with state = Failed (Printf.sprintf "%%%%[ Error: %s ]%%%%" e) }

let run ?(budget = max_int) m =
  let rec go m k = if k >= budget || m.state <> Running then m else go (step m) (k + 1) in
  go m 0

let status m = m.state
let page m = List.rev m.painted
let pages m = List.rev m.finished
let stack m = List.map show m.operands
let output m = List.rev m.printed
let span m = m.last
let steps m = m.count
