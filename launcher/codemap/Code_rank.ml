(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Code_rank.mli *)

type use = { own : int; others : int; files : int }

(* claude: a definition in the call graph: the other files its calls
 * reach; its depth, the longest chain of callers above it, and its
 * height, of callees below it (a cycle one step); its lines *)
type place = { reach : int; depth : int; height : int; lines : int }

(* a definition by its file, its line and its (last) name; and the files
 * using it, how many times each *)
type t = {
  uses : (string * int * string, use) Hashtbl.t;
  users : (string * int * string, (string, int) Hashtbl.t) Hashtbl.t;
  links : (string * string, int) Hashtbl.t; (* claude: a file's uses of another's definitions *)
  reach : (string * int, place) Hashtbl.t; (* claude: a definition (its file, its head's line): its calls' reach, its place in the call stack *)
}

let last_name (s : string) : string = match String.rindex_opt s '.' with Some i -> String.sub s (i + 1) (String.length s - i - 1) | None -> s

(* claude: an assembly label as written (_system_call) and as used from C
 * (system_call, Highlight_asm's dname): the lookups try both *)
let find_either tbl path line name =
  match Hashtbl.find_opt tbl (path, line, last_name name) with
  | Some v -> Some v
  | None when String.length name > 1 && name.[0] = '_' -> Hashtbl.find_opt tbl (path, line, String.sub name 1 (String.length name - 1))
  | None -> None

(*****************************************************************************)
(* Reach *)
(*****************************************************************************)

(* claude: a file's definitions' heads, sorted: a line's definition is
 * the last head at or above it *)
let heads (f : Code_file.t) : int array =
  (* claude: and OCaml's let () = ... at the left margin, a program's main
   * body: a head of its own, unnamed, or its calls were the definition
   * above it's *)
  let units =
    if not (List.exists (Filename.check_suffix f.path) [ ".ml"; ".mll"; ".mly" ]) then []
    else
      List.filter
        (fun l ->
          match f.lines.(l) with
          | (sp : Highlight_code.span) :: _ when sp.col = 0 ->
              let s = String.concat "" (List.map (fun (sp : Highlight_code.span) -> sp.text) f.lines.(l)) in
              let starts p = String.length s >= String.length p && String.sub s 0 (String.length p) = p in
              starts "let()" || starts "let_=" || starts "let () " || starts "let _ "
          | _ -> false)
        (List.init (Code_file.nlines f) Fun.id)
  in
  List.filter_map (fun (l, _, (cat : Highlight_code.category)) -> match cat with Def_function | Def_value | Def_type | Def_module -> Some l | _ -> None) f.defs
  @ units
  |> List.sort_uniq compare |> Array.of_list

let head_of (hs : int array) (line : int) : int option =
  let rec go lo hi = if lo >= hi then lo else let m = (lo + hi + 1) / 2 in if hs.(m) <= line then go m hi else go lo (m - 1) in
  if Array.length hs = 0 || hs.(0) > line then None else Some hs.(go 0 (Array.length hs - 1))

(* claude: the strongly connected components of a graph, Tarjan's (a
 * stack of its own: a chain of calls can be deep, and a browser's stack
 * is small): each node's component, numbered as they finish, callees
 * first (an edge between two goes from a higher number to a lower) *)
let components (succ : int list array) : int array * int =
  let n = Array.length succ in
  let index = Array.make n (-1) and low = Array.make n 0 and on = Array.make n false and comp = Array.make n (-1) in
  let stack = ref [] and counter = ref 0 and ncomp = ref 0 in
  let visit v =
    index.(v) <- !counter;
    low.(v) <- !counter;
    incr counter;
    stack := v :: !stack;
    on.(v) <- true
  in
  let finish v =
    let c = !ncomp in
    incr ncomp;
    let rec pop () =
      match !stack with
      | w :: rest ->
          stack := rest;
          on.(w) <- false;
          comp.(w) <- c;
          if w <> v then pop ()
      | [] -> ()
    in
    pop ()
  in
  for v0 = 0 to n - 1 do
    if index.(v0) < 0 then begin
      visit v0;
      (* the search's frames: a node and its edges left *)
      let frames = ref [ (v0, ref succ.(v0)) ] in
      while !frames <> [] do
        match !frames with
        | (v, rest) :: up -> (
            match !rest with
            | w :: more ->
                rest := more;
                if index.(w) < 0 then begin
                  visit w;
                  frames := (w, ref succ.(w)) :: !frames
                end
                else if on.(w) then low.(v) <- min low.(v) index.(w)
            | [] ->
                frames := up;
                if low.(v) = index.(v) then finish v;
                (match up with (u, _) :: _ -> low.(u) <- min low.(u) low.(v) | [] -> ()))
        | [] -> ()
      done
    end
  done;
  (comp, !ncomp)

(* claude: each node's reach, the files of the nodes its edges lead to,
 * transitively, but its own: the components taken callees first, each
 * one's files its members' and its successors', a bit per file *)
let reach_counts (succ : int list array) ((comp, ncomp) : int array * int) (file_of : int array) (nfiles : int) : int array =
  let n = Array.length succ in
  let words = (nfiles + 63) / 64 in
  let members = Array.make ncomp [] in
  Array.iteri (fun v c -> members.(c) <- v :: members.(c)) comp;
  let bits = Array.make ncomp Bytes.empty in
  for c = 0 to ncomp - 1 do
    let b = Bytes.make (words * 8) '\000' in
    List.iter
      (fun w ->
        let fi = file_of.(w) in
        Bytes.set b (fi / 8) (Char.chr (Char.code (Bytes.get b (fi / 8)) lor (1 lsl (fi mod 8))));
        List.iter
          (fun x ->
            if comp.(x) <> c then begin
              let bx = bits.(comp.(x)) in
              for k = 0 to words - 1 do Bytes.set_int64_le b (k * 8) (Int64.logor (Bytes.get_int64_le b (k * 8)) (Bytes.get_int64_le bx (k * 8))) done
            end)
          succ.(w))
      members.(c);
    bits.(c) <- b
  done;
  let popcount (b : Bytes.t) =
    let c = ref 0 in
    Bytes.iter (fun ch -> let x = ref (Char.code ch) in while !x <> 0 do incr c; x := !x land (!x - 1) done) b;
    !c
  in
  let counts = Array.map popcount bits in
  Array.init n (fun v -> max 0 (counts.(comp.(v)) - 1))

(* claude: each node's depth and height, its component's: callees first,
 * so going up the numbers the heights, down them the depths *)
let depth_height (succ : int list array) ((comp, ncomp) : int array * int) : int array * int array =
  let n = Array.length succ in
  let height = Array.make ncomp 0 and depth = Array.make ncomp 0 in
  let by_from = Array.make ncomp [] and by_to = Array.make ncomp [] in
  for v = 0 to n - 1 do
    List.iter (fun w -> let a = comp.(v) and b = comp.(w) in if a <> b then begin by_from.(a) <- b :: by_from.(a); by_to.(b) <- a :: by_to.(b) end) succ.(v)
  done;
  for c = 0 to ncomp - 1 do List.iter (fun b -> height.(c) <- max height.(c) (height.(b) + 1)) by_from.(c) done;
  for c = ncomp - 1 downto 0 do List.iter (fun a -> depth.(c) <- max depth.(c) (depth.(a) + 1)) by_to.(c) done;
  (Array.init n (fun v -> depth.(comp.(v))), Array.init n (fun v -> height.(comp.(v))))

let compute ?roots (files : (string * Code_file.t Lazy.t) list) : t =
  let ix = Code_names.index files in
  (* claude: the calls, definition to definition: a node a head, by its
   * file and line; an edge from the head a reference is under to the
   * head of what it resolves to *)
  let nodes = Hashtbl.create 4096 and node_list = ref [] and nnodes = ref 0 in
  let file_ids = Hashtbl.create 1024 and nfiles = ref 0 in
  let file_id p = match Hashtbl.find_opt file_ids p with Some i -> i | None -> let i = !nfiles in incr nfiles; Hashtbl.replace file_ids p i; i in
  let node p l = match Hashtbl.find_opt nodes (p, l) with Some i -> i | None -> let i = !nnodes in incr nnodes; Hashtbl.replace nodes (p, l) i; node_list := (p, l) :: !node_list; i in
  let edges = ref [] in
  let heads_of = Hashtbl.create 1024 in
  let heads_in p = match Hashtbl.find_opt heads_of p with Some hs -> hs | None -> let hs = match List.assoc_opt p files with Some lf -> heads (Lazy.force lf) | None -> [||] in Hashtbl.replace heads_of p hs; hs in
  let h = Hashtbl.create 4096 and users = Hashtbl.create 4096 and links = Hashtbl.create 1024 in
  let get k = Option.value (Hashtbl.find_opt h k) ~default:{ own = 0; others = 0; files = 0 } in
  List.iter
    (fun (path, lf) ->
      let (f : Code_file.t) = Lazy.force lf in
      (* its own uses: the names bound to one of its top-level
       * definitions, but the definition itself *)
      List.iter
        (fun (d : Highlight_code.definition) ->
          match Hashtbl.find_opt f.uses (d.dline, d.dcol) with
          | Some occs ->
              let k = (path, d.dline, last_name d.dname) in
              let u = get k in
              Hashtbl.replace h k { u with own = u.own + List.length occs - 1 }
          | None -> ())
        f.definitions;
      (* the others': every name defined elsewhere, resolved, if sure;
       * a file counted once a definition *)
      let seen = Hashtbl.create 64 in
      let own_heads = heads_in path in
      Array.iteri
        (fun line -> List.iter (fun (r : Highlight_code.reference) ->
             match Code_names.find_in ?roots ix ~from:path f r with
             | c :: _, true ->
                 (match (head_of own_heads line, head_of (heads_in c.path) c.line) with
                 | Some a, Some b when (path, a) <> (c.path, b) -> edges := (node path a, node c.path b) :: !edges
                 | _ -> ());
                 let k = (c.path, c.line, r.rname) in
                 let u = get k in
                 let first = not (Hashtbl.mem seen k) in
                 if first then Hashtbl.replace seen k ();
                 Hashtbl.replace h k { u with others = u.others + 1; files = (u.files + if first then 1 else 0) };
                 let by = match Hashtbl.find_opt users k with Some by -> by | None -> let by = Hashtbl.create 8 in Hashtbl.replace users k by; by in
                 Hashtbl.replace by path (1 + Option.value (Hashtbl.find_opt by path) ~default:0);
                 if c.path <> path then Hashtbl.replace links (path, c.path) (1 + Option.value (Hashtbl.find_opt links (path, c.path)) ~default:0)
             | _ -> ()))
        f.refs)
    files;
  let n = !nnodes in
  let succ = Array.make n [] in
  List.iter (fun (a, b) -> if not (List.mem b succ.(a)) then succ.(a) <- b :: succ.(a)) !edges;
  let where = Array.of_list (List.rev !node_list) in
  let file_of = Array.map (fun (p, _) -> file_id p) where in
  let comps = components succ in
  let counts = reach_counts succ comps file_of !nfiles in
  let depth, height = depth_height succ comps in
  (* a definition's lines: its head's to the next head's *)
  let lines_of (p, l) =
    let hs = heads_in p in
    let nl = match List.assoc_opt p files with Some lf -> Code_file.nlines (Lazy.force lf) | None -> l + 1 in
    let next = Array.fold_left (fun acc h -> if h > l && h < acc then h else acc) nl hs in
    next - l
  in
  let reach = Hashtbl.create (max 16 n) in
  Array.iteri (fun i k -> Hashtbl.replace reach k { reach = counts.(i); depth = depth.(i); height = height.(i); lines = lines_of k }) where;
  { uses = h; users; links; reach }

let reach (t : t) (path : string) (line : int) : int = match Hashtbl.find_opt t.reach (path, line) with Some p -> p.reach | None -> 0

let places (t : t) : (string * int * place) list = Hashtbl.fold (fun (p, l) pl acc -> (p, l, pl) :: acc) t.reach []

let links (t : t) : (string * string * int) list = Hashtbl.fold (fun (a, b) n acc -> (a, b, n) :: acc) t.links [] |> List.sort compare

let users (t : t) (path : string) (line : int) (name : string) : (string * int) list =
  match find_either t.users path line name with
  | Some by -> List.sort (fun (a, n) (b, m) -> compare (m, a) (n, b)) (Hashtbl.fold (fun p n acc -> (p, n) :: acc) by [])
  | None -> []

let uses (t : t) (path : string) (line : int) (name : string) : use =
  Option.value (find_either t.uses path line name) ~default:{ own = 0; others = 0; files = 0 }

(* codemap's multiplier_use: NoUse to HugeUse *)
let bucket (n : int) : float = if n <= 0 then 0.9 else if n = 1 then 1.3 else if n < 5 then 1.7 else if n < 20 then 2.1 else if n < 100 then 2.7 else 3.3

(* codemap's size_font_multiplier_of_categ, by kind *)
let weight (c : Highlight_code.category) : float =
  match c with Def_module | Def_type -> 5. | Def_function -> 3.5 | Def_value -> 3. | Constructor -> 1.2 | Field -> 1.7 | _ -> 1.

let score (t : t) (path : string) (line : int) (name : string) (c : Highlight_code.category) : float =
  let u = uses t path line name in
  weight c *. bucket (u.others + (u.own / 3))

(*****************************************************************************)
(* Saved *)
(*****************************************************************************)

(* claude: a line a fact, tab-separated: U a definition's uses, B its
 * uses by one file, L a link, R a definition's reach (a bundle made
 * before has none: nothing reaches); paths and names hold no tab nor
 * newline *)
let to_string (t : t) : string =
  let b = Buffer.create (1 lsl 20) in
  Hashtbl.iter (fun (p, l, n) (u : use) -> Printf.bprintf b "U\t%s\t%d\t%s\t%d\t%d\t%d\n" p l n u.own u.others u.files) t.uses;
  Hashtbl.iter (fun (p, l, n) by -> Hashtbl.iter (fun q k -> Printf.bprintf b "B\t%s\t%d\t%s\t%s\t%d\n" p l n q k) by) t.users;
  Hashtbl.iter (fun (a, c) n -> Printf.bprintf b "L\t%s\t%s\t%d\n" a c n) t.links;
  Hashtbl.iter (fun (p, l) (x : place) -> Printf.bprintf b "R\t%s\t%d\t%d\t%d\t%d\t%d\n" p l x.reach x.depth x.height x.lines) t.reach;
  Buffer.contents b

let of_string (s : string) : t =
  let uses = Hashtbl.create 4096 and users = Hashtbl.create 4096 and links = Hashtbl.create 1024 and reach = Hashtbl.create 4096 in
  List.iter
    (fun line ->
      match String.split_on_char '\t' line with
      | [ "U"; p; l; n; o; x; f ] -> Hashtbl.replace uses (p, int_of_string l, n) { own = int_of_string o; others = int_of_string x; files = int_of_string f }
      | [ "B"; p; l; n; q; k ] ->
          let key = (p, int_of_string l, n) in
          let by = match Hashtbl.find_opt users key with Some by -> by | None -> let by = Hashtbl.create 8 in Hashtbl.replace users key by; by in
          Hashtbl.replace by q (int_of_string k)
      | [ "L"; a; c; n ] -> Hashtbl.replace links (a, c) (int_of_string n)
      | [ "R"; p; l; n; d; hh; k ] -> Hashtbl.replace reach (p, int_of_string l) { reach = int_of_string n; depth = int_of_string d; height = int_of_string hh; lines = int_of_string k }
      | _ -> ())
    (String.split_on_char '\n' s);
  { uses; users; links; reach }
