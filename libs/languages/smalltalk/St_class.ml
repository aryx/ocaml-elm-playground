(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_class.mli *)

module M = St_memory

type oop = M.oop

let f_superclass = 0
let f_method_dict = 1
let f_format = 2
let f_inst_vars = 3
let f_organization = 4
let f_name = 5
let f_category = 6
let f_class_pool = 7
let f_comment = 8

(*****************************************************************************)
(* Formats *)
(*****************************************************************************)

type kind = Fixed | Indexable | Byte_indexable | Float_kind | Method_kind

let kinds = [| Fixed; Indexable; Byte_indexable; Float_kind; Method_kind |]

let format (m : M.t) (cls : oop) : int * kind =
  let f = M.int_of (M.fetch m cls f_format) in
  (f lsr 3, kinds.(f land 7))

let encode_format (n : int) (k : kind) : int =
  let k = match k with Fixed -> 0 | Indexable -> 1 | Byte_indexable -> 2 | Float_kind -> 3 | Method_kind -> 4 in
  (n lsl 3) lor k

(*****************************************************************************)
(* Classes *)
(*****************************************************************************)

let superclass (m : M.t) (cls : oop) : oop = M.fetch m cls f_superclass
let is_meta (m : M.t) (cls : oop) : bool = cls <> M.nil && M.class_of m cls = (M.known m).metaclass
let this_class (m : M.t) (cls : oop) : oop = if is_meta m cls then M.fetch m cls f_name else cls
let metaclass (m : M.t) (cls : oop) : oop = if is_meta m cls then cls else M.class_of m cls

let name (m : M.t) (cls : oop) : string =
  if cls = M.nil then "nil"
  else if is_meta m cls then M.string_of m (M.fetch m (this_class m cls) f_name) ^ " class"
  else M.string_of m (M.fetch m cls f_name)

let strings_of_array (m : M.t) (a : oop) : string list =
  if a = M.nil then [] else Array.to_list (Array.map (M.string_of m) (M.fields m a))

let own_inst_var_names (m : M.t) (cls : oop) : string list = strings_of_array m (M.fetch m cls f_inst_vars)

let rec inst_var_names (m : M.t) (cls : oop) : string list =
  if cls = M.nil then [] else inst_var_names m (superclass m cls) @ own_inst_var_names m cls

let category (m : M.t) (cls : oop) : string =
  let cls = this_class m cls in
  let c = M.fetch m cls f_category in
  if c = M.nil then "" else M.string_of m c

let comment (m : M.t) (cls : oop) : string =
  let c = M.fetch m (this_class m cls) f_comment in
  if c = M.nil then "" else M.string_of m c

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let new_association (m : M.t) (k : oop) (v : oop) : oop = M.alloc m ~cls:(M.known m).association (M.Pointers [| k; v |])

(* the SystemDictionary's one field *)
let associations (m : M.t) : oop array =
  let sd = (M.known m).smalltalk in
  if sd = M.nil then [||] else M.fields m (M.fetch m sd 0)

let find_assoc (m : M.t) (assocs : oop array) (name : string) : oop option =
  let sym = M.symbol m name in
  Array.find_opt (fun a -> M.fetch m a 0 = sym) assocs

let global (m : M.t) (name : string) : oop option = find_assoc m (associations m) name

let declare_global (m : M.t) (name : string) (v : oop) : oop =
  match global m name with
  | Some a ->
      M.store m a 1 v;
      a
  | None ->
      let a = new_association m (M.symbol m name) v in
      let sd = (M.known m).smalltalk in
      M.store m sd 0 (M.new_array m (Array.append (associations m) [| a |]));
      a

let globals (m : M.t) : (string * oop) list =
  Array.to_list (associations m) |> List.map (fun a -> (M.string_of m (M.fetch m a 0), M.fetch m a 1))

let classes (m : M.t) : oop list =
  globals m
  |> List.filter (fun (n, v) -> (not (M.is_int v)) && v <> M.nil && is_meta m (M.class_of m v) && name m v = n)
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

let class_var_names (m : M.t) (cls : oop) : string list =
  let pool = M.fetch m (this_class m cls) f_class_pool in
  if pool = M.nil then [] else Array.to_list (M.fields m pool) |> List.map (fun a -> M.string_of m (M.fetch m a 0))

let rec class_var (m : M.t) (cls : oop) (name : string) : oop option =
  if cls = M.nil then None
  else
    let c = this_class m cls in
    let pool = M.fetch m c f_class_pool in
    match if pool = M.nil then None else find_assoc m (M.fields m pool) name with
    | Some a -> Some a
    | None -> class_var m (superclass m c) name

let definition (m : M.t) (cls : oop) : string =
  let cls = this_class m cls in
  let sup = superclass m cls in
  let n, kind = format m cls in
  ignore n;
  let verb =
    match kind with
    | Indexable -> "variableSubclass:"
    | Byte_indexable -> "variableByteSubclass:"
    | Fixed | Float_kind | Method_kind -> "subclass:"
  in
  Printf.sprintf "%s %s #%s\n\tinstanceVariableNames: '%s'\n\tclassVariableNames: '%s'\n\tpoolDictionaries: ''\n\tcategory: '%s'"
    (if sup = M.nil then "nil" else name m sup)
    verb (name m cls)
    (String.concat " " (own_inst_var_names m cls))
    (String.concat " " (class_var_names m cls))
    (category m cls)

(*****************************************************************************)
(* Methods *)
(*****************************************************************************)

let dict (m : M.t) (cls : oop) : oop array * oop array =
  let d = M.fetch m cls f_method_dict in
  (M.fields m (M.fetch m d 0), M.fields m (M.fetch m d 1))

let local_method (m : M.t) (cls : oop) (sel : oop) : oop option =
  let sels, meths = dict m cls in
  let rec find i = if i >= Array.length sels then None else if sels.(i) = sel then Some meths.(i) else find (i + 1) in
  find 0

let rec lookup (m : M.t) (cls : oop) (sel : oop) : oop option =
  if cls = M.nil then None else match local_method m cls sel with Some meth -> Some meth | None -> lookup m (superclass m cls) sel

let selectors (m : M.t) (cls : oop) : string list =
  let sels, _ = dict m cls in
  Array.to_list sels |> List.map (M.string_of m) |> List.sort compare

let organization (m : M.t) (cls : oop) : (string * string list) list =
  let o = M.fetch m cls f_organization in
  if o = M.nil then []
  else
    Array.to_list (M.fields m o)
    |> List.map (fun a -> (M.string_of m (M.fetch m a 0), Array.to_list (M.fields m (M.fetch m a 1)) |> List.map (M.string_of m)))

let set_organization (m : M.t) (cls : oop) (org : (string * string list) list) : unit =
  let org = List.filter (fun (_, sels) -> sels <> []) org in
  let assocs =
    List.map
      (fun (cat, sels) ->
        new_association m (M.new_string m cat) (M.new_array m (Array.of_list (List.map (M.symbol m) (List.sort compare sels)))))
      org
  in
  M.store m cls f_organization (M.new_array m (Array.of_list assocs))

let categories (m : M.t) (cls : oop) : string list = List.map fst (organization m cls)
let category_selectors (m : M.t) (cls : oop) (cat : string) : string list = try List.assoc cat (organization m cls) with Not_found -> []

let category_of (m : M.t) (cls : oop) (sel : string) : string option =
  List.find_map (fun (cat, sels) -> if List.mem sel sels then Some cat else None) (organization m cls)

let install (m : M.t) (cls : oop) (sel : oop) (meth : oop) ~(category : string) : unit =
  let d = M.fetch m cls f_method_dict in
  let sels, meths = dict m cls in
  let rec find i = if i >= Array.length sels then None else if sels.(i) = sel then Some i else find (i + 1) in
  (match find 0 with
  | Some i -> meths.(i) <- meth
  | None ->
      M.store m d 0 (M.new_array m (Array.append sels [| sel |]));
      M.store m d 1 (M.new_array m (Array.append meths [| meth |])));
  let s = M.string_of m sel in
  let org = organization m cls |> List.map (fun (c, sels) -> (c, List.filter (( <> ) s) sels)) in
  let org = if List.mem_assoc category org then List.map (fun (c, sels) -> if c = category then (c, s :: sels) else (c, sels)) org else org @ [ (category, [ s ]) ] in
  set_organization m cls org

(*****************************************************************************)
(* Defining a class *)
(*****************************************************************************)

let new_method_dict (m : M.t) : oop =
  M.alloc m ~cls:(M.known m).method_dictionary (M.Pointers [| M.new_array m [||]; M.new_array m [||] |])

let subclasses (m : M.t) (cls : oop) : oop list = List.filter (fun c -> superclass m c = cls) (classes m)

let is_class (m : M.t) (v : oop) : bool = (not (M.is_int v)) && v <> M.nil && is_meta m (M.class_of m v)

(* the named fields of a class's instances, its superclass's first *)
let rec refresh_format (m : M.t) (cls : oop) : unit =
  let sup = superclass m cls in
  let inherited = if sup = M.nil then 0 else fst (format m sup) in
  let _, kind = format m cls in
  M.store m cls f_format (M.of_int (encode_format (inherited + List.length (own_inst_var_names m cls)) kind));
  List.iter (refresh_format m) (subclasses m cls)

let define_class (m : M.t) ~(superclass : oop) ~(name : string) ~(kind : kind) ~(inst_vars : string list)
    ~(class_vars : string list) ~(category : string) : oop * bool =
  let k = M.known m in
  let existing = match global m name with Some a when is_class m (M.fetch m a 1) -> Some (M.fetch m a 1) | _ -> None in
  let cls, meta, changed =
    match existing with
    | Some cls ->
        let changed =
          own_inst_var_names m cls <> inst_vars || superclass <> M.fetch m cls f_superclass
          || snd (format m cls) <> kind
        in
        (cls, M.class_of m cls, changed)
    | None ->
        let meta = M.alloc m ~cls:k.metaclass (M.Pointers (Array.make 6 M.nil)) in
        let cls = M.alloc m ~cls:meta (M.Pointers (Array.make 9 M.nil)) in
        ignore (declare_global m name cls);
        (cls, meta, false)
  in
  let f = M.fields m cls and mf = M.fields m meta in
  f.(f_superclass) <- superclass;
  if f.(f_method_dict) = M.nil then f.(f_method_dict) <- new_method_dict m;
  f.(f_format) <- M.of_int (encode_format 0 kind);
  f.(f_inst_vars) <- M.new_array m (Array.of_list (List.map (M.new_string m) inst_vars));
  f.(f_name) <- M.symbol m name;
  f.(f_category) <- M.new_string m category;
  (* the class variables: the old ones keep their values *)
  let old = if f.(f_class_pool) = M.nil then [||] else M.fields m f.(f_class_pool) in
  let pool =
    List.map
      (fun v ->
        match Array.find_opt (fun a -> M.string_of m (M.fetch m a 0) = v) old with
        | Some a -> a
        | None -> new_association m (M.symbol m v) M.nil)
      class_vars
  in
  f.(f_class_pool) <- (if pool = [] then M.nil else M.new_array m (Array.of_list pool));
  (* the metaclass: its superclass is the superclass's metaclass, or
   * Class at the top ("Object class superclass == Class") *)
  mf.(f_superclass) <-
    (if superclass = M.nil then match global m "Class" with Some a -> M.fetch m a 1 | None -> M.nil
     else M.class_of m superclass);
  if mf.(f_method_dict) = M.nil then mf.(f_method_dict) <- new_method_dict m;
  mf.(f_format) <- M.of_int (encode_format 9 Fixed);
  mf.(f_inst_vars) <- M.new_array m [||];
  mf.(f_name) <- cls;
  refresh_format m cls;
  (cls, changed)

let remove (m : M.t) (cls : oop) (sel : oop) : unit =
  let d = M.fetch m cls f_method_dict in
  let sels, meths = dict m cls in
  let keep = List.filter (fun (s, _) -> s <> sel) (List.combine (Array.to_list sels) (Array.to_list meths)) in
  M.store m d 0 (M.new_array m (Array.of_list (List.map fst keep)));
  M.store m d 1 (M.new_array m (Array.of_list (List.map snd keep)));
  let s = M.string_of m sel in
  set_organization m cls (organization m cls |> List.map (fun (c, sels) -> (c, List.filter (( <> ) s) sels)))
