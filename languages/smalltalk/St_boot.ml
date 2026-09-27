(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_boot.mli *)

open St_ast
module M = St_memory
module C = St_class
module I = St_interp

exception Error of string

let quiet_host : I.host =
  {
    transcript = (fun _ -> ());
    milliseconds = (fun () -> int_of_float (Sys.time () *. 1000.));
    inspect = (fun _ -> ());
    mouse = (fun () -> (0, 0, 0));
  }

let class_named (m : M.t) (name : string) : M.oop =
  match C.global m name with Some a -> M.fetch m a 1 | None -> raise (Error ("no class " ^ name))

(* a definition: its superclass's name, the class's, the kind, the
 * instance and class variables, the category *)
type definition = { sup : string; name : string; kind : C.kind; ivs : string list; cvs : string list; cat : string }

let words (s : string) : string list =
  String.split_on_char ' ' s |> List.concat_map (String.split_on_char '\t') |> List.concat_map (String.split_on_char '\n')
  |> List.filter (( <> ) "")

let definition (text : string) : definition option =
  match St_parse.parse_doit text with
  | {
   body =
     [
       Expr
         {
           e =
             Send
               ( { e = Var sup; _ },
                 sel,
                 [ { e = Lit (L_symbol name); _ }; { e = Lit (L_string ivs); _ }; { e = Lit (L_string cvs); _ }; _; { e = Lit (L_string cat); _ } ] );
           _;
         };
     ];
   _;
  } -> (
      let kind =
        match (name, sel) with
        | "Float", _ -> Some C.Float_kind
        | "CompiledMethod", _ -> Some C.Method_kind
        | _, "subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:" -> Some C.Fixed
        | _, "variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:" -> Some C.Indexable
        | _, "variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:" -> Some C.Byte_indexable
        | _ -> None
      in
      match kind with Some kind -> Some { sup; name; kind; ivs = words ivs; cvs = words cvs; cat } | None -> None)
  | _ -> None
  | exception St_parse.Error _ -> None

let line_of (text : string) (pos : int) : int =
  let n = ref 1 in
  String.iteri (fun i c -> if i < pos && c = '\n' then incr n) text;
  !n

let boot ?(host = quiet_host) () : I.vm =
  let m = M.create () in
  let k = M.known m in
  let items = List.concat_map (fun (file, text) -> List.map (fun i -> (file, text, i)) (St_chunk.read text)) St_kernel.files in
  let defs =
    List.filter_map (fun (_, _, i) -> match i with St_chunk.Doit (t, _) -> definition t | St_chunk.Methods _ -> None) items
  in
  (* 1. the classes, empty *)
  let table = Hashtbl.create 64 in
  let metas =
    List.map
      (fun d ->
        let meta = M.alloc m ~cls:M.nil (M.Pointers (Array.make 6 M.nil)) in
        let cls = M.alloc m ~cls:meta (M.Pointers (Array.make 9 M.nil)) in
        Hashtbl.replace table d.name cls;
        meta)
      defs
  in
  let get n = match Hashtbl.find_opt table n with Some c -> c | None -> raise (Error ("the kernel defines no " ^ n)) in
  k.metaclass <- get "Metaclass";
  List.iter (fun meta -> M.set_class m meta k.metaclass) metas;
  k.small_integer <- get "SmallInteger";
  k.string <- get "String";
  k.symbol <- get "Symbol";
  k.array <- get "Array";
  k.float <- get "Float";
  k.character <- get "Character";
  k.compiled_method <- get "CompiledMethod";
  k.method_context <- get "MethodContext";
  k.block_context <- get "BlockContext";
  k.message <- get "Message";
  k.association <- get "Association";
  k.point <- get "Point";
  k.large_positive <- get "LargePositiveInteger";
  k.large_negative <- get "LargeNegativeInteger";
  k.method_dictionary <- get "MethodDictionary";
  M.set_class m M.nil (get "UndefinedObject");
  k.true_ <- M.alloc m ~cls:(get "True") (M.Pointers [||]);
  k.false_ <- M.alloc m ~cls:(get "False") (M.Pointers [||]);
  k.characters <- Array.init 256 (fun i -> M.alloc m ~cls:k.character (M.Pointers [| M.of_int i |]));
  k.smalltalk <- M.alloc m ~cls:(get "SystemDictionary") (M.Pointers [| M.new_array m [||] |]);
  k.special_selectors <- Array.map (M.symbol m) St_bytecode.special_selectors;
  ignore (C.declare_global m "Smalltalk" k.smalltalk);
  List.iter (fun d -> ignore (C.declare_global m d.name (get d.name))) defs;
  (* 2. the classes filled *)
  List.iter
    (fun d ->
      let superclass = if d.sup = "nil" then M.nil else get d.sup in
      ignore (C.define_class m ~superclass ~name:d.name ~kind:d.kind ~inst_vars:d.ivs ~class_vars:d.cvs ~category:d.cat))
    defs;
  (* 3. the methods *)
  List.iter
    (fun (file, text, item) ->
      match item with
      | St_chunk.Methods { class_name; meta; category; methods } ->
          let cls = get class_name in
          let cls = if meta then M.class_of m cls else cls in
          List.iter
            (fun (src, pos) ->
              try ignore (St_compile.compile_and_install m ~cls ~category ~declare:true src)
              with St_compile.Error (p, msg) ->
                raise (Error (Printf.sprintf "%s:%d: %s>>%s" file (line_of text (pos + p)) (C.name m cls) msg)))
            methods
      | St_chunk.Doit _ -> ())
    items;
  (* 4. the machine, and the rest of the chunks run *)
  let vm = I.create m host in
  St_primitives.install vm;
  List.iter
    (fun (file, text, item) ->
      match item with
      | St_chunk.Doit (src, pos) when definition src = None -> (
          let empty = match St_parse.parse_doit src with { body = []; _ } -> true | _ -> false | exception St_parse.Error _ -> false in
          if not empty then
            match I.evaluate vm src with
            | Ok _ -> ()
            | Error msg -> raise (Error (Printf.sprintf "%s:%d: %s" file (line_of text pos) msg)))
      | _ -> ())
    items;
  vm
