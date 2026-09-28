(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Highlight_ml.mli.
 *
 * After pfff's highlight_ml.ml (codemap's), its token pass: one walk
 * over the tokens, a few facts remembered on the way (the parameters
 * and locals of the definition we are in, whether we are in a type),
 * each token's category chosen from its neighbours:
 *
 *   let f x y = ...     the let's name, then its arguments: Def_function
 *   let v = ...         no arguments: Def_value (a fun after = too: a function)
 *   M.x                 M a Module, x a Global
 *   (x : t)             after a colon, until the parenthesis closes: a type
 *   type t = A | B      a type definition: Def_type, then types and constructors
 *
 * A let is at the top when the token before it can end an expression
 * (after "=", "in", "(", "->"... a let is inside one): no indentation
 * needed, and a let in a module's struct is at its top too.
 *)

open Highlight_code

(* a token after which an expression is over, so a let is a new item *)
let ends_expression (t : Token_ml.t) : bool =
  match t.kind with
  | Lident | Uident | Int | Float | Char | String | Type_var | Label -> true
  | Keyword -> List.mem t.text [ "struct"; "end"; "done"; "true"; "false"; "sig" ]
  | Punctuation -> List.mem t.text [ ")"; "]"; "}"; ";;"; "|]" ]
  | _ -> false

(* the keywords that start an item of a structure or a signature *)
let item_keywords = [ "let"; "type"; "module"; "val"; "external"; "exception"; "open"; "include"; "class"; "method"; "end" ]

let control_keywords =
  [ "if"; "then"; "else"; "match"; "with"; "when"; "try"; "for"; "while"; "do"; "done"; "to"; "downto"; "function"; "fun" ]

let module_keywords = [ "module"; "struct"; "sig"; "end"; "open"; "include"; "functor" ]

(* caps, caps_net: a capability's name, by the repository's habit *)
let is_caps (s : string) : bool = String.length s >= 4 && String.sub s 0 4 = "caps"

(* a banner comment: (*****...*) *)
let is_banner (t : Token_ml.t) : bool = t.kind = Comment && String.length t.text >= 6 && String.sub t.text 0 6 = "(*****"

(*****************************************************************************)
(* From the tokens: a guess *)
(*****************************************************************************)

(* claude: what the neighbours say; the tree then says better where it
 * can ("From the tree" below) *)
let guess (toks : Token_ml.t list) : (Token_ml.t * category) list =
  (* the code, without the comments, for looking at neighbours *)
  let code = Array.of_list (List.filter (fun (t : Token_ml.t) -> t.kind <> Comment) toks) in
  let m = Array.length code in
  let cat = Array.make m Normal in
  let decided = Array.make m false in
  let text i = if i >= 0 && i < m then code.(i).text else "" in
  let kind i : Token_ml.kind option = if i >= 0 && i < m then Some code.(i).kind else None in
  let decide i c = if i >= 0 && i < m then (cat.(i) <- c; decided.(i) <- true) in
  (* what we know, walking *)
  let params = Hashtbl.create 16 and locals = Hashtbl.create 16 in
  let depth = ref 0 in
  let type_depth = ref None (* in a type since a colon, opened at this depth *) in
  let type_def = ref false (* in a type definition, until the next item *) in
  let last_binder = ref `None in
  (* the names bound by a pattern from [from] to one of [stop] *)
  let bind_names (from : int) (stop : string list) (c : category) (into : (string, unit) Hashtbl.t) : unit =
    let d = ref 0 and in_type = ref false and j = ref from in
    while !j < m && (not (!d = 0 && List.mem (text !j) stop)) && not (kind !j = Some Keyword && List.mem (text !j) item_keywords) do
      (match (kind !j, text !j) with
      | Some Punctuation, ("(" | "[" | "{") -> incr d
      | Some Punctuation, (")" | "]" | "}") ->
          decr d;
          in_type := false
      | Some Operator, ":" -> in_type := true
      | Some Lident, name when (not !in_type) && text (!j - 1) <> "." && name <> "_" ->
          Hashtbl.replace into name ();
          decide !j (if is_caps name then Capability else c)
      | Some Label, l ->
          (* ~x punned: x is bound too *)
          let name = String.sub l 1 (String.length l - 1) in
          if name <> "" && name.[String.length name - 1] <> ':' then Hashtbl.replace into name ()
      | _ -> ());
      incr j
    done
  in
  (* a let's (or an and's) name at [i], and its arguments *)
  let binding (i : int) (top : bool) : unit =
    let i = if text i = "rec" then i + 1 else i in
    if kind i = Some Lident then begin
      let fn = match text (i + 1) with "=" -> List.mem (text (i + 2)) [ "fun"; "function" ] | ":" -> false | _ -> true in
      decide i (if not top then Local else if fn then Def_function else Def_value);
      if not top then Hashtbl.replace locals (text i) ();
      bind_names (i + 1) [ "=" ] Parameter (if top then params else locals)
    end
    else if text i = "(" && kind (i + 1) = Some Operator && text (i + 2) = ")" then
      decide (i + 1) (if top then Def_function else Local)
    else bind_names i [ "=" ] (if top then Def_value else Local) (if top then params else locals)
  in
  (* the type's name after "type" (or "and"), past its parameters *)
  let type_name (i : int) : unit =
    let j = ref i in
    while !j < m && (List.mem (text !j) [ "nonrec"; "("; ")"; ","; "+"; "-" ] || kind !j = Some Type_var) do
      incr j
    done;
    if kind !j = Some Lident then decide !j Def_type
  in
  for i = 0 to m - 1 do
    let t = code.(i) in
    (* a new item: the type we were in is over *)
    if t.kind = Keyword && List.mem t.text item_keywords then type_depth := None;
    (match (t.kind, t.text) with
    | Punctuation, ("(" | "[" | "{" | "[|") -> incr depth
    | Punctuation, (")" | "]" | "}" | "|]") -> (
        decr depth;
        match !type_depth with Some d when !depth < d -> type_depth := None | _ -> ())
    | Operator, "=" | Punctuation, ";" -> ( match !type_depth with Some d when !depth <= d -> type_depth := None | _ -> ())
    | Keyword, "in" -> type_depth := None
    | _ -> ());
    if not decided.(i) then begin
      let in_type = !type_depth <> None || !type_def in
      let c : category =
        match t.kind with
        | Keyword -> (
            match t.text with
            | "let" ->
                let top = t.col = 0 || i = 0 || ends_expression code.(i - 1) in
                if top then (
                  Hashtbl.reset params;
                  Hashtbl.reset locals;
                  type_def := false);
                last_binder := if top then `Let_top else `Let;
                if not (List.mem (text (i + 1)) [ "open"; "module"; "exception" ]) then binding (i + 1) top;
                Keyword
            | "and" ->
                (match !last_binder with
                | `Type -> type_name (i + 1)
                | `Let_top -> binding (i + 1) true
                | `Let -> binding (i + 1) false
                | `None -> ());
                Keyword
            | "type" ->
                if text (i - 1) <> "module" && text (i - 1) <> ":" then begin
                  last_binder := `Type;
                  type_def := true;
                  type_name (i + 1)
                end;
                Keyword
            | "exception" ->
                if kind (i + 1) = Some Uident then decide (i + 1) Def_type;
                type_def := true;
                Keyword_control
            | ("val" | "external" | "method") as k ->
                type_def := false;
                let j = if List.mem (text (i + 1)) [ "mutable"; "virtual"; "private" ] then i + 2 else i + 1 in
                if kind j = Some Lident then begin
                  (* a function if its type has an arrow, before the next item *)
                  let rec arrow l =
                    l < m && (not (kind l = Some Keyword && List.mem (text l) item_keywords)) && (text l = "->" || arrow (l + 1))
                  in
                  decide j (if k = "external" || arrow (j + 1) then Def_function else Def_value)
                end;
                Keyword
            | "module" ->
                type_def := false;
                let j = if List.mem (text (i + 1)) [ "type"; "rec" ] then i + 2 else i + 1 in
                if kind j = Some Uident then decide j Def_module;
                Keyword_module
            | "fun" ->
                bind_names (i + 1) [ "->" ] Parameter params;
                Keyword_control
            | "true" | "false" -> Constructor
            | k when List.mem k module_keywords -> Keyword_module
            | k when List.mem k control_keywords -> Keyword_control
            | _ -> Keyword)
        | Uident ->
            if t.text = "Cap" && text (i + 1) = "." then Capability
            else if text (i + 1) = "." || List.mem (text (i - 1)) [ "open"; "include" ] then Module
            else Constructor
        | Lident ->
            if is_caps t.text then Capability
            else if text (i - 1) = "." && kind (i - 2) = Some Uident then if text (i - 2) = "Cap" then Capability else Global
            else if text (i - 1) = "." then Normal (* a field *)
            else if in_type then
              (* a field's name in a record type; a label in a type *)
              if text (i + 1) = ":" then if !type_def then Normal else Label else Type
            else if Hashtbl.mem params t.text then Parameter
            else if Hashtbl.mem locals t.text then Local
            else Normal
        | Label -> Label
        | Type_var -> Type_var
        | Int | Float -> Number
        | Char | String -> String
        | Operator -> Operator
        | Punctuation -> if String.length t.text >= 2 && (t.text.[1] = '@' || t.text.[1] = '%') then Attribute else Punctuation
        | Directive -> Attribute
        | Error -> Error
        | Comment -> Comment
      in
      cat.(i) <- c
    end;
    (* after a colon, a type *)
    if t.kind = Operator && t.text = ":" && !type_depth = None then type_depth := Some !depth
  done;
  (* the comments back in their places: banners, and the title between two *)
  let all = Array.of_list toks in
  let n = Array.length all in
  let k = ref 0 and out = ref [] in
  for i = 0 to n - 1 do
    let t = all.(i) in
    let c =
      if t.kind <> Comment then (
        incr k;
        cat.(!k - 1))
      else if is_banner t || (i > 0 && is_banner all.(i - 1) && i + 1 < n && is_banner all.(i + 1)) then Comment_section
      else Comment
    in
    out := (t, c) :: !out
  done;
  List.rev !out

(*****************************************************************************)
(* From the tree *)
(*****************************************************************************)

(* claude: what the tree says of a name (Parse_ml, Ast_ml): the scopes
 * (a parameter until its function ends, a local until its let's body or
 * its case ends), the fields (p.x, { x = ... }, a record type's), the
 * definitions, the constructors, types and modules where they are used.
 * A token index to its category; what is not in it keeps the guess. *)
open Ast_ml

(* the names in scope: a parameter or a local, and its binding's token *)
type env = (string * (category * int)) list

(* claude: and [binds], a name's token to its binding's (a binding's to
 * its own): where a use is bound, its uses those bound to one token *)
let resolve (file : file) :
    (int, category) Hashtbl.t
    * (int, int) Hashtbl.t
    * ((int * Highlight_code.space * string) list * (int * string list * Highlight_code.space * string list) list * string list) =
  let out = Hashtbl.create 1024 in
  let binds = Hashtbl.create 256 in
  let mark (n : name) (c : category) = if n.tok >= 0 then Hashtbl.replace out n.tok c in
  (* a name bound as [c]: in scope after *)
  let bind (n : name) (c : category) : string * (category * int) =
    mark n c;
    if n.tok >= 0 then Hashtbl.replace binds n.tok n.tok;
    (n.text, (c, n.tok))
  in
  (* claude: the top-level names defined so far (plan_codemap_naming.md,
   * level 2), the latest first: values, types, constructors, each its
   * own namespace as in OCaml. A name no local binds is bound to the
   * latest of its kind; a module's struct keeps its own to itself *)
  let top_values : env ref = ref [] and top_types : env ref = ref [] and top_constrs : env ref = ref [] in
  (* claude: and for other files (level 3): the file's definitions, a
   * nested module's by their path (N.x: [prefix], the modules we are in,
   * innermost first; none in an unnamed struct, [depth] deeper than
   * [prefix]), its names defined elsewhere (M.x, or not defined here)
   * with the modules opened around them ([local_opens], let open M in,
   * M.(e)), its top-level opens; tokens' indexes *)
  let defs = ref [] and refs = ref [] and opens = ref [] and depth = ref 0 in
  let prefix = ref [] and local_opens = ref [] in
  let record (tok : int) (space : Highlight_code.space) (text : string) =
    if tok >= 0 && !depth = List.length !prefix then defs := (tok, space, String.concat "." (List.rev (text :: !prefix))) :: !defs
  in
  let define (r : env ref) (space : Highlight_code.space) (n : name) (c : category) =
    if n.tok >= 0 then begin
      Hashtbl.replace binds n.tok n.tok;
      r := (n.text, (c, n.tok)) :: !r;
      record n.tok space n.text
    end
  in
  let refer (r : env ref) (space : Highlight_code.space) (l : longid) =
    match List.rev l with
    | [ n ] -> (
        match List.assoc_opt n.text !r with
        | Some (_, b) -> if n.tok >= 0 && b >= 0 then Hashtbl.replace binds n.tok b
        | None -> if n.tok >= 0 then refs := (n.tok, [], space, !local_opens) :: !refs)
    | n :: ms -> if n.tok >= 0 then refs := (n.tok, List.rev_map (fun (m : name) -> m.text) ms, space, !local_opens) :: !refs
    | [] -> ()
  in
  (* the last name of a module's path: M.N is N *)
  let last_name (l : longid) = match List.rev l with n :: _ -> Some n.text | [] -> None in
  (* inside module [n]'s body: its definitions are n's, N.x *)
  let in_module (n : name) (f : unit -> unit) =
    prefix := n.text :: !prefix;
    f ();
    prefix := List.tl !prefix
  in
  let scoped (f : unit -> unit) =
    let v, t, c = (!top_values, !top_types, !top_constrs) in
    incr depth;
    f ();
    decr depth;
    top_values := v;
    top_types := t;
    top_constrs := c
  in
  (* M.N.x: the modules, then the last as [last] *)
  let path (l : longid) (last : category) =
    List.iteri (fun i (n : name) -> mark n (if i = List.length l - 1 then last else Module)) l
  in
  let rec ty (t : ty) =
    match t with
    | Tany -> ()
    | Tvar n -> mark n Type_var
    | Tarrow (a, b) -> ty a; ty b
    | Ttuple ts -> List.iter ty ts
    | Tconstr (l, ts) -> path l Type; refer top_types Type l; List.iter ty ts
    | Tobject ms -> List.iter (fun (m, t) -> mark m Field; ty t) ms
    | Tvariant (tags, others) -> List.iter (fun (n, ts) -> mark n Constructor; List.iter ty ts) tags; List.iter ty others
    | Tpoly (ns, t) -> List.iter (fun n -> mark n Type_var) ns; ty t
    | Tpackage l -> path l Module
  in
  (* a pattern's names, bound as [c]; returned, in scope after *)
  let rec pat (c : category) (p : pat) : env =
    match p with
    | Pany | Pconst -> []
    | Pvar n -> [ bind n c ]
    | Ptuple ps | Plist ps -> List.concat_map (pat c) ps
    | Pconstr (l, arg) -> path l Constructor; refer top_constrs Constr l; (match arg with Some p -> pat c p | None -> [])
    | Pvariant (n, arg) -> mark n Constructor; (match arg with Some p -> pat c p | None -> [])
    | Precord fields ->
        List.concat_map
          (fun (l, p) ->
            path l Field;
            (* { x } binds x, at the field's token (a field it stays) *)
            match p with
            | Some p -> pat c p
            | None -> (
                match List.rev l with
                | n :: _ ->
                    if n.tok >= 0 then Hashtbl.replace binds n.tok n.tok;
                    [ (n.text, (c, n.tok)) ]
                | [] -> []))
          fields
    | Por (a, b) -> pat c a @ pat c b
    | Palias (p, n) -> bind n c :: pat c p
    | Pconstraint (p, t) -> ty t; pat c p
    | Pmodule n -> mark n Module; []
    | Popen (l, p) -> path l Module; pat c p
    | Pinner p -> pat c p
  in
  let rec expr (env : env) (e : expr) =
    match e with
    | Econst -> ()
    (* in scope, or not a parameter nor a local at all: the guess, which
     * knows a definition's locals but not where their scopes end, is
     * overruled *)
    | Eident [ n ] -> (
        match List.assoc_opt n.text env with
        | Some (c, b) ->
            mark n c;
            if n.tok >= 0 && b >= 0 then Hashtbl.replace binds n.tok b
        | None ->
            mark n Normal;
            refer top_values Value [ n ])
    | Eident l -> path l Global; refer top_values Value l
    | Econstr (l, arg) -> path l Constructor; refer top_constrs Constr l; Option.iter (expr env) arg
    | Evariant (n, arg) -> mark n Constructor; Option.iter (expr env) arg
    | Etuple es | Elist es | Eseq es | Emisc es -> List.iter (expr env) es
    | Erecord (base, fields) ->
        Option.iter (expr env) base;
        List.iter (fun (l, v) -> path l Field; Option.iter (expr env) v) fields
    | Efield (e, l) -> expr env e; path l Field
    | Esetfield (e, l, v) -> expr env e; path l Field; expr env v
    | Eapply (f, args) -> expr env f; List.iter (expr env) args
    | Elet (recursive, bs, body) ->
        let bound = List.concat_map (fun b -> pat Local b.bpat) bs in
        List.iter (binding (if recursive then bound @ env else env)) bs;
        expr (bound @ env) body
    | Eletop (bs, body) ->
        let bound = List.concat_map (fun b -> pat Local b.bpat) bs in
        List.iter (binding env) bs;
        expr (bound @ env) body
    | Efun (ps, body) -> expr (params env ps @ env) body
    | Efunction cs -> cases env cs
    | Ematch (e, cs) -> expr env e; cases env cs
    | Eif (c, a, b) -> expr env c; expr env a; Option.iter (expr env) b
    | Ewhile (c, b) -> expr env c; expr env b
    | Efor (i, a, b, body) ->
        let bound = bind i Local in
        expr env a; expr env b; expr (bound :: env) body
    | Econstraint (e, t) -> expr env e; ty t
    | Eletmodule (n, m, body) -> mark n Module; modexpr m; expr env body
    | Eopen (m, e) ->
        modexpr m;
        (* let open M in e, M.(e): M's names seen first in e *)
        let saved = !local_opens in
        (match m with Mident l -> Option.iter (fun n -> local_opens := n :: !local_opens) (last_name l) | _ -> ());
        expr env e;
        local_opens := saved
    | Enewtype (n, e) -> mark n Type; expr env e
    | Epack m -> modexpr m
  (* a function's parameters, and their defaults *)
  and params (env : env) (ps : param list) : env =
    List.concat_map (fun (p, default) -> Option.iter (expr env) default; pat Parameter p) ps
  and cases env cs =
    List.iter
      (fun c ->
        let bound = pat Local c.cpat @ env in
        Option.iter (expr bound) c.guard;
        expr bound c.cbody)
      cs
  (* a local let's binding: its parameters in scope in its body *)
  and binding (env : env) (b : binding) =
    Option.iter ty b.bty;
    expr (params env b.bparams @ env) b.bbody
  and modexpr (m : modexpr) =
    match m with
    | Mident l -> path l Module
    | Mstruct is -> scoped (fun () -> List.iter item is)
    | Mfunctor (ps, m) -> List.iter (fun (n, t) -> mark n Module; Option.iter modtype t) ps; modexpr m
    | Mapply (a, b) -> modexpr a; modexpr b
    | Mconstraint (m, t) -> modexpr m; modtype t
    | Munpack e -> expr [] e
  and modtype (t : modtype) =
    match t with
    | MTident l -> path l Module
    | MTsig is -> scoped (fun () -> List.iter item is)
    | MTfunctor (ps, t) -> List.iter (fun (n, t) -> mark n Module; Option.iter modtype t) ps; modtype t
    | MTwith (t, cs) -> modtype t; List.iter (fun (l, u) -> path l Type; ty u) cs
    | MTtypeof m -> modexpr m
  (* a top-level let: its name a definition, a function's or a value's *)
  and top_binding (b : binding) =
    (match b.bpat with
    | Pvar n ->
        let fn = b.bparams <> [] || (match b.bbody with Efun _ | Efunction _ -> true | _ -> false) in
        mark n (if fn then Def_function else Def_value)
    | p -> ignore (pat Def_value p));
    binding [] b
  and constr (c : constr) =
    mark c.cname Constructor;
    List.iter ty c.cargs;
    List.iter field c.crecord;
    Option.iter ty c.cres
  and field (f : Ast_ml.field) = mark f.fname Field; ty f.fty
  and item (it : item) =
    match it with
    | Ilet (recursive, bs) ->
        (* its names seen by its bodies if rec, after them if not *)
        let names () =
          List.iter
            (fun b ->
              match b.bpat with
              | Pvar n -> define top_values Value n Def_value
              | p ->
                  let bound = pat Def_value p in
                  top_values := bound @ !top_values;
                  List.iter (fun (text, (_, tok)) -> record tok Highlight_code.Value text) bound)
            bs
        in
        if recursive then (names (); List.iter top_binding bs) else (List.iter top_binding bs; names ())
    | Itype ds ->
        (* recursive: all its types and constructors seen by all *)
        List.iter
          (fun d ->
            define top_types Type d.tname Def_type;
            match d.tkind with Kvariant cs -> List.iter (fun c -> define top_constrs Constr c.cname Constructor) cs | _ -> ())
          ds;
        List.iter
          (fun d ->
            mark d.tname Def_type;
            List.iter (fun n -> mark n Type_var) d.tparams;
            Option.iter ty d.tmanifest;
            match d.tkind with
            | Kabstract | Kopen -> ()
            | Kvariant cs -> List.iter constr cs
            | Krecord fs -> List.iter field fs)
          ds
    | Iexception c -> constr c; mark c.cname Def_type; define top_constrs Constr c.cname Def_type
    | Iexternal (n, t) -> mark n Def_function; ty t; define top_values Value n Def_function
    | Ival (n, t) -> mark n (match t with Tarrow _ -> Def_function | _ -> Def_value); ty t; define top_values Value n Def_value
    | Imodule (n, m) -> mark n Def_module; in_module n (fun () -> modexpr m)
    | Imodsig (n, t) -> mark n Def_module; in_module n (fun () -> modtype t)
    | Imodtype (n, t) -> mark n Def_module; Option.iter modtype t
    | Iopen m ->
        (match m with Mident l when !depth = 0 -> ( match List.rev l with n :: _ -> opens := n.text :: !opens | [] -> ()) | _ -> ());
        modexpr m
    | Iinclude m -> modexpr m
    | Ieval e -> expr [] e
    | Iclass es -> List.iter (expr []) es
  in
  List.iter item file.items;
  (out, binds, (List.rev !defs, List.rev !refs, List.rev !opens))

(* the guess, and over it what the tree says: of a name only (a
 * lowercase or an uppercase one), and not of a capability, which the
 * repository's habits say better than the grammar (Cap.x, caps) *)
let categorize_bound (toks : Token_ml.t list) =
  let tree, binds, others = resolve (Parse_ml.parse toks) in
  (* an array, not List.mapi: a recursion a token, which a browser's
   * stack does not hold (the lines' comment below) *)
  ( Array.to_list
      (Array.mapi
         (fun i ((t : Token_ml.t), c) ->
           match Hashtbl.find_opt tree i with
           | Some c' when (t.kind = Lident || t.kind = Uident) && c <> Capability -> (t, c')
           | _ -> (t, c))
         (Array.of_list (guess toks))),
    binds,
    others )

let categorize (toks : Token_ml.t list) : (Token_ml.t * category) list =
  let cats, _, _ = categorize_bound toks in
  cats

(* claude: rev_map and rev, not List.map: OCaml 4.14's map recurses once
 * a token, which natively's stack holds but a browser's does not -- on
 * the web the biggest files (Tui_turbo.ml, 9,600 tokens) overflowed it,
 * and the code map, given up on them, showed them plain
 *
 *   old: List.map (fun ...) (categorize (Lexer_ml.tokens src))
 *)
let analyze (src : string) : analysis =
  let toks = Lexer_ml.tokens src in
  let cats, binds, (defs, refs, opens) = categorize_bound toks in
  (* claude: Array.map, not List.map (above) *)
  let places = Array.map (fun (t : Token_ml.t) -> (t.line, t.col, t.text)) (Array.of_list toks) in
  {
    spans = Highlight_code.lines src (List.rev (List.rev_map (fun ((t : Token_ml.t), c) -> (t.line, t.col, t.text, c)) cats));
    occurrences = Highlight_code.occurrences places binds;
    definitions = List.rev (List.rev_map (fun (tok, space, name) -> Highlight_code.definition ~name places tok space 3) defs);
    references = List.rev (List.rev_map (fun (tok, path, space, opens) -> Highlight_code.reference ~opens places tok path space) refs);
    opens;
    includes = [];
  }

let lines (src : string) : span list array = (analyze src).spans
