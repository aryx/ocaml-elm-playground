(* Ast_ml: an OCaml file's tree, as a highlighter needs it.

   What a compiler's tree keeps -- every construct, typed, desugared --
   is more than colouring asks for; what colouring asks for, a token's
   lexer cannot say: whether a lowercase name is a parameter, a local,
   a field or a function being defined. So this tree keeps the names,
   each with its token (its index in the file's tokens, comments
   included: [Parse_ml]), and the constructs that decide what a name is
   -- where a binding starts and ends (let, fun, match, a module), what
   is a field (p.x, {x = ...}), a type, a constructor. The rest (an
   operator, a constant) is only there to hold the names under it.

   Parse_ml builds it; Highlight_ml walks it with the names in scope. *)

(* a name as written, and its token *)
type name = { text : string; tok : int }

(* M.N.x: the modules, then the last *)
type longid = name list

type ty =
  | Tany (* _ *)
  | Tvar of name (* 'a *)
  | Tarrow of ty * ty (* label:a -> b; the label is the lexer's *)
  | Ttuple of ty list
  | Tconstr of longid * ty list (* int, 'a list, (a, b) Hashtbl.t *)
  | Tobject of (name * ty) list (* < m : t; .. >, a capability's *)
  | Tvariant of (name * ty list) list * ty list (* [ `A | `B of t | other ] *)
  | Tpoly of name list * ty (* 'a. t *)
  | Tpackage of longid (* (module S) *)

type pat =
  | Pany
  | Pvar of name
  | Pconst
  | Ptuple of pat list
  | Pconstr of longid * pat option (* Some p, M.C *)
  | Pvariant of name * pat option (* `A p *)
  | Precord of (longid * pat option) list (* { x; y = p } *)
  | Plist of pat list (* [p; q], [| |] *)
  | Por of pat * pat
  | Palias of pat * name (* p as x *)
  | Pconstraint of pat * ty
  | Pmodule of name (* (module M) *)
  | Popen of longid * pat (* M.(p) *)
  | Pinner of pat (* lazy p, exception p *)

type expr =
  | Econst
  | Eident of longid (* x, M.x, ( + ) *)
  | Econstr of longid * expr option (* Some e, M.C *)
  | Evariant of name * expr option (* `A e *)
  | Etuple of expr list
  | Elist of expr list (* [a; b], [| |] *)
  | Erecord of expr option * (longid * expr option) list (* { e with x = a; y } *)
  | Efield of expr * longid (* e.x, e.M.x *)
  | Esetfield of expr * longid * expr (* e.x <- a *)
  | Eapply of expr * expr list (* f a ~x:b, an operator's two sides *)
  | Elet of bool * binding list * expr (* let rec? ... in e *)
  | Eletop of binding list * expr (* let* p = a in e *)
  | Efun of param list * expr
  | Efunction of case list
  | Ematch of expr * case list (* and try *)
  | Eif of expr * expr * expr option
  | Eseq of expr list
  | Ewhile of expr * expr
  | Efor of name * expr * expr * expr
  | Econstraint of expr * ty (* (e : t), (e :> t) *)
  | Eletmodule of name * modexpr * expr
  | Eopen of modexpr * expr (* let open M in e, M.(e), M.[e] *)
  | Enewtype of name * expr (* fun (type a) -> e *)
  | Epack of modexpr (* (module M) *)
  | Emisc of expr list (* assert, lazy, a method's call: the names under it *)

(* a parameter: its pattern, and an optional one's default (?(x = 1)) *)
and param = pat * expr option

(* let f x y : t = e: [bpat] f, [bparams] x y *)
and binding = { bpat : pat; bparams : param list; bty : ty option; bbody : expr }

and case = { cpat : pat; guard : expr option; cbody : expr }

and modexpr =
  | Mident of longid
  | Mstruct of item list
  | Mfunctor of (name * modtype option) list * modexpr
  | Mapply of modexpr * modexpr
  | Mconstraint of modexpr * modtype
  | Munpack of expr (* (val e) *)

and modtype =
  | MTident of longid
  | MTsig of item list
  | MTfunctor of (name * modtype option) list * modtype
  | MTwith of modtype * (longid * ty) list (* S with type t = u *)
  | MTtypeof of modexpr

and item =
  | Ilet of bool * binding list
  | Itype of typedecl list
  | Iexception of constr
  | Iexternal of name * ty
  | Ival of name * ty (* in a signature *)
  | Imodule of name * modexpr (* and module rec *)
  | Imodsig of name * modtype (* module M : S, in a signature *)
  | Imodtype of name * modtype option
  | Iopen of modexpr
  | Iinclude of modexpr
  | Ieval of expr (* a top-level expression: let () = ... ; ... *)
  | Iclass of expr list (* classes: their expressions only *)

and typedecl = { tname : name; tparams : name list; tmanifest : ty option; tkind : tkind }

and tkind =
  | Kabstract
  | Kvariant of constr list
  | Krecord of field list
  | Kopen (* .. *)

(* a constructor's declaration: C of a * b, C of { x : t }, C : a -> t *)
and constr = { cname : name; cargs : ty list; crecord : field list; cres : ty option }

and field = { fname : name; fty : ty }

(* a file: its items, and where the parser gave up and skipped to the
   next item (token indices, from and to excluded) *)
type file = { items : item list; skipped : (int * int) list }
