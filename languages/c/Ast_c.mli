(* Ast_c: a C file's tree, as much as a highlighter needs.

   As Ast_ml: not a compiler's tree, which would keep every operator and
   every type's detail, but the names and what they are -- a function
   defined, a parameter, a local and its block, a field, a type, a
   label -- so that Highlight_c can colour each name as what it is, with
   C's scopes. The rest is kept only to reach the names inside it: an
   operator's operands, not the operator.

   A declaration is C's: specifiers (static unsigned long, struct S
   {...}, a typedef's name) then declarators, each a name inside a type
   ( *p, a[10], ( *f)(int) ): [ty] is the two put together, so that a
   name is a function when its type is one ([Tfunc], not a pointer to
   one). *)

(* a name, and its token: its index in the file's tokens (Lexer_c's,
   comments included); -1 when it has none *)
type name = { text : string; tok : int }

type ty =
  | Tbase (* int, unsigned long: keywords only *)
  | Tname of name (* a typedef's name, or a name taken for one: Proc, ulong *)
  | Tstruct of name option * decl list option (* struct or union S, { its fields } *)
  | Tenum of name option * (name * expr option) list option
  | Tptr of ty
  | Tarray of ty * expr option
  | Tfunc of ty * decl list (* its result, its parameters *)
  | Ttypeof of expr

(* a declared name (none: an abstract declarator, a parameter's type
   alone, an anonymous struct member), its type, its initializer *)
and decl = { dname : name option; dty : ty; dinit : init option }

and init =
  | Iexpr of expr
  | Ilist of (designator list * init) list (* { .x = 1, [2] = 3, 4 } *)

and designator = Dfield of name | Dindex of expr

and expr =
  | Econst (* 42, "s", 'c' *)
  | Eident of name
  | Efield of expr * name (* e.x, e->x *)
  | Ecall of expr * expr list
  | Ecast of ty * expr
  | Etype of ty (* sizeof(T), and a macro's argument that is a type: va_arg(l, int) *)
  | Ecompound of ty * init (* (T){ ... } *)
  | Estmt of stmt list (* gcc's ({ ... }) *)
  | Emisc of expr list (* an operator's operands, a comma's, a ?:'s *)

and stmt =
  | Sexpr of expr
  | Sdecl of spec * decl list (* a local declaration *)
  | Sblock of stmt list (* a scope *)
  | Sfor of stmt option * expr list * stmt (* for(int i = 0; ...): its own scope *)
  | Snest of expr list * stmt list (* if, while, switch, return, case, a macro's loop: expressions then statements *)
  | Slabel of name * stmt
  | Sgoto of name

(* the specifiers: the declared names' type before their declarators *)
and spec = { typedef : bool; base : ty }

(* a #define: its name, its parameters if it takes some, the names in
   its body *)
type define = { mname : name; mparams : name list option; mbody : name list }

type item =
  | Ifunc of spec * decl * decl list * stmt list (* a function's definition: its declarator, its K&R parameters' declarations, its body *)
  | Idecl of spec * decl list (* a typedef, a global, a prototype, a struct *)
  | Imacro of expr (* a macro used at the top: FOO(x) *)

type file = {
  items : item list;
  defines : define list;
  skipped : (int * int) list; (* what did not parse: from a token, to one (excluded) *)
}
