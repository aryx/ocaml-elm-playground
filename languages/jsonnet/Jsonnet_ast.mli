(* Jsonnet_ast: a jsonnet program as a tree, what Jsonnet_parse builds
   and Jsonnet_eval runs.

   Some of jsonnet's sugar is taken off by the parser, so it is not here
   (the spec's desugaring): a method f(x): e is the field f: function(x)
   e; local f(x) = e is local f = function(x) e; e { ... } is e + { ... };
   a field's :::, ::, : is its [visibility], a +: its [bool]. [At] marks
   where an expression starts in the text, for the errors. *)

(* a field's :, :: (hidden from the output) or ::: (shown, even if an
   object below hid it) *)
type visibility = Default | Hidden | Visible

type expr =
  | Null
  | Bool of bool
  | Num of float
  | Str of string
  | Self
  | Dollar (* $, the outermost object *)
  | Var of string
  | Array of expr list
  | Array_comp of expr * comp list (* [e for x in a if c] *)
  | Object of member list
  (* { local x = e, [k]: v for x in a }: its locals, the key, the value *)
  | Object_comp of (string * expr) list * expr * expr * comp list
  | Field of expr * string (* e.f *)
  | Index of expr * expr (* e[i] *)
  | Slice of expr * expr option * expr option * expr option (* e[a:b:c] *)
  | Super_field of string
  | Super_index of expr
  | In_super of expr (* e in super *)
  | Call of expr * expr list * (string * expr) list (* the positional arguments, then the named *)
  | Local of (string * expr) list * expr
  | If of expr * expr * expr option
  | Binary of string * expr * expr
  | Unary of string * expr
  | Function of param list * expr
  | Assert of expr * expr option * expr
  | Import of string (* the path, relative to the importing file's already resolved *)
  | Importstr of string
  | Error of expr
  | At of int * expr

(* a parameter, and its default *)
and param = string * expr option

and comp = For of string * expr | If_comp of expr

and member =
  | Field_m of field_name * bool * visibility * expr (* the name, +:, the visibility, the value *)
  | Local_m of string * expr
  | Assert_m of expr * expr option

and field_name = Fixed of string | Computed of expr
