(* Jsonnet_parse: jsonnet's grammar (the spec's, https://jsonnet.org/ref/spec.html),
   by recursive descent, a function per level of precedence, lowest
   first:

     expr    := local binds ; expr | if expr then expr [else expr]
              | function ( params ) expr | assert expr [: expr] ; expr
              | error expr | binary
     binary  := the operators, || && | ^ & (== !=) (< <= > >= in)
                (<< >>) (+ -) (times / %), each level left associative
     unary   := (- + ! ~) unary | postfix
     postfix := primary ( .id | [e] | [a:b:c] | (args) | { object } )*
     primary := null | true | false | self | $ | string | number | id
              | ( expr ) | { object } | [ array ] | super.id | super[e]
              | import string | importstr string | local, if, ... as expr

   An object's members, separated by commas: fields (id, 'string' or
   [expr], then : :: ::: +: +:: or +:::, or a method's (params) first),
   locals and asserts; or a comprehension, { [k]: v for x in a if c }.

   [path] is the file's, for its imports: import 'b.libsonnet' in
   dir/a.jsonnet is dir/b.libsonnet, resolved here (Jsonnet_ast.Import).

   Worked example (the tests'): {a+: 1, f(x):: x * 2} is Object
   [Field_m (Fixed "a", true, Default, Num 1.); Field_m (Fixed "f",
   false, Hidden, Function (["x", None], x * 2))]. *)

(* the tree, or the mistake and its line *)
val parse : path:string -> string -> (Jsonnet_ast.expr, int * string) result

(* [resolve ~from p]: [p] seen from the file [from], ".." and "." taken
   off *)
val resolve : from:string -> string -> string
