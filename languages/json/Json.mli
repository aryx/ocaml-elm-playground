(* Json: JSON read, for configuration files (.codemapconfig).

   JSON is JavaScript's literals alone -- objects, arrays, strings,
   numbers, true, false, null -- so its text is cut by JavaScript's
   lexer (Js_lexer: the strings' escapes decoded, the numbers read), and
   only the grammar is here, a recursive descent of five rules. Being
   JavaScript's lexer, it also takes comments (// and /* */) and a comma
   before a closing bracket, as jsonnet does, whose configuration files
   are JSON's superset.

   Worked example (the tests'):

     { "colors": { "kernel": "#e08030" }, "depth": 2, "x": [true, null] }

     Object [ ("colors", Object [ ("kernel", String "#e08030") ]);
              ("depth", Number 2.); ("x", Array [ Bool true; Null ]) ] *)

type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of t list
  | Object of (string * t) list (* in the text's order *)

(* the value of a text, or what is wrong with it and on which line *)
val parse : string -> (t, string) result

(* a field of an object, if it is one and has it *)
val member : string -> t -> t option
