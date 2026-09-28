(* Css_syntax: a style sheet's text read -- tokens, then rules and blocks.

   (notes_css_engine.md sections 1 and 2.) CSS's syntax is older and
   smaller than its properties, and CSS Syntax Level 3 wrote down the
   tokenizer every browser had converged on: identifiers, functions
   ("rgb("), at-keywords ("@media"), hashes ("#top"), strings, url()s,
   numbers with their kind (2, 50%, 1.5em), delimiters, and the
   punctuation. Tokens first, then **blocks** by matching brackets, then
   **rules** -- so that a "{" inside a string, a "}" inside a url() or a
   block inside @media never cut a rule in the wrong place, as a split
   on braces does (N5's first Css did).

     a:hover > .x { color: #f00 !important }

     Ident a, Colon, Ident hover, Whitespace, Delim >, Whitespace,
     Delim ., Ident x, Whitespace, then a { } block:
     Ident color, Colon, Whitespace, Hash f00, Whitespace, Delim !,
     Ident important

   A style sheet is **rules**: a qualified rule (a prelude, the
   selectors, and a { } block of declarations) or an at-rule
   (@media ... { rules }, @import url(x.css);). **Errors are skipped,
   not fatal**, CSS's own rule, the one that lets a browser read a sheet
   written for a newer one: a declaration that does not parse is dropped
   up to its ";", a rule up to its block's end, an unknown at-rule with
   its block.

   Not done: the tokens' exact positions (for a devtools' "line 3"),
   CSS nesting ("&:hover" inside a rule: its nested rules are dropped),
   @charset (a sheet is taken as UTF-8).

   Reference: W3C, "CSS Syntax Module Level 3", sections 4 (tokenization)
   and 5 (parsing). *)

type token =
  | Ident of string
  | Function of string (* "rgb(": the name, lowercase kept as written *)
  | At_keyword of string (* "@media": "media" *)
  | Hash of string (* "#f00": "f00" *)
  | String of string (* its escapes decoded *)
  | Url of string (* url(x.png), unquoted: "x.png" *)
  | Delim of char
  | Number of float
  | Percentage of float
  | Dimension of float * string (* 1.5em: 1.5, "em" *)
  | Whitespace
  | Colon
  | Semicolon
  | Comma

(* a token, a block ("(", "[" or "{" and what is inside), or a function
 * and its arguments *)
type component = Token of token | Block of char * component list | Func of string * component list

type declaration = { name : string; (* lowercased *) value : component list; important : bool }

type rule =
  | Style_rule of { prelude : component list; declarations : declaration list }
  | At_rule of { name : string; (* lowercased *) prelude : component list; block : component list option }

val tokenize : string -> token list

(* a text's components: tokens, blocks and functions *)
val components_of : string -> component list

(* spaces taken off both ends; a list cut at a token (commas) *)
val trim : component list -> component list
val split_on : token -> component list -> component list list

(* the rules of a style sheet's text *)
val parse_stylesheet : string -> rule list

(* the declarations of a style="..." attribute *)
val parse_declarations : string -> declaration list

(* an at-rule's block read as rules (@media's) or as declarations
 * (@font-face's) *)
val rules_of_block : component list -> rule list
val declarations_of_block : component list -> declaration list

(* components written back as text, spaces as one: "rgb(1, 2, 3)",
 * "0 auto", "calc(100% - 2em)" -- a declaration's value, as the
 * properties' readers take it *)
val to_string : component list -> string
