(* Css_values: what a declaration's value means -- lengths, colours,
   calc(), and var()'s substitution.

   (notes_css_engine.md section 5.) A declared value is text (Css_syntax's
   components); a computed value is a number or a colour. Between them:

   **Custom properties.** A property whose name starts with "--" is
   inherited and means nothing itself; var(--name, fallback) is replaced
   by its value (or the fallback) *before* the property using it is
   read, so one var() can make a colour here and a length there:

     :root { --accent: #36c; --gap: 8px }
     a     { color: var(--accent); margin: 0 var(--gap) }   #36c; "0 8px"

   **Lengths** are resolved as far as they can be when the style is
   computed: px, em (the element's font size -- its parent's for
   font-size itself), rem (the root's), pt, vw and vh (the window's);
   a percentage waits for the containing block's width, known only to
   the layout. So a computed length is a sum -- so many pixels, plus so
   much percent of what it will be measured against -- and calc() only
   adds to it:

     calc(100% - 2em), the font 16:   { px = -32; pct = 100 }
     in a block 600 wide:              600 - 32 = 568

   (min(), max() and clamp() of lengths that mix pixels and percentages
   are taken on their pixels: an approximation, said so.)

   **Colours**: #rgb, #rgba, #rrggbb, #rrggbbaa, rgb() and rgba() (with
   commas or spaces, numbers or percentages, an alpha), hsl() and
   hsla(), the 148 names of CSS Color Level 4, transparent, and
   currentcolor (the element's colour).

   Reference: W3C, CSS Custom Properties for Cascading Variables Level
   1 (sections 2, 3: var() and its substitution); CSS Values and Units
   Level 3 (lengths, calc()); CSS Color Level 4 (the syntax, the named
   colours). *)

(* so many pixels, plus [pct] percent of the containing block *)
type length = { px : float; pct : float }

type color = { r : int; g : int; b : int; a : float (* 0 to 1 *) }

(* what a length is measured with: the element's font size (its
 * parent's, for font-size), the root's, the window's size *)
type context = { em : float; rem : float; viewport_width : float; viewport_height : float }

(* [resolve l base]: the length in pixels, [base] the containing block's *)
val resolve : length -> float -> float

val px : float -> length
val zero : length

(* the value with each var() replaced ([lookup] a custom property's
 * value, if the element has it); None if a var() has neither a value
 * nor a fallback: the declaration is then as if absent *)
val substitute : (string -> Css_syntax.component list option) -> Css_syntax.component list -> Css_syntax.component list option

(* a length: a dimension, a percentage, 0, calc(), min(), max(),
 * clamp() *)
val length : context -> Css_syntax.component -> length option

(* a colour; [current] the element's for currentcolor *)
val color : current:color -> Css_syntax.component list -> color option

(* the value's parts, spaces dropped: "0 auto" is [0; auto] *)
val parts : Css_syntax.component list -> Css_syntax.component list

val black : color
val transparent : color
