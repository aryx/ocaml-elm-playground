(* Selectors: which elements a rule is for -- CSS's selectors, Level 3.

   (notes_css_engine.md section 3.) A selector is **compounds joined by
   combinators**:

     ul > li.item:not(.done)
       compound "ul"; combinator ">" (a child); compound "li.item:not(.done)":
       the name li, the class item, and :not(.done)

   Combinators: a space (a descendant, at any depth), ">" (a child),
   "+" (the sibling just before), "~" (any sibling before). A compound: a
   name or "*", then any of #id, .class, [attr], [attr=v], [attr~=v]
   (one of its words), [attr|=v], [attr^=v], [attr$=v], [attr*=v]
   (starts, ends, contains; "i" before the "]" to ignore case),
   pseudo-classes -- :first-child, :last-child, :only-child,
   :nth-child(an+b), :not(...), :link, :visited, :hover, :active,
   :focus, :root, :empty, :checked, :disabled -- and a pseudo-element
   at its end (::before, ::after: boxes the element makes).

   **Matching goes right to left**: the element must match the last
   compound; then, for a descendant combinator, some ancestor the one
   before it (the nearest that matches, then further up if what is left
   fails: backtracking), for a child its parent, for siblings the
   elements before it in its parent. Right to left, because most
   elements fail the last compound at once -- it names a class they do
   not have -- and nothing further is looked at.

   **Specificity** is (ids, classes + attributes + pseudo-classes, names
   + pseudo-elements), compared left to right; :not(x) counts as its
   most specific x; "*" nothing:

     ul > li.item:not(.done)     (0, 2, 2)
     #nav a:hover                (1, 1, 1)

   :first-of-type and :last-of-type are read as :first-child and
   :last-child (the same on most pages; the difference an exercise).
   The pages are static here: :hover, :active and :focus never match
   (a pointer's state would make every move restyle the page: an
   exercise), and :visited is the host's.

   Reference: W3C, "Selectors Level 3" (2018), sections 3 to 16;
   notes_css_engine.md section 3. *)

type attr_op = Exists | Equals | Includes | Dash | Prefix | Suffix | Substring

type pseudo_class =
  | First_child
  | Last_child
  | Only_child
  | Nth_child of int * int (* an+b *)
  | Not of complex list
  | Link
  | Visited
  | Hover
  | Active
  | Focus
  | Root
  | Empty
  | Checked
  | Disabled

and simple =
  | Type of string (* lowercased *)
  | Universal
  | Id of string
  | Class of string
  | Attr of string * attr_op * string * bool (* the name, the test, the value, case ignored *)
  | Pseudo of pseudo_class
  | Pseudo_element of string (* "before", "after" *)

and combinator = Descendant | Child | Next_sibling | Subsequent_sibling

(* compounds from left to right, each with the combinator joining it to
 * the next: "ul > li" is [ ([Type "ul"], Some Child); ([Type "li"], None) ] *)
and complex = (simple list * combinator option) list

(* a selector list, "h1, h2": None if any part is not understood (the
 * whole rule is then dropped, as CSS says) *)
val parse : Css_syntax.component list -> complex list option
val parse_string : string -> complex list option

val specificity : complex -> int * int * int

(* the pseudo-element a selector ends with, if any: its rule styles
 * that generated box, not the element *)
val pseudo_element : complex -> string option

(* [matches ?visited sel ancestors e]: [ancestors] the element's, the
 * nearest first; [visited] whether a link's href was visited (never, by
 * default). A selector with a pseudo-element matches the element whose
 * box it is. *)
val matches : ?visited:(string -> bool) -> complex -> Dom.element list -> Dom.element -> bool

(* written back: "ul > li.item:not(.done)" *)
val to_string : complex -> string
