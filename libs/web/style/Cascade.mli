(* Cascade: for each element and each property, the declaration that
   wins -- over the browser's style sheet and the page's, @media
   evaluated, the rules indexed.

   (notes_css_engine.md section 4.) The declarations that apply to an
   element are sorted, the last winning:

     1. origin and importance: the browser's sheet (the user agent's,
        ua.css), then the page's (its <style>s, its <link>ed sheets),
        then the page's !important, then the browser's !important;
     2. specificity (Selectors), an element's style="..." above any
        selector;
     3. the order in which they were written.

     p { color: black }  .x { color: green !important }  #a { color: red }
     <p id=a class=x>    green: !important beats a higher specificity

   **@media** rules count only if their query holds for this window:
   a list of queries, any of them true -- each a media type (screen,
   all, print never) and features joined by "and" (min-width,
   max-width, min-height, max-height, orientation,
   prefers-color-scheme: light, prefers-reduced-motion: reduce never),
   "not" and "only" understood. So (max-width: 800px) is false in
   TinyChrome's 976 pixels, and Wikipedia's rules for phones sleep.
   @supports is taken as true for what the query names (a browser that
   reads CSS3 syntax), @import's sheets are the caller's to fetch, and
   the rest of the at-rules (@font-face, @keyframes...) are skipped.

   **The index.** Trying every rule on every element is rules times
   elements: Wikipedia's 1,391 rules on an article's ten thousand
   elements are fourteen million tries. So each rule is filed under its
   last compound's most telling part -- its id, else its first class,
   else its name, else "any" -- and an element tries only the rules
   filed under its id, its classes, its name, and "any": a few dozen.
   WebKit's and Blink's rule sets do the same. And the **ancestor
   filter**: what a rule's selector asks of the element's ancestors (the
   ids, classes and names of its compounds joined by descendant and
   child combinators) is checked against the ancestors' own, kept as
   the tree is walked -- a long selector like Wikipedia's "html.night
   .mw-parser-output a" fails there at once, without walking up
   (WebKit's selector filter, a Bloom filter there; a counted table
   here). On a Wikipedia article (5,637 elements, 1,557 rules), the
   cascade went from 0.95 s to 0.54 s.

   What is given back is the **declared value** of each property, still
   text (Css_syntax's components): Computed makes values of them,
   var() included, since a custom property is inherited like a colour.

   Reference: W3C, CSS Cascading and Inheritance Level 4, section 6 (the
   cascade's order); Media Queries Level 3; notes_css_engine.md
   section 4. *)

type origin = User_agent | Author

(* a style sheet's rules, and whose they are *)
type sheet = { origin : origin; rules : Css_syntax.rule list }

(* the window's: its size in pixels *)
type media = { width : float; height : float }

(* whether a media query list holds: "screen and (max-width: 800px),
 * print" *)
val media_matches : media -> Css_syntax.component list -> bool

(* [cascade ?visited media sheets root]: each element's declared values,
 * the winning declaration of each property (custom properties
 * included), its style= counted *)
val cascade :
  ?visited:(string -> bool) ->
  media ->
  sheet list ->
  Dom.element ->
  Dom.element ->
  (string * Css_syntax.component list) list

(* [find_element table e]: [e]'s value in a table filed by
 * [Hashtbl.hash], found by identity (two equal paragraphs are two) *)
val find_element : (int, Dom.element * 'a) Hashtbl.t -> Dom.element -> 'a option

(* the style rules of the sheets that count for [media], their @media
 * and @supports opened: how many a page has, for the devtools *)
val rules : media -> sheet list -> (origin * Selectors.complex * Css_syntax.declaration list) list
