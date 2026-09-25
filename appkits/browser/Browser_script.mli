(* Browser_script: a page's scripts, and the page they see -- the DOM.

   (notes_javascript.md section 9, plan_tiny_firefox.md J3.) The engine
   (libs/languages/javascript) knows nothing of pages; this module gives
   it one. A script reaches the page through **host objects**:
   [document], and an object per element it asks for, whose properties
   and methods are OCaml functions over the page's tree:

     document.getElementById("count")     the element whose id= is "count"
     document.querySelector("ul li.done") the first one Css.matches
     el.textContent = "3"                 its children replaced by the text "3"
     el.innerHTML = "<b>3</b>"            by Html_tree's parse of the string
     el.style.color = "red"               "color: red" in its style= (Css, N5)
     el.className = "done"                its class=: another rule may match now
     el.appendChild(document.createElement("li"))

   **The copy.** The browser's tree (Dom) is a value, built once and
   read by everything since TinyMosaic: the looks, the layout, the hit
   test, the forms. A script needs to change it. So the script works on
   a **mutable copy** -- nodes with a parent, their children and
   attributes changeable -- thawed from the page's tree, and [tree]
   freezes it back into a Dom.element when the browser wants to lay the
   page out again ([changed] says whether it must). An element keeps the
   same host object for as long as it lives, so [getElementById("x") ===
   getElementById("x")] and a variable holding an element stays good.
   Real engines mutate one tree and lay it out again incrementally;
   here the pages are small and a whole layout is milliseconds.

     page's Dom  --thaw-->  nodes  <--  scripts (through the host objects)
                              |
       layout  <--  Dom  <--freeze (when changed)

   **The scripts** of the page, its <script> elements, run in order once
   the page is read -- as the attribute defer asks, rather than as the
   parser meets them, so a script can find every element whatever its
   place. An error goes to the console (its line and message) and the
   next script still runs, as in every browser.

   What it keeps of the DOM (plan_tiny_firefox.md): document's
   getElementById, querySelector, querySelectorAll, createElement,
   createTextNode, body, title; an element's tagName, id, className,
   textContent, innerHTML, getAttribute, setAttribute, removeAttribute,
   style, value, children, firstChild, parentNode, appendChild,
   removeChild, insertBefore, remove. Not: the node types but elements
   and text, NodeList's liveness, ranges, the forms' own interface. A
   form's field typed into keeps its text in the browser (Browser_page's
   values), not in the tree; [value] reads the value= attribute. *)

(* a page with its scripts: the engine, the copy of its tree, the console *)
type t

(* [create ?seed ?log tree]: the tree thawed, document defined; the
 * console's lines also given to [log] as they come *)
val create : ?seed:int -> ?log:(string -> unit) -> Dom.element -> t

(* the page's <script>s, in order: each one's error in the console *)
val run_scripts : t -> unit

(* a script of the host's (a console's line typed, a test's): its value,
 * or its error; errors also in the console *)
val eval : t -> string -> (Js_value.value, Js_eval.error) result

(* the tree as the scripts left it, frozen *)
val tree : t -> Dom.element

(* whether a script changed the tree since the last [tree] *)
val changed : t -> bool

(* what the console printed, the oldest first: console.log's lines, and
 * the errors as "Uncaught TypeError: ... (line 3)" *)
val console : t -> string list

(* the engine, for the events (J4): its functions called by the host *)
val engine : t -> Js_eval.t
