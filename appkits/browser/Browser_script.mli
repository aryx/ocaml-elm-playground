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

   **Events** (notes_javascript.md section 10). The browser runs one
   thing at a time, a **task**: the page's scripts at load, one event's
   handlers, one timer's function, each run to its end (a script is
   never interrupted: while it runs, the page does not move); then, if
   the tree changed, the browser lays the page out again -- once,
   however many changes. A click is found in the layout (Hit.element_at)
   and dispatched here ([click]); it **bubbles**: the handlers of the
   element it fell on, then of its parent, up to the document, unless
   one calls event.stopPropagation(); event.preventDefault() -- or an
   onclick="..." returning false, Netscape 2's way -- cancels what the
   browser would have done next (follow the link). HyperCard's path,
   a quarter century before (appkits/hypertalk):

     HyperCard (1987)                        the DOM (1998)
     button -> card -> background -> stack   element -> parents -> body -> document
     "pass mouseUp" goes on                  bubbling goes on unless stopped
     the card's script answers every button  a handler on <ul> answers every <li>

   Handlers are addEventListener's (in order), el.onclick = f, and the
   onclick="..." attribute, its text compiled once into a function of
   event. setTimeout and setInterval run on the page's clock, which the
   host moves ([advance]): the frame clock in TinyFirefox, so that a
   golden frame sees the same ticks each run. alert only queues its
   message for the browser to show after the task ([take_alerts]): a
   real one stops the script until OK, which a script that is an OCaml
   call cannot do.

   What it keeps of the DOM (plan_tiny_firefox.md): document's
   getElementById, querySelector, querySelectorAll, createElement,
   createTextNode, body, title; an element's tagName, id, className,
   textContent, innerHTML, getAttribute, setAttribute, removeAttribute,
   style, value, children, firstChild, parentNode, appendChild,
   removeChild, insertBefore, remove, addEventListener,
   removeEventListener; setTimeout, setInterval, clearTimeout,
   clearInterval, alert. Not: the node types but elements
   and text, NodeList's liveness, ranges, the forms' own interface. A
   form's field typed into keeps its text in the browser (Browser_page's
   values), not in the tree; [value] reads the value= attribute. *)

(* a page with its scripts: the engine, the copy of its tree, the console *)
type t

(* [create ?seed ?log tree]: the tree thawed, document defined; the
 * console's lines also given to [log] as they come *)
val create : ?seed:int -> ?log:(string -> unit) -> Dom.element -> t

(* the page's <script>s, in order: each one's error in the console;
 * then the document's DOMContentLoaded and load listeners *)
val run_scripts : t -> unit

(* a script of the host's (a console's line typed, a test's): its value,
 * or its error; errors also in the console *)
val eval : t -> string -> (Js_value.value, Js_eval.error) result

(* the tree as the scripts left it, frozen; its elements are the ones
 * [click] and [input] take (the layout of this tree gives them) *)
val tree : t -> Dom.element

(* a click on an element of the last [tree]: dispatched, bubbling;
 * whether a handler prevented the default (the link not followed) *)
val click : t -> Dom.element -> bool

(* a key pressed ("a", "Enter", "ArrowUp"): keydown at the body,
 * bubbling to the document, event.key the key; whether prevented *)
val key : t -> string -> bool

(* a form's field typed into: its value= the text, then its input
 * event *)
val input : t -> Dom.element -> string -> unit

(* the page's clock moved on by [ms]: the timers due run, the earliest
 * first, each a task (a thousand at most per call) *)
val advance : t -> float -> unit

(* the messages alert() queued since the last call, the oldest first *)
val take_alerts : t -> string list

(* whether a script changed the tree since the last [tree] *)
val changed : t -> bool

(* what the console printed, the oldest first: console.log's lines, and
 * the errors as "Uncaught TypeError: ... (line 3)" *)
val console : t -> string list

(* the engine: a console's line typed, the host calling a script's
 * function *)
val engine : t -> Js_eval.t
