(* Dom: a page as a tree -- elements with attributes and children, and
   text.

   The Document Object Model is what the rest of a browser works on:
   the looks are computed on it, the layout walks it, a click finds its
   way back to it (and, in browsers with scripts, JavaScript changes
   it). Html_tree builds it from the tokens; nothing here knows HTML's
   rules, only the shape:

     html                       the root, always there (Html_tree
      +- head                    makes the three of them even when the
      |   +- title               page wrote none)
      |       +- "Lunch"
      +- body
          +- h1
          |   +- "Menu"
          +- p
              +- "Soup of the day"

   The real DOM has more kinds of node (the document itself, comments,
   the doctype, processing instructions) and each node knows its
   parent; ours keeps elements and text, and is a value: built once,
   then only read, so a parent is where you came from.

   Reference: WHATWG, "DOM Living Standard", section 4 (nodes, trees);
   notes_browser.md section 4. *)

type node = Element of element | Text of string

and element = {
  name : string; (* lowercased: "p" *)
  attributes : (string * string) list;
  children : node list;
}

(* the value of an attribute *)
val attribute : string -> element -> string option

(* the elements named so, in document order, [root] included *)
val find_all : string -> element -> element list

(* the text inside, in document order, concatenated *)
val text_content : element -> string

(* the tree without the text nodes that are only spaces and newlines
 * (the source's indentation, which layout ignores): what a person
 * reading the tree wants, and the tests' way to write it *)
val without_blank_text : element -> element

(* the tree as indented lines, two spaces a level, an element as
 * [name attr="value"], a text quoted with its newlines as \n:
 *
 *   html
 *     head
 *       title
 *         "Lunch"
 *)
val to_lines : element -> string list
