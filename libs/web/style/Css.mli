(* Css: style sheets -- CSS1's rules, matched against the tree and
   cascaded into each element's declarations.

   Mosaic's looks were the browser's (Looks' table); Netscape let the
   page choose some (<font>, <center>, bgcolor=) by writing them into
   the HTML, tag by tag. Cascading Style Sheets (Håkon Wium Lie's
   proposal, 1994; CSS1, W3C, December 1996; Internet Explorer 3 that
   year, Netscape 4 the next) took the looks out of the HTML into rules
   of their own:

     p.note, blockquote { color: #660000; font-style: italic }
     ^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     selectors            declarations: a property, a value

   in a <style> element of the page (or a file it links to), or in the
   style="..." attribute of one element. The words of CSS1 kept here:

     selector      what it matches                  specificity
     p             an element of that name          (0, 0, 1)
     .note         class="note" (one of its words)  (0, 1, 0)
     #top          id="top"                         (1, 0, 0)
     p.note        both: the counts added           (0, 1, 1)
     ul li         an li inside a ul, at any depth  (0, 0, 2)
     *             any element                      (0, 0, 0)

   **The cascade**, for one element and one property: of the rules
   whose selector matches it, the one of the highest **specificity**
   wins -- (ids, classes, names) compared left to right -- and between
   equals, the later in the sheet; the element's style="..." beats
   them all. Worked example (notes_browser.md section 10, the tests'):

     p        { color: black }       (0, 0, 1)
     .intro   { color: green }       (0, 1, 0)
     p.intro  { color: red }         (0, 1, 1)
     #top     { color: blue }        (1, 0, 0)

     <p class=intro>              red: p.intro beats .intro and p
     <p class=intro id=top>       blue
     <p id=top style="color: gray">  gray

   What no rule says of an element, **inheritance** gives (colour, the
   font, the alignment: its parent's) or the property's initial value
   (margins); that is Looks.styled's business, as is what each
   property means. And before the page's rules, the browser's own:
   Looks' table, the "user agent style sheet", which is what Mosaic's
   table always was.

   Not done: the other selectors (a:link and a:visited, CSS1's pseudo
   classes; > and +, CSS2's), !important, @import, <link
   rel=stylesheet> (a sheet fetched like a picture), the shorthand
   properties but margin.

   Reference: W3C, "Cascading Style Sheets, level 1" (1996), sections
   1 (the basic concepts), 3 (the cascade: 3.2, the order) and 5 (the
   properties); Håkon Wium Lie's thesis, "Cascading Style Sheets"
   (2005), chapter 3. *)

(* an element's name, its id and classes: one link of a selector *)
type simple = { name : string option; id : string option; classes : string list }

(* the simple selectors from the outermost ancestor to the element
 * itself: "ul li" is [ul; li] *)
type selector = simple list

(* a rule for one selector (a group, "h1, h2 { }", is a rule for each) *)
type rule = { selector : selector; declarations : (string * string) list }

type sheet = rule list

(* the rules of a style sheet's text; what does not parse is skipped *)
val parse : string -> sheet

(* "color: red; font-size: 2em": properties lowercased, values trimmed *)
val declarations : string -> (string * string) list

(* (ids, classes, names) *)
val specificity : selector -> int * int * int

(* [matches selector ancestors e]: [ancestors] the element's, the
 * nearest first *)
val matches : selector -> Dom.element list -> Dom.element -> bool

(* the text of a page's <style> elements, in order *)
val page_sheet : Dom.element -> string

(* [cascade sheet root]: each element's declarations, cascaded -- the
 * matching rules by specificity then order, then its style=; a
 * property once, its winning value *)
val cascade : sheet -> Dom.element -> Dom.element -> (string * string) list
