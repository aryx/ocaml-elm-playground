(* Html_lexer: a page's text cut into tokens -- tags, text, comments.

   The tokenizer is the first stage that knows it is reading HTML.
   Characters in, tokens out (notes_browser.md section 3):

     <p class=intro>Caf&eacute; <a href="menu.html">menu</a>

     Start_tag "p" [class = "intro"]
     Text      "Café "
     Start_tag "a" [href = "menu.html"]
     Text      "menu"
     End_tag   "a"

   Nothing here knows that a <p> cannot hold another <p>, or that </a>
   closes the <a>: that is the tree's business (Html_tree). A token is
   only what the characters say -- and where its names come from
   (Dtd.origin): Netscape's extensions added no syntax, only names and
   values, so a start tag is marked when its element is Netscape's,
   and a core element's Netscape attributes are kept apart:

     <font size=+1>   Start_tag "font" [size = "+1"] {Netscape}
     <hr noshade>     Start_tag "hr" [] {Netscape: noshade = ""}

   It is a **state machine**, the WHATWG's (HTML, 13.2.5 "Tokenization"),
   reading a character at a time, the state saying what the character
   means. Here each state is a function and a transition is a tail call
   to the next one, so the machine reads as the spec's list:

                     letter            space           letter
     data ---'<'---> tag open ------> tag name -------> attribute name
      ^                |   '/'           |  '>': emit      | '='
      |                |   '!'           v                 v
      |                |    -> markup    data           before value
      |                |       (<!-- comment, <!DOCTYPE)   | '"'  '\''  other
      |                |                                   v
      |                '-- another char: the '<' was text   value (quoted,
      '------------------------------------------------ '>'  or up to a space)

   What makes it HTML's and not a textbook's, each checked by the tests:

   - **A '<' that starts no tag is text**: "a < b", "<3", "< p>". Only
     '<' followed by a letter, '/', '!' or '?' does anything.
   - **Names are case-insensitive**, lowercased: <P>, <p>; HREF, href.
   - **Three ways to quote a value**: "double", 'single', or none (up to
     a space or '>': <td align=center>); and no value at all (<hr
     noshade>, the value ""). A name given twice keeps its first value.
   - **Entities are decoded** in text and in attribute values
     (Entities.decode): &amp; in an href is an "&".
   - **Some elements' content is not HTML**: inside <script> and
     <style> nothing is a tag until the matching end tag (RAWTEXT: "if
     (a<b)" is not a tag); <title> and <textarea> the same, but with
     entities decoded (RCDATA). The tokenizer needs to know these names
     -- the one place it knows elements -- because the tree comes too
     late to tell it.
   - **Comments**: <!-- ... -->; <!--> and <!---> are empty ones (the
     spec's), and one never closed runs to the end. <!DOCTYPE ...> is a
     token of its own. <? ... > and any other <! ... > are "bogus
     comments", read up to the '>' and kept as comments.
   - **A tag cut off by the end of the page is dropped**, as the spec
     says: "<a href=" at the very end is nothing.
   - **Line ends are normalized** first: CR LF and a lone CR become LF.

   Not done, the WHATWG's cases we treat another way (its tokenizer has
   80 states, ours a dozen): character references inside RAWTEXT (none
   there, as the spec says, and we agree), the "&copy=2" rule for
   attributes (Entities.mli), <plaintext>, CDATA sections (only in
   SVG and MathML), and the parse errors (the spec names each one; we
   recover the same way, silently).

   Reference: WHATWG HTML Living Standard, 13.2.5 "Tokenization" (the
   data, tag open, end tag open, tag name, attribute name and value,
   comment, DOCTYPE, RCDATA and RAWTEXT states); Mothra's rdhtml.c and
   MMM's lexhtml.mll, the same job in C and ocamllex (see
   notes_browser_related_work.md). *)

(* a name (lowercased) and its value (entities decoded) *)
type attribute = string * string

type token =
  | Doctype of string (* after "<!DOCTYPE", trimmed: "html", "HTML PUBLIC ..." *)
  | Start_tag of {
      name : string;
      attributes : attribute list; (* all of a Netscape element's *)
      extensions : attribute list; (* a core element's Netscape ones *)
      origin : Dtd.origin; (* the element's *)
      self_closing : bool; (* <br/> *)
    }
  | End_tag of string
  | Text of string (* consecutive text is one token *)
  | Comment of string

(* the tokens of a page's text (UTF-8, Charset's) *)
val tokenize : string -> token list

(* a token as the notes and the tests write it: Start_tag "p" [class =
 * "intro"], Text "Café\n" (a newline as \n) *)
val to_string : token -> string

(* the value of an attribute, if the tag has it: attribute "href" attrs *)
val attribute : string -> attribute list -> string option
