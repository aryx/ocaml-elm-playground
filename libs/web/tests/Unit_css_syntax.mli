(* Css_syntax: the notes' tokens (a selector with a combinator, a hash,
 * !important), blocks that a split on braces cuts wrong (a "}" in a
 * string, a ";" in a url()), a bad declaration skipped, @media's rules,
 * an unknown at-rule kept for its reader to skip, values written back *)
val tests : Testo.t list
