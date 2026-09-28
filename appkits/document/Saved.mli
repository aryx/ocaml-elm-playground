(* A value written down, and read back: OCaml's own Marshal, behind a
 * line saying what it is (plan_io.md).
 *
 * Marshal writes any value that is data -- a sheet, a text and its
 * looks, a bitmap, a drawing -- keeping what is shared shared, and
 * reads it back as it was. So an application needs no file format of
 * its own. Three things it will not do, which are why this module is
 * more than two calls:
 *
 * - **It believes the type it is told.** [Marshal.from_string] returns
 *   whatever type the caller expects; a file written by a program whose
 *   types have changed since is read as garbage, and can crash it. So
 *   the text starts with a line naming the kind of document and its
 *   version -- ["drawing 1"] -- checked before anything is unmarshalled,
 *   which turns that crash into "not a file I can read". (A version is
 *   only as good as whoever changes it when the type changes.)
 * - **It cannot write functions** in a way another program can read
 *   (and js_of_ocaml not at all): a TinyOpenDoc part, being a record of
 *   functions, saves its data, never itself.
 * - **It does not check that the data is all there.** Marshal's header
 *   says how long the data is, so a truncated file is refused here
 *   before it is read.
 *
 *   drawing 1\n<Marshal's bytes>
 *)

(* [to_string ~magic value]: the line, then the value *)
val to_string : magic:string -> 'a -> string

(* [of_string ~magic text]: the value, if [text] starts with [magic]'s
 * line and all of the data follows it; the caller says its type,
 * which the line is the only check of *)
val of_string : magic:string -> string -> 'a option
