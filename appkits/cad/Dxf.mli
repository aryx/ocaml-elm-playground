(* DXF, the Drawing Interchange Format (Autodesk, AutoCAD Release 1,
 * 1982): a drawing as text, read and written, so that other programs
 * -- LibreCAD, Inkscape, a laser cutter's -- open ours, and ours
 * opens theirs.
 *
 * The whole format is one idea: a list of *pairs*, each two lines, a
 * group code (an integer saying what the value is) and the value.
 *
 *     0          code 0: a new thing starts, and its type
 *     LINE
 *     8          code 8: its layer
 *     0
 *     10         codes 10, 20, 30: its first point's x, y, z
 *     0.0
 *     20
 *     0.0
 *     11         codes 11, 21: its second point
 *     100.0
 *     ...
 *
 * Things are grouped in SECTIONs (HEADER, TABLES, BLOCKS, ENTITIES),
 * each ended by ENDSEC, the file by EOF. A reader keeps the pairs it
 * knows and skips the rest, which is why a file written by AutoCAD 2024
 * still opens in a reader of 1982's codes: that tolerance made DXF the
 * lingua franca of CAD.
 *
 * Written: the version of AutoCAD Release 12 (AC1009), the layers with
 * their colours, the blocks, and LINE, CIRCLE, ARC, INSERT and
 * DIMENSION (its type and points only: a reader draws it again).
 * Read: the same, and LWPOLYLINE (Release 14's polylines, as lines;
 * their bulges, arcs, are left out). *)

val to_string : Cad_drawing.t -> string

(* the drawing, or why not *)
val of_string : string -> (Cad_drawing.t, string) result

(* the file as its pairs *)
val pairs : string -> ((int * string) list, string) result
