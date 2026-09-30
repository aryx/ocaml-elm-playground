(* A drawing's figures (appkits/draw) as Playground shapes, shared by
 * TinyMacDraw and the drawing part of compound documents
 * (Part_drawing): a fill as a grey, an outline as strokes -- thin
 * rectangles, turned -- and text through Stroke_text. The shapes are in
 * the drawing's own coordinates; a part fits them to its rectangle by
 * grouping and scaling them. *)

(* a grey, 0 black to 1 white *)
val grey : float -> Playground.color

(* [segment color width a b]: a stroke from a to b *)
val segment : Playground.color -> float -> Figure.point -> Figure.point -> Playground.shape

val figure : Figure.t -> Playground.shape list
