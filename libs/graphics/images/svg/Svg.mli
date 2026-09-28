(* Svg: a picture as shapes -- the small part of SVG that logos and
   icons use, turned into pixels.

   A PNG is pixels; an SVG (Scalable Vector Graphics, W3C, 2001) is a
   drawing's description, XML: shapes filled and stroked, in a
   coordinate system of its own that its viewBox maps onto any size.
   The web's logos and icons are SVG now -- Hacker News' "Y", its vote
   arrows, Wikipedia's wordmark, GitHub's 242 icons, Google's "G" --
   because one file is sharp at every size.

     <svg viewBox="0 0 24 24" width="48" height="48">
       <path d="M4 12 L12 4 L20 12 Z" fill="#f60"/>
     </svg>

   **Reading** ([parse]): XML as far as SVG files need -- elements and
   their attributes, <x/>, comments, <?xml ...?>, <!DOCTYPE>, CDATA and
   text skipped -- into a tree of [node]s (a browser gives its inline
   <svg>'s from its own DOM, as the same nodes).

   **The shapes**, each made a list of polygons (its contours) in the
   picture's pixels:

     rect (x y width height), circle (cx cy r), ellipse (cx cy rx ry),
     line, polyline, polygon (points), and path (d) with its commands,
     absolute and relative (lower case): M L H V (lines), C S (cubic
     Bézier curves, S's first control point the mirror of the last),
     Q T (quadratic ones), A (elliptical arcs: SVG's endpoint form
     converted to a centre and angles, appendix F.6), Z (closed);

   the curves flattened into lines (Curve.flatten, a tenth of a pixel
   off at most), through the transforms of the elements around them
   (transform: translate, scale, rotate, matrix; composed as Affine)
   and the viewBox's (scaled uniformly and centred, SVG's default
   preserveAspectRatio "xMidYMid meet").

   **Painting**: fill (default black; none; a colour: #rgb, #rrggbb,
   rgb(), a few names, currentColor the caller's colour) with its rule
   (nonzero, evenodd), stroke (default none) of stroke-width, and the
   opacities (opacity, fill-opacity, stroke-opacity) -- attributes or
   style="fill: ...", inherited down the tree as CSS properties are. A
   shape is filled by Fill.polygons_aa (its stroke by Stroke.contours
   first), into a framebuffer that has no alpha: so each shape's
   coverage is drawn white on black, then laid onto the picture in its
   colour at that coverage times its opacity -- Porter and Duff's "over".

   Worked example (the tests'): a 4 by 4 picture, <rect x=1 y=1
   width=2 height=2 fill=red/>: its 4 middle pixels opaque red, the
   rest transparent.

   Not done: gradients and patterns (fill="url(#...)", drawn grey), text,
   <use> and <symbol>, clipping, masks, filters, markers, dashes, CSS
   <style> sheets inside the SVG, stroke joins other than round.

   Reference: W3C, Scalable Vector Graphics (SVG) 1.1, chapters 8
   (paths), 9 (basic shapes), 11 (painting), 7 (coordinate systems),
   appendix F.6 (arcs); T. Porter, T. Duff, "Compositing Digital
   Images", SIGGRAPH '84. *)

type node = { name : string; attributes : (string * string) list; children : node list }

(* the <svg> element of an SVG file's text, if it has one *)
val parse : string -> node option

(* whether bytes look like an SVG file: "<svg" or "<?xml ... <svg" first *)
val sniff : string -> bool

(* its size in pixels: width= and height=, one of them and the viewBox's
 * ratio, or the viewBox's; None if it says none *)
val size : node -> (float * float) option

(* [render ?color node ~width ~height]: the picture, [color] (black)
 * being currentColor *)
val render : ?color:int * int * int -> node -> width:int -> height:int -> Rgba_image.t
