(* Where things go, which is the part courses skip (notes_gui.md
 * section 5). One idea, Flutter's (2017), and three rules:
 *
 *        parent
 *          |  "you may be 0..400 wide, 0..200 tall"   constraints DOWN
 *          v
 *        child
 *          |  "then I am 180 x 40"                    sizes UP
 *          v
 *        parent places it at (10, 10)                 the PARENT positions
 *
 * Two passes and nothing else: [measure] walks down with the room
 * available and comes back with sizes, [arrange] walks down again
 * with the rectangle each thing got. Rows, columns, spacing,
 * centering and "as wide as there is room" all fall out of those
 * three rules -- which is the reason to teach this one rather than
 * the alternatives:
 *
 *   - Tk's *geometry managers* (Ousterhout; Tcl 1988, Tk 1991, and
 *     [grid] later still, Tk 4.1 in 1996): a separate object owns
 *     placement, which is why Tk code never computes a coordinate --
 *     but also why two managers in one window fight. Worth being
 *     precise about, since Tk is both older and more influential:
 *     for one row or column, [pack] and the code below are the *same
 *     algorithm* under different names --
 *
 *       Tk's cavity, shrinking as each slave takes its parcel   the walk below
 *       -expand 1, dividing the leftover cavity                 expand, spacer
 *       -fill, across the packing direction                     stretch
 *       -anchor center inside the parcel (the default)          centered across
 *       -padx, -pady                                            pad
 *
 *     What really differs is the *order*, and one thing follows from
 *     it: Tk computes a widget's requested size first, and it cannot
 *     depend on the room allotted -- which is why a wrapping label
 *     there needs -wraplength set by hand, or the old
 *     [bind . <Configure>] trick. Constraints going down first is
 *     what lets a child answer "given 300 wide, I am 80 tall" in the
 *     same pass, and that is the shape of every text layout,
 *     Knuth-Plass included.
 *
 *     What Tk has and this does not is [grid]: weights, spans and
 *     sticky edges, aligning columns *across* rows, which rows of
 *     rows cannot do. A spreadsheet and a dialog both want it, so it
 *     comes back when TinyVisiCalc does;
 *   - NeXT's and Cocoa's *springs and struts* (1988): each widget
 *     says which of its edges and which of its dimensions are
 *     elastic, and resizing squeezes them. Direct, and hopeless once
 *     a window gets small;
 *   - CSS *flexbox* (2009-2018): the same constraint idea as here,
 *     with twenty years of vocabulary on it (flex-grow, flex-shrink,
 *     flex-basis, align-items, justify-content...) and a sizing
 *     algorithm famous for its corner cases;
 *   - absolute coordinates, which every game in this repository uses
 *     and which the widgets of phase 1 used: perfect until something
 *     changes size.
 *
 * And the ancestor of the whole family: TeX's *boxes and glue*
 * (Knuth, 1978). A line of type is boxes (the letters) with glue
 * between them (spaces that can stretch and shrink by stated
 * amounts); a paragraph is those lines stacked. [spacer] below is
 * glue with infinite stretch, and [row] is an hbox.
 *
 * Honest about what it buys here: the playground's screen is 1000 x
 * 1000 whatever the window, and its [Resized] message is a TODO, so
 * today this arranges (a column that spaces itself, a panel that fits
 * its contents, buttons of one width) rather than resizes. The day
 * the window's size arrives, these same three rules are what will
 * make it work, unchanged.
 *
 * Worked example, the panel of examples/GuiWidgets.ml -- three rows
 * of 200 x 40, 10 apart, in a 400 x 400 area centered on the origin:
 *
 *   let panel = center (column ~gap:10. [ leaf A s; leaf B s; leaf C s ])
 *   measure (loose 400. 400.) panel  =  (200., 140.)   (3*40 + 2*10)
 *   arrange area panel               =  A at y = 50, B at y = 0,
 *                                       C at y = -50
 *
 * (a column runs from its top, and y is up here, so the steps are
 * downwards; without the [center] the column would start at the top
 * of the 400 x 400 area instead, at y = 180.)
 *
 * One rule to keep in mind, since it is what makes the two passes fit
 * on a page: **a leaf takes exactly the rectangle its parent gives
 * it**. Its measured size is what it *asks* for; a row, a column or a
 * [center] grants it, while [pad] and [expand] hand over what is
 * left. So a button at its own size inside a big empty area is
 * [pad 20. (center (leaf Save size))], and not [pad 20. (leaf ...)],
 * which fills.
 *)

(* How much room a parent offers: a width between [min_w] and
 * [max_w], a height between [min_h] and [max_h] *)
type constraints = { min_w : float; max_w : float; min_h : float; max_h : float }

(* [loose w h]: anything up to w x h -- what a parent usually offers *)
val loose : float -> float -> constraints

(* [tight w h]: exactly w x h, no choice -- what a stretched child gets *)
val tight : float -> float -> constraints

(* A tree of things to place. The leaves carry whatever you want to
 * find them by afterwards -- a variant, a string, an int -- since
 * [arrange] hands back ['a * box] pairs and a program looks up the
 * widget it is about to ask for. *)
type 'a t

(* [leaf key (w, h)]: something that knows its size, e.g. a button
 * (Immediate.button_size gives that size) *)
val leaf : 'a -> float * float -> 'a t

(* fixed empty space, e.g. before a "Quit" button *)
val space : float -> 'a t

(* empty space that takes whatever is left: TeX's glue, Flutter's
 * Spacer. Two of them around a thing center it; one before it pushes
 * it to the end.
 *
 * With one warning, since it catches everybody once: a spacer takes
 * whatever room it is *offered*, so a row containing one is as wide
 * as the room, and a [center] offers all of it. Centring a column
 * that contains a row with a spacer therefore makes the column as
 * wide as the screen. When what you want is a gap of a known size
 * inside something that should stay its natural width, that is
 * [space]. *)
val spacer : 'a t

(* [expand child]: the child takes the room left over along the axis
 * it is in, shared with the other flexible children (Flutter's
 * Expanded, CSS's flex-grow) *)
val expand : 'a t -> 'a t

(* [stretch child]: the child fills the *other* axis -- all the width
 * of the column it is in, all the height of its row (Flutter's
 * CrossAxisAlignment.stretch). This is how a panel's buttons come out
 * the same width. *)
val stretch : 'a t -> 'a t

(* [pad n child]: n of empty space on all four sides *)
val pad : float -> 'a t -> 'a t

(* [center child]: the child at the size it asked for, in the middle
 * of the room given *)
val center : 'a t -> 'a t

(* [row ~gap children] left to right, [column ~gap children] top to
 * bottom, with [gap] between them (0 by default). Across the axis a
 * child keeps the size it asked for and is centered, unless it is
 * [stretch]ed. *)
val row : ?gap:float -> 'a t list -> 'a t
val column : ?gap:float -> 'a t list -> 'a t

(* Pass one, constraints down and sizes up: how big [t] wants to be in
 * the room [constraints] offers *)
val measure : constraints -> 'a t -> float * float

(* Pass two, the parent positions: the rectangle each leaf ends up
 * with, given the rectangle [t] itself gets, in the order the leaves
 * were written.
 *
 *   let places = Layout.arrange panel (Layout.column ~gap:10. rows) in
 *   let b = List.assoc Reset places in
 *   if Gui.button_in computer b "reset" then ...
 *)
val arrange : Widget.box -> 'a t -> ('a * Widget.box) list
