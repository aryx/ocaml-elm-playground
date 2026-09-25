(* Computed: each element's style as values -- what layout reads.

   (notes_css_engine.md section 6.) The cascade gives each property's
   winning declaration as text; this module makes a value of each, in a
   record: lengths in pixels (or pixels plus a percentage the layout
   resolves, Css_values), colours as numbers, keywords as variants.

   What no declaration gives is **inherited** from the parent (the
   colour, the font, the alignment, white-space, line-height,
   visibility, list-style, and the custom properties) or takes its
   **initial value** (display inline, margins 0, background
   transparent...), as each property's definition says; the keywords
   inherit, initial and unset ask for either. font-size is computed
   first: an em everywhere else is this element's size, in font-size
   its parent's.

   **Shorthands** are expanded before: margin and padding (one to four
   values: top, right, bottom, left), border and border-top and its
   sides (a width, a style, a colour in any order), border-width,
   border-style, border-color, background (its colour, the rest
   ignored), font (style, weight, size, "/" line-height, family), flex
   (grow, shrink, basis), gap, inset, list-style (its type).

   The page's defaults are a style sheet like its own, the user agent's
   (ua.css, embedded as Ua_sheet): CSS 2.1's appendix D, the table of
   Looks.mli written in the language the pages use.

   Not computed: the properties layout does not use yet (transforms,
   shadows, animations, grid's), and ::before and ::after's content. *)

type display =
  | Inline
  | Block
  | Inline_block
  | List_item
  | Flex
  | Inline_flex
  | Grid (* laid out as a block: plan_tiny_chrome.md *)
  | Table
  | Table_row_group
  | Table_row
  | Table_cell
  | Table_caption
  | Display_none
  | Contents (* no box of its own: its children's in its place *)

type position = Static | Relative | Absolute | Fixed | Sticky

(* a length that may be auto (widths, margins, offsets), or none (max-width) *)
type size = Auto | Len of Css_values.length

type family = Serif | Sans_serif | Monospace
type white_space = Normal | Pre | Nowrap | Pre_wrap | Pre_line
type text_align = Align_left | Align_right | Align_center | Align_justify
type vertical_align = Baseline | Middle | Top | Bottom | Text_top | Text_bottom | Sub | Super
type line_height = Line_normal | Factor of float | Line_px of float
type side = Side_none | Side_left | Side_right | Side_both
type flex_direction = Row | Row_reverse | Column | Column_reverse
type align = Start | End | Center | Stretch | Space_between | Space_around | Space_evenly | Align_baseline

type t = {
  display : display;
  position : position;
  float : side; (* Side_left, Side_right or Side_none *)
  clear : side;
  top : size;
  right : size;
  bottom : size;
  left : size;
  width : size;
  height : size;
  min_width : Css_values.length;
  min_height : Css_values.length;
  max_width : size; (* Auto: none *)
  max_height : size;
  margin : size * size * size * size; (* top, right, bottom, left *)
  padding : Css_values.length * Css_values.length * Css_values.length * Css_values.length;
  border_width : float * float * float * float; (* 0 where the style is none or hidden *)
  border_color : Css_values.color * Css_values.color * Css_values.color * Css_values.color;
  border_box : bool; (* box-sizing: border-box *)
  color : Css_values.color;
  background : Css_values.color;
  font_size : float;
  bold : bool; (* font-weight 600 and more *)
  italic : bool;
  family : family;
  line_height : line_height;
  text_align : text_align;
  underline : bool;
  line_through : bool;
  uppercase : bool; (* text-transform: uppercase *)
  white_space : white_space;
  vertical_align : vertical_align;
  list_style : string; (* disc, circle, square, decimal, none... *)
  visible : bool; (* visibility: visible, and opacity not 0 *)
  overflow_hidden : bool; (* overflow other than visible: hidden, clip, auto, scroll (clipped, no scrollbar) *)
  flex_direction : flex_direction;
  flex_wrap : bool;
  justify_content : align;
  align_items : align;
  align_self : align option; (* None: the container's align-items *)
  flex_grow : float;
  flex_shrink : float;
  flex_basis : size;
  row_gap : Css_values.length;
  column_gap : Css_values.length;
  custom : (string * Css_syntax.component list) list; (* the custom properties, inherited *)
}

(* the root's parent: what the root inherits (the initial values, a
 * font of 16 pixels) *)
val initial : t

(* [compute media ~root_font_size ~parent declared]: an element's
 * style, [declared] its cascaded declarations (Cascade) *)
val compute : Cascade.media -> root_font_size:float -> parent:t -> (string * Css_syntax.component list) list -> t

(* the browser's own style sheet, parsed *)
val user_agent_sheet : Cascade.sheet

(* [styles media sheets root]: every element's computed style, the
 * browser's sheet first, then [sheets] (the page's); with [quirks]
 * (false), quirks mode's rules for a page without a DOCTYPE: a table's
 * fonts and alignment not inherited *)
val styles : ?visited:(string -> bool) -> ?quirks:bool -> Cascade.media -> Cascade.sheet list -> Dom.element -> Dom.element -> t
