(* Menu_view: a frame of the menu drawn, at Menu_layout's places: the
 * header and its tabs, the section's title and the filter bar, the grid
 * of thumbnails (the host's: PNGs in the binary natively, URLs on the
 * web), and on the right the chosen one: its screenshot, what the
 * catalogue says of it, its code as a small code map.
 *
 * The look: a dark cabinet's colours, the chosen thumbnail's frame
 * pulsing, scanlines over everything (thin translucent rectangles, a
 * CRT's gaps between its lines).
 *
 * The previews: a second on a program, and its picture comes alive,
 * the program itself playing in the screenshot's place (the host's
 * [preview]).
 *
 * With the code map open (the model's [code]), the frame is the map's,
 * under a line of what the program brought. *)

open Menu_model

val view : host -> Playground.computer -> model -> Playground.shape list
