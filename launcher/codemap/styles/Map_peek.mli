(* Map_peek: a definition's body read over the map, without leaving
   it. At the ground or the street, a click on a name opens a box in
   the middle of the map with the lines of its definition (found
   elsewhere if defined elsewhere, Code_map: t.peek), laid out by
   Code_ground, the letters as big as the box allows (18 pixels at
   most), painted once. A click inside on another name peeks at that
   one, over the first: a stack, each box shifted right and down and a
   little smaller, the ones under it dimmed. The wheel scrolls a long
   one (t.peek_scroll); a click outside or Escape closes it.

   Inside, the peek behaves as the ground does: the name hovered glows,
   and the X-ray's plates tint its lines. *)

(* the peek on the map (the top of the stack): its entry [pe] and file
   [pf]; its lines' layout [pg], from the box's inner corner; the box
   (bx, by, bw, bh) and the inner one where the code is (ix, iy, iw,
   ih), in pixels; the lines asked for, [first] to [last], and those the
   scroll shows, [shown_first] to [shown_last] *)
type peek = {
  pe : Code_map_base.entry;
  pf : Code_file.t;
  pg : Code_ground.t;
  bx : float;
  by : float;
  bw : float;
  bh : float;
  ix : float;
  iy : float;
  iw : float;
  ih : float;
  first : int;
  last : int;
  shown_first : int;
  shown_last : int;
}

(* the peek open, if any, where it is for this camera *)
val peek_geom : Code_map_base.t -> Code_map_base.camera -> peek option

(* a pixel in the peek's box *)
val inside_peek : peek -> float -> float -> bool

(* the stack of peeks drawn, the last on top ([q], Code_map_base.style's
   labels') *)
val peek_shapes : Code_map_base.t -> Code_map_base.camera -> float -> Playground.shape list

(* a name hovered in the peek: its binding and uses glowing in the peek,
   and outside it on the map, where the same file is laid out *)
val peek_glow : Code_map_base.t -> Code_map_base.camera -> Playground.shape list
