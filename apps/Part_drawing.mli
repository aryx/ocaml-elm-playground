(* A drawing, as a part of a compound document (appkits/embed):
 * TinyMacDraw's engine (Figure, Drawing) behind the functions a
 * document asks of a part -- the fourth kind of part, added without
 * the hosts knowing anything of drawings but a line in their registry
 * and their Insert menu.
 *
 * The drawing has a page of its own, 300 by 200, fitted to the part's
 * rectangle (the shapes grouped and scaled, the mouse mapped back).
 * Active, it is TinyMacDraw's arrow: click to select, Shift to add,
 * drag to move, a handle to resize, Backspace to delete; its menu adds
 * shapes, fills them, brings them forward and sends them back.
 *
 * It saves with Marshal (appkits/document/Saved): a drawing is plain
 * data, so it needs no format of its own -- and a text that is not a
 * drawing it can read comes back as a placeholder, kept whole. *)

val kind : string
val make : Drawing.t -> Component.part
val load : string -> Component.part
