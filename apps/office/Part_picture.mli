(* A picture, as a part of a compound document (appkit_embed):
 * TinyMacPaint's engine (Bitmap, Paint, Seed_fill) behind the four
 * functions a document asks of a part. Active, a drag paints with the
 * tool and pattern its Picture menu chose. *)

val kind : string
val make : Bitmap.t -> Component.part
val load : string -> Component.part
