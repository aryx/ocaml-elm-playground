(* What was last cut or copied.
 *
 * Cut, copy and paste were invented at Xerox PARC by Larry Tesler and
 * Tim Mott for the Gypsy editor (1974-75), out of the typesetter's
 * literal scissors and paste pot, and they are the one piece of
 * application vocabulary that everybody knows. The idea is so small
 * that the interesting part is where the text *lives*:
 *
 *   in the program   a value, like this one: one application, shared
 *                    between its documents, and gone when it exits
 *   in the system    every application at once -- which needs the
 *                    platform: SDL has SDL_SetClipboardText natively,
 *                    and a browser has an asynchronous API that asks
 *                    the person's permission. That is a backend's
 *                    business, and this module deliberately does not
 *                    reach for it
 *
 * Worth knowing about the real thing, since it explains a lot of
 * surprises: a system clipboard usually holds the *same* content in
 * several formats at once (text, HTML, an image), and what you get
 * depends on what the receiving program asks for -- which is why
 * pasting into one program keeps the bold and into another does not.
 * Here there is one format, and it is a string. *)

type t

val empty : t
val put : string -> t -> t

(* nothing has been cut or copied yet: paste should do nothing, not
 * paste an empty string *)
val get : t -> string option
val has : t -> bool
