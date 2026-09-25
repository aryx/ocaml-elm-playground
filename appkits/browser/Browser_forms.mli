(* Browser_forms: what a person does to a page's form, and what it
 * makes the browser do.
 *
 *   a click on       a text field, a password, a textarea  -> it takes the keys
 *                    a checkbox                            -> it turns
 *                    a radio button                        -> it, and no other of its name in its form
 *                    a select                              -> its next option (Motif popped a menu)
 *                    a submit button                       -> its form sent, the button named in it
 *                    a reset button                        -> its form as the page gave it
 *   a key in a field  Backspace, Return (its form sent; a new line in a
 *                    textarea), Escape (the keys given up)
 *
 * A submission is a request: the form's fields (Forms.submission)
 * encoded (Urlencoded), sent to its action resolved against the page --
 * a GET with them as the URL's query, a POST with them as the body.
 * The values are the page's (Browser_page.values), kept with it, so
 * that the history gives a half-filled form back half filled. *)

type effect =
  | Nothing
  | Focus of Dom.element (* the field takes the keys *)
  | Unfocus (* it gives them up *)
  | Changed of Browser_page.t (* a value changed: the page with it *)
  | Submit of { url : string; post : (string * string) option; page : Browser_page.t }
      (* go there (a POST's content type and body), the page left as it is *)

(* the request a submission makes: the URL, and a POST's content type and
 * body *)
val submission : Browser_page.t -> Forms.form -> submitter:Dom.element option -> string * (string * string) option

(* a click on a control *)
val click : Browser_page.t -> Dom.element -> effect

(* a key (its name lowercased: "backspace", "enter"...) while a field
 * has the keys *)
val key : Browser_page.t -> Dom.element -> string -> effect

(* text typed while a field has the keys: the page with it added *)
val typed : Browser_page.t -> Dom.element -> string -> Browser_page.t
