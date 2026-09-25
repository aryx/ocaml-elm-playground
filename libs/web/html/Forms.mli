(* Forms: the fill-out form, what a page can ask -- its controls, what
   they hold, and what a submission sends.

   Mosaic 2.0 (1993) added fill-out forms to HTML, and they made the web
   a place to do things, not only to read: a search, an order, a
   guest book. A <form> says where its answer goes (action=, a URL)
   and how (method=, GET or POST); inside it, the controls:

     <input>               a text field (type=text, the default), a
                           password (its letters shown as stars), a
                           checkbox, a radio button (one of those of the
                           same name), a submit or reset button, a
                           hidden value
     <select>              one choice among its <option>s
     <textarea>            several lines of text

   Each control holds a [value] -- the text typed, whether it is
   checked, which option is selected -- starting as the page says
   (value=, checked, selected, the textarea's text). The browser keeps
   the values; the page is not changed (a DOM changed by typing is
   JavaScript's world).

   **What a submission sends**, HTML's "successful controls", in the
   page's order: each control of the form with a name, and --
     text, password, hidden, textarea   (name, its text)
     checkbox, radio                    (name, its value=, "on" if none), if checked
     select                             (name, the selected option's value=, or its text)
     submit                             (name, value=), only the button clicked
     reset                              never
   then encoded (networking's Urlencoded, the browser's business) and
   sent: GET, the action's URL with them as its query; POST, as the
   request's body.

     <form action=/search><input name=q value="café au lait">
     <select name=lang><option>en<option selected>fr</select>
     <input type=submit name=go value=Search></form>
       submitted by the button: q=caf%C3%A9+au+lait&lang=fr&go=Search

   Not done: <input type=image> (a click's x and y sent), file uploads
   (multipart/form-data), <select multiple>, disabled controls, and
   the controls of HTML5 (dates, colours, numbers, validation).

   Reference: RFC 1866 (HTML 2.0), section 8, "Forms"; HTML 4.01,
   section 17.13.2, "Successful controls". *)

type kind =
  | Text
  | Password
  | Checkbox
  | Radio
  | Submit
  | Reset
  | Hidden
  | Select of (string * string) list (* each option's label and value *)
  | Textarea

(* what a control holds *)
type value = { text : string; checked : bool; selected : int }

type control = {
  element : Dom.element; (* the page's: the key of its value (==) *)
  kind : kind;
  name : string option;
  initial : value;
}

type form = {
  action : string; (* as the page wrote it: resolved by the browser *)
  post : bool;
  controls : control list; (* in the page's order *)
}

(* the element as a control, if it is one (input, select, textarea) *)
val control : Dom.element -> control option

(* a button's label: its value=, else Mosaic's words, "Submit Query"
 * and "Reset" *)
val label : control -> string

(* the page's forms; controls outside any form are no form's *)
val forms : Dom.element -> form list

(* the form a control is in, if any (==) *)
val form_of : form list -> Dom.element -> form option

(* the fields a submission sends, in order: [value] gives each control's
 * value now, [submitter] the button clicked (none: Return in a field);
 * encoded by the browser (networking's Urlencoded) *)
val submission : form -> value:(Dom.element -> value) -> submitter:Dom.element option -> (string * string) list
