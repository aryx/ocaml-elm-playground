(* Gui: buttons, sliders and checkboxes, in the playground's own idiom.

   A settings screen, whole:

   {[
     let update computer model =
       if Gui.button computer ~at:(0., 120.) "Reset" then initial
       else
         { model with
           gravity = Gui.slider computer ~at:(0., 60.) ~from:0. ~to_:2000. model.gravity;
           sound = Gui.checkbox computer ~at:(0., 0.) "sound" model.sound }

     let view computer model =
       Gui.draw () @ [ ... the rest of the picture ... ]
   ]}

   The widgets are asked for in [update] -- [button] answers "was I
   clicked this frame", [slider] answers "what is my value now" -- and
   [draw] gives [view] the shapes they drew. There is no button
   object, no callback, no message type: a widget lives as long as the
   [if] around it. That is {i immediate mode} (Casey Muratori, 2005;
   Dear ImGui, 2014), and it is the paradigm that fits here because
   [Playground.game]'s update is [computer -> 'memory -> 'memory],
   with nowhere for a callback to go.

   Widgets are placed by their center, like every shape in the
   playground, and they size themselves from their label and the
   theme. Rows, columns and "as wide as there is room" are layout, and
   live one level down, in [gui/Layout] (see below).

   The one piece of mutable state in these libraries lives here, and
   it is worth saying why: [update] and [view] are two functions, so
   the widgets your update asked for are remembered until your view
   asks for them with {!draw}. Calling [draw] ends the frame; the next
   widget call starts the next one. A program that never calls [draw]
   asks questions and shows nothing.

   What it does not do, yet: selection inside a field, scrolling, and
   anything retained. Underneath is [gui/] (see
   docs/claude_notes/tutorials/notes_gui.md), where the same widgets
   are values rather than a global, and where the three rival
   architectures will sit beside this one. *)

(* [button computer ~at label]: a button centered at [at], true the
   frame it is clicked -- pressed and released inside it *)
val button :
  ?enabled:bool -> Playground.computer -> at:Playground.number * Playground.number -> string -> bool

(* [checkbox computer ~at label checked]: a tick box with [label]
   beside it, and the value it has after this frame *)
val checkbox :
  Playground.computer ->
  at:Playground.number * Playground.number ->
  string ->
  bool ->
  bool

(* [slider computer ~at ~from ~to_ v]: a slider showing [v], and the
   value it has after this frame -- [v] unless it is being dragged *)
val slider :
  Playground.computer ->
  at:Playground.number * Playground.number ->
  from:Playground.number ->
  to_:Playground.number ->
  Playground.number ->
  Playground.number

(* [field computer ~at text]: a box to type in, and the text it holds
   after this frame. Click it to give it the keys (or Tab to it), and
   the characters go there:

   {[
     let update computer model = { model with name = Gui.field computer ~at:(0., 0.) model.name }
   ]}

   The text stays in the model, like a slider's value; what the
   toolkit keeps is only the caret. *)
val field :
  ?enabled:bool -> Playground.computer -> at:Playground.number * Playground.number -> string -> string

(* [text_area computer ~at edit]: several lines to type into, holding
   a {!Text_edit.t} -- a piece table, which carries its own caret, its
   own selection and every version of itself, so that undo (Control-Z)
   belongs to the text rather than to the toolkit *)
val text_area :
  Playground.computer ->
  at:Playground.number * Playground.number ->
  Text_edit.t ->
  Text_edit.t

(* [progress computer ~at fraction]: a bar filled [fraction] of the
   way (0 to 1); it shows and answers nothing *)
val progress : Playground.computer -> at:Playground.number * Playground.number -> Playground.number -> unit

(* [menu computer ~at items chosen]: a dropdown, and which item is
   chosen after this frame. While its items show, they take the mouse
   from every other widget. *)
val menu :
  Playground.computer -> at:Playground.number * Playground.number -> string list -> int -> int

(* [label computer ~at s]: [s], in the theme's color and size *)
val label : Playground.computer -> at:Playground.number * Playground.number -> string -> unit

(* {1 The same widgets, where a layout put them}

   [~at] places a widget at a point, at the size it asks for, which
   is all a settings screen needs. A panel wants them arranged
   instead -- a column, evenly spaced, all of one width -- and that is
   [gui/Layout] (constraints down, sizes up): it takes the sizes the
   widgets ask for and gives back a rectangle each.

   {[
     type panel = Reset | Radius

     let rows =
       Layout.(center (column ~gap:10.
          [ leaf Reset (Gui.button_size "reset");
            stretch (leaf Radius (Gui.slider_size ())) ]))

     let update computer model =
       let places = Layout.arrange (Gui.area computer) rows in
       if Gui.button_in computer (List.assoc Reset places) "reset" then initial
       else { model with radius =
                Gui.slider_in computer (List.assoc Radius places)
                  ~from:10. ~to_:150. model.radius }
   ]} *)

(* [~enabled:false] greys a button or a field: the button answers
   false whatever the mouse does, and the field takes neither the keys
   nor a place in the tab order *)
val button_in : ?enabled:bool -> Playground.computer -> Widget.box -> string -> bool
val checkbox_in : Playground.computer -> Widget.box -> string -> bool -> bool

val slider_in :
  Playground.computer ->
  Widget.box ->
  from:Playground.number ->
  to_:Playground.number ->
  Playground.number ->
  Playground.number

val label_in : Playground.computer -> Widget.box -> string -> unit
val field_in : ?enabled:bool -> Playground.computer -> Widget.box -> string -> string
val text_area_in : Playground.computer -> Widget.box -> Text_edit.t -> Text_edit.t
val progress_in : Playground.computer -> Widget.box -> Playground.number -> unit
val menu_in : Playground.computer -> Widget.box -> string list -> int -> int

(* what each widget asks for, in the current theme, to build the
   layout's leaves with *)
val button_size : string -> Playground.number * Playground.number
val checkbox_size : string -> Playground.number * Playground.number
val slider_size : unit -> Playground.number * Playground.number
val label_size : string -> Playground.number * Playground.number
val field_size : unit -> Playground.number * Playground.number
val text_area_size : unit -> Playground.number * Playground.number
val progress_size : unit -> Playground.number * Playground.number
val menu_size : string list -> Playground.number * Playground.number

(* the whole screen as a box, the usual thing to lay out inside *)
val area : Playground.computer -> Widget.box

(* Has the toolkit got the mouse -- is a menu's list showing?
   The widgets know this for themselves; a program that reads
   [computer.mouse] to work out what its own drawing was clicked on
   (a spreadsheet's cells, a canvas) has to ask, or it will act on the
   click that was meant for a menu item. *)
val modal : unit -> bool

(* the shapes of the widgets this frame asked for, back to front; for
   [view], and it ends the frame *)
val draw : unit -> Playground.shape list

(* {1 For the other three architectures}

   [gui/Retained], [gui/Mvc] and [gui/Mvu] do their own wiring and
   answer in [Widget.paint] rather than shapes, so a program that
   compares them (examples/GuiFourWays.ml) needs the two ends of the
   adapter on their own: what the person is doing, and how to draw the
   result. Nothing else in the playground needs these. *)

val input : Playground.computer -> Widget.input
val shapes : Widget.paint list -> Playground.shape list

(* the colors and sizes the widgets use, to read (e.g. [row], to space
   widgets by hand) or to replace *)
val theme : unit -> Theme.t
val set_theme : Theme.t -> unit
