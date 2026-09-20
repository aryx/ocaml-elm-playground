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
   theme. (Rows, columns and "as wide as there is room" are layout,
   which comes later.)

   The one piece of mutable state in these libraries lives here, and
   it is worth saying why: [update] and [view] are two functions, so
   the widgets your update asked for are remembered until your view
   asks for them with {!draw}. Calling [draw] ends the frame; the next
   widget call starts the next one. A program that never calls [draw]
   asks questions and shows nothing.

   What it does not do, yet: text fields (typing needs focus), layout,
   menus, scrolling, and anything retained. Underneath is [gui/]
   (see docs/claude_notes/tutorials/notes_gui.md), where the same
   widgets are values rather than a global, and where the three rival
   architectures will sit beside this one. *)

(* [button computer ~at label]: a button centered at [at], true the
   frame it is clicked -- pressed and released inside it *)
val button : Playground.computer -> at:Playground.number * Playground.number -> string -> bool

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

(* [label computer ~at s]: [s], in the theme's color and size *)
val label : Playground.computer -> at:Playground.number * Playground.number -> string -> unit

(* the shapes of the widgets this frame asked for, back to front; for
   [view], and it ends the frame *)
val draw : unit -> Playground.shape list

(* the colors and sizes the widgets use, to read (e.g. [row], to space
   widgets by hand) or to replace *)
val theme : unit -> Theme.t
val set_theme : Theme.t -> unit
