(* Big bang: the world programs of How to Design Programs, on top of the
   Playground.

   How to Design Programs (Matthias Felleisen, Robert Bruce Findler,
   Matthew Flatt and Shriram Krishnamurthi, MIT Press, 2001, second
   edition 2018, free at https://htdp.org) teaches programming with
   pictures and games, in Racket, with two libraries: 2htdp/image, images
   that compose -- a circle beside a square, above a text --, and
   2htdp/universe, whose [big-bang] runs a "world program": a world (any
   value), a function drawing it, and functions making the next world
   when time passes, a key is pressed, the mouse moves. Its classic first
   game, a rocket landing:

     Racket:  (big-bang 0
                [to-draw (lambda (y) (place-image ROCKET 50 y SCENE))]
                [on-tick add1]
                [stop-when (lambda (y) (> y 300))])

     OCaml:   big_bang 0.
                ~to_draw:(fun y -> place_image rocket 50. y scene)
                ~on_tick:(fun y -> y +. 1.)
                ~stop_when:(fun y -> y > 300.) ()

   (see examples/BigBangRocket.ml, and examples/BigBangWorm.ml, HtDP's
   worm game). Like Logo.mli, a way of programming from elsewhere, taught
   to children for decades, made a layer of the playground.

   Big-bang is Elm's architecture before Elm (HtDP's first edition is
   from 2001, Elm's from 2012): the world is the model, [to_draw] the
   view, the handlers the update. So [big_bang] is a thin layer over
   Playground.game, and what it adds is small but real:

   - Images that know their size, and compose ([beside], [above],
     [overlay], [place_image]): the playground's shapes don't carry
     their size, and every game places everything by hand. Here an image
     is a shape and its width and height.

   - Events rather than states: the playground says which keys are down
     (polling); big-bang calls [on_key] once when a key is pressed, with
     its name ("left", " ", "a"). The events come from comparing the keys
     down now with the ones down at the last frame ([key_events]): the
     same trick as Scene2d.pressed, and the way most event systems are
     built on top of hardware that only has states.

   - HtDP's coordinates in a scene: from its top-left corner, y going
     down, as on most screens and in most drawing programs -- not the
     playground's, centered, y going up.

   Is the playground a superset of big-bang, then? Almost, for the game
   loop: to_draw and on_tick are view and update (a slower tick rate is a
   counter), key and mouse events come from the states, stop_when is a
   flag in the model. Not quite, for four things big-bang has:
     1. images measured, text included: HtDP can measure (text "Score"
        24 "black"); here a text's width is only estimated ([text]),
        because the playground's words are drawn by each backend's own
        font (the software rasterizer's Hershey font could measure them:
        an exercise);
     2. the final world as a value: in Racket, (big-bang ...) returns
        the last world when the program stops, so runs can follow each
        other and be tested; run_app returns nothing, and can't in a
        browser, where the loop is callbacks;
     3. images compared for equality, which HtDP's tests do
        ((check-expect (render w) ...)): shapes are data here, but
        nothing promises it;
     4. universe, several world programs and a server exchanging
        messages: the playground has no networking yet
        (plan_networking_teaching.md; a Universe.ml would come with it).
   And the playground has what big-bang doesn't: animations as functions
   of time (spin, wave), the screen's size, sound (Audio), 3D.

   Worth it? For a reader of HtDP, yes: its book, exercises and courses
   can be followed nearly line for line in OCaml. For others, it's a
   second way to write the same programs as Playground.game, and a
   beginner shouldn't learn both: pick one. The book's real value is its
   design recipe (data definitions, signatures, examples written before
   the code), a way of teaching, not a library; the library is the
   small part, which is why this module is small, and not all of
   2htdp: no pinholes, no alignment arguments ("top", "left"), colors as
   the playground's rather than strings, modes as a variant rather than
   "solid" and "outline".
*)

open Playground

(*****************************************************************************)
(* {1 Images} *)
(*****************************************************************************)

(* an image: a picture that knows its width and height *)
type image

type mode = Solid | Outline

val circle : number -> mode -> color -> image (* its radius *)
val ellipse : number -> number -> mode -> color -> image (* width, height *)
val rectangle : number -> number -> mode -> color -> image
val square : number -> mode -> color -> image

(* an equilateral triangle, pointing up, of this side *)
val triangle : number -> mode -> color -> image

(* [text str size color]: [size] pixels high; its width estimated, 0.6
 * of the size per character (see above) *)
val text : string -> number -> color -> image

(* a white scene framed in black, of this width and height, to place
 * images on *)
val empty_scene : number -> number -> image

(* [overlay top bottom]: [top] over [bottom], their centers together;
 * as big as the bigger one *)
val overlay : image -> image -> image

(* [beside a b]: [a] left of [b], their centers level; e.g. a 20 x 10
 * image beside a 30 x 40 one: 50 x 40 *)
val beside : image -> image -> image

(* [above a b]: [a] over [b], their centers aligned; a 20 x 10 above a 30
 * x 40: 30 x 50 *)
val above : image -> image -> image

(* [place_image i x y scene]: [i]'s center at (x, y) in [scene], from its
 * top-left corner, y going down; the result the scene's size (what
 * sticks out isn't cut, as HtDP does: the playground can't clip) *)
val place_image : image -> number -> number -> image -> image

val width : image -> number
val height : image -> number

(* the image as a playground shape, centered on (0, 0) *)
val to_shape : image -> shape

(*****************************************************************************)
(* {1 Events} *)
(*****************************************************************************)

(* [key_events before now]: the keys pressed since [before], and the ones
 * released, by their HtDP names: "left", "right", "up", "down", " ",
 * "\r", "escape", and the letters as themselves. E.g. with "ArrowLeft"
 * down before, and "ArrowLeft" and "a" now: (["a"], []). *)
val key_events : keyboard -> keyboard -> string list * string list

(*****************************************************************************)
(* {1 The world} *)
(*****************************************************************************)

(* big_bang's model: the world, and what it keeps to make events *)
type 'w world

(* [big_bang init ~to_draw ...]: the world program starting with [init]:
 * [to_draw] draws it (its image centered on the screen), [on_tick] makes
 * the next world every [tick_rate] seconds (1/60 by default: every frame;
 * HtDP's default is 1/28), [on_key] and [on_release] when a key is
 * pressed and released, [on_mouse] when the mouse moves or its button
 * goes down or up ("button-down", "button-up", "drag", "move"), at (x, y)
 * in the scene's coordinates; [stop_when] ends it, drawing
 * [last_picture] if given. *)
val big_bang :
  'w ->
  to_draw:('w -> image) ->
  ?on_tick:('w -> 'w) ->
  ?tick_rate:number ->
  ?on_key:('w -> string -> 'w) ->
  ?on_release:('w -> string -> 'w) ->
  ?on_mouse:('w -> number -> number -> string -> 'w) ->
  ?stop_when:('w -> bool) ->
  ?last_picture:('w -> image) ->
  unit ->
  ('w world game, msg) app
