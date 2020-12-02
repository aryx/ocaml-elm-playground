(** {1 Entry points } *)

(** The main entry points of this library are:
- {!val:picture}
- {!val:animation}
- {!val:game}

The important types are:
- {!type:shape}
- {!type:computer}

*)

(** {1 Basic types} *)

(** {2 Numbers} *)

(** A number like `1` or `3.14` or `-120`.
*)
(* It is more flexible to use float than int for graphical operations.
 * Consider using 'open Basics' to have the +/-/... operators working on 
 * floats.
 *)
type number = float

(** {2 Time} *)

(** The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).
*)
type time = Time of number

(** Create an angle that cycles from 0 to 360 degrees over time.

Here is an [`animation`](#animation) with a spinning triangle:

{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ triangle orange 50
          |> rotate (spin 8 time)
      ]
]}
It will do a full rotation once every eight seconds. Try changing the `8` to
a `2` to make it do a full rotation every two seconds. It moves a lot faster!
*)
val spin : number -> time -> number

(** Smoothly wave between two numbers.

Here is an [`animation`](#animation) with a circle that resizes:
{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ circle lightBlue (wave 50 90 7 time)
      ]
]}
The radius of the circle will cycles between 50 and 90 every seven seconds.
It kind of looks like it is breathing.
*)
val wave : number -> number -> number -> time -> number

(** Zig zag between two numbers.

Here is an [`animation`](#animation) with a rectangle that tips back and forth:
{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ rectangle lightGreen 20 100
          |> rotate (zigzag -20 20 4 time)
      ]
]}
It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds.
*)
val zigzag : number -> number -> number -> time -> number

(** {2 Colors} *)

(** Represents a color.

The colors below, like `red` and `green`, come from the [Tango palette][tango].
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like `lightYellow`, `yellow`,
and `darkYellow`.

[tango]: https://en.wikipedia.org/wiki/Tango_Desktop_Project
*)
type color = Color.t (* Hex of string | Rgb of int * int * int *)

(** RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:
{[
    brightBlue = rgb 18 147 216
    brightGreen = rgb 119 244 8
    brightPurple = rgb 94 28 221
]}
Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton][] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/
*)
val rgb : int -> int -> int -> color

val white : color
val black : color
val red : color
val green : color
val blue : color
val yellow : color
val brown : color

val lightYellow : color
val lightPurple : color
val gray : color
val darkGray : color

(** {1:shapes Shapes } *)

(** Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!
*)
type shape = {
  x : number;
  y : number;
  angle : number;
  scale : number;
  alpha : number;
  form : form;
}
and form =
    Circle of color * number
  | Oval of color * number * number
  | Rectangle of color * number * number
  | Ngon of color * int * number
  | Polygon of color * (number * number) list
  | Image of number * number * string
  | Words of color * string
  | Group of shape list

(** {2 Basic Shapes } *)

(** Make circles:
{[
    dot = circle red 10
    sun = circle yellow 300
]}
You give a color and then the radius. So the higher the number, the larger
the circle.
*)
val circle : color -> number -> shape

(** Make ovals:
{[
    football = oval brown 200 100
]}
You give the color, and then the width and height. So our `football` example
is 200 pixels wide and 100 pixels tall.
*)
val oval : color -> number -> number -> shape

(** Make rectangles. This example makes a red cross:
{[
    import Playground exposing (..)

    main =
      picture
        [ rectangle red 20 60
        , rectangle red 60 20
        ]
]}
You give the color, width, and then height. So the first shape is vertical
part of the cross, the thinner and taller part.
*)
val rectangle : color -> number -> number -> shape

(** Make squares. Here are two squares combined to look like an empty box:
{[
    import Playground exposing (..)

    main =
      picture
        [ square purple 80
        , square white 60
        ]
]}
The number you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.
*)
val square : color -> number -> shape

(** Make triangles. So if you wanted to draw the Egyptian pyramids, you could
do a simple version like this:
{[
    import Playground exposing (..)

    main =
      picture
        [ triangle darkYellow 200
        ]
]}
The number is the "radius", so the distance from the center to each point of
the pyramid is `200`. Pretty big!
*)
val triangle : color -> number -> shape

(** Make pentagons:
{[
    import Playground exposing (..)

    main =
      picture
        [ pentagon darkGrey 100
        ]
]}
You give the color and then the radius. So the distance from the center to each
of the five points is 100 pixels.
*)
val pentagon : color -> number -> shape

(** Make hexagons:
{[
    import Playground exposing (..)

    main =
      picture
        [ hexagon lightYellow 50
        ]
]}
The number is the radius, the distance from the center to each point.

If you made more hexagons, you could [`move`](#move) them around to make a
honeycomb pattern!
*)
val hexagon : color -> number -> shape

(** Make octogons:
{[
    import Playground exposing (..)

    main =
      picture
        [ octagon red 100
        ]
]}
You give the color and radius, so each point of this stop sign is 100 pixels
from the center.
*)
val octagon : color -> number -> shape

(** Make any shape you want! Here is a very thin triangle:
{[
    import Playground exposing (..)

    main =
      picture
        [ polygon black [ (-10,-20), (0,100), (10,-20) ]
        ]
]}
**Note:** If you [`rotate`](#rotate) a polygon, it will always rotate around
`(0,0)`. So it is best to build your shapes around that point, and then use
[`move`](#move) or [`group`](#group) so that rotation makes more sense.
*)
val polygon : color -> (number * number) list -> shape

(** {2 Images } *)

(** Add some image from the internet:
{[
    import Playground exposing (..)

    main =
      picture
        [ image 96 96 "https://elm-lang.org/images/turtle.gif"
        ]
]}
You provide the width, height, and then the URL of the image you want to show.
*)
val image : number -> number -> string -> shape

(** {2 Words } *)

(** Show some words!
{[
    import Playground exposing (..)

    main =
      picture
        [ words black "Hello! How are you?"
        ]
]}
You can use [`scale`](#scale) to make the words bigger or smaller.
*)
val words : color -> string -> shape

(** {2 Groups } *)

(** Put shapes together so you can [`move`](#move) and [`rotate`](#rotate)
them as a group. Maybe you want to put a bunch of stars in the sky:
{[
    import Playground exposing (..)

    main =
      picture
        [ star
            |> move 100 100
            |> rotate 5
        , star
            |> move -120 40
            |> rotate 20
        , star
            |> move 80 -150
            |> rotate 32
        , star
            |> move -90 -30
            |> rotate -16
        ]

    star =
      group
        [ triangle yellow 20
        , triangle yellow 20
            |> rotate 180
        ]
]}
*)
val group : shape list -> shape

(** {2 Move Shapes } *)

(** Move a shape by some number of pixels:
{[
    import Playground exposing (..)

    main =
      picture
        [ square red 100
            |> move -60 60
        , square yellow 100
            |> move 60 60
        , square green 100
            |> move 60 -60
        , square blue 100
            |> move -60 -60
        ]
]}
*)
val move : number -> number -> shape -> shape

(** Move shapes to the left.
{[
    import Playground exposing (..)

    main =
      picture
        [ circle yellow 10
            |> moveLeft 80
            |> moveUp 30
        ]
]}
*)
val move_left : number -> shape -> shape

(** Move shapes to the right.
{[
    import Playground exposing (..)

    main =
      picture
        [ square purple 20
            |> moveRight 80
            |> moveDown 100
        ]
]}
*)
val move_right : number -> shape -> shape

(** Move a shape up by some number of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:
{[
    import Playground exposing (..)

    main =
      picture
        [ rectangle brown 40 200
        , circle green 100
            |> moveUp 180
        ]
]}
*)
val move_up : number -> shape -> shape

(** Move a shape down by some number of pixels. So if you wanted to put the sky
above the ground, you could move the sky up and the ground down:
{[
    import Playground exposing (..)

    main =
      picture
        [ rectangle lightBlue 200 100
            |> moveUp 50
        , rectangle lightGreen 200 100
            |> moveDown 50
        ]
]}
*)
val move_down : number -> shape -> shape

(** Move the `x` coordinate of a shape by some amount. Here is a square that
moves back and forth:
{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ square purple 20
          |> moveX (wave 4 -200 200 time)
      ]
]}
Using `moveX` feels a bit nicer here because the movement may be positive or negative.
*)
val move_x : number -> shape -> shape

(** Move the `y` coordinate of a shape by some amount. Maybe you want to make
grass along the bottom of the screen:
{[
    import Playground exposing (..)

    main =
      game view update 0

    update computer memory =
      memory

    view computer count =
      [ rectangle green computer.screen.width 100
          |> moveY computer.screen.bottom
      ]
]}
Using `moveY` feels a bit nicer when setting things relative to the bottom or
top of the screen, since the values are negative sometimes.
*)
val move_y : number -> shape -> shape

(** {2:transformations Customize Shapes } *)

(** Make a shape bigger or smaller. So if you wanted some [`words`](#words) to
be larger, you could say:
{[
    import Playground exposing (..)

    main =
      picture
        [ words black "Hello, nice to see you!"
            |> scale 3
        ]
]}
*)
val scale : number -> shape -> shape

(** Rotate shapes in degrees.
{[
    import Playground exposing (..)

    main =
      picture
        [ words black "These words are tilted!"
            |> rotate 10
        ]
]}
The degrees go **counter-clockwise** to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).
*)
val rotate : number -> shape -> shape

(** Fade a shape. This lets you make shapes see-through or even completely
invisible. Here is a shape that fades in and out:
{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ square orange 30
      , square blue 200
          |> fade (zigzag 0 1 3 time)
      ]
]}
The number has to be between `0` and `1`, where `0` is totally transparent
and `1` is completely solid.
*)
val fade : number -> shape -> shape

(** {1 Computer } *)

(* todo: group *)

val default_width : float
val default_height : float

(** Get the dimensions of the screen. If the screen is 800 by 600, you will see
a value like this:
{[
    { width = 800
    , height = 600
    , top = 300
    , left = -400
    , right = 400
    , bottom = -300
    }
]}
This can be nice when used with [`moveY`](#moveY) if you want to put something
on the bottom of the screen, no matter the dimensions.
*)
type screen = {
  width : number;
  height : number;
  top : number;
  left : number;
  right : number;
  bottom : number;
}
val to_screen : number -> number -> screen

(** Figure out what is going on with the mouse.

You could draw a circle around the mouse with a program like this:
{[
    import Playground exposing (..)

    main =
      game view update 0

    view computer memory =
      [ circle yellow 40
          |> moveX computer.mouse.x
          |> moveY computer.mouse.y
      ]

    update computer memory =
      memory
]}
You could also use `computer.mouse.down` to change the color of the circle
while the mouse button is down.
*)
type mouse = { mx : number; my : number; mdown : bool; mclick : bool; }

(** Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:
{[
    { up = True, down = False, left = False, right = True
    , space = False, enter = False, shift = False, backspace = False
    , keys = Set.fromList ["ArrowUp","ArrowRight"]
    }
]}

So if you want to move a character based on arrows, you could write an update
like this:
{[
    update computer y =
      if computer.keyboard.up then
        y + 1
      else
        y
]}
Check out [`toX`](#toX) and [`toY`](#toY) which make this even easier!

**Note:** The `keys` set will be filled with the name of all keys which are
down right now. So you will see things like `"a"`, `"b"`, `"c"`, `"1"`, `"2"`,
`"Space"`, and `"Control"` in there. Check out [this list][list] to see the
names used for all the different special keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
[member]: /packages/elm/core/latest/Set#member
*)
type keyboard = {
  kup : bool;
  kdown : bool;
  kleft : bool;
  kright : bool;
  kw : bool;
  ks : bool;
  ka : bool;
  kd : bool;
  kspace : bool;
  kenter : bool;
  kshift : bool;
  kbackspace : bool;
  keys : string Set_.t;
}

(** Turn the LEFT and RIGHT arrows into a number.
{[
    toX { left = False, right = False, ... } == 0
    toX { left = True , right = False, ... } == -1
    toX { left = False, right = True , ... } == 1
    toX { left = True , right = True , ... } == 0
]}
So to make a square move left and right based on the arrow keys, we could say:
{[
    import Playground exposing (..)

    main =
      game view update 0

    view computer x =
      [ square green 40
          |> moveX x
      ]

    update computer x =
      x + toX computer.keyboard
]}
*)
val to_x : keyboard -> number

(** Turn the UP and DOWN arrows into a number.
{[
    toY { up = False, down = False, ... } == 0
    toY { up = True , down = False, ... } == 1
    toY { up = False, down = True , ... } == -1
    toY { up = True , down = True , ... } == 0
]}

This can be used to move characters around in games just like [`toX`](#toX):
{[
    import Playground exposing (..)

    main =
      game view update (0,0)

    view computer (x,y) =
      [ square blue 40
          |> move x y
      ]

    update computer (x,y) =
      ( x + toX computer.keyboard
      , y + toY computer.keyboard
      )
]}
*)
val to_y : keyboard -> number

val to_x2 : keyboard -> number

val to_y2 : keyboard -> number

(** If you just use `toX` and `toY`, you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So `toXY` turns the arrow keys into an `(x,y)` pair such that the distance is
normalized:
{[
    toXY { up = True , down = False, left = False, right = False, ... } == (1, 0)
    toXY { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    toXY { up = False, down = False, left = False, right = True , ... } == (0, 1)
]}

Now when you go up/right, you are still going 1 pixel per update.
{[
    import Playground exposing (..)

    main =
      game view update (0,0)

    view computer (x,y) =
      [ square green 40
          |> move x y
      ]

    update computer (x,y) =
      let
        (dx,dy) = toXY computer.keyboard
      in
      (x + dx, y + dy)
]}
*)
val to_xy : keyboard -> number * number

(** When writing a [`game`](#game), you can look up all sorts of information
about your computer:

  - [`Mouse`](#Mouse) - Where is the mouse right now?
  - [`Keyboard`](#Keyboard) - Are the arrow keys down?
  - [`Screen`](#Screen) - How wide is the screen?
  - [`Time`](#Time) - What time is it right now?

So you can use expressions like `computer.mouse.x` and `computer.keyboard.enter`
in games where you want some mouse or keyboard interaction.
*)
type computer = {
  mouse : mouse;
  keyboard : keyboard;
  screen : screen;
  time : time;
}

val initial_computer : computer


(** {1 The Application} *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> shape list;
  subscriptions : 'model -> 'msg Sub.t;
}

(** {1 Playgrounds} *)

(** {2 Pictures} *)

type msg1 = Resized1 of int * int

(** Make a picture! Here is a picture of a triangle with an eyeball:
{[
    import Playground exposing (..)

    main =
      picture
        [ triangle green 150
        , circle white 40
        , circle black 10
        ]
]}
*)
val picture : shape list -> (screen, msg1) app

(** {2 Animations} *)

type msg =
    Tick of number
  | Resized of int * int
  | KeyChanged of bool * string
  | MouseMove of (number * number)
  | MouseClick
  | MouseButton of bool

type animation

(** Create an animation!

Once you get comfortable using [`picture`](#picture) to layout shapes, you can
try out an `animation`. Here is square that zigzags back and forth:
{[
    import Playground exposing (..)

    main =
      animation view

    view time =
      [ square blue 40
          |> moveX (zigzag -100 100 2 time)
      ]
]}
We need to define a `view` to make our animation work.

Within `view` we can use functions like [`spin`](#spin), [`wave`](#wave),
and [`zigzag`](#zigzag) to move and rotate our shapes.
*)
val animation : (time -> shape list) -> (animation, msg) app

(** {2 Games} *)

type 'memory game

(** Create a game!

Once you get comfortable with [`animation`](#animation), you can try making a
game with the keyboard and mouse. Here is an example of a green square that
just moves to the right:
{[
    import Playground exposing (..)

    main =
      game view update 0

    view computer offset =
      [ square green 40
          |> moveRight offset
      ]

    update computer offset =
      offset + 0.03
]}
This shows the three important parts of a game:

1. `memory` - makes it possible to store information. So with our green square,
we save the `offset` in memory. It starts out at `0`.
2. `view` - lets us say which shapes to put on screen. So here we move our
square right by the `offset` saved in memory.
3. `update` - lets us update the memory. We are incrementing the `offset` by
a tiny amount on each frame.

The `update` function is called about 60 times per second, so our little
changes to `offset` start to add up pretty quickly!

This game is not very fun though! Making a `game` also gives you access to the
[`Computer`](#Computer), so you can use information about the [`Mouse`](#Mouse)
and [`Keyboard`](#Keyboard) to make it interactive! So here is a red square that
moves based on the arrow keys:
{[
    import Playground exposing (..)

    main =
      game view update (0,0)

    view computer (x,y) =
      [ square red 40
          |> move x y
      ]

    update computer (x,y) =
      ( x + toX computer.keyboard
      , y + toY computer.keyboard
      )
]}
Notice that in the `update` we use information from the keyboard to update the
`x` and `y` values. These building blocks let you make pretty fancy games!
*)
val game :
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) -> 'memory -> ('memory game, msg) app

