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

(** A number like [1] or [3.14] or [-120].

It is more flexible to use [float] than [int] for graphical operations.
Consider using [open Basics] to have the [+], [-], and other
arithmetic operators working on floats instead of having to use
the [+.], [-.], etc.
 *)
type number = float

(** {2 Time} *)

(** The current time.

Helpful when making an {!val:animation} with functions like
{!spin}, {!wave}, and {!zigzag}.
*)
type time = Time of number

(** Create an angle that cycles from 0 to 360 degrees over time.

Here is an {!val:animation} with a spinning triangle:

{[
    open Playground

    let view time =
      [ triangle orange 50
          |> rotate (spin 8 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
It will do a full rotation once every eight seconds. Try changing the [8] to
a [2] to make it do a full rotation every two seconds. It moves a lot faster!
*)
val spin : number -> time -> number

(** Smoothly wave between two numbers.

Here is an {!val:animation} with a circle that resizes:
{[
    open Playground

    view time =
      [ circle lightBlue (wave 50 90 7 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
The radius of the circle will cycles between 50 and 90 every seven seconds.
It kind of looks like it is breathing.
*)
val wave : number -> number -> number -> time -> number

(** Zig zag between two numbers.

Here is an {!val:animation} with a rectangle that tips back and forth:
{[
    open Playground

    view time =
      [ rectangle lightGreen 20 100
          |> rotate (zigzag -20 20 4 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds.
*)
val zigzag : number -> number -> number -> time -> number

(** {2 Colors} *)

(** Represents a color.

The colors below, like [red] and [green], come from the 
{{:https://en.wikipedia.org/wiki/Tango_Desktop_Project}Tango palette}.
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like [lightYellow], [yellow],
and [darkYellow].
*)
type color = Color.t (* Hex of string | Rgb of int * int * int *)

(** RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:
{[
    let brightBlue = rgb 18 147 216
    let brightGreen = rgb 119 244 8
    let brightPurple = rgb 94 28 221
]}
Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like {{:http://paletton.com/}paletton} to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.
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

(** Shapes help you make a [picture], [animation], or [game].

Read on to see examples of {!circle}, {!rectangle},
{!val:words}, {!image}, and many more!
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
    let dot = circle red 10
    let sun = circle yellow 300
]}
You give a color and then the radius. So the higher the number, the larger
the circle.
*)
val circle : color -> number -> shape

(** Make ovals:
{[
    let football = oval brown 200 100
]}
You give the color, and then the width and height. So our [football] example
is 200 pixels wide and 100 pixels tall.
*)
val oval : color -> number -> number -> shape

(** Make rectangles. This example makes a red cross:
{[
    open Playground

    let app =
      picture
        [ rectangle red 20 60
        ; rectangle red 60 20
        ]

    let main = Playground.run_app app
]}
You give the color, width, and then height. So the first shape is vertical
part of the cross, the thinner and taller part.
*)
val rectangle : color -> number -> number -> shape

(** Make squares. Here are two squares combined to look like an empty box:
{[
    open Playground

    let app =
      picture
        [ square purple 80
        ; square white 60
        ]

    let main = Playground.run_app app
]}
The number you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.
*)
val square : color -> number -> shape

(** Make triangles. So if you wanted to draw the Egyptian pyramids, you could
do a simple version like this:
{[
    open Playground

    let app =
      picture
        [ triangle darkYellow 200
        ]

    let main = Playground.run_app app
]}
The number is the "radius", so the distance from the center to each point of
the pyramid is [200]. Pretty big!
*)
val triangle : color -> number -> shape

(** Make pentagons:
{[
    open Playground

    let app =
      picture
        [ pentagon darkGrey 100
        ]

    let main = Playground.run_app app
]}
You give the color and then the radius. So the distance from the center to each
of the five points is 100 pixels.
*)
val pentagon : color -> number -> shape

(** Make hexagons:
{[
    open Playground

    let app =
      picture
        [ hexagon lightYellow 50
        ]

    let main = Playground.run_app app
]}
The number is the radius, the distance from the center to each point.

If you made more hexagons, you could {!move} them around to make a
honeycomb pattern!
*)
val hexagon : color -> number -> shape

(** Make octogons:
{[
    open Playground

    let app =
      picture
        [ octagon red 100
        ]

    let main = Playground.run_app app
]}
You give the color and radius, so each point of this stop sign is 100 pixels
from the center.
*)
val octagon : color -> number -> shape

(** Make any shape you want! Here is a very thin triangle:
{[
    open Playground

    let app =
      picture
        [ polygon black [ (-10,-20), (0,100), (10,-20) ]
        ]

    let main = Playground.run_app app
]}
**Note:** If you {!rotate} a polygon, it will always rotate around
[(0,0)]. So it is best to build your shapes around that point, and then use
{!move} or {!group} so that rotation makes more sense.
*)
val polygon : color -> (number * number) list -> shape

(** {2 Images } *)

(** Add some image from the internet:
{[
    open Playground

    let app =
      picture
        [ image 96 96 "https://elm-lang.org/images/turtle.gif"
        ]

    let main = Playground.run_app app
]}
You provide the width, height, and then the URL of the image you want to show.
*)
val image : number -> number -> string -> shape

(** {2 Words } *)

(** Show some words!
{[
    open Playground

    let app =
      picture
        [ words black "Hello! How are you?"
        ]

    let main = Playground.run_app app
]}
You can use {!val:scale} to make the words bigger or smaller.
*)
val words : color -> string -> shape

(** {2 Groups } *)

(** Put shapes together so you can {!move} and {!rotate}
them as a group. Maybe you want to put a bunch of stars in the sky:
{[
    open Playground

    let star =
      group
        [ triangle yellow 20
        ; triangle yellow 20
            |> rotate 180
        ]

    let app =
      picture
        [ star
            |> move 100 100
            |> rotate 5
        ; star
            |> move -120 40
            |> rotate 20
        ; star
            |> move 80 -150
            |> rotate 32
        ; star
            |> move -90 -30
            |> rotate -16
        ]
    let main = Playground.run_app app
]}
*)
val group : shape list -> shape

(** {2 Move Shapes } *)

(** Move a shape by some number of pixels:
{[
    open Playground

    let app =
      picture
        [ square red 100
            |> move -60 60
        ; square yellow 100
            |> move 60 60
        ; square green 100
            |> move 60 -60
        ; square blue 100
            |> move -60 -60
        ]

    let main = Playground.run_app app
]}
*)
val move : number -> number -> shape -> shape

(** Move shapes to the left.
{[
    open Playground

    let app =
      picture
        [ circle yellow 10
            |> moveLeft 80
            |> moveUp 30
        ]

    let main = Playground.run_app app
]}
*)
val move_left : number -> shape -> shape

(** Move shapes to the right.
{[
    open Playground

    let app =
      picture
        [ square purple 20
            |> moveRight 80
            |> moveDown 100
        ]

    let main = Playground.run_app app
]}
*)
val move_right : number -> shape -> shape

(** Move a shape up by some number of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:
{[
    open Playground

    let app =
      picture
        [ rectangle brown 40 200
        , circle green 100
            |> moveUp 180
        ]

    let main = Playground.run_app app
]}
*)
val move_up : number -> shape -> shape

(** Move a shape down by some number of pixels. So if you wanted to put the sky
above the ground, you could move the sky up and the ground down:
{[
    open Playground

    let app =
      picture
        [ rectangle lightBlue 200 100
            |> moveUp 50
        ; rectangle lightGreen 200 100
            |> moveDown 50
        ]

    let main = Playground.run_app app
]}
*)
val move_down : number -> shape -> shape

(** Move the [x] coordinate of a shape by some amount. Here is a square that
moves back and forth:
{[
    open Playground

    view time =
      [ square purple 20
          |> moveX (wave 4 -200 200 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
Using [moveX] feels a bit nicer here because the movement may be positive or negative.
*)
val move_x : number -> shape -> shape

(** Move the [y] coordinate of a shape by some amount. Maybe you want to make
grass along the bottom of the screen:
{[
    open Playground

    let update computer memory =
      memory

    let view computer count =
      [ rectangle green computer.screen.width 100
          |> moveY computer.screen.bottom
      ]

    let app =
      game view update 0

    let main = Playground.run_app app
]}
Using [moveY] feels a bit nicer when setting things relative to the bottom or
top of the screen, since the values are negative sometimes.
*)
val move_y : number -> shape -> shape

(** {2:transformations Customize Shapes } *)

(** Make a shape bigger or smaller. So if you wanted some {!words} to
be larger, you could say:
{[
    open Playground

    let app =
      picture
        [ words black "Hello, nice to see you!"
            |> scale 3
        ]

    let main = Playground.run_app app
]}
*)
val scale : number -> shape -> shape

(** Rotate shapes in degrees.
{[
    open Playground

    let app =
      picture
        [ words black "These words are tilted!"
            |> rotate 10
        ]

    let main = Playground.run_app app
]}
The degrees go {b counter-clockwise} to match the direction of the
{{:https://en.wikipedia.org/wiki/Unit_circle} unit circle}.
*)
val rotate : number -> shape -> shape

(** Fade a shape. This lets you make shapes see-through or even completely
invisible. Here is a shape that fades in and out:
{[
    open Playground

    view time =
      [ square orange 30
      ; square blue 200
          |> fade (zigzag 0 1 3 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
The number has to be between [0] and [1], where [0] is totally transparent
and [1] is completely solid.
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
    ; height = 600
    ; top = 300
    ; left = -400
    ; right = 400
    ; bottom = -300
    }
]}
This can be nice when used with {!move_y} if you want to put something
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
    open Playground

    let view computer memory =
      [ circle yellow 40
          |> moveX computer.mouse.x
          |> moveY computer.mouse.y
      ]

    let update computer memory =
      memory

    let app =
      game view update 0

    let main = Playground.run_app app
]}
You could also use [computer.mouse.down] to change the color of the circle
while the mouse button is down.
*)
type mouse = { mx : number; my : number; mdown : bool; mclick : bool; }

(** Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:
{[
    { kup = True, kdown = False, kleft = False, kright = True
    , kspace = False, kenter = False, kshift = False, kbackspace = False
    , keys = Set.fromList ["ArrowUp","ArrowRight"]
    }
]}

So if you want to move a character based on arrows, you could write an update
like this:
{[
    let update computer y =
      if computer.keyboard.up then
        y + 1
      else
        y
]}
Check out {!to_x} and {!to_y} which make this even easier!

{b Note:} The [keys] set will be filled with the name of all keys which are
down right now. So you will see things like ["a"], ["b"], ["c"], ["1"], ["2"],
["Space"], and ["Control"] in there. Check out 
{{:https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values}this list} to see the
names used for all the different special keys! From there, you can use
[Set.member] to check for whichever key you want. E.g.
[Set.member "Control" computer.keyboard.keys].
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
    to_x { left = False, right = False, ... } == 0
    to_x { left = True , right = False, ... } == -1
    to_x { left = False, right = True , ... } == 1
    to_x { left = True , right = True , ... } == 0
]}
So to make a square move left and right based on the arrow keys, we could say:
{[
    open Playground

    let view computer x =
      [ square green 40
          |> moveX x
      ]

    let update computer x =
      x + toX computer.keyboard


    let app =
      game view update 0
    let main = Playground.run_app app
]}
*)
val to_x : keyboard -> number

(** Turn the UP and DOWN arrows into a number.
{[
    to_y { up = False, down = False, ... } == 0
    to_y { up = True , down = False, ... } == 1
    to_y { up = False, down = True , ... } == -1
    to_y { up = True , down = True , ... } == 0
]}

This can be used to move characters around in games just like {!toX}:
{[
    open Playground

    let view computer (x,y) =
      [ square blue 40
          |> move x y
      ]

    let update computer (x,y) =
      ( x + toX computer.keyboard
      , y + toY computer.keyboard
      )

    let app =
      game view update (0,0)

    let main = Playground.run_app app
]}
*)
val to_y : keyboard -> number

val to_x2 : keyboard -> number

val to_y2 : keyboard -> number

(** If you just use [to_x] and [to_y], you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So [to_xy] turns the arrow keys into an [(x,y)] pair such that the distance is
normalized:
{[
    to_xy { up = True , down = False, left = False, right = False, ... } == (1, 0)
    to_xy { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    to_xy { up = False, down = False, left = False, right = True , ... } == (0, 1)
]}

Now when you go up/right, you are still going 1 pixel per update.
{[
    open Playground

    let view computer (x,y) =
      [ square green 40
          |> move x y
      ]

    let update computer (x,y) =
      let
        (dx,dy) = toXY computer.keyboard
      in
      (x + dx, y + dy)

    let app =
      game view update (0,0)

    let main = Playground.run_app app
]}
*)
val to_xy : keyboard -> number * number

(** When writing a {!game}, you can look up all sorts of information
about your computer:

  - {!type:mouse} - Where is the mouse right now?
  - {!type:keyboard} - Are the arrow keys down?
  - {!type:screen} - How wide is the screen?
  - {!type:time} - What time is it right now?

So you can use expressions like [computer.mouse.x] and [computer.keyboard.enter]
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
    open Playground

    let app =
      picture
        [ triangle green 150
        ; circle white 40
        ; circle black 10
        ]

    let main = Playground.run_app app
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

Once you get comfortable using {!val:picture} to layout shapes, you can
try out an [animation]. Here is square that zigzags back and forth:
{[
    open Playground

    view time =
      [ square blue 40
          |> moveX (zigzag -100 100 2 time)
      ]

    let app =
      animation view

    let main = Playground.run_app app
]}
We need to define a [view] to make our animation work.

Within [view] we can use functions like {!spin}, {!wave},
and {!zigzag} to move and rotate our shapes.
*)
val animation : (time -> shape list) -> (animation, msg) app

(** {2 Games} *)

type 'memory game

(** Create a game!

Once you get comfortable with {!val:animation}, you can try making a
game with the keyboard and mouse. Here is an example of a green square that
just moves to the right:
{[
    open Playground

    let view computer offset =
      [ square green 40
          |> moveRight offset
      ]

    let update computer offset =
      offset + 0.03

    let app =
      game view update 0

    let main = Playground.run_app app
]}
This shows the three important parts of a game:
+ [memory] - makes it possible to store information. So with our green square, we save the [offset] in memory. It starts out at [0].
+ [view] - lets us say which shapes to put on screen. So here we move our square right by the [offset] saved in memory.
+ [update] - lets us update the memory. We are incrementing the [offset] by a tiny amount on each frame.

The [update] function is called about 60 times per second, so our little
changes to [offset] start to add up pretty quickly!

This game is not very fun though! Making a [game] also gives you access to the
{!Computer}, so you can use information about the {!type:mouse}
and {!type:keyboard} to make it interactive! So here is a red square that
moves based on the arrow keys:
{[
    open Playground

    let view computer (x,y) =
      [ square red 40
          |> move x y
      ]

    let update computer (x,y) =
      ( x + toX computer.keyboard
      , y + toY computer.keyboard
      )

    let app =
      game view update (0,0)

    let main = Playground.run_app app
]}
Notice that in the [update] we use information from the keyboard to update the
[x] and [y] values. These building blocks let you make pretty fancy games!
*)
val game :
  (computer -> 'memory -> shape list) ->
  (computer -> 'memory -> 'memory) -> 'memory -> ('memory game, msg) app
