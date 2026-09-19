(* Turtle graphics, the Logo way, on top of the Playground.

   Logo (Seymour Papert, Wally Feurzeig and Cynthia Solomon, BBN, 1967)
   gave children a turtle: a cursor with a position, a heading and a pen,
   moved by commands given from its own point of view -- go forward,
   turn right -- rather than by coordinates. To draw a square, walk like
   the turtle would:

     Logo:   repeat 4 [forward 100 right 90]
     OCaml:  repeat 4 [ forward 100.; right 90. ]

   Logo's brackets are lists, and so are OCaml's: a program here is a
   [command list], the same shape as in Logo. A Logo procedure is an
   OCaml function returning a command, and recursion works the same:

     Logo:   to tree :size
               if :size < 5 [stop]
               forward :size left 30 tree :size * 0.7
               right 60 tree :size * 0.7 left 30 back :size
             end

     OCaml:  let rec tree size =
               if size < 5. then stop
               else block [ forward size; left 30.; tree (size * 0.7);
                            right 60.; tree (size * 0.7); left 30.;
                            back size ]

   (the turtle comes back to where it started, facing the same way: the
   trick that makes recursive drawings compose, Papert's "state-
   transparent" procedures).

   The turtle starts at the center, (0, 0), heading up (north), its pen
   down, black, 2 pixels wide. Headings are Logo's: in degrees,
   clockwise, 0 up, 90 right (not the math convention of Playground's
   [rotate]: counterclockwise from the right).

   A program is data: [draw] runs it into shapes, [picture] shows the
   result, [animation] shows the turtle drawing it, at a given speed, as
   children watched it (and still do: the drawing's order teaches as
   much as the drawing). E.g., a whole application, 36 squares turned:

     open Logo
     let app = animation [ repeat 36 [ repeat 4 [ forward 100.; right 90. ]; right 10. ] ]
     let main = Playground_platform.run_app app

   Not here: Logo the language (its interpreter, its words, its
   lists), only its turtle, the part every language borrowed; and no
   [arc]/[circle]: [repeat 360 [ forward 1.; right 1. ]] is the classic
   first lesson (turtle geometry's "Total Turtle Trip Theorem": a closed
   path turns 360 degrees in all).

   References: Seymour Papert, "Mindstorms: Children, Computers, and
   Powerful Ideas", 1980; Harold Abelson and Andrea diSessa, "Turtle
   Geometry: The Computer as a Medium for Exploring Mathematics", MIT
   Press, 1981 (the fractals of examples/Fractals.ml, and the 3D turtle
   of playground3d/Logo3d.mli); Brian Harvey, "Computer Science Logo
   Style", MIT Press, 1997, and his Berkeley Logo (UCBLogo), whose names
   are used here (setxy, setheading, filled, label),
   https://people.eecs.berkeley.edu/~bh/logo.html; Python's turtle
   module, the same idea as a library,
   https://docs.python.org/3/library/turtle.html.
*)

type number = Playground.number

type command

(* {1 Moving} *)

(* [forward distance]: walk ahead, drawing a line if the pen is down *)
val forward : number -> command

(* [back distance]: walk backwards, still facing the same way *)
val back : number -> command

(* [left degrees], [right degrees]: turn where it stands *)
val left : number -> command
val right : number -> command

(* back to (0, 0), heading up, without drawing *)
val home : command

(* [set_xy x y]: go to (x, y), drawing if the pen is down *)
val set_xy : number -> number -> command

(* [set_heading degrees]: face that way (0 up, 90 right, clockwise) *)
val set_heading : number -> command

(* {1 The pen} *)

val pen_up : command
val pen_down : command
val pen_color : Playground.color -> command

(* [pen_size pixels]: the lines' width *)
val pen_size : number -> command

(* [filled color program]: run [program], filling the region its path
   encloses with [color] (under its lines), e.g. a filled square:
   [filled red [ repeat 4 [ forward 100.; right 90. ] ]] *)
val filled : Playground.color -> command list -> command

(* [label text]: write [text] where the turtle is (upright, not turned
   with it), in the pen's color *)
val label : string -> command

val hide_turtle : command
val show_turtle : command

(* {1 Programs} *)

(* [repeat n program]: [program], [n] times *)
val repeat : int -> command list -> command

(* [block program]: [program], as one command: a procedure's body *)
val block : command list -> command

(* nothing: a recursion's base case, Logo's [stop] *)
val stop : command

(* Logo's abbreviations, for the Logo feel: fd, bk, lt, rt, pu, pd *)
val fd : number -> command
val bk : number -> command
val lt : number -> command
val rt : number -> command
val pu : command
val pd : command

(* {1 Drawing} *)

(* [draw program]: the drawing, and the turtle on top (a triangle
   pointing where it heads), unless hidden *)
val draw : command list -> Playground.shape list

(* [draw_upto work program]: the drawing as it is after [work]: the
   turtle walked [work] pixels in all, a degree turned counting as
   0.25 pixel (so that the turtle is seen turning, but a 360-step
   circle doesn't take ages); a line being drawn is drawn up to where
   the turtle is, a region is filled once its path is done.
   [draw_upto infinity] = [draw]. *)
val draw_upto : number -> command list -> Playground.shape list

(* [work program]: all of its work, in the same unit: e.g. 490 for the
   square above (4 x 100 walked, 4 x 90 degrees turned, 0.25 each); how long it
   takes to draw at [speed] pixels per second: [work program / speed] *)
val work : command list -> number

(* {1 Applications} *)

(* [picture program]: the drawing, done *)
val picture : command list -> (Playground.screen, Playground.msg1) Playground.app

(* [animation ?speed program]: the turtle drawing it, walking [speed]
   pixels per second (default 300); space draws it again *)
val animation : ?speed:number -> command list -> (unit Scene2d.t Playground.game, Playground.msg) Playground.app
