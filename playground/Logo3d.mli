(* Turtle graphics in 3D: playground/Logo.mli's turtle, flying.

   In 2D, the turtle has a position and a heading; in 3D it needs a
   whole frame, three directions at right angles: where it heads (H),
   its left (L), and its up (U), the way a plane has a nose, wings and
   a top. Each turn rotates two of them around the third:

                 U                 left/right   around U   (yaw)
                 |  H              up/down      around L   (pitch)
                 | /               roll_left/   around H   (roll)
                 |/                roll_right
          L -----*

   so that, as in 2D, every command is from the turtle's own point of
   view: "up 90." makes it climb, whatever way it faces. That's the
   whole of the 3D turtle; the programs are Logo's:

     repeat 4 [ forward 100.; right 90. ]            (* a square *)
     repeat 4 [ forward 100.; right 90.; up 30. ]    (* no longer flat *)

   The turtle starts at (0, 0, 0) heading up (+y), its left towards -x,
   its up towards the viewer (+z): seen from the front, a 2D program
   draws the same picture as with Logo.mli, which a camera can then turn
   around. Lines are square tubes, [pen_size] wide, turned with the
   turtle (its roll shows).

   The frame and its rotations are those of Prusinkiewicz and
   Lindenmayer's "The Algorithmic Beauty of Plants" (Springer, 1990,
   section 1.5, "Modeling in three dimensions: turtle interpretation of
   symbols"), whose L-systems are 3D turtle programs: + and - are
   left/right, & and ^ down/up, | a U-turn, and \ and / the rolls
   (their \, "roll left", lifts the left side: here [roll_right], named
   after where the turtle banks); see LogoFractals3d.ml. The 3D
   turtle itself is from Abelson and diSessa's "Turtle Geometry" (MIT
   Press, 1981), chapter 10, "The Three-Dimensional Turtle".

   Not here: [filled], [label], [set_xy] (their 3D versions are left as
   exercises); here only: [dot], a small ball (an octahedron) where the
   turtle is, for leaves and flowers. *)

type number = Playground.number

type command

(*****************************************************************************)
(* {1 Moving} *)
(*****************************************************************************)

val forward : number -> command
val back : number -> command

(* yaw: turn left or right where it stands, around its up *)
val left : number -> command
val right : number -> command

(* pitch: point the nose up or down, around its left *)
val up : number -> command
val down : number -> command

(* roll: bank to the left or to the right, around its heading *)
val roll_left : number -> command
val roll_right : number -> command

(* back to (0, 0, 0), in the starting frame, without drawing *)
val home : command

(*****************************************************************************)
(* {1 The pen} *)
(*****************************************************************************)

val pen_up : command
val pen_down : command
val pen_color : Playground.color -> command

(* [pen_size size]: the tubes' width, 2 at first *)
val pen_size : number -> command

(* [dot size]: a small ball, [size] across, in the pen's color *)
val dot : number -> command

val hide_turtle : command
val show_turtle : command

(*****************************************************************************)
(* {1 Programs} *)
(*****************************************************************************)

val repeat : int -> command list -> command
val block : command list -> command
val stop : command

(* Logo's abbreviations *)
val fd : number -> command
val bk : number -> command
val lt : number -> command
val rt : number -> command
val pu : command
val pd : command

(*****************************************************************************)
(* {1 Drawing} *)
(*****************************************************************************)

(* [draw program]: the drawing, and the turtle (a pyramid pointing
   where it heads, its top towards its up) *)
val draw : command list -> Playground3d.shape3d list

(* [draw_upto work program], [work program]: as in Logo.mli (a degree
   turned, pitched or rolled worth 0.25) *)
val draw_upto : number -> command list -> Playground3d.shape3d list
val work : command list -> number

(* [camera_around program angle]: a camera looking at the whole
   drawing, from [angle] degrees around it (0: from the front, +z), a
   little above it. The drawing's size is found once, by
   [camera_around program]: keep that function for every frame. *)
val camera_around : command list -> number -> Playground3d.camera

(*****************************************************************************)
(* {1 Applications} *)
(*****************************************************************************)

(* [animation ?speed program]: the turtle drawing it, [speed] units a
   second (default 300), the camera turning around it (once every 20
   seconds); space draws it again *)
val animation : ?speed:number -> command list -> (unit Scene2d.t, Playground.msg) Playground3d.app3d
