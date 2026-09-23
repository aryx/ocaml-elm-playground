(* Particles: many small things, each born, moving and dying by itself.

   William Reeves named them, for the Genesis effect of Star Trek II
   (1982): a wall of fire spreading over a planet, "a class of fuzzy
   objects" -- fire, smoke, water, sparks -- that no surface can model,
   made instead of thousands of points, each with a few numbers, none
   of them designed ("Particle Systems", SIGGRAPH 1983). Every game
   engine has one since, and a game's sparks, dust and debris are
   still his model:

     born      at a place, at once or at a rate, with a speed, a
               direction within a cone, a life, a size, a spin -- each
               drawn at random between two bounds
     moving    by its velocity, pulled by gravity, slowed by drag
     dying     when its life is spent

             . '          a burst: [count] particles, speeds in
          .  '  ' .       [speed], directions in a cone [spread] wide
        -- * --  .        around [direction]; then each on its own:
          ' . ' .         falling, slowing, fading, gone
             ' .

   Nothing here collides with anything, and nothing is physics: a
   particle is decoration (see Juice.mli). The motion is still the
   physics engine's step, semi-implicit Euler (physics/2d/Integrate.mli):
   the velocity first, then the position with the new velocity.

   Randomness is a seed. Each draw is Hash.mli's number for the seed
   and a count of draws so far, so the same seed and the same bursts
   give the same particles, frame for frame -- in a replay, and in a
   golden frame test.

   A cap. Each particle is a shape to draw, and the software rasterizer
   and the SVG backend pay per shape: a burst past [cap] drops the
   oldest particles first.

   Worked example (checked by the tests). A particle thrown straight
   up at 400 pixels a second, gravity 800 down: in the real world it
   rises 100 pixels in half a second and is back at 0 after one; stepped
   at 60 frames a second it rises to 96.67 (frames 29 and 30) and is at
   -6.67 after one second -- Euler's error, which is 1/120 of a second's
   worth of speed each step, and invisible in a spark. With a drag of 2
   a second and no gravity, a particle at 100 is at 36.2 after half a
   second (exactly: 100 e^-1 = 36.8), having gone 30.9 pixels.

   Honest about scale: a few hundred particles, as shapes; a GPU
   particle system draws millions, as points, in a shader. *)

(* what a burst makes; every pair is (least, most), each particle's
 * value drawn between them *)
type recipe = {
  count : int;
  speed : float * float; (* pixels a second *)
  direction : float; (* degrees: 0 right, 90 up *)
  spread : float; (* degrees, the whole cone: 360 all around *)
  life : float * float; (* seconds *)
  size : float * float; (* pixels *)
  spin : float; (* degrees a second, at most, either way *)
  gravity : float; (* pixels a second, a second: negative falls *)
  drag : float; (* a second: 0 none, 2 loses 2% a frame *)
}

(* a particle, and ['a], what the caller made of its [tone] (a color) *)
type 'a particle = {
  x : float;
  y : float;
  vx : float;
  vy : float;
  angle : float; (* degrees *)
  spin : float;
  size : float;
  age : float; (* seconds *)
  life : float;
  gravity : float;
  drag : float;
  data : 'a;
}

type 'a t

(* [empty ?cap ~seed ()]: no particle; at most [cap] (400 by default) *)
val empty : ?cap:int -> seed:int -> unit -> 'a t

(* [burst recipe ~data x y t]: [recipe.count] particles born at (x, y);
 * [data tone] makes each one's ['a] from a number in [0, 1] drawn for
 * it (e.g. a color out of a palette) *)
val burst : recipe -> data:(float -> 'a) -> float -> float -> 'a t -> 'a t

(* [step ~dt t]: [dt] seconds later: each particle moved (velocity,
 * then position), older, and gone if its life is spent *)
val step : dt:float -> 'a t -> 'a t

(* the particles alive, the oldest first *)
val particles : 'a t -> 'a particle list
