(* Lehmer: random numbers from a formula, the same ones every time.

   A computer can't flip a coin, so games use a *pseudo*-random
   generator: a number, the *seed*, and a formula giving the next one
   from it. The sequence looks random, and is entirely determined by the
   first seed -- which is the point here: a game that keeps its seed in
   its model replays the same way (golden frames, a bug reproduced), and
   two computers given the same seed at the start draw the same numbers
   (lockstep networking, plan_networking_teaching.md). OCaml's global
   Random is the opposite: a hidden state, shared by the whole program,
   and seeded from the clock by Random.self_init.

   The formula is D. H. Lehmer's (1949, for ENIAC), multiply and take
   the remainder:

       seed' = 16807 * seed  mod  (2^31 - 1)

   with the constants Park and Miller called the *minimal standard*
   (1988): 2^31 - 1 = 2147483647 is prime and 16807 = 7^5 makes every
   seed from 1 to 2^31 - 2 come back only after all the others -- a
   period of 2,147,483,646. Seed 0 is never used (it would stay 0).

   The subtle part is computing it at all. 16807 * seed needs 46 bits;
   native OCaml's ints have 63, but in a browser (js_of_ocaml) they have
   32, and the product overflows: the same program would draw other
   numbers on the web. Schrage's trick (1979) never goes past 31 bits:
   write m = a q + r, with q = m / a = 127773 and r = m mod a = 2836
   (r < q is what makes it work), then

       a * seed  mod  m  =  a * (seed mod q)  -  r * (seed / q)
                            (plus m if that is negative)

   each product below 2^31. So both platforms give the same sequence,
   bit for bit.

   Worked example (checked by the tests): from seed 1, the sequence
   starts 16807, 282475249, 1622650073, 984943658, 1144108930, and the
   10,000th is 1043618065 -- the check value Park and Miller give,
   so that an implementation can test itself.

   One more trap, and a real one: the generator only multiplies, so
   seed 2's sequence is exactly twice seed 1's (mod m), forever, and
   seeds 1, 2, 3 -- what people type -- start nearly the same game (from
   1, the first draw is 16807 / 2^31 = 0.000008; from 2, 0.000016). So
   a seed given by a person is *scrambled* first: [scramble] hashes it
   with MurmurHash3's finalizer (Austin Appleby, 2008), three xor-shifts
   and two multiplications that make every bit of the result depend on
   every bit of the number, on Int32 so that it wraps the same way
   everywhere. Seeds 1 and 2 then start unrelated games.

   Not a good generator by today's standards (xorshift, Marsaglia
   2003, and PCG, O'Neill 2014, are faster and pass statistical tests
   it fails), and never for cryptography; enough for a game, and the
   simplest one that is portable.

   References: D. H. Lehmer, "Mathematical methods in large-scale
   computing units" (1949); Stephen K. Park and Keith W. Miller, "Random
   Number Generators: Good Ones Are Hard to Find", Communications of the
   ACM 31(10) (1988); Linus Schrage, "A More Portable Fortran
   Random Number Generator", ACM TOMS 5(2) (1979); Knuth, The Art of
   Computer Programming, volume 2, chapter 3. *)

(* between 1 and 2^31 - 2 *)
type t = private int

(* any int made a valid seed, as is (0 and the multiples of 2^31 - 1
 * become 1): for the worked example, and a seed already drawn *)
val of_int : int -> t

(* a seed from a number a person gave (seed=1): hashed first, so that
 * neighbour numbers give unrelated sequences *)
val scramble : int -> t

(* the next seed: 16807 * seed mod (2^31 - 1), by Schrage's trick *)
val next : t -> t

(* the seed as a number in [0, 1) *)
val to_unit : t -> float
