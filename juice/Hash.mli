(* Random numbers that are the same every time: a hash of a seed and a
   number.

   Juice wants randomness -- where a shake goes, which way a spark
   flies -- but not a random game: the same seed must give the same
   shake and the same sparks, in a replay, in a golden frame test, in
   the browser as natively. So no global [Random], and no generator's
   state hidden somewhere: a function, from a seed and a point (a
   lattice point of the noise, the next draw of a burst) to a number in
   [-1, 1], unrelated to its neighbours'.

   It is Park and Miller's "minimal standard" generator (1988),
   x -> 16807 x mod (2^31 - 1), a few steps of it from the seed and the
   point, computed by Schrage's method (1979), so that no product
   reaches 2^31:

     16807 x mod m  =  16807 (x mod q) - r (x / q)      (+ m if negative)
                       q = m / 16807 = 127773,  r = m mod 16807 = 2836

   That matters here: OCaml's ints have 63 bits natively and 32 in a
   browser (js_of_ocaml), and a hash whose products overflowed would
   give the two different numbers -- a shake or a burst that differs on
   the web. A first version started the generator from seed and point
   and took three steps: neighbouring points then came out correlated
   (-0.15), the generator being linear; an xor of the high bits between
   the steps (x lxor (x lsr 13)) brings it to 0.002.

   Worked example (checked by the tests): [hash ~seed:1] is -0.1084 at
   0, 0.5113 at 1, 0.5300 at 2.

   Honest about scale: 20 bits of point (it wraps after 1048576) and 10
   of seed (1024 seeds); a generator for effects, not for statistics
   or cards (Cards.mli has Microsoft's, which a deal number must
   reproduce exactly). *)

(* [hash ~seed i]: a number in [-1, 1], the same for the same seed and
 * point, unrelated to its neighbours' *)
val hash : seed:int -> int -> float

(* [unit ~seed i]: the same number in [0, 1] *)
val unit : seed:int -> int -> float
