(* Small dense matrices of floats: what a layer of a network is
 * (notes_ai_learning.md section 2).
 *
 * A layer of m neurons over n inputs is an m x n matrix of weights, a
 * vector of m biases, and a function to squash with, and the whole
 * forward pass of a network is one line per layer:
 *
 *     a = f(W a' + b)
 *
 * So everything the networks need is here, and nothing else: no
 * decompositions, no solvers, no BLAS, no C. A matrix is its shape and
 * a flat array of its rows, in the open, because half the teaching in
 * a matrix library is that there is nothing inside it.
 *
 *     rows = 2, cols = 3          data = [| 1.; 2.; 3.;
 *     ┌           ┐                         4.; 5.; 6. |]
 *     │ 1   2   3 │               (row-major: the element at (r, c)
 *     │ 4   5   6 │                is data.(r * cols + c))
 *     └           ┘
 *
 * {1 The product, twice}
 *
 * [mul_simple] is the definition, three nested loops:
 *
 *     for i, for j:  c(i,j) = sum over k of  a(i,k) * b(k,j)
 *
 * and it is slow for one reason that has nothing to do with
 * arithmetic: walking down a column of [b] jumps [b.cols] floats at
 * every step, so almost every read is a cache miss. [mul_fast] copies
 * [b] transposed first, so both walks are along rows, and then adds
 * four products at a time to give the processor independent work
 * (except where [b] is a column vector -- a network's forward pass and
 * nothing else -- which is contiguous already and is used as it is):
 *
 *     b, by column              b transposed, by row
 *     ┌ . x . . ┐   each read   ┌ . . . . ┐   each read is the next
 *     │ . x . . │   a jump      │ x x x x │   float in memory
 *     │ . x . . │               │ . . . . │
 *     └ . x . . ┘               └ . . . . ┘
 *
 * Measured here (ai/tests prints it, and the numbers are this
 * machine's), on square matrices, the time for one product:
 *
 *        n      simple      fast
 *       64      0.9 ms     0.6 ms      1.5x
 *      128      7.5 ms     4.0 ms      1.9x
 *      256     65.2 ms    29.9 ms      2.2x
 *
 * Twice, not ten times: the arithmetic is the same, and only the
 * reading changes. The gap grows with n because a column of [b] stops
 * fitting in the cache, which is why the real libraries (BLAS, and
 * Goto's papers on how its kernels are written) are organised around
 * memory rather than multiplication -- and why they go much further
 * than this, blocking the work so that a *piece* of each matrix stays
 * in cache across many products. That is the next factor of five, and
 * it is not here: this is the version you can read. Both versions
 * are kept and both are tested to agree, as graphics/Opti.mli keeps
 * the simple rasterizer beside the fast one; [fast] switches, and
 * [mul] is whichever it says.
 *
 * References: Kazushige Goto, Robert van de Geijn, "Anatomy of
 * High-Performance Matrix Multiplication", 2008; Ulrich Drepper, "What
 * Every Programmer Should Know About Memory", 2007. *)

type t = {
  rows : int;
  cols : int;
  data : float array; (* rows * cols, row-major *)
}

(*****************************************************************************)
(* {1 Making them} *)
(*****************************************************************************)

val create : int -> int -> t (* rows, cols, all zero *)
val init : int -> int -> (int -> int -> float) -> t
val of_lists : float list list -> t
val to_lists : t -> float list list
val identity : int -> t

(* [random ~seed ~spread rows cols]: every element drawn evenly from
 * -spread to spread. A network cannot start at zero -- every neuron of
 * a layer would then compute the same thing and learn the same thing
 * for ever -- so it starts small and different. [seed] makes a run
 * repeatable. *)
val random : seed:int -> ?spread:float -> int -> int -> t

(* a column matrix, n x 1: what a layer is given and what it answers *)
val vector : float array -> t
val to_vector : t -> float array

(*****************************************************************************)
(* {1 Reading and writing} *)
(*****************************************************************************)

val get : t -> int -> int -> float
val set : t -> int -> int -> float -> unit
val row : t -> int -> float array
val same_shape : t -> t -> bool

(*****************************************************************************)
(* {1 Arithmetic} *)
(*****************************************************************************)
(* [add], [sub] and [times] (elementwise, the Hadamard product -- which
   is what backpropagation needs, not the matrix product) raise
   [Invalid_argument] on shapes that do not match, as does [mul] when
   the inner dimensions disagree. *)

val add : t -> t -> t
val sub : t -> t -> t
val times : t -> t -> t
val scale : float -> t -> t
val map : (float -> float) -> t -> t
val map2 : (float -> float -> float) -> t -> t -> t
val sum : t -> float
val transpose : t -> t

(* the product: [mul] is [mul_fast] or [mul_simple], as [fast] says *)
val mul : t -> t -> t
val mul_simple : t -> t -> t
val mul_fast : t -> t -> t

(* true: use the faster product (the default) *)
val fast : bool ref
