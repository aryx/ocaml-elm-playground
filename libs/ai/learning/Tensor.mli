(* Automatic differentiation on whole arrays: Grad, a matrix at a time
 * (notes_ai_learning.md section 15).
 *
 * [Grad] makes a node of the graph for every number: a network of
 * four thousand weights reading a name of six letters is thirty
 * thousand nodes, each a record, a closure and a list, and that
 * bookkeeping is where the time goes -- not the arithmetic.
 *
 * Nothing in the idea says a node has to be one number. Here a node
 * is a whole matrix, its slope a matrix of the same shape, and an
 * operation knows how to send a matrix of slopes back:
 *
 *     Grad    y = a * b          a's slope  +=  y's slope * b
 *     Tensor  Y = A B            A's slope  +=  Y's slope * B^T
 *                                B's slope  +=  A^T * Y's slope
 *
 * The same rule -- each input's slope is the output's times the
 * *other* input -- with transposes so that the shapes fit. Every
 * operation below is its scalar twin with the loop moved inside, and
 * [backward] is [Grad.backward] word for word: the nodes in the order
 * of what depends on what, each sending its slopes to what it was
 * made from. This is what PyTorch and JAX are, with the loops on a
 * GPU.
 *
 * Example: a layer of two neurons on three inputs, and a loss that
 * is the sum of what comes out --
 *
 *     let w = value (Matrix.of_lists [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]) in
 *     let x = value (Matrix.of_lists [ [ 1. ]; [ 0. ]; [ -1. ] ]) in
 *     let y = mul w x in                  (* [ -2. ]; [ -2. ] *)
 *     backward (sum y);
 *     slope w                             (* [ 1.; 0.; -1. ]; [ 1.; 0.; -1. ] : x, for each row *)
 *     slope x                             (* [ 5. ]; [ 7. ]; [ 9. ] : the columns of w, summed *)
 *
 * four nodes, where [Grad] makes more than twenty.
 *
 * What it buys, measured on [Gpt]'s gradient for one name, the same
 * model both ways (the losses and every slope equal to ten decimals,
 * Unit_gpt):
 *
 *     the model                 numbers    on Grad      here
 *     microgpt's, 16 wide         4,192      4.0 ms    0.41 ms    10x
 *     32 wide                    14,528     18   ms    0.98 ms    19x
 *     64 wide, 2 layers         102,784    217   ms    6.0  ms    36x
 *     128 wide, 4 layers        795,392   2050   ms   44    ms    47x
 *
 * Ten to fifty times, growing with the model: the larger the
 * matrices, the more of the time is arithmetic and the less is
 * bookkeeping. The last line is about 800 million multiplications a
 * second, in plain OCaml loops.
 *
 * A third of that came from one operation, [mul_t]. A layer is
 * X W^T, and written [mul x (transpose w)] it copies W turned, the
 * product turns it back to read along its rows, and the way back
 * makes two more products and two more copies. [mul_t] reads the rows
 * of both as they lie, and sends the slopes back a row at a time
 * ([direct], off, is the long way: 0.80, 2.6, 16 and 165 ms above,
 * two to four times slower). notes_opti_ocaml.md, section 21.
 *
 * A matrix is all there is: no third dimension, no broadcasting but
 * the two operations that say so ([scale_rows], [row_mean]). A batch
 * of texts is a loop over texts. (The Little Learner's extended
 * operators, any function of rank n lifted to rank n + 1, are the
 * elegant way past that, and an exercise.)
 *
 * References: Seppo Linnainmaa, 1970, and Andreas Griewank, Andrea
 * Walther, *Evaluating Derivatives*, 2008 (reverse mode, as in
 * Grad.mli); Adam Paszke et al., "Automatic differentiation in
 * PyTorch", 2017; Mike Giles, "Collected Matrix Derivative Results
 * for Forward and Reverse Mode Algorithmic Differentiation", 2008
 * (the product's rule above, and its relatives); Daniel Friedman,
 * Anurag Mendhekar, *The Little Learner*, 2023. *)

(*****************************************************************************)
(* {1 Values} *)
(*****************************************************************************)

type t

(* a matrix the graph knows about. Not copied: it must not be changed
 * while the graph is in use. *)
val value : Matrix.t -> t
val of_ : t -> Matrix.t

(* the slopes of whatever [backward] was called on with respect to
 * each number of this value: zeros until then. Rewritten by the next
 * [backward]. *)
val slope : t -> Matrix.t

(* the one number of a 1 by 1 value: a loss *)
val number : t -> float

(*****************************************************************************)
(* {1 Arithmetic} *)
(*****************************************************************************)

val add : t -> t -> t (* the same shapes *)
val sub : t -> t -> t
val mul : t -> t -> t (* the matrix product *)
val times : t -> t -> t (* element by element *)

(* [mul_t a b]: a times the transpose of b, each row of [a] against
 * each row of [b]. A layer on every row at once is [mul_t x w], and
 * attention's scores [mul_t queries keys]. The same numbers as
 * [mul a (transpose b)], without the copies: see [direct]. *)
val mul_t : t -> t -> t

(* false: [mul_t] the long way, through [transpose] and [mul], to time
 * what the direct one saves *)
val direct : bool ref
val scale : float -> t -> t (* every number times a constant *)
val shift : float -> t -> t (* every number plus a constant *)
val transpose : t -> t

(* a function of each number *)
val tanh_ : t -> t
val relu : t -> t
val pow : t -> float -> t

(* every number added: a 1 by 1 value *)
val sum : t -> t

(*****************************************************************************)
(* {1 Picking and joining} *)
(*****************************************************************************)

(* [rows a which]: the rows asked for, in the order asked. With [a] a
 * table of a row per token and [which] a text's tokens, it is the
 * embedding lookup, and its slope goes back to those rows only. *)
val rows : t -> int array -> t

(* [cols a first count]: so many columns, from [first] *)
val cols : t -> int -> int -> t

(* values of the same height, side by side *)
val join_cols : t list -> t

(*****************************************************************************)
(* {1 A board} *)
(*****************************************************************************)
(* A network that reads a board as a list of unrelated numbers has to
 * learn that three in a row on the left is three in a row on the
 * right. A *convolution* does not: it is one small layer, looking at
 * a square and its eight neighbours, applied at every square with the
 * same weights. What it learns about a shape it knows everywhere.
 *
 * With the board as a matrix, a row per square and a column per
 * channel (a channel is one kind of thing a square can hold: my
 * pieces, the other's, then whatever the layers before made of them),
 * it is two operations already here:
 *
 *     patches            each square's row becomes its neighbourhood's:
 *                        9 squares, channel by channel, zeros off the board
 *     mul_t  . weights   a layer on every row: a row of weights per new
 *                        channel, 9 times the old channels long
 *
 * so the product that a layer is ([mul_t]) is the product a
 * convolution is, and nothing new has a slope to get wrong but the
 * copying. (LeCun et al., 1989, for digits; the same idea is every
 * board network since AlphaGo.) *)

(* [patches a ~height ~width]: [a] a row per square of a board, row
 * after row, a column per channel; each square with its 3 by 3
 * neighbourhood, nine times the columns *)
val patches : t -> height:int -> width:int -> t

(* the same numbers in another shape, row after row: a board's squares
 * laid end to end for a layer that reads them all *)
val reshape : t -> int -> int -> t

(*****************************************************************************)
(* {1 A row at a time} *)
(*****************************************************************************)

(* each row's mean, as a column *)
val row_mean : t -> t

(* [scale_rows a s]: each row of [a] times its own number of the
 * column [s] *)
val scale_rows : t -> t -> t

(* [add_row a b]: the row [b] added to every row of [a] -- a layer's
 * biases, one per neuron, the same for every example *)
val add_row : t -> t -> t

(* each row made shares summing to 1. [causal]: row r over its first
 * r + 1 numbers only, the others 0 -- in a square of every token
 * against every token, a token attends to itself and to those before
 * it, never after. *)
val softmax_rows : ?causal:bool -> t -> t

(* [cross_entropy scores answers]: a row of scores per example, the
 * right answer of each; the mean of -log (the softmax of the
 * row).(answer), a 1 by 1 value. One operation and not two, because
 * the slope of the two together is the simple one: the share given
 * minus the share deserved ([Grad.cross_entropy]). *)
val cross_entropy : t -> int array -> t

(* [cross_entropy_to scores deserved]: the same against shares deserved
 * instead of one answer, a row of them per example: the mean of
 * -sum_c deserved.(c) log (the softmax of the row).(c). What a policy
 * is taught with when the lesson is "this much on each move"
 * (Alphazero.mli). *)
val cross_entropy_to : t -> Matrix.t -> t

(*****************************************************************************)
(* {1 Going backwards} *)
(*****************************************************************************)

(* [backward v]: every [slope] that led to [v], whose own is 1 *)
val backward : t -> unit

(* how many nodes went into a value *)
val nodes : t -> int
