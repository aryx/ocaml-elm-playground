(* The DX7's 32 algorithms: six operators, who modulates whom (see
 * notes_synth.md; plan_synth_teaching.md, TinyDX7, D1).
 *
 * Fm.mli has two sines, one wobbling the other's phase. The DX7 has
 * six, its *operators*, each a sine with its own frequency ratio and
 * envelope, and an *algorithm* says how they're wired: which modulate
 * which (a modulator's output added to its target's phase), and which
 * are heard (the *carriers*, their outputs summed). Yamaha offered 32
 * wirings and no others -- a fixed menu, printed on the DX7's panel:
 *
 *     algorithm 1          algorithm 5          algorithm 32
 *
 *          6 <-+ fb
 *          |---+
 *          5                6*  4   2*          6* 5  4  3  2  1
 *          |                |   |   |           |  |  |  |  |  |
 *     2    4                5   3   1           +--+--+--+--+--+
 *     |    |                |   |   |                  out
 *     1    3                +---+---+
 *     |    |                   out              6*: 6 fed back
 *     +----+
 *      out
 *
 * Algorithm 1 is two stacks: a two-operator one (2 on 1, Chowning's
 * pair, Fm.mli) and a four-operator one, deep, bright. Algorithm 5
 * is three pairs side by side, three FM sounds mixed (the E.PIANO's
 * shape). Algorithm 32 is six sines heard: an organ, additive
 * synthesis (TinyHammond's drawbars). Between them, every mixture of
 * depth and breadth.
 *
 * One operator per algorithm is also *fed back*: its own output, the
 * average of its last two samples, added to its own phase, scaled by
 * 2^(fb - 8), fb 0 to 7 (0 none). A sine modulating itself turns into
 * a sawtooth, then into noise as fb grows -- the only way to a
 * sawtooth, a noise, on a machine that has only sines. (The level
 * counts as much as fb: a step of fb is a doubling, as 6 dB of the
 * operator's level is.) In algorithms
 * 4 and 6 the loop goes through more than one operator (4 back to 6,
 * 5 back to 6): the table below keeps that loop; Dexed's engine runs
 * it as 6 fed back on itself.
 *
 * The table is written here as who modulates whom, (modulator,
 * target) pairs, checked in Unit_fm_algorithm against Dexed's own
 * table (a byte per operator: the bus it reads, the bus it writes,
 * whether it's heard, whether it's fed back) decoded. A modulator is
 * always a higher operator than its target, so running the operators
 * from 6 down to 1 computes every modulator before its targets.
 *
 * Units: an operator's amplitude is in *cycles* of the phase it
 * modulates (Dx_envelope.gain: 2 at full, 4 pi radians); a carrier's
 * output the same, summed.
 *
 * Worked example (Unit_fm_algorithm): algorithm 1 with only 2 and 1
 * sounding, 2 at 1 / (2 pi) cycles on 1 at 1 cycle: Fm.render's index
 * 1, sample for sample (to 1e-9); a lone operator at full level and 431
 * Hz, fed back: at fb 5 its harmonics 2 to 5 at -7.2, -11.6, -14.8,
 * -17.4 dB, a sawtooth's (-6, -9.5, -12, -14) darkened a little, the
 * noise between them 58 dB down; at fb 6, buzzing, the noise 7.7 dB
 * down; at fb 7 the noise 5.5 dB *above* the harmonics. *)

type t = {
  number : int; (* 1 to 32 *)
  edges : (int * int) list; (* (modulator, target), operators 1 to 6 *)
  carriers : int list; (* the operators heard *)
  feedback : int * int; (* (from, to): to's phase gets from's output *)
}

(* the 32, in order *)
val all : t list

(* [get n]: algorithm [n], 1 to 32 *)
val get : int -> t

(* [modulators alg op]: the operators modulating [op] *)
val modulators : t -> int -> int list

(* the running operators: their phases, their last two outputs *)
type state

val create : unit -> state

(* [sample alg state ~feedback ~increments ~amplitudes]: one sample of
 * the six operators: [increments.(op - 1)] the phase an operator moves
 * a sample (its frequency / the rate), [amplitudes.(op - 1)] its
 * amplitude in cycles; [feedback] 0 to 7. Returns the carriers' sum,
 * in cycles *)
val sample : t -> state -> feedback:int -> increments:float array -> amplitudes:float array -> float

(* [output state op]: operator [op]'s last output, in cycles (for a
 * panel's meters) *)
val output : state -> int -> float
