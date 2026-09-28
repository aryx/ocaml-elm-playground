(* Resampling: a recorded sound played faster or slower, or at another
 * rate (see notes_audio.md section 9, "Samples").
 *
 * A synthesized voice changes pitch by changing a number, its
 * frequency. A recording has no such number: to play it an octave up,
 * read it twice as fast, sample 0, 2, 4, ...; a fifth up, 1.5 times as
 * fast: sample 0, 1.5, 3, 4.5, ... -- and there is no sample 1.5. How
 * to make one up is the audio twin of scaling an image
 * (graphics/2d's image filtering), with the same three answers:
 *
 *   nearest   the sample at round(position): cheap, a staircase; the
 *             jumps are new frequencies, distortion (the Amiga's Paula
 *             chip read its samples this way, and a tracker's high
 *             notes had that grit)
 *   linear    between the two neighbours, a straight line: the
 *             staircase's corners gone, much of its distortion too
 *   cubic     a curve through four neighbours (Catmull-Rom): smoother,
 *             closer again
 *
 *       samples   *     *           nearest   *--*  *--*
 *                    *     *                     *--*  *--*
 *       read at 1.5: between them   linear    *-._   _.-*
 *                                                 *-*
 *
 * Example (Unit_resample): a sine read a fifth faster (2^(7/12)) is a
 * sine a fifth higher, and whatever else is in it is the method's
 * error. Its loudest, below the sine, to 689 Hz: nearest -39.6 dB,
 * linear -79.2, cubic -112.5; to 2756 Hz: -27.3, -54.6, -75.3 -- the
 * higher the note, the fewer samples a period to guess between, and
 * the worse, as a finer pattern is for image filtering. (At exactly 1.5
 * the reads fall only on whole and half samples, and the cubic looks
 * better than it is: -133 dB; a real pitch visits every fraction.) The
 * price of reading faster, whatever the method: the
 * recording is shorter by as much (a voice sped up is a chipmunk's, and
 * lasts less: pitch and time go together, on tape and here), and read
 * faster, its highs can pass the Nyquist frequency and fold (an
 * exercise: low-pass first, the way a good sampler does).
 *
 * References: Julius O. Smith III, "Digital Audio Resampling Home
 * Page", https://ccrma.stanford.edu/~jos/resample/; Edwin Catmull,
 * Raphael Rom, "A Class of Local Interpolating Splines", 1974. *)

type kind = Nearest | Linear | Cubic

val kinds : kind list
val name : kind -> string

(* the kind Synth uses to play a recording at another pitch: Linear by
 * default, switched by examples/AudioSampler.ml *)
val kind : kind ref

(* [read kind s position]: [s] at a position between two samples (0 past
 * either end) *)
val read : kind -> Signal.t -> float -> float

(* [faster kind k s]: [s] read [k] times as fast: its pitch times [k],
 * its length divided by [k] *)
val faster : kind -> float -> Signal.t -> Signal.t

(* [to_rate kind rate s]: [s], sampled [rate] times a second, at
 * Signal.rate instead (a file recorded at 22,050, played at 44,100: read
 * at half speed) *)
val to_rate : kind -> int -> Signal.t -> Signal.t
