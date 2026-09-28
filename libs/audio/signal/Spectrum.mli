(* The spectrum: which frequencies a sound contains (see notes_audio.md
 * section 6).
 *
 * Joseph Fourier (1822): every periodic signal is a sum of sines. The
 * discrete Fourier transform (DFT) finds them in N samples: for each
 * frequency bin k (k cycles over the N samples, k rate / N Hz), how
 * much of it there is, by correlating the signal with a sine and a
 * cosine of that frequency:
 *
 *     X[k] = sum over n of x[n] (cos (2 pi k n / N) - i sin (2 pi k n / N))
 *
 * a complex number: its size how much, its angle the phase. N bins, N
 * sums of N terms: N^2 multiplications, 16.8 million for N = 4096.
 * The fast Fourier transform (FFT: Cooley and Tukey, 1965, after Gauss,
 * 1805) gets the same numbers in N log2 N, 49,152 for 4096: split the
 * samples into the even ones and the odd ones, transform each half
 * (recursively), and combine them with a "butterfly" -- the sums of the
 * two halves share all their work. Both here: the simple one checks the
 * fast one (the tests), the fast one is the one used.
 *
 *     amplitude              a 440 Hz square: its odd harmonics, at
 *        |                   1, 1/3, 1/5, ... (Oscillator.mli), bars
 *        |                   at 440, 1320, 2200, ...
 *        |   |
 *        |   |    |    |   |
 *        +---+----+----+---+----> frequency
 *           440 1320 2200 3080
 *
 * Examples, N = 8: an impulse, [1 0 0 0 0 0 0 0], has every bin at 1
 * (it contains all frequencies equally: a click); a cosine making one
 * cycle over the 8 samples has bins 1 and 7 at 4 (N/2: the positive
 * and the negative frequency) and the others at 0.
 *
 * [magnitudes] scales so that a sine of amplitude A shows as A at its
 * bin. A frequency between two bins leaks into its neighbours (the N
 * samples cut the wave in the middle of a period, a jump that isn't
 * in the sound); a window (Hann's: fading the ends to 0) trades that
 * leak for a wider peak: for the display.
 *
 * References: Joseph Fourier, Théorie analytique de la chaleur, 1822;
 * James Cooley, John Tukey, "An Algorithm for the Machine Calculation
 * of Complex Fourier Series", Mathematics of Computation, 1965; Julius
 * O. Smith III, Mathematics of the Discrete Fourier Transform, 2007,
 * https://ccrma.stanford.edu/~jos/mdft/ *)

(* complex numbers, as (real, imaginary) *)
type complex = float * float

(* [dft x]: the definition, N^2 *)
val dft : float array -> complex array

(* [fft x]: the same, N log N; N a power of 2 *)
val fft : float array -> complex array

(* [magnitudes spectrum]: bins 0 to N/2 (the frequencies up to
 * Nyquist; the others mirror them), each 2 |X[k]| / N: a sine's
 * amplitude (the bin 0, the average, |X[0]| / N) *)
val magnitudes : complex array -> float array

(* [bin_frequency ~n k]: k rate / n Hz *)
val bin_frequency : n:int -> int -> float

(* [hann x]: x faded to 0 at both ends (1 - cos), against the leak *)
val hann : float array -> float array

(* [of_signal ?window s]: the magnitudes of the first power of 2 samples
 * of [s] (at most 4096), Hann-windowed by default *)
val of_signal : ?window:bool -> Signal.t -> float array

(* [peak mags]: the bin with the most, 0 excluded *)
val peak : float array -> int
