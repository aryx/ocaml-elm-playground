(* The VCO, a synthesizer's oscillator, played live (see notes_synth.md
 * section 3).
 *
 * Oscillator.mli has the waveforms, a formula of the phase, and
 * PolyBLEP; this is the oscillator of a synthesizer's voice, run a
 * block at a time (Instrument.mli's pattern), its phase kept from block
 * to block, with what the voice does to it:
 *
 *  - its frequency given per *sample*, a block of them: a vibrato (an
 *    LFO), a glide, a pitch wheel move it smoothly within a block (the
 *    V in VCO: a voltage controlling the frequency, continuously);
 *  - the pulse's width given per sample too: pulse-width modulation, an
 *    LFO moving the width, so the harmonics' recipe (Oscillator.mli's
 *    |sin (pi k width)|) shifts all the time, one oscillator sounding
 *    like several (the Juno's strings);
 *  - hard sync: restarted by another oscillator, the master, at each of
 *    its periods.
 *
 * {1 Hard sync}
 *
 *     master  /|  /|  /|          the slave restarted at each of the
 *            / | / | / |          master's periods: the master's
 *     slave /|/|/|/|/|/|/         pitch, the slave's frequency now its
 *           ^   ^   ^             *shape* (how many of its periods fit,
 *           restarts              and where the last one is cut)
 *
 * Sweeping the slave's frequency then sweeps the harmonics like a vowel,
 * the pitch staying put: the Prophet-5's sync lead (1978).
 *
 * The restart is a jump, of height h = (the wave at 0) - (the wave where
 * the slave was), and it falls *between* two samples: the master's phase
 * reaches 1 at a fraction of the step from one sample to the next. The
 * BLEP (Oscillator.mli) corrects a jump over the sample before and the
 * sample after; placed at the right fraction, with d the time from the
 * jump to the next sample (in samples, 0 to 1):
 *
 *     the sample before:  + h/2 d^2
 *     the sample after:   + h/2 (2d - d^2 - 1)
 *
 * (PolyBLEP's two halves for a jump of 2, at u = d - 1 and u = d,
 * scaled by h/2.) The sample before is computed knowing the jump is
 * coming: the master says, at each sample, whether and where it wraps
 * before the next one. The slave's own jump at its phase 0 is then not
 * the one that happened, so its own correction is left out there.
 *
 * Measured (Unit_vco): a sawtooth synced to a 1001 Hz master at 2.37
 * times its frequency, its loudest alias below 5 kHz from -29.5 dB
 * (naive) to -69.9 dB (corrected); corrected but as if the restart
 * fell on the next sample, -30.7, hardly better than naive: the
 * restarts land anywhere between samples, and a jump moved to the
 * sample is a jump at the wrong time, which is itself an alias. The
 * fraction is the point.
 *
 * References: Eli Brandt, "Hard Sync Without Aliasing", ICMC 2001;
 * Vesa Välimäki, Antti Huovilainen, "Oscillator and Filter Algorithms
 * for Virtual Analog Synthesis", Computer Music Journal 30(2), 2006. *)

type shape = Sine | Triangle | Sawtooth | Pulse

val shapes : shape list
val name : shape -> string

type t

(* at phase 0 *)
val create : unit -> t

(* [fill ?band_limited ?width ?sync t shape ~frequency out]: the next
 * [Array.length out] samples of [t], [frequency] (in Hz) and [width]
 * (a pulse's, 0.5 by default: the square; kept within 0.01 and 0.99)
 * one per sample; restarted by [sync], filled for the same block just
 * before; band-limited unless [band_limited] is false *)
val fill :
  ?band_limited:bool -> ?width:Signal.t -> ?sync:t -> t -> shape -> frequency:Signal.t -> Signal.t -> unit

(* [fill_sync_at_sample]: the same, the restarts' jumps corrected as if
 * they fell on the sample (Unit_vco's comparison) *)
val fill_sync_at_sample : ?width:Signal.t -> sync:t -> t -> shape -> frequency:Signal.t -> Signal.t -> unit
