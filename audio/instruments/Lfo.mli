(* The LFO, a low-frequency oscillator: a wave too slow to hear, used to
 * turn a knob (see notes_synth.md section 4).
 *
 * Between 0.1 and 20 Hz, a wave is not a pitch but a movement: sent to
 * an oscillator's pitch, a vibrato; to a filter's cutoff, a wah that
 * comes and goes; to the volume, a tremolo; to a pulse's width, the
 * shifting timbre of pulse-width modulation. The shapes, over one
 * period, from -1 to 1:
 *
 *     sine    triangle   square    saw up    saw down   sample & hold
 *     .-.       /\        .--.        /|       |\         .--
 *    /   \     /  \       |  |       / |       | \           '--.
 *         '-      \/         '--    /  |       |  \      --'     '--
 *
 * Sine, triangle and square in step with Oscillator's (at a quarter of
 * the period: 1, 1, 1); sample and hold a new random value at the start
 * of each period, held until the next: the burbling "computer" of 1970s
 * films, and a way to vary something a little at every note.
 *
 * Naive waveforms, on purpose: Oscillator.mli's PolyBLEP smooths a jump
 * because its harmonics go past Nyquist and fold back; a 5 Hz square's
 * harmonics reach 22 kHz around the 4,410th, 73 dB below the first --
 * and an LFO's output is never heard anyway, only what it moves. (What
 * it moves may care: a square sent to a volume clicks at each jump. So
 * a destination that must not jump smooths its input, as Instrument.mli
 * ramps its knobs.)
 *
 * Worked example, a vibrato: 6 Hz, 0.3 semitone deep, the frequency
 * times 2^(0.3 x / 12) for the LFO's x from -1 to 1: between 0.98282
 * and 1.01748 of the note (a singer's, and Pitch_effect.mli's Vibrato, which
 * is this rendered ahead of time). Its rate from a tempo: a quarter
 * note at 120 beats a minute is 120 / 60 = 2 Hz, an eighth 4 Hz.
 *
 * The phase goes on from block to block (Instrument.mli's pattern), so
 * a rate changed between blocks changes the speed, not the position.
 *
 * References: the Minimoog's oscillator 3 turned into an LFO by
 * switching it off the keyboard; the ARP 2600's (1971) separate LFO
 * and sample-and-hold; Will Pirkle, Designing Software Synthesizer
 * Plug-Ins in C++, 2014, chapter 5. *)

type shape = Sine | Triangle | Square | Saw_up | Saw_down | Sample_and_hold

val shapes : shape list
val name : shape -> string

(* [value shape phase]: the waveform at [phase], in [0, 1), from -1 to
 * 1 (sample and hold has no formula: 0) *)
val value : shape -> float -> float

type t

(* [create ?seed ()]: at phase 0; [seed] for sample and hold's random
 * values (Noise.lcg), the same values for the same seed *)
val create : ?seed:int -> unit -> t

(* [fill t shape ~rate out]: the next [Array.length out] values, [rate]
 * in Hz *)
val fill : t -> shape -> rate:float -> Signal.t -> unit

(* [unipolar x]: -1..1 as 0..1, for what can't go negative (a tremolo's
 * volume) *)
val unipolar : float -> float

(* [of_tempo ~bpm ~beats]: the rate at which one period lasts [beats]
 * beats (1 a quarter note, 0.5 an eighth, 4 a bar of 4/4) *)
val of_tempo : bpm:float -> beats:float -> float
