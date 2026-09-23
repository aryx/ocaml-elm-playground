(* Paula: one of the Amiga's four sound channels (see
 * notes_audio_formats.md section 1).
 *
 * The Amiga's custom chip Paula (Glenn Keller, 1985) played four
 * channels of 8-bit samples straight from memory, each on its own
 * clock: a channel counts down from its *period*, 3,546,895 times a
 * second, and at 0 it moves to the next byte and starts again. So the
 * period is the pitch, the sample rate being 3,546,895 / period, and
 * the chip never mixes nor filters: each byte is simply held until the
 * next (a "sample and hold") and scaled by the channel's volume, 0 to
 * 64. Here, a channel computed at 44,100 samples a second:
 *
 *     sample:  bytes k, k+1, ...    read at rate / 44,100 bytes a sample
 *                                    position += 8,287 / 44,100 = 0.188
 *     byte  |‾‾‾‾‾|_____|‾‾‾‾      for C-2: each byte held for 5.3 of ours
 *            k     k+1   k+2
 *
 * and when the position passes the sample's end, it goes back by the
 * loop's length if the sample loops (a sustained note), or the channel
 * falls silent.
 *
 * How to read between two bytes is the teaching switch: [Hold], as
 * Paula did -- its steps are a sampled signal's images, the bright,
 * slightly gritty sound of Amiga music, which a composer counted on --
 * or [Linear] and [Cubic] (Resample.mli), smoother, as today's players
 * offer. (A sample read faster than it was recorded should be low-passed
 * first, notes_audio.md section 12's exercise: not done here either.)
 *
 * Worked example: a 32-byte looped square at C-2 (period 428, 8,287.1
 * Hz) sounds 8,287.1 / 32 = 259.0 Hz; at C-3 (214), twice that, 517.9
 * Hz.
 *
 * References: Jay Miner, Joe Decuir, Glenn Keller: the Amiga's chip set;
 * Commodore, Amiga Hardware Reference Manual, 1985 (the audio chapter:
 * periods, volumes, the four DMA channels). *)

type reading = Hold | Linear | Cubic

val readings : reading list
val reading_name : reading -> string

(* a channel *)
type t

(* silent *)
val create : unit -> t

(* [trigger c sample ~loop_start ~loop_length ~offset]: [sample] (-1 to
 * 1) from byte [offset], looping over [loop_start, loop_start +
 * loop_length) once there if [loop_length] > 0 *)
val trigger : t -> Signal.t -> loop_start:int -> loop_length:int -> offset:int -> unit

(* the period (the pitch) and the volume (0 to 64), changed while it
 * plays *)
val set_period : t -> int -> unit
val set_volume : t -> int -> unit
val stop : t -> unit

(* [next reading c]: its next sample, at Signal.rate, times its volume /
 * 64 *)
val next : reading -> t -> float

val playing : t -> bool
