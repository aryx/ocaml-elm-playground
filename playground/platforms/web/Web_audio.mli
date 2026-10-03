(* Web_audio: the Playground's sound in a browser, through the Web Audio
 * API. The same sound as natively, every sample ours (Audio.pull, the
 * Mixer): the browser only plays them.
 *
 * Each frame, the samples its audio clock will need next are copied
 * into an AudioBuffer and scheduled right after the previous one, so
 * that they play back to back with no gap:
 *
 *     the audio clock ----now-------------------------------->
 *     scheduled       ...====|=======|=======|
 *                                             ^ next_start: this
 *                                               frame's buffer goes here
 *                            <---- ahead ---->
 *
 * How far ahead is the trade: a frame later than that and the clock
 * runs past the end of the sound, a gap, heard as a cut; further ahead
 * and a key is heard later. So it starts at 50 ms (three frames, the
 * native queue's), grows by 30 ms at each gap, up to 300 ms, and comes
 * back by 12 ms a second without one: a program that keeps up loses
 * nothing, a slow page pays in latency instead of in cuts.
 *
 * Browsers start an AudioContext suspended until the page gets a click
 * or a key (their autoplay policy); until then the samples are pulled
 * and dropped, so that sounds do not pile up.
 *
 * References: https://www.w3.org/TR/webaudio/
 *)

(* after a frame's [ticks] updates: their samples (44,100 a second,
 * 735 a tick) pulled from the mixer and scheduled *)
val play_audio : int -> unit

(* called on an input event: the context resumed if the browser had it
 * suspended *)
val resume_audio : unit -> unit
