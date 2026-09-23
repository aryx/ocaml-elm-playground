(* [check name samples]: [samples] written to actual/[name].wav and
 * compared sample by sample (as 16-bit values) with golden/[name].wav,
 * failing with where they differ, or that there is no golden yet;
 * audio/tests' Golden_wav does the same for audio/'s sounds *)
val check : string -> Signal.t -> unit
