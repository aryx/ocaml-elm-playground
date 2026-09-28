(* Golden WAVs: a sound compared sample by sample with the one approved
 * after listening, like the golden frames (tests/common). For the tests
 * of what plays sound outside audio/ itself: the formats' players, the
 * music applications' voices. (audio/tests' Golden_wav has its own,
 * the first one.)
 *
 * Run from the test's directory in the build: [name].wav written to
 * actual/, compared with golden/ (the test's dune file's deps); no
 * golden yet, or a sample that differs, fails with where to listen. *)

(* [check ~dir name samples]: mono; [dir] the test directory, for the
 * message ("apps/music/tests") *)
val check : dir:string -> string -> Signal.t -> unit

(* [check_stereo ~dir name samples]: the same for two channels, each as
 * its own mono file, [name]_left.wav and [name]_right.wav (Wav.read
 * mixes a stereo file down) *)
val check_stereo : dir:string -> string -> Signal.stereo -> unit
