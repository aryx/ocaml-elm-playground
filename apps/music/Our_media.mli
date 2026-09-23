(* Media of our own, for the music programs to start from: no song,
 * recording or picture by someone else in the repository (see
 * plan_audio_formats.md), so each is made here, by audio/'s and
 * graphics/'s own writers -- the same bytes a file of that kind has. *)

(* TinySoundtracker's song: four instruments synthesized here (a pulse
 * lead, a triangle bass, a kick, a snare), two patterns (a melody, then
 * its chords as arpeggios, over the same bass and drums) *)
val soundtracker_song : Mod.song

(* the playlist TinyMediaPlayer starts with, a file's name and bytes
 * each: a round in ABC and the same as a MIDI file, a tune in solfege,
 * the tracker's song as a MOD, a bell as a WAV, a picture as PNG, GIF
 * and JPEG (the Image examples' demo picture), an animated GIF (a ball
 * bouncing, written here without compression: see Our_media.ml), a
 * sprite as XPM (TinyMario's) *)
val playlist : (string * string) list
