(* Media of our own, for TinyMediaPlayer to start from: no song,
 * recording or picture by someone else in the repository (see
 * plan_audio_formats.md), so each is made here, by audio/'s and
 * graphics/'s own writers -- the same bytes a file of that kind has. *)

(* the playlist TinyMediaPlayer starts with, a file's name and bytes
 * each: a round in ABC and the same as a MIDI file, a tune in solfege,
 * TinySoundtracker's song as a MOD (Our_songs.mli), a bell as a WAV, a
 * picture as PNG, GIF and JPEG (the Image examples' demo picture), an
 * animated GIF (a ball bouncing, written here without compression: see
 * Our_media.ml), a sprite as XPM (TinyMario's), and a video, raw as
 * Y4M, FLC, AVI (with a sound) and MPEG-1 (ours, and ffmpeg's), filmed
 * by our own 2D rasterizer; last, the bell and chirps in stereo as MP3
 * and MP2 (LAME's and twolame's, we have no encoder), and the clip with
 * its sound as an .mpg (ffmpeg's); each item's bytes made when first
 * forced *)
val playlist : (string * string Lazy.t) list

(* the frames of our video, drawn by graphics/2d: the pictures the Y4M,
 * FLC and AVI items hold, for the tests to compare with *)
val clip : Rgba_image.t list Lazy.t
