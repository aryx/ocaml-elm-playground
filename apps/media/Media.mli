(* Media: what a file is, and what to do with it -- the part of a media
 * player (VLC, 2001; Windows' Media Player, 1991) before any playing.
 *
 * A player is given bytes and a name, and the name can lie (a PNG saved
 * as .wav) or say nothing (a file from a URL). So it looks at the bytes
 * first: nearly every format begins with a **magic number**, a few
 * bytes chosen to be unlikely anywhere else, which is how Unix's file(1)
 * (1973) and VLC's demuxers, trying each in turn, recognize a file:
 *
 *     "RIFF" .... "WAVE"        WAV (RIFF, Microsoft and IBM, 1991)
 *     "MThd"                    a Standard MIDI File (1988)
 *     "M.K." at byte 1080       a MOD (and the other tags: Mod.mli)
 *     89 "PNG" 0D 0A 1A 0A      PNG (1996): a high byte, then a DOS and a
 *                               Unix newline and a DOS end-of-file, so a
 *                               transfer that mangles either shows
 *     "GIF87a", "GIF89a"        GIF (1987, 1989)
 *     FF D8 FF                  JPEG (a start-of-image marker, then the next)
 *     "/* XPM */"               XPM (a C comment: the file is C source)
 *     "X:" first                an ABC tune (its first field, the number)
 *
 * and only then the name: Ultimate Soundtracker's modules have no tag
 * (.mod), and a tune in solfege is plain text (.doremi, .txt).
 *
 * Opened, a file is one of four things to a player: a [Sound], samples
 * to play (a recording, or a tune rendered by audio/'s synthesizer,
 * with its notes for a piano roll); a [Module], a song played live by
 * its own player (Mod_player.mli), too long to render ahead; a
 * [Picture]; a [Movie], pictures in time, decoded as they're shown
 * (Movie.mli): so far a GIF's frames, and as graphics/videos/ grows,
 * the video formats (plan_video_teaching.md). *)

type kind = Wav | Midi | Mod | Abc | Solfege | Png | Gif | Jpeg | Xpm

val kind_name : kind -> string

(* [sniff ~name bytes]: what it is, by its bytes, else by its name's
 * extension; None if neither says *)
val sniff : name:string -> string -> kind option

type media =
  | Sound of { samples : Signal.stereo; notes : Midi.note list (* none for a recording *) }
  | Module of Mod.song
  | Picture of Rgba_image.t
  | Movie of Movie.t

(* [open_ ~name bytes]: what it is and what it holds, or why not *)
val open_ : name:string -> string -> (kind * media, string) result

(* how long a sound or a movie lasts, in seconds; None for the others *)
val duration : media -> float option
