(* ID3: the tags of an MP3, what a music player shows instead of a file's
 * name -- the artist, the title, the album.
 *
 * MPEG audio has nowhere to put them: a file is frames and nothing else
 * (Mpeg_audio_header.mli). Eric Kemp's ID3 (1996) put them where a
 * decoder, looking for the next frame's sync, skips them as junk: the
 * file's last 128 bytes, fixed fields padded with zeros or spaces.
 *
 *     "TAG"  title 30  artist 30  album 30  year 4  comment 30  genre 1
 *
 * ID3v1.1 (Michael Mutschler, 1997) took the comment's last two bytes
 * for the track's number: a zero, then the number. Thirty bytes are few
 * ("The Rise and Fall of Ziggy Stardust and the Spiders from Mars" does
 * not fit), and a tag at the end is read last -- over a modem, when the
 * song is over. ID3v2 (Martin Nilsson, 1998) went first, and open-ended:
 *
 *     "ID3" major rev flags size(4 x 7 bits)   then frames:
 *     id(4) size(4) flags(2) data   ...        (v2.2: id(3) size(3))
 *
 * a frame per field, named by four letters -- TIT2 the title, TPE1 the
 * artist, TALB the album, TRCK the track, TSSE the encoder's settings --
 * a text frame's data an encoding's byte then the text: 0 Latin-1, 1
 * UTF-16 after a byte order mark, 2 UTF-16 big-endian, 3 UTF-8. The
 * sizes are "syncsafe", 7 bits a byte, so that no byte of the tag's
 * header is FF, the start of a frame's sync; v2.4 made the frames'
 * sizes syncsafe too (v2.3's are plain), a difference every reader has
 * to know. Mpeg_audio_header.first_frame skips a v2 tag the same way.
 *
 * Winamp (1997) showed "%artist% - %title%" when it had both, the file's
 * name otherwise ([display]). Not read here: unsynchronisation (an FF
 * followed by an inserted 00, for the hardware players of 1998),
 * compressed and encrypted frames, pictures (APIC), and v1's genre as a
 * name (a byte, an index in Winamp's list of 148).
 *
 * References: https://id3.org/ID3v1, https://id3.org/id3v2.3.0,
 * https://id3.org/id3v2.4.0-structure (mirrored at
 * https://mutagen-specs.readthedocs.io/en/latest/id3/). *)

type t = {
  title : string;
  artist : string;
  album : string;
  year : string;
  track : int option;
}

(* no field at all *)
val empty : t

(* [v1 bytes]: the ID3v1 tag at the end of [bytes], its fields without
 * their padding; None if the last 128 bytes don't start with "TAG" *)
val v1 : string -> t option

(* [v1_to_string t]: the 128 bytes, an ID3v1.1 tag (a track number if
 * there is one), each field cut to its size; genre 255, none *)
val v1_to_string : t -> string

(* [v2_frames bytes]: an ID3v2 tag's text frames at the start of
 * [bytes], each frame's id and its text in UTF-8 (v2.2's three-letter
 * ids as they are); [] if there is no tag *)
val v2_frames : string -> (string * string) list

(* [read bytes]: the fields from both tags, v2's first, v1's for the
 * ones v2 hasn't; None if there is neither tag *)
val read : string -> t option

(* [display ~name tag]: what Winamp's playlist shows, "artist - title",
 * the title alone, or else the file's name without its extension *)
val display : name:string -> t option -> string
