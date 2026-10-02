(* Vorbis: the free codec of sound, decoded -- what an .ogg file holds,
   and a WebM file's first sound.

   In September 1998 the Fraunhofer institute wrote to the authors of
   MP3 encoders that the format was patented and its licence had a
   price. Christopher Montgomery ("Monty"), who had been working on a
   codec of his own, set out to finish one that nobody would have to
   pay for: Vorbis, of the Xiph.Org Foundation, its format frozen in
   2000 and its version 1.0 out in July 2002. It is a little better
   than MP3 at the same rate, and what it has kept are the places
   where a licence could not be asked for: games (a console's or a
   telephone's music, for twenty years), Wikipedia's sounds, Spotify's
   streams, and the web, where Firefox 3.5 played it with <audio> in
   2009 and where WebM (2010) made it VP8's companion. Opus (2012),
   from the same people, has replaced it in what is made now.

   It is a transform codec, as MP3 and AAC are: sound cut in blocks
   that overlap by half, each block turned into its frequencies (the
   MDCT), the frequencies written with as few bits as an ear allows.
   A packet is a block, and its frequencies are sent as two things
   multiplied:

     the floor     the spectrum's outline: a curve of a few points
                   joined by lines, in decibels -- how loud each
                   region is, which is also how coarse the ear lets
                   it be
     the residue   what is left when the spectrum is divided by its
                   floor: numbers near 0, sent as vectors of a
                   codebook, in passes each finer than the last

     packet --> mode (long or short block) --> floors, residues
            --> two channels uncoupled --> floor x residue
            --> inverse MDCT --> window --> added to the block before

   Three things are its own.

   The codebooks are in the stream. MP3's Huffman tables are in the
   standard; a Vorbis file starts with three headers, and the third
   (the setup: most of the first kilobytes of an .ogg) holds every
   table the decoder will need: the Huffman codes, the vectors they
   stand for, the floors' points, the modes. A decoder knows almost
   nothing; an encoder can be improved for years without a decoder
   changing, which is what happened. The cost is the start: a few
   kilobytes before the first sound, too much for a telephone call,
   and one of the reasons for Opus.

   A code is not sent, only its length, entry by entry; the codes are
   the ones given by taking, for each entry in turn, the first free
   code of that length, lowest first ([codewords]). The lengths
   2 4 4 4 4 2 3 3 give

     00  0100  0101  0110  0111  10  110  111

   and an entry of length 0 is not used.

   An entry is a number (a floor's point) or a vector (a residue's):
   the vectors are either written out, or the points of a lattice --
   each coordinate one of a few values, the entry's number read as
   digits in that base.

   Two channels are coupled without loss ("square polar mapping"):
   one carries the louder of the two, the other how far apart they
   are, which for sounds in the middle is near nothing. The residue
   of the two is then interleaved and coded as one vector (residue
   kind 2), so that a codebook's vector spans both.

   The blocks are of two sizes (256 and 2048 samples for 44,100 Hz):
   long ones, fine in frequency, for a held note; short ones, fine in
   time, for a drum, whose noise a long block would spread before the
   stroke (the pre-echo). Each block is multiplied by a window whose
   slopes cross their neighbours', and the overlapping halves are
   added: the errors of the transform, which folds a block onto half
   as many numbers, cancel between a block and the next (Princen and
   Bradley's time-domain aliasing cancellation). So a packet gives no
   sound by itself: the first decoded gives none, and each one after
   gives what lies between its middle and the middle of the one
   before.

   The transform is the slow part: n/2 frequencies to n samples, by
   its definition n^2/2 cosines. Here it is a cosine transform
   ([dct4_simple], the definition; [dct4], the same by a Fourier
   transform of a quarter of the block), unfolded by its symmetries.

   Not done: floors of kind 0 (line spectral pairs: the first
   encoders', none since 2001), a file with several streams, seeking,
   the comment header (a title, an artist: skipped).

   A packet cut short is not an error: what was read of it is played
   (an encoder may cut a packet to keep its rate).

   What ffmpeg's own decoder gives for a file of ffmpeg's own encoder
   is not what libvorbis gives; the tests compare with libvorbis, the
   reference.

   References: Xiph.Org Foundation, Vorbis I specification (the
   codebooks, section 3; the packet, section 4; floor 1, section 7;
   residues, section 8); J. Princen and A. Bradley, Analysis/synthesis
   filter bank design based on time domain aliasing cancellation, IEEE
   Transactions on Acoustics, Speech, and Signal Processing, 1986;
   RFC 5215 (Vorbis over RTP, where the three headers are told
   apart from the sound). Ogg.mli for the file around it. *)

(* a decoder, and the half block it keeps from the packet before *)
type t

(* from the first and third of a stream's three headers (the second is
 * the comments); fails (Failure) on headers that are not Vorbis's *)
val create : identification:string -> setup:string -> t

val channels : t -> int

(* samples a second *)
val rate : t -> int

(* [decode t packet]: the samples a packet finishes, each channel's,
 * from -1 to 1 (none for the first packet, or one that is not sound) *)
val decode : t -> string -> float array array

(* a stream's packets, its three headers first: the decoder and the
 * whole sound, each channel *)
val of_packets : string list -> t * float array array

(* an .ogg file's sound, cut at the length it says *)
val of_ogg : string -> t * float array array

(* each entry's code from the entries' lengths, -1 for an entry of
 * length 0 (the worked example) *)
val codewords : int array -> int array

(* u.(n) = the sum over k of x.(k) cos (pi / m (n + 1/2) (k + 1/2)), by
 * the definition; and by a Fourier transform (m a power of two) *)
val dct4_simple : float array -> float array

val dct4 : float array -> float array
