(* Ogg: a file of packets -- the container of Vorbis, Theora and Opus,
   read as far as its packets.

   A codec's output is packets, of any size; a file or a stream needs
   them delimited, and a listener who joins in the middle needs a
   place to start. Ogg (Xiph.Org, 2000) cuts the packets into *pages*:

     "OggS"            where a page starts: what to look for after a
                       loss, or when tuning in
     version, flags    a first page, a last one, a packet continued
     granule position  the time, in the codec's own unit (samples)
     serial, sequence  which stream of the file, which page of it
     a checksum
     a segment table   up to 255 lengths of up to 255 bytes: a packet
                       is the segments up to one shorter than 255 (so
                       a packet of 255 bytes is 255 then 0); it may go
                       on in the next page

   so that a page is found, checked and skipped without knowing what
   is inside. Read here: the packets, in order, of a file with one
   stream. Not checked: the checksum; not used: the times.

   Reference: RFC 3533, The Ogg Encapsulation Format Version 0 (2003).
   The name is from a move in the game Netrek, not from Terry
   Pratchett's Nanny Ogg (Vorbis is from his Small Gods). *)

(* a file's packets, in order *)
val packets : string -> string list

(* how many samples a stream of sound has, each channel: its last
 * page's granule position (the last block decoded gives more, the
 * encoder's padding: to cut) *)
val length : string -> int option
