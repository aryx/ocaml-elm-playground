(* Websocket: a two-way stream of messages, the only socket a browser has.

   A web page can't open a TCP or UDP socket: it could scan the local
   network, or pretend to be a mail client. What it can do is HTTP, and
   WebSocket (RFC 6455, 2011) is HTTP turned into a lasting two-way
   channel: the page asks, in HTTP, to change protocols, and the server
   agrees; from then on the same TCP connection carries *messages*, in
   both directions, whenever either side wants.

     browser                                   server
     GET /play HTTP/1.1
     Upgrade: websocket
     Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==      ----->
                                               HTTP/1.1 101 Switching Protocols
                                               Upgrade: websocket
                       <-----                  Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
     frames, both ways, as long as it lasts

   The accept is the proof the server read the request (and isn't a
   plain web server fooled into it): base64 (SHA-1 (key ^
   "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")), the RFC's fixed GUID
   (Sha1.mli). The example above is the RFC's own (section 1.3).

   A message is a frame -- two bytes of header, then the payload:

      FIN  opcode       MASK  length (7 bits: <126 as is; 126: 16 bits
       |   (1 text,      |    follow; 127: 64 bits follow)
       |    2 binary,    |       |
       v    8 close...)  v       v
      [1 000 0010]      [1 0000101]  [mask: 4 bytes]  [payload, xored]

   A frame from a client is *masked*: its payload xored with 4 bytes
   the client chose, byte i with mask[i mod 4]. Not for secrecy (the
   mask is in the frame): so that a script in a page can't make the
   bytes on the wire look like a request of another protocol to a proxy
   on the way (the "cache poisoning" attacks of 2010, which is why the
   mask must be unpredictable). Frames from the server aren't masked.

   Worked examples (checked by the tests, RFC 6455 section 5.7): "Hello"
   unmasked is 81 05 48 65 6c 6c 6f; masked with 37 fa 21 3d it is
   81 85 37 fa 21 3d 7f 9f 4d 51 58; 256 bytes of binary begin 82 7E 01
   00 (126, then the length in 16 bits), 65,536 begin 82 7F 00 00 00 00
   00 01 00 00.

   Not done: fragmented messages (FIN = 0, then continuation frames;
   our messages are small and whole), extensions, subprotocols;
   ping/pong and close are decoded, and are the caller's business.

   References: RFC 6455, "The WebSocket Protocol" (Ian Fette and Alexey
   Melnikov, 2011); Lin-Shung Huang et al., "Talking to Yourself for Fun
   and Profit" (W2SP 2011), the attacks masking stops. *)

(*****************************************************************************)
(* The handshake *)
(*****************************************************************************)

(* the Sec-WebSocket-Accept for a Sec-WebSocket-Key *)
val accept : string -> string

(* the client's request, for [path] on [host] ("127.0.0.1:8765") *)
val request : host:string -> path:string -> key:string -> string

(* the server's answer, for the key it received *)
val response : key:string -> string

(* in bytes received: the handshake (a request or a response) if it is
 * whole -- up to its empty line -- its headers, and where what follows
 * it starts *)
val handshake : string -> ((string * string) list * int) option

(*****************************************************************************)
(* Frames *)
(*****************************************************************************)

type opcode = Continuation | Text | Binary | Close | Ping | Pong

type frame = { fin : bool; opcode : opcode; payload : string }

(* a frame's bytes; masked with [mask] (4 bytes) if given, as a
 * client's must be *)
val encode : ?mask:string -> frame -> string

type decoded =
  | Frame of frame * int (* the frame, and its length in the bytes *)
  | Incomplete (* more bytes needed *)
  | Bad of string (* not a frame: close the connection *)

(* the frame at the start of [bytes] (unmasked if it was masked) *)
val decode : string -> decoded
