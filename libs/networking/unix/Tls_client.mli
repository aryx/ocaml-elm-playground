(* Tls_client: TLS 1.3 over a real socket -- our own (Tls13.mli), with
   the system's trusted roots.

   Tls13 is a pure machine, bytes in and bytes out; this is the rest:
   the TCP connection (Tcp.mli), 96 bytes of randomness from
   /dev/urandom (the hello's random, the session id, the X25519 key --
   a pure machine rolls no dice), the roots read once from the system's
   bundle (/etc/ssl/certs/ca-certificates.crt, or where the other
   systems keep it), the chain checked with them at the time of day
   (X509.verify), and the loop that carries the bytes between the socket
   and the machine, never waiting.

   Reading the system's roots and the kernel's randomness is part of
   what reaching a host over TLS means, so it is done under the same
   authority, Cap.network -- as curl, which this replaces, did.

   Two faces: a Transport.t of lines (CR LF), for POP3 and SMTP
   (TinyEudora's Gmail), installed as Transport.tls by the native 2D
   platforms; and [exchange], one request and its whole answer, for
   HTTPS (Http_client).

   Worked examples (checked by the tests): a handshake with a local
   `openssl s_server` over each of our two ciphers, its self-signed
   certificate the one root trusted, a page asked and received; the same
   refused when the root is not trusted, or the host is another. By
   hand: Gmail's POP3 and SMTP, and the web servers TinyChrome visits. *)

type t

(* the system's roots, read once *)
val system_roots : < Cap.network ; .. > -> X509.t list

(* [connect caps ~host ~port ()]: the TCP connection made (waiting for
 * it) and the ClientHello sent; the handshake goes on in [step].
 * [trust]: the roots, the system's unless given *)
val connect : < Cap.network ; .. > -> ?trust:X509.t list -> host:string -> port:int -> unit -> (t, string) result

(* what can be done without waiting: bytes read and given to the
 * machine, its answers written *)
val step : t -> unit

val state : t -> Tls13.state

(* the machine, to ask it what it saw (the chain, the cipher) *)
val machine : t -> Tls13.t

(* application data: sent once the handshake is done (queued before) *)
val send : t -> string -> unit

(* the application data arrived since the last call *)
val receive : t -> string

val close : t -> unit

(* the connection as lines, CR LF, for Transport.tls *)
val lines : t -> Transport.t

(* the same, opened: what Transport.set_tls installs *)
val connect_lines : < Cap.network ; .. > -> host:string -> port:int -> (Transport.t, string) result

(* [exchange caps ~host ~port request]: connect, send [request], read
 * until the server closes (or [timeout] seconds of silence) *)
val exchange : ?trust:X509.t list -> ?timeout:float -> < Cap.network ; .. > -> host:string -> port:int -> string -> (string, string) result
