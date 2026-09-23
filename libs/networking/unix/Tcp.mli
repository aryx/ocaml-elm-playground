(* Tcp: a connection to another computer, the operating system's side.

   Everything in networking/ is bytes in, bytes out; this module is
   where the bytes leave the machine. TCP (Vint Cerf and Bob Kahn, 1974;
   RFC 793, 1981) gives two programs a *stream*: the bytes written at
   one end arrive at the other, all of them, in order -- over a network
   that loses, duplicates and reorders packets (the retransmissions and
   the reordering are the kernel's work, not ours). The Berkeley sockets
   (4.2BSD, 1983) are the API every system kept:

     client                                server
     getaddrinfo "elm-lang.org" -> address
     socket                                socket, bind, listen
     connect  --------------------------->  accept
     write "GET / ..."  ----------------->  read
     read  <------------------------------  write "HTTP/1.1 200 ..."
     read = 0 (end of stream) <-----------  close
     close

   A name becomes an address through DNS (getaddrinfo asks the
   system's resolver; a name may have several addresses, IPv4 and IPv6,
   tried in the order given until one answers). A stream has no
   messages, only bytes: a read gives whatever has arrived, maybe half a
   header, so a protocol must say where its messages end (Http.mli) --
   or, as here, read until the other side closes.

   Every read and write gives up after [timeout] seconds of silence
   (SO_RCVTIMEO, SO_SNDTIMEO), so a server that never answers doesn't
   freeze the program; the connect itself still waits for the kernel's
   own timeout (a minute or two) on a host that doesn't answer at all.

   Reaching another computer is an authority, not a right: every
   function here takes a capability, [< Cap.network; .. >] (plan_caps.md),
   which a program gets only from Cap.main and hands down to the code it
   trusts with it (the open row [..]: any capabilities that include the
   network, with no coercion at the call); the
   capability is asked for the host before any socket is opened. A
   function without one in its type can't reach the network.

   Reference: W. Richard Stevens, "UNIX Network Programming" (1990;
   the calls above in the third edition's volume 1, 2003, chapter 4,
   "Elementary TCP Sockets");
   RFC 793 (TCP) and RFC 791 (IP), Jon Postel (1981). *)

(* a connection to [host] (a name, or an address: "127.0.0.1", "::1")
 * on [port], each of its addresses tried in turn; raises Unix.Unix_error
 * (or Failure for a name that doesn't resolve) *)
val connect : ?timeout:float -> < Cap.network ; .. > -> host:string -> port:int -> unit -> Unix.file_descr

(* write all of [s] (a write may take only part of it) *)
val send_all : Unix.file_descr -> string -> unit

(* read until the other side closes the connection *)
val receive_all : Unix.file_descr -> string

(* [exchange ~host ~port s]: connect, send [s], read everything the
 * other side sends until it closes, close: one request of a protocol
 * that closes after answering (HTTP with "Connection: close") *)
val exchange : ?timeout:float -> < Cap.network ; .. > -> host:string -> port:int -> string -> string
