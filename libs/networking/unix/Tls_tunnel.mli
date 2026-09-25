(* Tls_tunnel: a TLS connection through someone else's program, until
   TLS is ours (plan_teaching_other.md).

   A server of today speaks its protocol -- POP3, SMTP -- only inside
   TLS: Gmail's POP3 on 995, not 110. TLS (the handshake, the
   certificates, the ciphers) is a book of its own, so for now it is
   done by openssl, run beside us, and we talk to it in plain lines:

       Pop3's machine --lines--> openssl s_client --TLS--> pop.gmail.com:995
                      <-lines--  (a pipe each way)  <-TLS--

   which is how stunnel (1998) gave TLS to the programs of the 1990s
   that knew only plain text. openssl checks the server's certificate
   (-verify_return_error: a certificate that does not check out ends
   the connection, rather than a warning nobody reads), and sends our
   lines with CR LF, as the protocols want them.

   A Transport.t: [send] a line, [receive] the complete lines arrived
   (never waiting: the pipe is non-blocking), [status] what is going
   on. The program ends when the server closes the connection, after
   QUIT. Installed as Transport.tunnel by the native 2D platforms. *)

val connect : < Cap.exec ; .. > -> host:string -> port:int -> (Transport.t, string) result
