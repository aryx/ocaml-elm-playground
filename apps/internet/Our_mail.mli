(* TinyEudora's built-in mailboxes, so that it shows something with no
 * server, and the golden frames something deterministic -- as the
 * browsers' built-in site (Site.mli) is. Their files are mail/*.mbox,
 * embedded by dune at build time (Our_mail_files).
 *
 * The messages were written for what they show (plan_tiny_eudora.md):
 * in In, a thread of five replies (their References, for threading), a
 * forged sender whose envelope and Received: line give it away, a
 * digest of two messages, a picture attached (base64), French in
 * quoted-printable with an encoded-word subject, a line starting with
 * "From " (">From" in the file), and the lunch of Mail.mli's worked
 * example; in Out, a reply sent; one message each in Trash and in the
 * user's own mailbox, Projects. The reader is bob@tiny. *)

(* the mailboxes, in the order of Eudora's Mailbox menu: their names
 * and their mbox files *)
val mailboxes : (string * string) list
