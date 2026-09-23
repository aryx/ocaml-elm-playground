(* The transport of a browser (Transport.mli), what the web platforms,
 * 2D and 3D, install (Transport.set_connect): net=relay over the page's
 * own WebSocket, the packets as binary messages. A web page has no
 * UDP: net=host and net=join are refused, for native programs only. *)
val connect : Cap.network -> Transport.role -> (Transport.t, string) result
