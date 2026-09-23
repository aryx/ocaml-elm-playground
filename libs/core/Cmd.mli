(* Elm's commands: what [init] and [update] ask the platform to do.

   A program's [update] is pure: it can't download a file, it can only
   say it wants one. It returns, with the new model, a command -- a
   value describing the work -- and the platform (Playground_platform)
   performs it, later, then gives the answer back to [update] as a
   message, like a key press:

     update msg model --(model, Http_get (url, on_answer))--> platform
                                                                 | fetches url,
                                                                 | frames go on
     update (on_answer (Ok text)) model  <-----------------------'

   So a test can look at the command [update] returned, without a
   network, and the same program runs natively and in a browser, each
   platform performing the command its own way.

   A command that reaches the network carries the program's capability
   to do so (Cap.network, plan_caps.md): it was checked where the
   command was built, in the program, and the platform only performs
   it -- a program that wasn't given the network can't even write one.

   Programs build them with Playground.Http.get (Elm's spelling); the
   constructors are here for the platforms. *)

(* Elm's Http.Error; [Network_error] says why (Elm's doesn't) *)
type http_error =
  | Bad_url of string
  | Timeout
  | Network_error of string
  | Bad_status of int (* the server answered, but not 2xx: 404 *)
  | Bad_body of string

type 'msg t =
  | None
  (* the message given back to [update] at the next frame *)
  | Msg of 'msg
  (* a GET of the URL, its body given back as a message (or why not);
   * with the authority to reach the network *)
  | Http_get of Cap.network * string * ((string, http_error) result -> 'msg)
  | Batch of 'msg t list

(* nothing to do *)
val none : 'msg t

(* several commands, performed in parallel, their answers coming back
 * in any order: Elm's Cmd.batch *)
val batch : 'msg t list -> 'msg t

(* the commands of a batch, flattened, None dropped: what a platform
 * performs *)
val to_list : 'msg t -> 'msg t list
