(* Worker: a pool of threads, for the calls that block and can't be
   turned into a state machine cheaply.

   Http_request never blocks, except for one call: getaddrinfo, the
   host's name resolved by the C library, which waits for a DNS server
   (milliseconds from the cache, seconds from a slow network). curl's
   https:// (Commands) blocks the whole way. Netscape's answer (NSPR,
   its portable runtime, 1994-1998) was threads: the blocking call made
   on another thread, the window's thread going on. That is this
   module: [submit] gives a job to the pool and returns at once; one of
   the pool's threads runs it; the frame loop [poll]s the job each
   frame, as it steps an Http_request.

     frame loop (main thread)             pool (n threads)
     ------------------------             ----------------
     submit f ---> queue [f; ...] ------> wait for a job   (Condition.wait)
       (Condition.signal)                 run f ()         (blocks: DNS)
     poll job: None                         ...
     poll job: None                       job.result <- r  (under the mutex)
     poll job: Some r  <------------------'

   The queue and each job's result are shared between threads, so each
   is read and written under the pool's mutex (Mutex, Condition:
   Dijkstra's and Hoare's monitors, 1965-1974, as POSIX threads spell
   them).

   What OCaml 4.14's threads give, precisely: **concurrency, not
   parallelism**. One runtime lock is shared by all the threads; only
   the thread holding it runs OCaml code. A thread in a blocking system
   call (getaddrinfo, read, curl's transfer, the frame loop's sleep)
   releases it, and that is where another runs. So a job waiting on the
   network costs the frames nothing; a job computing (a JPEG decoded)
   would take the frame loop's time just the same. Parallelism is
   OCaml 5's domains (an exercise, plan_browser_teaching.md). And in a
   browser (js_of_ocaml) there are no threads at all: JavaScript's one
   thread and its event loop, the web platform doing the waiting.

   Reference: Andrew Birrell, "An Introduction to Programming with
   Threads" (DEC SRC report 35, 1989): mutexes, condition variables,
   and the pool of workers taking jobs from a queue. *)

type t

(* a job submitted, its result to come *)
type 'a job

(* [create n]: a pool of [n] threads, waiting for jobs *)
val create : int -> t

(* [submit pool f]: [f ()] to be run by one of the pool's threads; returns
 * at once. The jobs start in the order submitted. *)
val submit : t -> (unit -> 'a) -> 'a job

(* the job's result once it has run ([Error] for the exception it
 * raised), [None] until then; never waits *)
val poll : 'a job -> ('a, exn) result option
