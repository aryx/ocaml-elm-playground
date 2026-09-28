(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Worker.mli *)

type t = {
  (* guards [queue] and every job's [result] *)
  mutex : Mutex.t;
  (* signaled when a job is queued *)
  queued : Condition.t;
  queue : (unit -> unit) Queue.t;
}

type 'a job = { pool : t; mutable result : ('a, exn) result option }

(* a thread of the pool: take a job, run it outside the mutex (it
 * blocks: holding the mutex meanwhile would stop the others), forever *)
let rec work (pool : t) : unit =
  Mutex.lock pool.mutex;
  (* a loop, not an if: a wakeup can find the queue emptied by another
   * thread first *)
  while Queue.is_empty pool.queue do
    Condition.wait pool.queued pool.mutex
  done;
  let job = Queue.pop pool.queue in
  Mutex.unlock pool.mutex;
  job ();
  work pool

let create (n : int) : t =
  let pool = { mutex = Mutex.create (); queued = Condition.create (); queue = Queue.create () } in
  for _ = 1 to n do
    ignore (Thread.create work pool : Thread.t)
  done;
  pool

let submit (pool : t) (f : unit -> 'a) : 'a job =
  let job = { pool; result = None } in
  let run () =
    let r = try Ok (f ()) with e -> Error e in
    Mutex.lock pool.mutex;
    job.result <- Some r;
    Mutex.unlock pool.mutex
  in
  Mutex.lock pool.mutex;
  Queue.push run pool.queue;
  Condition.signal pool.queued;
  Mutex.unlock pool.mutex;
  job

let poll (job : 'a job) : ('a, exn) result option =
  Mutex.lock job.pool.mutex;
  let r = job.result in
  Mutex.unlock job.pool.mutex;
  r
