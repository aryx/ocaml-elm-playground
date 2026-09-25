(* networking/unix/Worker: a job submitted returns at once; four jobs
 * that wait, on four threads, wait at the same time; a job's exception
 * comes back as its result *)
val tests : Testo.t list
