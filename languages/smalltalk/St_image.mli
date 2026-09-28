(* St_image: the whole object memory as bytes, and back.

   Smalltalk's persistence: you keep the world, not files. Saving
   writes every object -- the classes, their methods, the globals,
   whatever the Workspace made -- and loading gives the same world
   back, the methods accepted since the boot in it. (The Blue Book's
   image was the same idea, its format the object table as the machine
   had it in memory; ours is our own.)

   The format: a line saying what it is, then the objects the virtual
   machine knows by name (St_memory.known), then each object: its index
   in the table, its class, and its body -- fields (as oops), bytes, a
   float's 64 bits, or a method's both. Numbers are written in LEB128
   varints; an oop as its tag and its payload, a SmallInteger's value
   zigzagged, so that the same bytes are read with 63-bit ints and with
   the web's 32. The processes are not saved: a loaded world is at
   rest. *)

val save : St_memory.t -> string

(* raises Failure on bytes that are not an image *)
val load : string -> St_memory.t

(* a running system from an image: the machine and its primitives *)
val load_vm : ?host:St_interp.host -> string -> St_interp.vm
