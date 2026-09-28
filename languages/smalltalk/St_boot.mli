(* St_boot: from the kernel's text to a running Smalltalk.

   Smalltalk-80 never booted: its image was the world, saved and loaded,
   each one made by the previous one since 1976. Here there is no image
   to start from, so the world is made from the text of its classes
   (St_kernel.mli, kernel/*.st), in four steps:

   1. the classes, empty: an object and a metaclass for every class
      definition in the text, before anything is read -- so that the
      Array, String, Symbol, Character objects made next have their
      classes. nil, true, false, the 256 Characters, the Smalltalk
      dictionary with a global per class;
   2. the classes filled from their definitions (St_class.define_class):
      superclasses, formats, instance variables; the knot at the top
      tied by the same code: Object's metaclass's superclass is Class,
      found as a global, and every metaclass is an instance of
      Metaclass, itself a class whose metaclass is an instance of
      Metaclass;
   3. every method compiled, by the OCaml compiler (St_compile.mli) --
      a Smalltalk compiler would need a running Smalltalk to run it, the
      chicken and the egg every self-hosted language has to break once;
   4. the virtual machine made, and the chunks that are not definitions
      run: from here, everything is Smalltalk.

   A mistake in the kernel is an OCaml exception naming its file and
   line. *)

exception Error of string

(* a host that throws the Transcript away *)
val quiet_host : St_interp.host

(* the running system *)
val boot : ?host:St_interp.host -> unit -> St_interp.vm

(* a class by name, from the globals *)
val class_named : St_memory.t -> string -> St_memory.oop
