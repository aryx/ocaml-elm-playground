(* St_class: a class, as the virtual machine and the tools read it.

   A class is an object like any other; its fields are the ones the
   kernel declares (kernel/Kernel-Classes.st), and the virtual machine
   knows their places:

     Behavior          0 superclass  1 methodDict  2 format
     ClassDescription  3 instanceVariables  4 organization
     Class             5 name  6 category  7 classPool  8 comment
     Metaclass         5 thisClass

   The **format** is a SmallInteger, fields * 8 + kind: how many named
   fields an instance has, and whether it has indexed ones -- pointers,
   bytes, a float, a CompiledMethod's mix (kind 0 to 4).

   The **method dictionary** is two Arrays side by side, selectors and
   methods, searched in order (the interpreter's cache makes the
   search rare: St_interp.mli). The Blue Book hashed it; a hundred
   selectors do not need it.

   The **organization**, what the Browser's third pane lists, is an
   Array of Associations, a category's name with the Array of its
   selectors.

   And each class has its **metaclass**, whose only instance it is: the
   class's own methods ("Point x: 3 y: 4") are the metaclass's. The
   knot at the top (Blue Book, chapter 16):

       Object class superclass == Class
       Metaclass class class == Metaclass

   The globals, "Smalltalk", are a SystemDictionary whose one field is
   an Array of Associations. A global in a method is its Association,
   a literal of the method: "push literal variable" reads its value, so
   redefining a global is seen by every method at once. *)

type oop = St_memory.oop

(* the field numbers above *)
val f_superclass : int
val f_method_dict : int
val f_format : int
val f_inst_vars : int
val f_organization : int
val f_name : int
val f_category : int
val f_class_pool : int
val f_comment : int

(*****************************************************************************)
(* Formats *)
(*****************************************************************************)

type kind = Fixed | Indexable | Byte_indexable | Float_kind | Method_kind

val format : St_memory.t -> oop -> int * kind (* named fields, kind *)
val encode_format : int -> kind -> int

(*****************************************************************************)
(* Classes *)
(*****************************************************************************)

val superclass : St_memory.t -> oop -> oop
val is_meta : St_memory.t -> oop -> bool

(* a metaclass's class, and a class's metaclass *)
val this_class : St_memory.t -> oop -> oop
val metaclass : St_memory.t -> oop -> oop

(* "Point", or "Point class" *)
val name : St_memory.t -> oop -> string

(* all the instance variables' names, inherited ones first *)
val inst_var_names : St_memory.t -> oop -> string list

(* the class's own *)
val own_inst_var_names : St_memory.t -> oop -> string list
val category : St_memory.t -> oop -> string
val comment : St_memory.t -> oop -> string

(* the classes, from the globals, in alphabetical order *)
val classes : St_memory.t -> oop list

(* "Object subclass: #Point instanceVariableNames: 'x y' ..." *)
val definition : St_memory.t -> oop -> string

(*****************************************************************************)
(* Methods *)
(*****************************************************************************)

(* in this class only *)
val local_method : St_memory.t -> oop -> oop -> oop option

(* up the superclass chain *)
val lookup : St_memory.t -> oop -> oop -> oop option

(* add or replace a method, filed under a category *)
val install : St_memory.t -> oop -> oop -> oop -> category:string -> unit
val remove : St_memory.t -> oop -> oop -> unit
val selectors : St_memory.t -> oop -> string list

(* the categories, in order, and the selectors of one *)
val categories : St_memory.t -> oop -> string list
val category_selectors : St_memory.t -> oop -> string -> string list
val category_of : St_memory.t -> oop -> string -> string option

(*****************************************************************************)
(* Globals and class variables *)
(*****************************************************************************)

val new_association : St_memory.t -> oop -> oop -> oop
val global : St_memory.t -> string -> oop option (* the Association *)
val declare_global : St_memory.t -> string -> oop -> oop (* the Association, made or updated *)
val globals : St_memory.t -> (string * oop) list (* name, value *)

(* a class variable's Association, searched up the superclass chain,
 * from a metaclass's class too *)
val class_var : St_memory.t -> oop -> string -> oop option

(* a class's own class variables' names *)
val class_var_names : St_memory.t -> oop -> string list

(*****************************************************************************)
(* Defining a class *)
(*****************************************************************************)

(* the classes whose superclass this one is *)
val subclasses : St_memory.t -> oop -> oop list

(* what "Object subclass: #Point instanceVariableNames: 'x y' ..."
 * does: the class and its metaclass made, or the existing class of
 * that name changed in place (its subclasses' formats following), and
 * the global set. The class, and whether its instance variables
 * changed -- then its methods, and its subclasses', must be compiled
 * again (St_compile.recompile), since they use the variables' indexes.
 * The instances of a changed class are left as they were (the Blue
 * Book mutated them with become:, an exercise). *)
val define_class :
  St_memory.t ->
  superclass:oop ->
  name:string ->
  kind:kind ->
  inst_vars:string list ->
  class_vars:string list ->
  category:string ->
  oop * bool

(* a new, empty MethodDictionary *)
val new_method_dict : St_memory.t -> oop
