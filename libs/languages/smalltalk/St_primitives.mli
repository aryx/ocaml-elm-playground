(* St_primitives: what the virtual machine does itself.

   A method may name a primitive, "<primitive: 60>"; when it is sent,
   the primitive runs first, and the method's Smalltalk code only if
   the primitive fails -- an index out of bounds, an argument of the
   wrong class. So "at:" is a primitive whose failure is handled in
   Smalltalk ("self error: 'index out of bounds'"), and SmallInteger's
   "+" one whose failure (an overflow) falls back on the kernel's
   LargePositiveInteger, written in Smalltalk.

   The numbers are the Blue Book's (chapter 29), where it has one:

     1-17     SmallInteger: + - < > <= >= = ~= * / \\ // quo: bitAnd:
              bitOr: bitXor: bitShift:
     18       @, a Point
     40       SmallInteger asFloat
     41-50    Float: + - < > <= >= = ~= * /
     51       Float truncated; 55 sqrt, 56 sin, 57 arcTan, 58 ln, 59 exp
     60-62    at:  at:put:  size (fields, or bytes as SmallIntegers)
     63-64    a String's at: at:put:, Characters
     68-69    a CompiledMethod's objectAt: objectAt:put:
     70-71    new  new:
     72       become:
     73-74    instVarAt:  instVarAt:put:
     75       identityHash (asOop)
     80       blockCopy:
     81       value, value:, value:value:... (the arity checked)
     82       valueWithArguments:
     83       perform:, perform:with:...
     84       perform:withArguments:
     90       Sensor mousePoint; 91 the buttons (4 red, 2 yellow, 1 blue)
     96       BitBlt copyBits (St_bitblt.mli)
     105      replaceFrom:to:with:startingAt:
     110      ==
     111      class

   and ours, from 120 (the Blue Book did these in Smalltalk, over the
   display and the files it had):

     120      Character value:, the unique Character of a code
     122      String asSymbol
     123      String = String, 124 String < String, 125 String hash
     130      Float printString
     140      the Transcript shows a String
     141      suspend the process, with a label: error:, halt, the
              debugger's notifier (St_debug.mli)
     142      Behavior compile:classified:, the Browser's accept
     143      Class subclass:instanceVariableNames:classVariableNames:
              poolDictionaries:category:, a class defined
     144      the millisecond clock
     145      Behavior allInstances
     146      Smalltalk garbageCollect: entries freed
     147      Behavior canUnderstand:, 148 includesSelector:
     149      Behavior selectors, an Array of Symbols
     150      String asNumber (the lexer's numbers)
     151-152  variableSubclass:..., variableByteSubclass:...
     153      shallowCopy
     154      SmallInteger asLargeInteger, 155 LargeInteger normalize
     156      CompiledMethod selector, 157 methodClass
     158      inspect, an Inspector opened by the host *)

val install : St_interp.vm -> unit
