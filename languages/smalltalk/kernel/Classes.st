"Classes are objects: Behavior (what makes instances and finds
methods), ClassDescription (instance variables' names, the methods'
categories), Class and Metaclass. And the objects of the machine
itself: the SystemDictionary Smalltalk, method dictionaries, compiled
methods, the contexts, and Time."!

!Behavior methodsFor: 'instance creation'!
new
	<primitive: 70>
	^self error: 'This class cannot make instances with new'!

new: anInteger
	<primitive: 71>
	^self error: 'This class cannot make indexed instances'!

basicNew
	<primitive: 70>
	self primitiveFailed!

basicNew: anInteger
	<primitive: 71>
	self primitiveFailed! !

!Behavior methodsFor: 'accessing'!
superclass
	^superclass!

methodDictionary
	^methodDict!

instSize
	"The named fields of an instance: the format's high bits."
	^format bitShift: -3!

isVariable
	^(format bitAnd: 7) between: 1 and: 2!

instanceVariableNames
	| names |
	names := superclass == nil ifTrue: [OrderedCollection new] ifFalse: [superclass instanceVariableNames].
	instanceVariables == nil ifFalse: [instanceVariables do: [:each | names add: each]].
	^names!

selectors
	<primitive: 149>
	self primitiveFailed!

allInstances
	<primitive: 145>
	self primitiveFailed!

subclasses
	| result |
	result := OrderedCollection new.
	Smalltalk classesDo: [:each | each superclass == self ifTrue: [result add: each]].
	^result!

withAllSuperclasses
	| result c |
	result := OrderedCollection new.
	c := self.
	[c == nil] whileFalse: [result add: c. c := c superclass].
	^result! !

!Behavior methodsFor: 'testing'!
isClass
	^true!

inheritsFrom: aClass
	| c |
	c := superclass.
	[c == nil] whileFalse: [c == aClass ifTrue: [^true]. c := c superclass].
	^false!

canUnderstand: aSymbol
	<primitive: 147>
	self primitiveFailed!

includesSelector: aSymbol
	<primitive: 148>
	self primitiveFailed! !

!Behavior methodsFor: 'compiling'!
compile: code classified: category
	"What the Browser's accept does: the selector, or the compiler's
	complaint, a String."
	<primitive: 142>
	self primitiveFailed!

compile: code
	^self compile: code classified: 'as yet unclassified'! !

!Behavior methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: self name! !

!Class methodsFor: 'accessing'!
name
	^name!

category
	^category!

comment
	^comment!

comment: aString
	comment := aString! !

!Class methodsFor: 'subclass creation'!
subclass: nameSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: pools category: categoryString
	<primitive: 143>
	self primitiveFailed!

variableSubclass: nameSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: pools category: categoryString
	<primitive: 151>
	self primitiveFailed!

variableByteSubclass: nameSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: pools category: categoryString
	<primitive: 152>
	self primitiveFailed! !

!Metaclass methodsFor: 'accessing'!
name
	^thisClass name , ' class'!

soleInstance
	^thisClass!

category
	^thisClass category! !

!SystemDictionary methodsFor: 'accessing'!
at: key
	^self at: key ifAbsent: [self error: 'Key not found: ', key]!

at: key ifAbsent: aBlock
	| a |
	a := self associationAt: key ifAbsent: [nil].
	a == nil ifTrue: [^aBlock value].
	^a value!

at: key put: value
	"The same Association kept: the methods that use the global see
	its new value (St_class.mli)."
	| a |
	a := self associationAt: key ifAbsent: [nil].
	a == nil ifFalse: [a value: value. ^value].
	associations := associations copyWith: (Association key: key asSymbol value: value).
	^value!

associationAt: key ifAbsent: aBlock
	| k |
	k := key asSymbol.
	associations do: [:each | each key == k ifTrue: [^each]].
	^aBlock value!

includesKey: key
	self associationAt: key ifAbsent: [^false].
	^true!

keys
	^associations collect: [:each | each key]!

classesDo: aBlock
	associations do: [:each | (each value isClass and: [each value name == each key]) ifTrue: [aBlock value: each value]]!

garbageCollect
	"The entries freed."
	<primitive: 146>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'Smalltalk'! !

!MethodDictionary methodsFor: 'accessing'!
keys
	^selectors!

at: aSymbol
	1 to: selectors size do: [:i | (selectors at: i) == aSymbol ifTrue: [^methods at: i]].
	^self error: 'Key not found'!

size
	^selectors size! !

!CompiledMethod methodsFor: 'accessing'!
selector
	<primitive: 156>
	self primitiveFailed!

methodClass
	<primitive: 157>
	self primitiveFailed! !

!CompiledMethod methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: self methodClass name; nextPutAll: '>>'; nextPutAll: self selector! !

!ContextPart methodsFor: 'controlling'!
blockCopy: numArgs
	"What the compiler sends for each [...]: a BlockContext that will
	start after the jump that follows this send (St_compile.mli)."
	<primitive: 80>
	self primitiveFailed! !

!ContextPart methodsFor: 'accessing'!
sender
	^sender!

method
	^self subclassResponsibility!

receiver
	^self subclassResponsibility! !

!MethodContext methodsFor: 'accessing'!
method
	^method!

receiver
	^receiver!

home
	^self! !

!MethodContext methodsFor: 'printing'!
printOn: aStream
	"Point(Object)>>printString: the receiver's class, and the class
	the method was found in if another"
	| mclass |
	mclass := method methodClass.
	aStream nextPutAll: receiver class name.
	mclass == receiver class ifFalse: [aStream nextPut: $(; nextPutAll: mclass name; nextPut: $)].
	aStream nextPutAll: '>>'; nextPutAll: method selector! !

!BlockContext methodsFor: 'accessing'!
method
	^home method!

receiver
	^home receiver!

home
	^home!

numArgs
	^nargs! !

!BlockContext methodsFor: 'evaluating'!
value
	"Evaluate the block. It runs in this same context each time: the
	Blue Book's blocks are not reentrant (St_compile.mli)."
	<primitive: 81>
	^self error: 'Wrong number of arguments'!

value: arg
	<primitive: 81>
	^self error: 'Wrong number of arguments'!

value: arg1 value: arg2
	<primitive: 81>
	^self error: 'Wrong number of arguments'!

value: arg1 value: arg2 value: arg3
	<primitive: 81>
	^self error: 'Wrong number of arguments'!

value: arg1 value: arg2 value: arg3 value: arg4
	<primitive: 81>
	^self error: 'Wrong number of arguments'!

valueWithArguments: anArray
	<primitive: 82>
	^self error: 'Wrong number of arguments'! !

!BlockContext methodsFor: 'controlling'!
whileTrue: aBlock
	"Sent only when the compiler could not inline it."
	[self value] whileTrue: [aBlock value].
	^nil!

whileFalse: aBlock
	[self value] whileFalse: [aBlock value].
	^nil!

whileTrue
	[self value] whileTrue.
	^nil!

repeat
	[true] whileTrue: [self value]! !

!BlockContext methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: '[] in '.
	home printOn: aStream! !

!Time class methodsFor: 'general inquiries'!
millisecondClock
	<primitive: 144>
	self primitiveFailed!

millisecondsToRun: aBlock
	| start |
	start := self millisecondClock.
	aBlock value.
	^self millisecondClock - start! !

!TextCollector methodsFor: 'accessing'!
show: aString
	"The Transcript's text, given to the host."
	<primitive: 140>
	^self show: aString printString!

nextPutAll: aString
	^self show: aString!

nextPut: aCharacter
	^self show: (String with: aCharacter)!

cr
	^self nextPut: Character cr!

tab
	^self nextPut: Character tab!

space
	^self nextPut: Character space!

print: anObject
	^self show: anObject printString!

display: anObject
	^self show: anObject displayString! !

!TextCollector methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'a TextCollector'! !
