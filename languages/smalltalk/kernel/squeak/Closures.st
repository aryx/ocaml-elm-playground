"Squeak's kernel, the first file: the blocks. A kernel that defines
BlockClosure is compiled with closures (St_compile.mli): a block is no
longer a BlockContext sharing its method's temporaries but a
BlockClosure, an object holding what it needs from outside, and each
value of it runs in a context of its own. So a block can call itself,
and two blocks made by the same code do not share their arguments.

MethodContext's unused field gets its Squeak name: the closure a
context is running, nil for a method's."!

Object variableSubclass: #BlockClosure
	instanceVariableNames: 'outerContext startpc numArgs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

ContextPart variableSubclass: #MethodContext
	instanceVariableNames: 'method closureOrNil receiver'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

!BlockClosure methodsFor: 'accessing'!
outerContext
	^outerContext!

home
	^outerContext home!

method
	^outerContext method!

receiver
	^outerContext receiver!

numArgs
	^numArgs!

numCopiedValues
	^self size! !

!BlockClosure methodsFor: 'evaluating'!
value
	"Evaluate the block, in a new context: the virtual machine makes
	a MethodContext whose closure is the receiver (St_interp.mli)."
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

!BlockClosure methodsFor: 'controlling'!
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

!BlockClosure methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: '[] in '.
	self home printOn: aStream! !

!MethodContext methodsFor: 'accessing'!
closure
	^closureOrNil!

home
	"The context of the method a block's ^ returns from: for a
	closure's activation, up its outer contexts."
	^closureOrNil == nil ifTrue: [self] ifFalse: [closureOrNil home]! !

!MethodContext methodsFor: 'printing'!
printOn: aStream
	| mclass |
	closureOrNil == nil ifFalse: [aStream nextPutAll: '[] in '].
	mclass := method methodClass.
	aStream nextPutAll: receiver class name.
	mclass == receiver class ifFalse: [aStream nextPut: $(; nextPutAll: mclass name; nextPut: $)].
	aStream nextPutAll: '>>'; nextPutAll: method selector! !
