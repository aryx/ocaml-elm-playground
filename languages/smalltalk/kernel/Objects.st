"Object, nil, the Booleans, and the two small classes everything
uses: Association (key -> value) and Message (what doesNotUnderstand:
receives)."!

!Object methodsFor: 'comparing'!
== anObject
	"The same object: the oops equal. Never fails."
	<primitive: 110>
	self primitiveFailed!

= anObject
	"Equal: by default, the same object. Subclasses say more."
	^self == anObject!

~= anObject
	^(self = anObject) == false!

~~ anObject
	^(self == anObject) == false!

hash
	"Equal objects must have equal hashes."
	^self identityHash!

identityHash
	<primitive: 75>
	self primitiveFailed! !

!Object methodsFor: 'class membership'!
class
	<primitive: 111>
	self primitiveFailed!

isKindOf: aClass
	| c |
	c := self class.
	[c == nil] whileFalse: [c == aClass ifTrue: [^true]. c := c superclass].
	^false!

isMemberOf: aClass
	^self class == aClass!

respondsTo: aSymbol
	^self class canUnderstand: aSymbol! !

!Object methodsFor: 'testing'!
isNil
	^false!

notNil
	^true!

isString
	^false!

isSymbol
	^false!

isNumber
	^false!

isInteger
	^false!

isSmallInteger
	^false!

isCharacter
	^false!

isClass
	^false!

isArray
	^false!

ifNil: aBlock
	^self!

ifNotNil: aBlock
	^aBlock value: self! !

!Object methodsFor: 'accessing'!
at: index
	"The indexed field, from 1."
	<primitive: 60>
	self checkIndex: index!

at: index put: value
	<primitive: 61>
	self checkIndex: index!

basicAt: index
	<primitive: 60>
	self checkIndex: index!

basicAt: index put: value
	<primitive: 61>
	self checkIndex: index!

size
	"How many indexed fields."
	<primitive: 62>
	^0!

basicSize
	<primitive: 62>
	^0!

instVarAt: index
	<primitive: 73>
	self error: 'Index out of bounds'!

instVarAt: index put: value
	<primitive: 74>
	self error: 'Index out of bounds'!

yourself
	^self!

checkIndex: index
	index isInteger ifFalse: [^self error: 'Index must be an integer'].
	(index < 1 or: [index > self basicSize])
		ifTrue: [^self error: 'Index out of bounds: ', index printString].
	^self error: 'Improper store into indexable object'! !

!Object methodsFor: 'copying'!
shallowCopy
	"A new object with the same fields: they are shared, not copied."
	<primitive: 153>
	self primitiveFailed!

copy
	^self shallowCopy!

deepCopy
	| new |
	new := self shallowCopy.
	1 to: self class instSize do: [:i | new instVarAt: i put: (self instVarAt: i) copy].
	^new! !

!Object methodsFor: 'printing'!
printString
	"What 'print it' shows: printOn: into a stream on a String."
	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream.
	^aStream contents!

printOn: aStream
	"By default, the class's name with an article: a Point, an Object."
	| title |
	title := self class name.
	aStream nextPutAll: ((title at: 1) isVowel ifTrue: ['an '] ifFalse: ['a ']).
	aStream nextPutAll: title!

displayString
	^self printString!

displayNl
	Transcript show: self displayString; cr!

printNl
	Transcript show: self printString; cr! !

!Object methodsFor: 'error handling'!
error: aString
	"Stop the process, a notifier titled aString: St_debug.mli.
	Proceeding answers nil."
	<primitive: 141>!

doesNotUnderstand: aMessage
	"No method for aMessage selector, up the whole superclass chain."
	^self error: 'Message not understood: ', aMessage selector!

halt
	^self error: 'Halt'!

primitiveFailed
	^self error: 'Primitive failed'!

subclassResponsibility
	^self error: 'My subclass should have overridden one of my messages'!

shouldNotImplement
	^self error: 'This message is not appropriate for this object'!

mustBeBoolean
	^self error: 'NonBoolean receiver--proceed for truth'!

cannotReturn: value
	^self error: 'Context cannot return'! !

!Object methodsFor: 'message handling'!
perform: aSymbol
	<primitive: 83>
	^self error: 'Wrong number of arguments'!

perform: aSymbol with: anObject
	<primitive: 83>
	^self error: 'Wrong number of arguments'!

perform: aSymbol with: first with: second
	<primitive: 83>
	^self error: 'Wrong number of arguments'!

perform: aSymbol withArguments: anArray
	<primitive: 84>
	^self error: 'Wrong number of arguments'! !

!Object methodsFor: 'system primitives'!
become: otherObject
	"Swap the two objects: every reference to one now reaches the
	other (St_memory.mli)."
	<primitive: 72>
	self primitiveFailed!

inspect
	"An Inspector on the receiver."
	<primitive: 158>!

-> anObject
	^Association key: self value: anObject! !

!UndefinedObject methodsFor: 'testing'!
isNil
	^true!

notNil
	^false!

ifNil: aBlock
	^aBlock value!

ifNotNil: aBlock
	^self! !

!UndefinedObject methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: 'class definition'!
subclass: nameSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames poolDictionaries: pools category: categoryString
	"A class with no superclass: Object's own definition."
	<primitive: 143>
	self primitiveFailed! !

!Boolean methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: (self ifTrue: ['true'] ifFalse: ['false'])! !

!Boolean methodsFor: 'copying'!
shallowCopy
	"true and false are unique."
	^self! !

!True methodsFor: 'controlling'!
ifTrue: trueBlock
	"Sent only when the argument is not a literal block: the compiler
	inlines the other cases into jumps (St_compile.mli)."
	^trueBlock value!

ifFalse: falseBlock
	^nil!

ifTrue: trueBlock ifFalse: falseBlock
	^trueBlock value!

ifFalse: falseBlock ifTrue: trueBlock
	^trueBlock value!

and: aBlock
	^aBlock value!

or: aBlock
	^true! !

!True methodsFor: 'logical operations'!
& aBoolean
	^aBoolean!

| aBoolean
	^true!

not
	^false! !

!False methodsFor: 'controlling'!
ifTrue: trueBlock
	^nil!

ifFalse: falseBlock
	^falseBlock value!

ifTrue: trueBlock ifFalse: falseBlock
	^falseBlock value!

ifFalse: falseBlock ifTrue: trueBlock
	^falseBlock value!

and: aBlock
	^false!

or: aBlock
	^aBlock value! !

!False methodsFor: 'logical operations'!
& aBoolean
	^false!

| aBoolean
	^aBoolean!

not
	^true! !

!Association class methodsFor: 'instance creation'!
key: aKey value: aValue
	^self new key: aKey value: aValue! !

!Association methodsFor: 'accessing'!
key
	^key!

value
	^value!

key: aKey value: aValue
	key := aKey.
	value := aValue!

value: aValue
	value := aValue! !

!Association methodsFor: 'comparing'!
= anAssociation
	^(anAssociation isKindOf: Association)
		and: [key = anAssociation key and: [value = anAssociation value]]!

hash
	^key hash! !

!Association methodsFor: 'printing'!
printOn: aStream
	key printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream! !

!Message methodsFor: 'accessing'!
selector
	^selector!

arguments
	^arguments! !

!Message methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'a Message ('; nextPutAll: selector.
	arguments do: [:each | aStream space; print: each].
	aStream nextPut: $)! !
