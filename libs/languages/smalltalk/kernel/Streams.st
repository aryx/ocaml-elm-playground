"Streams over collections: printString is printOn: into a WriteStream
on a String, and a ReadStream reads one element at a time."!

!Stream methodsFor: 'accessing'!
next
	^self subclassResponsibility!

nextPut: anObject
	^self subclassResponsibility!

nextPutAll: aCollection
	aCollection do: [:each | self nextPut: each].
	^aCollection!

atEnd
	^self subclassResponsibility!

do: aBlock
	[self atEnd] whileFalse: [aBlock value: self next]! !

!Stream methodsFor: 'printing'!
print: anObject
	anObject printOn: self!

cr
	self nextPut: Character cr!

tab
	self nextPut: Character tab!

space
	self nextPut: Character space! !

!PositionableStream class methodsFor: 'instance creation'!
on: aCollection
	^self basicNew on: aCollection! !

!PositionableStream methodsFor: 'private'!
on: aCollection
	collection := aCollection.
	readLimit := aCollection size.
	position := 0! !

!PositionableStream methodsFor: 'accessing'!
contents
	^collection copyFrom: 1 to: readLimit!

position
	^position!

position: anInteger
	position := anInteger!

reset
	position := 0!

isEmpty
	^readLimit = 0!

atEnd
	^position >= readLimit!

peek
	self atEnd ifTrue: [^nil].
	^collection at: position + 1!

skip: anInteger
	position := (position + anInteger min: readLimit) max: 0!

upTo: anObject
	| start |
	start := position + 1.
	[self atEnd or: [self next = anObject]] whileFalse.
	^collection copyFrom: start to: (self atEnd ifTrue: [position] ifFalse: [position - 1])!

upToEnd
	| start |
	start := position + 1.
	position := readLimit.
	^collection copyFrom: start to: readLimit!

skipSeparators
	[self atEnd not and: [self peek isSeparator]] whileTrue: [self next]! !

!ReadStream methodsFor: 'accessing'!
next
	self atEnd ifTrue: [^nil].
	position := position + 1.
	^collection at: position!

nextPut: anObject
	^self shouldNotImplement! !

!WriteStream class methodsFor: 'instance creation'!
with: aCollection
	^(self on: aCollection) setToEnd! !

!WriteStream methodsFor: 'private'!
on: aCollection
	super on: aCollection.
	readLimit := 0.
	writeLimit := aCollection size!

setToEnd
	position := readLimit := writeLimit!

growCollection
	| bigger |
	bigger := collection species new: collection size * 2 + 16.
	bigger replaceFrom: 1 to: collection size with: collection startingAt: 1.
	collection := bigger.
	writeLimit := collection size! !

!WriteStream methodsFor: 'accessing'!
nextPut: anObject
	position >= writeLimit ifTrue: [self growCollection].
	position := position + 1.
	collection at: position put: anObject.
	position > readLimit ifTrue: [readLimit := position].
	^anObject!

contents
	^collection copyFrom: 1 to: position!

next
	^self shouldNotImplement! !

!ReadWriteStream methodsFor: 'accessing'!
next
	self atEnd ifTrue: [^nil].
	position := position + 1.
	^collection at: position!

contents
	^collection copyFrom: 1 to: readLimit! !

!SequenceableCollection methodsFor: 'converting'!
writeStream
	^WriteStream on: self! !
