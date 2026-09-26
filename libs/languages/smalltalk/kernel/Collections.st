"The collections, and their iteration protocol -- do:, collect:,
select:, reject:, detect:, inject:into: -- the one the rest of the
world copied (Ruby's each, map, select, detect, inject; Python's
comprehensions). Every collection answers them once it has do:.

OrderedCollection and Set grow the Blue Book's way: a bigger copy,
then 'self become: newSelf' (St_memory.mli), so that everything that
knew the collection now knows the bigger one."!

!Collection class methodsFor: 'instance creation'!
with: anObject
	^self new add: anObject; yourself!

with: first with: second
	^self new add: first; add: second; yourself!

with: first with: second with: third
	^self new add: first; add: second; add: third; yourself!

with: first with: second with: third with: fourth
	^self new add: first; add: second; add: third; add: fourth; yourself!

withAll: aCollection
	^self new addAll: aCollection; yourself! !

!Collection methodsFor: 'adding'!
add: anObject
	^self subclassResponsibility!

addAll: aCollection
	aCollection do: [:each | self add: each].
	^aCollection! !

!Collection methodsFor: 'removing'!
remove: oldObject
	^self remove: oldObject ifAbsent: [self error: 'Object is not in the collection']!

remove: oldObject ifAbsent: aBlock
	^self subclassResponsibility!

removeAll: aCollection
	aCollection do: [:each | self remove: each].
	^aCollection! !

!Collection methodsFor: 'testing'!
includes: anObject
	self do: [:each | anObject = each ifTrue: [^true]].
	^false!

isEmpty
	^self size = 0!

notEmpty
	^self isEmpty not!

occurrencesOf: anObject
	| tally |
	tally := 0.
	self do: [:each | anObject = each ifTrue: [tally := tally + 1]].
	^tally! !

!Collection methodsFor: 'accessing'!
size
	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^tally! !

!Collection methodsFor: 'enumerating'!
do: aBlock
	^self subclassResponsibility!

do: aBlock separatedBy: separatorBlock
	| first |
	first := true.
	self do: [:each |
		first ifTrue: [first := false] ifFalse: [separatorBlock value].
		aBlock value: each]!

collect: aBlock
	| result |
	result := OrderedCollection new.
	self do: [:each | result add: (aBlock value: each)].
	^result!

select: aBlock
	| result |
	result := self species new.
	self do: [:each | (aBlock value: each) ifTrue: [result add: each]].
	^result!

reject: aBlock
	^self select: [:each | (aBlock value: each) == false]!

detect: aBlock
	^self detect: aBlock ifNone: [self error: 'Object is not in the collection']!

detect: aBlock ifNone: exceptionBlock
	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value!

inject: thisValue into: binaryBlock
	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue!

anySatisfy: aBlock
	self do: [:each | (aBlock value: each) ifTrue: [^true]].
	^false!

allSatisfy: aBlock
	self do: [:each | (aBlock value: each) ifFalse: [^false]].
	^true!

max
	^self inject: self anyOne into: [:a :b | a max: b]!

min
	^self inject: self anyOne into: [:a :b | a min: b]!

sum
	^self inject: 0 into: [:a :b | a + b]!

anyOne
	self do: [:each | ^each].
	^self error: 'Collection is empty'! !

!Collection methodsFor: 'converting'!
asArray
	| result i |
	result := Array new: self size.
	i := 0.
	self do: [:each | result at: (i := i + 1) put: each].
	^result!

asOrderedCollection
	^OrderedCollection withAll: self!

asBag
	^Bag withAll: self!

asSet
	^Set withAll: self!

asSortedCollection
	^SortedCollection withAll: self!

asSortedCollection: aBlock
	^(SortedCollection sortBlock: aBlock) addAll: self; yourself! !

!Collection methodsFor: 'private'!
species
	^self class! !

!Collection methodsFor: 'printing'!
printOn: aStream
	"OrderedCollection (1 2 3 ), as the Blue Book prints it."
	aStream nextPutAll: self class name; nextPutAll: ' ('.
	self do: [:element | element printOn: aStream. aStream space].
	aStream nextPut: $)! !

!SequenceableCollection methodsFor: 'comparing'!
= aCollection
	self class == aCollection class ifFalse: [^false].
	self size = aCollection size ifFalse: [^false].
	1 to: self size do: [:i | (self at: i) = (aCollection at: i) ifFalse: [^false]].
	^true!

hash
	^self size! !

!SequenceableCollection methodsFor: 'accessing'!
first
	self isEmpty ifTrue: [^self error: 'Collection is empty'].
	^self at: 1!

last
	self isEmpty ifTrue: [^self error: 'Collection is empty'].
	^self at: self size!

indexOf: anElement
	1 to: self size do: [:i | (self at: i) = anElement ifTrue: [^i]].
	^0!

allButFirst
	^self copyFrom: 2 to: self size!

allButLast
	^self copyFrom: 1 to: self size - 1! !

!SequenceableCollection methodsFor: 'enumerating'!
do: aBlock
	1 to: self size do: [:i | aBlock value: (self at: i)]!

reverseDo: aBlock
	self size to: 1 by: -1 do: [:i | aBlock value: (self at: i)]!

doWithIndex: aBlock
	1 to: self size do: [:i | aBlock value: (self at: i) value: i]!

keysAndValuesDo: aBlock
	1 to: self size do: [:i | aBlock value: i value: (self at: i)]!

with: aCollection do: aBlock
	1 to: self size do: [:i | aBlock value: (self at: i) value: (aCollection at: i)]!

collect: aBlock
	| result |
	result := self species new: self size.
	1 to: self size do: [:i | result at: i put: (aBlock value: (self at: i))].
	^result!

select: aBlock
	| result |
	result := OrderedCollection new.
	self do: [:each | (aBlock value: each) ifTrue: [result add: each]].
	^(self species inheritsFrom: ArrayedCollection)
		ifTrue: [(self species new: result size) replaceFrom: 1 to: result size with: result startingAt: 1]
		ifFalse: [result]! !

!SequenceableCollection methodsFor: 'copying'!
copyFrom: start to: stop
	| result |
	result := self species new: (stop - start + 1 max: 0).
	start to: stop do: [:i | result at: i - start + 1 put: (self at: i)].
	^result!

copyWith: newElement
	| result |
	result := self species new: self size + 1.
	result replaceFrom: 1 to: self size with: self startingAt: 1.
	result at: result size put: newElement.
	^result!

reversed
	| result n |
	n := self size.
	result := self species new: n.
	1 to: n do: [:i | result at: i put: (self at: n + 1 - i)].
	^result!

reverse
	^self reversed!

replaceFrom: start to: stop with: replacement startingAt: repStart
	| j |
	j := repStart.
	start to: stop do: [:i | self at: i put: (replacement at: j). j := j + 1]!

, aSequenceableCollection
	| result |
	result := self species new: self size + aSequenceableCollection size.
	result replaceFrom: 1 to: self size with: self startingAt: 1.
	result replaceFrom: self size + 1 to: result size with: aSequenceableCollection startingAt: 1.
	^result! !

!SequenceableCollection methodsFor: 'converting'!
readStream
	^ReadStream on: self! !

!ArrayedCollection class methodsFor: 'instance creation'!
new
	^self new: 0!

with: anObject
	^(self new: 1) at: 1 put: anObject; yourself!

with: first with: second
	^(self new: 2) at: 1 put: first; at: 2 put: second; yourself!

with: first with: second with: third
	^(self new: 3) at: 1 put: first; at: 2 put: second; at: 3 put: third; yourself!

with: first with: second with: third with: fourth
	^(self new: 4) at: 1 put: first; at: 2 put: second; at: 3 put: third; at: 4 put: fourth; yourself!

withAll: aCollection
	| result i |
	result := self new: aCollection size.
	i := 0.
	aCollection do: [:each | result at: (i := i + 1) put: each].
	^result! !

!ArrayedCollection methodsFor: 'accessing'!
size
	<primitive: 62>
	^0! !

!ArrayedCollection methodsFor: 'adding'!
add: anObject
	^self shouldNotImplement! !

!ArrayedCollection methodsFor: 'copying'!
replaceFrom: start to: stop with: replacement startingAt: repStart
	"The primitive when both are pointers or both bytes."
	<primitive: 105>
	^super replaceFrom: start to: stop with: replacement startingAt: repStart! !

!ArrayedCollection methodsFor: 'sorting'!
asSortedArray
	^self asSortedCollection asArray! !

!Array methodsFor: 'testing'!
isArray
	^true! !

!Array methodsFor: 'printing'!
printOn: aStream
	"#(1 $a 'b'), as a literal reads"
	aStream nextPutAll: '#('.
	self do: [:each | each printOn: aStream] separatedBy: [aStream space].
	aStream nextPut: $)! !

!String class methodsFor: 'instance creation'!
new
	^self new: 0! !

!String methodsFor: 'accessing'!
at: index
	"A Character: the bytes are characters."
	<primitive: 63>
	^self checkIndex: index!

at: index put: aCharacter
	<primitive: 64>
	aCharacter isCharacter ifFalse: [^self error: 'Strings only store Characters'].
	^self checkIndex: index!

size
	<primitive: 62>
	^0! !

!String methodsFor: 'comparing'!
= aString
	<primitive: 123>
	^false!

< aString
	<primitive: 124>
	^self error: 'Not a String'!

> aString
	^aString < self!

<= aString
	^(aString < self) not!

>= aString
	^(self < aString) not!

hash
	<primitive: 125>
	^0! !

!String methodsFor: 'testing'!
isString
	^true!

includesSubstring: aString
	| n |
	n := aString size.
	1 to: self size - n + 1 do: [:i | (self copyFrom: i to: i + n - 1) = aString ifTrue: [^true]].
	^false! !

!String methodsFor: 'converting'!
asSymbol
	<primitive: 122>
	self primitiveFailed!

asString
	^self!

asNumber
	"The number the text reads as, or nil."
	<primitive: 150>
	^nil!

asUppercase
	^self collect: [:c | c asUppercase]!

asLowercase
	^self collect: [:c | c asLowercase]!

displayString
	^self!

species
	^String! !

!String methodsFor: 'printing'!
printOn: aStream
	"'it''s': quoted, a quote doubled"
	aStream nextPut: $'.
	self do: [:c | aStream nextPut: c. c == $' ifTrue: [aStream nextPut: c]].
	aStream nextPut: $'! !

!Symbol class methodsFor: 'instance creation'!
new: anInteger
	^self error: 'Symbols are made by asSymbol, unique'! !

!Symbol methodsFor: 'comparing'!
= anObject
	"Symbols are unique: equal is identical."
	^self == anObject!

hash
	^self identityHash! !

!Symbol methodsFor: 'accessing'!
at: index put: aCharacter
	^self error: 'Symbols cannot be changed'! !

!Symbol methodsFor: 'converting'!
asSymbol
	^self!

asString
	| s |
	s := String new: self size.
	s replaceFrom: 1 to: self size with: self startingAt: 1.
	^s!

numArgs
	| n |
	n := 0.
	self do: [:c | c == $: ifTrue: [n := n + 1]].
	(n = 0 and: [(self at: 1) isLetter not]) ifTrue: [^1].
	^n! !

!Symbol methodsFor: 'testing'!
isSymbol
	^true! !

!Symbol methodsFor: 'printing'!
printOn: aStream
	aStream nextPut: $#; nextPutAll: self!

displayString
	^self asString! !

!Symbol methodsFor: 'copying'!
shallowCopy
	^self! !

!ByteArray methodsFor: 'accessing'!
size
	<primitive: 62>
	^0! !

!Interval class methodsFor: 'instance creation'!
from: start to: stop by: step
	^self new setFrom: start to: stop by: step! !

!Interval methodsFor: 'private'!
setFrom: startNumber to: stopNumber by: stepNumber
	start := startNumber.
	stop := stopNumber.
	step := stepNumber!

species
	^Array! !

!Interval methodsFor: 'accessing'!
size
	step > 0
		ifTrue: [stop < start ifTrue: [^0]]
		ifFalse: [start < stop ifTrue: [^0]].
	^stop - start // step + 1!

at: index
	(index >= 1 and: [index <= self size]) ifTrue: [^start + (step * (index - 1))].
	^self error: 'Index out of bounds'!

at: index put: anObject
	^self error: 'Intervals cannot be changed'! !

!Interval methodsFor: 'enumerating'!
do: aBlock
	| i |
	i := start.
	step > 0
		ifTrue: [[i <= stop] whileTrue: [aBlock value: i. i := i + step]]
		ifFalse: [[i >= stop] whileTrue: [aBlock value: i. i := i + step]]!

collect: aBlock
	| result i |
	result := Array new: self size.
	i := 0.
	self do: [:each | result at: (i := i + 1) put: (aBlock value: each)].
	^result! !

!Interval methodsFor: 'printing'!
printOn: aStream
	aStream nextPut: $(; print: start; nextPutAll: ' to: '; print: stop.
	step = 1 ifFalse: [aStream nextPutAll: ' by: '; print: step].
	aStream nextPut: $)! !

!OrderedCollection class methodsFor: 'instance creation'!
new
	^self new: 8!

new: anInteger
	^(self basicNew: anInteger) setIndices! !

!OrderedCollection methodsFor: 'private'!
setIndices
	firstIndex := self basicSize // 2 max: 1.
	lastIndex := firstIndex - 1!

setFirst: first last: last
	firstIndex := first.
	lastIndex := last!

grow
	"A copy with room at both ends, then become: -- the receiver is the
	bigger one from now on, for whoever holds it."
	| newSelf newFirst n |
	n := self basicSize * 2 + 8.
	newSelf := self class basicNew: n.
	1 to: self class instSize do: [:i | newSelf instVarAt: i put: (self instVarAt: i)].
	newFirst := n - self size // 2 + 1.
	1 to: self size do: [:i | newSelf basicAt: newFirst + i - 1 put: (self basicAt: firstIndex + i - 1)].
	newSelf setFirst: newFirst last: newFirst + self size - 1.
	self become: newSelf! !

!OrderedCollection methodsFor: 'accessing'!
size
	^lastIndex - firstIndex + 1!

at: index
	(index < 1 or: [index > self size]) ifTrue: [^self error: 'Index out of bounds: ', index printString].
	^self basicAt: index + firstIndex - 1!

at: index put: anObject
	(index < 1 or: [index > self size]) ifTrue: [^self error: 'Index out of bounds: ', index printString].
	^self basicAt: index + firstIndex - 1 put: anObject! !

!OrderedCollection methodsFor: 'adding'!
add: anObject
	^self addLast: anObject!

addLast: anObject
	lastIndex = self basicSize ifTrue: [self grow].
	lastIndex := lastIndex + 1.
	^self basicAt: lastIndex put: anObject!

addFirst: anObject
	firstIndex = 1 ifTrue: [self grow].
	firstIndex := firstIndex - 1.
	^self basicAt: firstIndex put: anObject! !

!OrderedCollection methodsFor: 'removing'!
removeFirst
	| answer |
	self isEmpty ifTrue: [^self error: 'Collection is empty'].
	answer := self basicAt: firstIndex.
	self basicAt: firstIndex put: nil.
	firstIndex := firstIndex + 1.
	^answer!

removeLast
	| answer |
	self isEmpty ifTrue: [^self error: 'Collection is empty'].
	answer := self basicAt: lastIndex.
	self basicAt: lastIndex put: nil.
	lastIndex := lastIndex - 1.
	^answer!

remove: oldObject ifAbsent: aBlock
	| index |
	index := firstIndex.
	[index <= lastIndex] whileTrue: [
		oldObject = (self basicAt: index) ifTrue: [
			index to: lastIndex - 1 do: [:i | self basicAt: i put: (self basicAt: i + 1)].
			self basicAt: lastIndex put: nil.
			lastIndex := lastIndex - 1.
			^oldObject].
		index := index + 1].
	^aBlock value! !

!OrderedCollection methodsFor: 'enumerating'!
do: aBlock
	firstIndex to: lastIndex do: [:i | aBlock value: (self basicAt: i)]!

collect: aBlock
	| result |
	result := OrderedCollection new: self size.
	self do: [:each | result addLast: (aBlock value: each)].
	^result!

select: aBlock
	| result |
	result := self copyEmpty.
	self do: [:each | (aBlock value: each) ifTrue: [result add: each]].
	^result!

copyEmpty
	^self class new! !

!OrderedCollection methodsFor: 'copying'!
copyFrom: start to: stop
	| result |
	result := OrderedCollection new.
	start to: stop do: [:i | result addLast: (self at: i)].
	^result!

, aCollection
	^self copy addAll: aCollection; yourself!

copy
	^self collect: [:each | each]!

reversed
	| result |
	result := OrderedCollection new.
	self do: [:each | result addFirst: each].
	^result! !

!SortedCollection class methodsFor: 'instance creation'!
new
	^self sortBlock: [:a :b | a <= b]!

sortBlock: aBlock
	^(self new: 8) setSortBlock: aBlock! !

!SortedCollection methodsFor: 'private'!
setSortBlock: aBlock
	sortBlock := aBlock!

copyEmpty
	^SortedCollection sortBlock: sortBlock! !

!SortedCollection methodsFor: 'adding'!
add: anObject
	"Put at the end, then moved down past the ones it sorts before:
	an insertion sort, one element at a time."
	| i |
	self addLast: anObject.
	i := lastIndex.
	[i > firstIndex and: [sortBlock value: anObject value: (self basicAt: i - 1)]] whileTrue: [
		self basicAt: i put: (self basicAt: i - 1).
		i := i - 1].
	self basicAt: i put: anObject.
	^anObject!

addFirst: anObject
	^self shouldNotImplement!

at: index put: anObject
	^self shouldNotImplement! !

!SortedCollection methodsFor: 'accessing'!
sortBlock
	^sortBlock!

median
	^self at: self size + 1 // 2! !

!SortedCollection methodsFor: 'enumerating'!
collect: aBlock
	"The answers are not sorted: an OrderedCollection."
	| result |
	result := OrderedCollection new: self size.
	self do: [:each | result addLast: (aBlock value: each)].
	^result! !

!Set class methodsFor: 'instance creation'!
new
	^self new: 16!

new: anInteger
	^(self basicNew: anInteger) setTally! !

!Set methodsFor: 'private'!
setTally
	tally := 0!

findIndexFor: anObject
	"Where anObject is, or the empty slot where it would go: its hash,
	then the next slots in turn (open addressing)."
	| index element |
	index := anObject hash \\ self basicSize + 1.
	[element := self basicAt: index.
	element == nil or: [self matches: element key: anObject]]
		whileFalse: [index := index \\ self basicSize + 1].
	^index!

matches: element key: anObject
	^element = anObject!

grow
	| newSelf |
	newSelf := self class new: self basicSize * 2.
	"the slots themselves: a Dictionary's do: gives its values"
	1 to: self basicSize do: [:i |
		(self basicAt: i) == nil ifFalse: [newSelf noCheckAdd: (self basicAt: i)]].
	self become: newSelf!

noCheckAdd: anObject
	self basicAt: (self findIndexFor: (self keyOf: anObject)) put: anObject.
	tally := tally + 1!

keyOf: element
	^element! !

!Set methodsFor: 'accessing'!
size
	^tally! !

!Set methodsFor: 'adding'!
add: newObject
	| index |
	newObject == nil ifTrue: [^newObject].
	index := self findIndexFor: newObject.
	(self basicAt: index) == nil ifTrue: [
		self basicAt: index put: newObject.
		tally := tally + 1.
		tally * 4 > (self basicSize * 3) ifTrue: [self grow]].
	^newObject! !

!Set methodsFor: 'removing'!
remove: oldObject ifAbsent: aBlock
	| index rest |
	index := self findIndexFor: oldObject.
	(self basicAt: index) == nil ifTrue: [^aBlock value].
	self basicAt: index put: nil.
	tally := tally - 1.
	"the ones after it, in its run, put again"
	rest := OrderedCollection new.
	index := index \\ self basicSize + 1.
	[(self basicAt: index) == nil] whileFalse: [
		rest add: (self basicAt: index).
		self basicAt: index put: nil.
		tally := tally - 1.
		index := index \\ self basicSize + 1].
	rest do: [:each | self noCheckAdd: each].
	^oldObject! !

!Set methodsFor: 'testing'!
includes: anObject
	^(self basicAt: (self findIndexFor: anObject)) ~~ nil! !

!Set methodsFor: 'enumerating'!
do: aBlock
	1 to: self basicSize do: [:i | | each | each := self basicAt: i. each == nil ifFalse: [aBlock value: each]]! !

!Dictionary methodsFor: 'private'!
matches: element key: aKey
	^element key = aKey!

keyOf: element
	^element key! !

!Dictionary methodsFor: 'accessing'!
at: key
	^self at: key ifAbsent: [self error: 'Key not found: ', key printString]!

at: key ifAbsent: aBlock
	| assoc |
	assoc := self basicAt: (self findIndexFor: key).
	assoc == nil ifTrue: [^aBlock value].
	^assoc value!

at: key put: value
	| index assoc |
	index := self findIndexFor: key.
	assoc := self basicAt: index.
	assoc == nil
		ifTrue: [
			self basicAt: index put: (Association key: key value: value).
			tally := tally + 1.
			tally * 4 > (self basicSize * 3) ifTrue: [self grow]]
		ifFalse: [assoc value: value].
	^value!

associationAt: key
	^self basicAt: (self findIndexFor: key)!

keys
	| result |
	result := Set new.
	self associationsDo: [:assoc | result add: assoc key].
	^result!

values
	| result |
	result := OrderedCollection new.
	self do: [:value | result add: value].
	^result!

keyAtValue: value
	self associationsDo: [:assoc | assoc value = value ifTrue: [^assoc key]].
	^nil! !

!Dictionary methodsFor: 'adding'!
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^anAssociation! !

!Dictionary methodsFor: 'removing'!
removeKey: key
	^self removeKey: key ifAbsent: [self error: 'Key not found']!

removeKey: key ifAbsent: aBlock
	| assoc |
	assoc := self basicAt: (self findIndexFor: key).
	assoc == nil ifTrue: [^aBlock value].
	super remove: key ifAbsent: [].
	^assoc value! !

!Dictionary methodsFor: 'testing'!
includesKey: key
	^(self basicAt: (self findIndexFor: key)) ~~ nil!

includes: value
	self do: [:each | each = value ifTrue: [^true]].
	^false! !

!Dictionary methodsFor: 'enumerating'!
associationsDo: aBlock
	super do: aBlock!

do: aBlock
	self associationsDo: [:assoc | aBlock value: assoc value]!

keysDo: aBlock
	self associationsDo: [:assoc | aBlock value: assoc key]!

keysAndValuesDo: aBlock
	self associationsDo: [:assoc | aBlock value: assoc key value: assoc value]!

collect: aBlock
	| result |
	result := Bag new.
	self do: [:each | result add: (aBlock value: each)].
	^result!

select: aBlock
	| result |
	result := Dictionary new.
	self associationsDo: [:assoc | (aBlock value: assoc value) ifTrue: [result add: assoc]].
	^result! !

!Dictionary methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: self class name; nextPutAll: ' ('.
	self associationsDo: [:assoc | aStream print: assoc; space].
	aStream nextPut: $)! !

!Bag class methodsFor: 'instance creation'!
new
	^self basicNew setContents! !

!Bag methodsFor: 'private'!
setContents
	contents := Dictionary new! !

!Bag methodsFor: 'adding'!
add: newObject
	^self add: newObject withOccurrences: 1!

add: newObject withOccurrences: anInteger
	contents at: newObject put: (contents at: newObject ifAbsent: [0]) + anInteger.
	^newObject! !

!Bag methodsFor: 'removing'!
remove: oldObject ifAbsent: aBlock
	| count |
	count := contents at: oldObject ifAbsent: [^aBlock value].
	count = 1
		ifTrue: [contents removeKey: oldObject]
		ifFalse: [contents at: oldObject put: count - 1].
	^oldObject! !

!Bag methodsFor: 'accessing'!
size
	| tally |
	tally := 0.
	contents do: [:count | tally := tally + count].
	^tally!

occurrencesOf: anObject
	^contents at: anObject ifAbsent: [0]! !

!Bag methodsFor: 'enumerating'!
do: aBlock
	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]! !
