"The numbers. SmallInteger's arithmetic is primitives (and even the
bytecodes' own, St_interp.mli); when one fails -- an overflow, or an
argument of another kind -- the method's Smalltalk runs instead:
overflows go on in LargePositiveInteger and LargeNegativeInteger,
written here over their bytes as in the Blue Book, and mixed kinds are
coerced to the more general one (retry:coercing:, generality: a
SmallInteger 20, a large one 40, a Fraction 60, a Float 80)."!

!Magnitude methodsFor: 'comparing'!
< aMagnitude
	^self subclassResponsibility!

> aMagnitude
	^aMagnitude < self!

<= aMagnitude
	^(aMagnitude < self) not!

>= aMagnitude
	^(self < aMagnitude) not!

between: min and: max
	^self >= min and: [self <= max]!

max: aMagnitude
	^self > aMagnitude ifTrue: [self] ifFalse: [aMagnitude]!

min: aMagnitude
	^self < aMagnitude ifTrue: [self] ifFalse: [aMagnitude]! !

!Character class methodsFor: 'instance creation'!
value: anInteger
	"The unique Character of a code: $a == (Character value: 97)."
	<primitive: 120>
	^self error: 'Not a character code'!

digitValue: anInteger
	^'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ' at: anInteger + 1!

cr
	^self value: 13!

lf
	^self value: 10!

tab
	^self value: 9!

space
	^self value: 32! !

!Character methodsFor: 'accessing'!
value
	^value!

asInteger
	^value!

asCharacter
	^self!

digitValue
	value <= 57 ifTrue: [^value - 48].
	value <= 90 ifTrue: [^value - 55].
	^value - 87! !

!Character methodsFor: 'comparing'!
< aCharacter
	^value < aCharacter asInteger!

= aCharacter
	^self == aCharacter!

hash
	^value! !

!Character methodsFor: 'testing'!
isCharacter
	^true!

isVowel
	^'aeiouAEIOU' includes: self!

isLetter
	^self isUppercase or: [self isLowercase]!

isUppercase
	^value between: 65 and: 90!

isLowercase
	^value between: 97 and: 122!

isDigit
	^value between: 48 and: 57!

isSeparator
	^value = 32 or: [value = 13 or: [value = 9 or: [value = 10]]]! !

!Character methodsFor: 'converting'!
asUppercase
	self isLowercase ifTrue: [^Character value: value - 32].
	^self!

asLowercase
	self isUppercase ifTrue: [^Character value: value + 32].
	^self!

asString
	^String with: self! !

!Character methodsFor: 'printing'!
printOn: aStream
	aStream nextPut: $$; nextPut: self!

displayString
	^String with: self! !

!Character methodsFor: 'copying'!
shallowCopy
	^self! !

!Number methodsFor: 'arithmetic'!
+ aNumber
	^self subclassResponsibility!

- aNumber
	^self + aNumber negated!

negated
	^0 - self!

abs
	self negative ifTrue: [^self negated].
	^self!

reciprocal
	^1 / self!

squared
	^self * self!

// aNumber
	^(self / aNumber) floor!

\\ aNumber
	^self - (self // aNumber * aNumber)!

quo: aNumber
	^(self / aNumber) truncated!

rem: aNumber
	^self - ((self quo: aNumber) * aNumber)!

raisedTo: anInteger
	| result |
	anInteger < 0 ifTrue: [^(self raisedTo: anInteger negated) reciprocal].
	result := 1.
	anInteger timesRepeat: [result := result * self].
	^result!

sqrt
	^self asFloat sqrt! !

!Number methodsFor: 'testing'!
isNumber
	^true!

negative
	^self < 0!

positive
	^self >= 0!

strictlyPositive
	^self > 0!

isZero
	^self = 0!

sign
	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0!

even
	^self \\ 2 = 0!

odd
	^self even not! !

!Number methodsFor: 'truncation and round off'!
truncated
	^self subclassResponsibility!

floor
	| t |
	t := self truncated.
	(self < 0 and: [t ~= self]) ifTrue: [^t - 1].
	^t!

ceiling
	| t |
	t := self truncated.
	(self > 0 and: [t ~= self]) ifTrue: [^t + 1].
	^t!

rounded
	^(self + (self sign / 2)) truncated!

asInteger
	^self truncated! !

!Number methodsFor: 'coercing'!
generality
	^self subclassResponsibility!

coerce: aNumber
	^self subclassResponsibility!

retry: aSymbol coercing: aNumber
	"Do the operation again, the less general number made like the
	other: 3 + (1/2) becomes (3/1) + (1/2)."
	aNumber isNumber ifFalse: [
		aSymbol == #= ifTrue: [^false].
		aSymbol == #~= ifTrue: [^true].
		^self error: 'Not a number: ', aNumber printString].
	self generality < aNumber generality
		ifTrue: [^(aNumber coerce: self) perform: aSymbol with: aNumber].
	self generality > aNumber generality
		ifTrue: [^self perform: aSymbol with: (self coerce: aNumber)].
	^self error: 'Coercion failed'! !

!Number methodsFor: 'intervals'!
to: stop
	^Interval from: self to: stop by: 1!

to: stop by: step
	^Interval from: self to: stop by: step!

to: stop do: aBlock
	"Sent only when the compiler could not inline it."
	| i |
	i := self.
	[i <= stop] whileTrue: [aBlock value: i. i := i + 1]!

to: stop by: step do: aBlock
	| i |
	i := self.
	step > 0
		ifTrue: [[i <= stop] whileTrue: [aBlock value: i. i := i + step]]
		ifFalse: [[i >= stop] whileTrue: [aBlock value: i. i := i + step]]! !

!Number methodsFor: 'converting'!
@ y
	"A Point."
	<primitive: 18>
	^Point x: self y: y!

asPoint
	^self @ self! !

!Number methodsFor: 'printing'!
printString: base
	| aStream |
	aStream := WriteStream on: (String new: 16).
	self printOn: aStream base: base.
	^aStream contents! !

!Integer methodsFor: 'testing'!
isInteger
	^true! !

!Integer methodsFor: 'arithmetic'!
+ anInteger
	"Here when a large integer is involved: SmallInteger's own + is a
	primitive."
	anInteger isInteger ifFalse: [^self retry: #+ coercing: anInteger].
	^self asLargeInteger digitPlus: anInteger asLargeInteger!

- anInteger
	anInteger isInteger ifFalse: [^self retry: #- coercing: anInteger].
	^self asLargeInteger digitPlus: anInteger negated asLargeInteger!

* anInteger
	anInteger isInteger ifFalse: [^self retry: #* coercing: anInteger].
	^self asLargeInteger digitTimes: anInteger asLargeInteger!

/ anInteger
	"An Integer if it divides exactly, else a Fraction."
	anInteger isInteger ifFalse: [^self retry: #/ coercing: anInteger].
	anInteger = 0 ifTrue: [^self error: 'Division by zero'].
	^(Fraction numerator: self denominator: anInteger) reduced!

quo: anInteger
	"Truncated towards zero."
	anInteger isInteger ifFalse: [^self retry: #quo: coercing: anInteger].
	anInteger = 0 ifTrue: [^self error: 'Division by zero'].
	^(self digitDivide: anInteger) at: 1!

rem: anInteger
	anInteger isInteger ifFalse: [^self retry: #rem: coercing: anInteger].
	anInteger = 0 ifTrue: [^self error: 'Division by zero'].
	^(self digitDivide: anInteger) at: 2!

// anInteger
	"Truncated towards minus infinity."
	| qr |
	anInteger isInteger ifFalse: [^self retry: #// coercing: anInteger].
	anInteger = 0 ifTrue: [^self error: 'Division by zero'].
	qr := self digitDivide: anInteger.
	((qr at: 2) ~= 0 and: [self negative ~~ anInteger negative]) ifTrue: [^(qr at: 1) - 1].
	^qr at: 1!

\\ anInteger
	"The remainder of //: the sign of the divisor."
	| r |
	anInteger isInteger ifFalse: [^self retry: #\\ coercing: anInteger].
	anInteger = 0 ifTrue: [^self error: 'Division by zero'].
	r := (self digitDivide: anInteger) at: 2.
	(r ~= 0 and: [r negative ~~ anInteger negative]) ifTrue: [^r + anInteger].
	^r!

< anInteger
	anInteger isInteger ifFalse: [^self retry: #< coercing: anInteger].
	^(self asLargeInteger compareWith: anInteger asLargeInteger) < 0!

= anInteger
	anInteger isNumber ifFalse: [^false].
	anInteger isInteger ifFalse: [^self retry: #= coercing: anInteger].
	^(self asLargeInteger compareWith: anInteger asLargeInteger) = 0!

hash
	^(self \\ 1073741823) hash!

negated
	^0 - self!

gcd: anInteger
	"Euclid's."
	| a b t |
	a := self abs.
	b := anInteger abs.
	[b = 0] whileFalse: [t := a \\ b. a := b. b := t].
	^a!

lcm: anInteger
	^(self * anInteger) abs // (self gcd: anInteger)!

factorial
	"100 factorial printString size = 158."
	| result |
	self < 0 ifTrue: [^self error: 'Not valid for negative integers'].
	result := 1.
	2 to: self do: [:i | result := result * i].
	^result!

timesRepeat: aBlock
	| i |
	i := 1.
	[i <= self] whileTrue: [aBlock value. i := i + 1]!

isPrime
	self < 2 ifTrue: [^false].
	2 to: self sqrtFloor do: [:i | self \\ i = 0 ifTrue: [^false]].
	^true!

sqrtFloor
	| r |
	r := self asFloat sqrt truncated.
	[r * r > self] whileTrue: [r := r - 1].
	[(r + 1) * (r + 1) <= self] whileTrue: [r := r + 1].
	^r! !

!Integer methodsFor: 'coercing'!
generality
	^40!

coerce: aNumber
	^aNumber asLargeInteger! !

!Integer methodsFor: 'converting'!
asFloat
	"A large integer's bytes, summed as floats."
	| f |
	f := 0.0.
	self digitLength to: 1 by: -1 do: [:i | f := f * 256.0 + (self digitAt: i)].
	self negative ifTrue: [^f negated].
	^f!

truncated
	^self!

asCharacter
	^Character value: self!

digitDivide: anInteger
	"The quotient and the remainder truncated towards zero, an Array:
	by the schoolbook's short division when the divisor is small
	enough for a digit times it to stay a SmallInteger, else bit by
	bit."
	| a b qr q r |
	a := self abs asLargeInteger.
	b := anInteger abs.
	qr := (b isSmallInteger and: [b < 4194304])
		ifTrue: [a shortDivide: b]
		ifFalse: [a longDivide: b].
	q := qr at: 1.
	r := qr at: 2.
	self negative == anInteger negative ifFalse: [q := q negated].
	self negative ifTrue: [r := r negated].
	^Array with: q with: r! !

!Integer methodsFor: 'printing'!
printOn: aStream
	self printOn: aStream base: 10!

printOn: aStream base: base
	self < 0 ifTrue: [aStream nextPut: $-. ^self negated printOn: aStream base: base].
	self >= base ifTrue: [self // base printOn: aStream base: base].
	aStream nextPut: (Character digitValue: self \\ base)! !

!SmallInteger class methodsFor: 'instance creation'!
new
	^self error: 'SmallIntegers are not made, they are'! !

!SmallInteger methodsFor: 'arithmetic'!
+ aNumber
	<primitive: 1>
	^super + aNumber!

- aNumber
	<primitive: 2>
	^super - aNumber!

< aNumber
	<primitive: 3>
	^super < aNumber!

> aNumber
	<primitive: 4>
	^super > aNumber!

<= aNumber
	<primitive: 5>
	^super <= aNumber!

>= aNumber
	<primitive: 6>
	^super >= aNumber!

= aNumber
	<primitive: 7>
	^super = aNumber!

~= aNumber
	<primitive: 8>
	^super ~= aNumber!

* aNumber
	<primitive: 9>
	^super * aNumber!

/ aNumber
	<primitive: 10>
	^super / aNumber!

\\ aNumber
	<primitive: 11>
	^super \\ aNumber!

// aNumber
	<primitive: 12>
	^super // aNumber!

quo: aNumber
	<primitive: 13>
	^super quo: aNumber!

bitAnd: anInteger
	<primitive: 14>
	^self error: 'Not a SmallInteger'!

bitOr: anInteger
	<primitive: 15>
	^self error: 'Not a SmallInteger'!

bitXor: anInteger
	<primitive: 16>
	^self error: 'Not a SmallInteger'!

bitShift: anInteger
	<primitive: 17>
	anInteger > 0 ifTrue: [^self asLargeInteger * (2 raisedTo: anInteger)].
	^self error: 'Not a SmallInteger'! !

!SmallInteger methodsFor: 'testing'!
isSmallInteger
	^true!

even
	^(self bitAnd: 1) = 0! !

!SmallInteger methodsFor: 'coercing'!
generality
	^20!

coerce: aNumber
	^aNumber!

asLargeInteger
	<primitive: 154>
	self primitiveFailed! !

!SmallInteger methodsFor: 'converting'!
asFloat
	<primitive: 40>
	self primitiveFailed!

hash
	^self! !

!SmallInteger methodsFor: 'copying'!
shallowCopy
	^self! !

!LargePositiveInteger methodsFor: 'digits'!
digitAt: index
	"A byte of the magnitude, least significant first; 0 past the end."
	<primitive: 60>
	^0!

digitAt: index put: value
	<primitive: 61>
	self primitiveFailed!

digitLength
	<primitive: 62>
	self primitiveFailed!

normalize
	"The leading zero bytes dropped, a SmallInteger if it fits."
	<primitive: 155>
	self primitiveFailed!

asLargeInteger
	^self!

negative
	^false!

negated
	| result |
	result := (self negative ifTrue: [LargePositiveInteger] ifFalse: [LargeNegativeInteger]) new: self digitLength.
	1 to: self digitLength do: [:i | result digitAt: i put: (self digitAt: i)].
	^result normalize!

abs
	self negative ifTrue: [^self negated].
	^self! !

!LargePositiveInteger methodsFor: 'digit arithmetic'!
digitPlus: arg
	"The sum of two large integers of any signs."
	self negative == arg negative
		ifTrue: [^(self magnitudeAdd: arg negative: self negative) normalize].
	(self magnitudeCompare: arg) >= 0
		ifTrue: [^(self magnitudeSubtract: arg negative: self negative) normalize].
	^(arg magnitudeSubtract: self negative: arg negative) normalize!

magnitudeAdd: arg negative: aBoolean
	| len result carry sum |
	len := (self digitLength max: arg digitLength) + 1.
	result := (aBoolean ifTrue: [LargeNegativeInteger] ifFalse: [LargePositiveInteger]) new: len.
	carry := 0.
	1 to: len do: [:i |
		sum := (self digitAt: i) + (arg digitAt: i) + carry.
		result digitAt: i put: (sum bitAnd: 255).
		carry := sum bitShift: -8].
	^result!

magnitudeSubtract: arg negative: aBoolean
	"The receiver's magnitude is the larger."
	| len result borrow diff |
	len := self digitLength.
	result := (aBoolean ifTrue: [LargeNegativeInteger] ifFalse: [LargePositiveInteger]) new: len.
	borrow := 0.
	1 to: len do: [:i |
		diff := (self digitAt: i) - (arg digitAt: i) - borrow.
		diff < 0
			ifTrue: [diff := diff + 256. borrow := 1]
			ifFalse: [borrow := 0].
		result digitAt: i put: diff].
	^result!

magnitudeCompare: arg
	"-1, 0 or 1."
	| a b |
	(self digitLength max: arg digitLength) to: 1 by: -1 do: [:i |
		a := self digitAt: i.
		b := arg digitAt: i.
		a < b ifTrue: [^-1].
		a > b ifTrue: [^1]].
	^0!

compareWith: arg
	self negative == arg negative ifFalse: [self negative ifTrue: [^-1] ifFalse: [^1]].
	self negative ifTrue: [^arg magnitudeCompare: self].
	^self magnitudeCompare: arg!

digitTimes: arg
	"The schoolbook's: each digit of one by each of the other."
	| la lb result carry prod k |
	la := self digitLength.
	lb := arg digitLength.
	result := (self negative == arg negative ifTrue: [LargePositiveInteger] ifFalse: [LargeNegativeInteger]) new: la + lb.
	1 to: la do: [:i |
		carry := 0.
		1 to: lb do: [:j |
			k := i + j - 1.
			prod := (self digitAt: i) * (arg digitAt: j) + (result digitAt: k) + carry.
			result digitAt: k put: (prod bitAnd: 255).
			carry := prod bitShift: -8].
		result digitAt: i + lb put: carry].
	^result normalize!

shortDivide: divisor
	"The magnitude by a small SmallInteger, a digit at a time from the
	top: the quotient and the remainder."
	| len q r |
	len := self digitLength.
	q := LargePositiveInteger new: len.
	r := 0.
	len to: 1 by: -1 do: [:i |
		r := r * 256 + (self digitAt: i).
		q digitAt: i put: r // divisor.
		r := r \\ divisor].
	^Array with: q normalize with: r!

longDivide: divisor
	"A bit at a time: slow, but only for large divisors."
	| q r bit |
	q := 0.
	r := 0.
	self digitLength * 8 to: 1 by: -1 do: [:i |
		bit := ((self digitAt: i - 1 // 8 + 1) bitShift: 0 - (i - 1 \\ 8)) bitAnd: 1.
		r := r * 2 + bit.
		q := q * 2.
		r >= divisor ifTrue: [r := r - divisor. q := q + 1]].
	^Array with: q with: r! !

!LargePositiveInteger methodsFor: 'bit manipulation'!
bitShift: anInteger
	anInteger >= 0 ifTrue: [^self * (2 raisedTo: anInteger)].
	^self // (2 raisedTo: anInteger negated)!

hash
	^self digitAt: 1! !

!LargeNegativeInteger methodsFor: 'testing'!
negative
	^true! !

!Fraction class methodsFor: 'instance creation'!
numerator: n denominator: d
	"Not reduced: Integer>>/ reduces it."
	^self new setNumerator: n denominator: d! !

!Fraction methodsFor: 'private'!
setNumerator: n denominator: d
	d < 0
		ifTrue: [numerator := n negated. denominator := d negated]
		ifFalse: [numerator := n. denominator := d]! !

!Fraction methodsFor: 'accessing'!
numerator
	^numerator!

denominator
	^denominator! !

!Fraction methodsFor: 'arithmetic'!
+ aNumber
	aNumber isNumber ifFalse: [^self retry: #+ coercing: aNumber].
	aNumber generality = self generality ifFalse: [^self retry: #+ coercing: aNumber].
	^(Fraction
		numerator: numerator * aNumber denominator + (aNumber numerator * denominator)
		denominator: denominator * aNumber denominator) reduced!

- aNumber
	aNumber isNumber ifFalse: [^self retry: #- coercing: aNumber].
	aNumber generality = self generality ifFalse: [^self retry: #- coercing: aNumber].
	^(Fraction
		numerator: numerator * aNumber denominator - (aNumber numerator * denominator)
		denominator: denominator * aNumber denominator) reduced!

* aNumber
	aNumber isNumber ifFalse: [^self retry: #* coercing: aNumber].
	aNumber generality = self generality ifFalse: [^self retry: #* coercing: aNumber].
	^(Fraction
		numerator: numerator * aNumber numerator
		denominator: denominator * aNumber denominator) reduced!

/ aNumber
	aNumber isNumber ifFalse: [^self retry: #/ coercing: aNumber].
	aNumber generality = self generality ifFalse: [^self retry: #/ coercing: aNumber].
	^(Fraction
		numerator: numerator * aNumber denominator
		denominator: denominator * aNumber numerator) reduced!

negated
	^Fraction numerator: numerator negated denominator: denominator!

reduced
	"Lowest terms, and an Integer when the denominator is 1."
	| g |
	numerator = 0 ifTrue: [^0].
	g := numerator gcd: denominator.
	g = denominator ifTrue: [^numerator // g].
	^Fraction numerator: numerator // g denominator: denominator // g! !

!Fraction methodsFor: 'comparing'!
< aNumber
	aNumber isNumber ifFalse: [^self retry: #< coercing: aNumber].
	aNumber generality = self generality ifFalse: [^self retry: #< coercing: aNumber].
	^numerator * aNumber denominator < (aNumber numerator * denominator)!

= aNumber
	aNumber isNumber ifFalse: [^false].
	aNumber generality = self generality ifFalse: [^self retry: #= coercing: aNumber].
	^numerator = aNumber numerator and: [denominator = aNumber denominator]!

hash
	^numerator hash bitXor: denominator hash! !

!Fraction methodsFor: 'truncation and round off'!
truncated
	^numerator quo: denominator! !

!Fraction methodsFor: 'coercing'!
generality
	^60!

coerce: aNumber
	^Fraction numerator: aNumber denominator: 1! !

!Fraction methodsFor: 'converting'!
asFloat
	^numerator asFloat / denominator asFloat! !

!Fraction methodsFor: 'printing'!
printOn: aStream
	aStream print: numerator; nextPut: $/; print: denominator! !

!Float methodsFor: 'arithmetic'!
+ aNumber
	<primitive: 41>
	^self retry: #+ coercing: aNumber!

- aNumber
	<primitive: 42>
	^self retry: #- coercing: aNumber!

< aNumber
	<primitive: 43>
	^self retry: #< coercing: aNumber!

> aNumber
	<primitive: 44>
	^self retry: #> coercing: aNumber!

<= aNumber
	<primitive: 45>
	^self retry: #<= coercing: aNumber!

>= aNumber
	<primitive: 46>
	^self retry: #>= coercing: aNumber!

= aNumber
	<primitive: 47>
	^self retry: #= coercing: aNumber!

~= aNumber
	<primitive: 48>
	^self retry: #~= coercing: aNumber!

* aNumber
	<primitive: 49>
	^self retry: #* coercing: aNumber!

/ aNumber
	<primitive: 50>
	aNumber = 0 ifTrue: [^self error: 'Division by zero'].
	^self retry: #/ coercing: aNumber!

sqrt
	<primitive: 55>
	self primitiveFailed!

sin
	<primitive: 56>
	self primitiveFailed!

cos
	^(self + 1.5707963267948966) sin!

arcTan
	<primitive: 57>
	self primitiveFailed!

ln
	<primitive: 58>
	self primitiveFailed!

exp
	<primitive: 59>
	self primitiveFailed! !

!Float methodsFor: 'truncation and round off'!
truncated
	"Too large for a SmallInteger: the high part and the low part."
	| q |
	<primitive: 51>
	q := (self / 65536.0) truncated.
	^q * 65536 + (self - (q * 65536.0)) truncated! !

!Float methodsFor: 'truncation and round off'!
rounded
	^(self + (self sign * 0.5)) truncated! !

!Float methodsFor: 'coercing'!
generality
	^80!

coerce: aNumber
	^aNumber asFloat!

asFloat
	^self!

hash
	^self truncated hash! !

!Float methodsFor: 'printing'!
printString
	<primitive: 130>
	self primitiveFailed!

printOn: aStream
	aStream nextPutAll: self printString! !

!Point class methodsFor: 'instance creation'!
x: xInteger y: yInteger
	^self new setX: xInteger setY: yInteger! !

!Point methodsFor: 'private'!
setX: xValue setY: yValue
	x := xValue.
	y := yValue! !

!Point methodsFor: 'accessing'!
x
	^x!

y
	^y! !

!Point methodsFor: 'arithmetic'!
+ arg
	| p |
	p := arg asPoint.
	^(x + p x) @ (y + p y)!

- arg
	| p |
	p := arg asPoint.
	^(x - p x) @ (y - p y)!

* arg
	| p |
	p := arg asPoint.
	^(x * p x) @ (y * p y)!

/ arg
	| p |
	p := arg asPoint.
	^(x / p x) @ (y / p y)!

// arg
	| p |
	p := arg asPoint.
	^(x // p x) @ (y // p y)!

abs
	^x abs @ y abs! !

!Point methodsFor: 'comparing'!
= aPoint
	^(aPoint isKindOf: Point) and: [x = aPoint x and: [y = aPoint y]]!

hash
	^x hash bitXor: y hash!

< aPoint
	^x < aPoint x and: [y < aPoint y]!

> aPoint
	^x > aPoint x and: [y > aPoint y]!

<= aPoint
	^x <= aPoint x and: [y <= aPoint y]!

>= aPoint
	^x >= aPoint x and: [y >= aPoint y]!

max: aPoint
	^(x max: aPoint x) @ (y max: aPoint y)!

min: aPoint
	^(x min: aPoint x) @ (y min: aPoint y)! !

!Point methodsFor: 'point functions'!
dist: aPoint
	| dx dy |
	dx := aPoint x - x.
	dy := aPoint y - y.
	^(dx * dx + (dy * dy)) sqrt!

transpose
	^y @ x!

corner: aPoint
	^Rectangle origin: self corner: aPoint!

extent: aPoint
	^Rectangle origin: self corner: self + aPoint! !

!Point methodsFor: 'converting'!
asPoint
	^self! !

!Point methodsFor: 'printing'!
printOn: aStream
	x printOn: aStream.
	aStream nextPut: $@.
	y printOn: aStream! !

!Rectangle class methodsFor: 'instance creation'!
origin: originPoint corner: cornerPoint
	^self new setOrigin: originPoint corner: cornerPoint! !

!Rectangle methodsFor: 'private'!
setOrigin: originPoint corner: cornerPoint
	origin := originPoint.
	corner := cornerPoint! !

!Rectangle methodsFor: 'accessing'!
origin
	^origin!

corner
	^corner!

width
	^corner x - origin x!

height
	^corner y - origin y!

extent
	^corner - origin!

area
	^self width * self height! !

!Rectangle methodsFor: 'rectangle functions'!
center
	"The Blue Book's example of compiled code (St_bytecode.mli):
	0 1 176 119 185 124."
	^origin + corner / 2!

containsPoint: aPoint
	^origin <= aPoint and: [aPoint < corner]!

intersects: aRectangle
	^(origin max: aRectangle origin) < (corner min: aRectangle corner)! !

!Rectangle methodsFor: 'comparing'!
= aRectangle
	^(aRectangle isKindOf: Rectangle) and: [origin = aRectangle origin and: [corner = aRectangle corner]]!

hash
	^origin hash bitXor: corner hash! !

!Rectangle methodsFor: 'printing'!
printOn: aStream
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream! !
