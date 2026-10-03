"Squeak's kernel, the seventh file: Etoys (Alan Kay, Scott Wallace,
John Maloney, Ted Kaehler, Disney then Viewpoints, 1997; 'Squeak
Etoys, Children and Learning', 2005), programming for children on top
of Morphic. Its first lesson is a car: paint one, open its viewer,
drag out 'car forward by 5' and 'car turn by 5', and it drives in a
circle. Change the 5s while it drives.

What it adds to Morphic is small, because a script is morphs:

- any morph can be told forward: and turn:, Logo's turtle (Papert was
  the inspiration): it has a heading, in degrees, 0 upwards, growing
  clockwise. Morph gets two instance variables for it;
- a morph's viewer (the halo's turquoise handle) shows what it has, x,
  y, heading, kept up to date, and what it can do, each as a phrase of
  tiles with a number one can type over. The !! beside a phrase does it
  once;
- a phrase dragged out of the viewer is a copy; put down on the world
  it becomes a script, a morph that holds phrases, and more phrases
  can be dropped into it. A script that is ticking does its phrases
  once a cycle: its step. There is no other interpreter: a phrase
  sends its message to its morph with perform:.

The car here is drawn, not painted (there are no painting tools): two
polygons turned by its heading. Left out: tests (yes, no), variables,
a script shown as the Smalltalk text it is."!

Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs color heading location'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

Morph subclass: #CarMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

StringMorph subclass: #UpdatingStringMorph
	instanceVariableNames: 'target selector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

StringMorph subclass: #NumberTileMorph
	instanceVariableNames: 'value fresh'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

RectangleMorph subclass: #PhraseTileMorph
	instanceVariableNames: 'target selector label argument'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

RectangleMorph subclass: #ScriptEditorMorph
	instanceVariableNames: 'target ticking title button'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

SystemWindow subclass: #ViewerMorph
	instanceVariableNames: 'target'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Etoys'!

!Morph methodsFor: 'etoys'!
heading
	"Where it points, in degrees: 0 upwards, 90 to the right."
	^heading == nil ifTrue: [0] ifFalse: [heading]!

heading: degrees
	heading := degrees \\ 360.
	self changed!

turn: degrees
	"Clockwise."
	self heading: self heading + degrees!

forward: distance
	"That far the way it points. Its place on the screen is in whole
	pixels; where it really is is kept beside (a step of 4 at 5
	degrees is a third of a pixel sideways: rounded away each time,
	it would never turn). Forgotten if something else moved it."
	| radians |
	(location == nil or: [location x rounded ~= bounds left or: [location y rounded ~= bounds top]])
		ifTrue: [location := bounds origin].
	radians := self heading degreesToRadians.
	location := (location x + (radians sin * distance)) @ (location y - (radians cos * distance)).
	self position: location x rounded @ location y rounded!

x
	^bounds left!

y
	^bounds top!

etoyName
	"What its tiles call it: its class's name without Morph."
	| name |
	name := self class name asString.
	^(name size > 5 and: [(name copyFrom: name size - 4 to: name size) = 'Morph'])
		ifTrue: [name copyFrom: 1 to: name size - 5]
		ifFalse: [name]!

openViewer
	^(ViewerMorph on: self) openInWorld; yourself!

isPhraseTile
	^false! !

!FormCanvas methodsFor: 'drawing'!
fillPolygon: points color: aColor
	"A convex polygon, a row at a time, as an oval: on each row, the
	leftmost and the rightmost of where its edges cross the row's
	middle."
	| top bottom n |
	self setColor: aColor.
	top := points inject: points first y into: [:a :p | a min: p y].
	bottom := points inject: points first y into: [:a :p | a max: p y].
	n := points size.
	(top rounded max: clip top) to: (bottom rounded min: clip bottom) - 1 do: [:row | | y left right |
		y := row + 0.5.
		left := nil.
		right := nil.
		1 to: n do: [:i | | a b x |
			a := points at: i.
			b := points at: i \\ n + 1.
			((a y <= y and: [b y > y]) or: [b y <= y and: [a y > y]]) ifTrue: [
				x := a x + (y - a y * (b x - a x) / (b y - a y)).
				(left == nil or: [x < left]) ifTrue: [left := x].
				(right == nil or: [x > right]) ifTrue: [right := x]]].
		left == nil ifFalse: [
			blt destX: left rounded destY: row; width: right rounded - left rounded height: 1.
			blt copyBits]]! !

!CarMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 44 @ 44.
	color := Color red! !

!CarMorph methodsFor: 'drawing'!
drawOn: aCanvas
	"Seen from above, its nose where it heads: the body, and the
	windshield near the nose."
	aCanvas fillPolygon: (self turned: #(13 4 31 4 31 40 13 40)) color: color.
	aCanvas fillPolygon: (self turned: #(15 12 29 12 29 19 15 19)) color: Color white!

turned: coordinates
	"Points given by pairs, x then y in its box of 44 by 44 with the
	nose upwards, turned by its heading around the box's middle:
	from the middle,

	    x' = x cos h - y sin h      y' = x sin h + y cos h"
	| cx cy radians cos sin points |
	cx := bounds left + (bounds width / 2.0).
	cy := bounds top + (bounds height / 2.0).
	radians := self heading degreesToRadians.
	cos := radians cos.
	sin := radians sin.
	points := Array new: coordinates size // 2.
	1 to: points size do: [:i | | px py |
		px := (coordinates at: i * 2 - 1) - 22.
		py := (coordinates at: i * 2) - 22.
		points at: i put: (cx + (cos * px) - (sin * py)) @ (cy + (sin * px) + (cos * py))].
	^points! !

!UpdatingStringMorph methodsFor: 'accessing'!
target: anObject selector: aSymbol
	"What it shows: what the object answers to that, printed."
	target := anObject.
	selector := aSymbol.
	self step! !

!UpdatingStringMorph methodsFor: 'stepping'!
step
	"Nobody tells it: it asks, each cycle."
	| new |
	new := (target perform: selector) printString.
	new = contents ifFalse: [self contents: new]! !

!NumberTileMorph methodsFor: 'initialize'!
initialize
	super initialize.
	fresh := false.
	self value: 5! !

!NumberTileMorph methodsFor: 'accessing'!
value
	^value!

value: aNumber
	value := aNumber.
	self contents: aNumber printString! !

!NumberTileMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	"The keys are for it: the next digit starts a new number."
	self world hand keyboardFocus: self.
	fresh := true!

keyStroke: anEvent
	"A digit, a minus that changes its sign, a backspace."
	| c |
	c := anEvent keyCharacter.
	c isDigit ifTrue: [
		self value: (fresh ifTrue: [0] ifFalse: [value abs]) * 10 + (c value - 48) * (value < 0 ifTrue: [-1] ifFalse: [1]).
		fresh := false].
	c value = 45 ifTrue: [self value: value negated].
	c value = 8 ifTrue: [self value: value abs // 10 * value sign].
	owner isPhraseTile ifTrue: [owner fit]! !

!NumberTileMorph methodsFor: 'drawing'!
drawOn: aCanvas
	"On white, as a place to type in; on yellow when the keys are
	for it."
	| w |
	w := self world.
	aCanvas fillRectangle: bounds color: ((w notNil and: [w hand keyboardFocus == self]) ifTrue: [Color yellow] ifFalse: [Color white]).
	super drawOn: aCanvas! !

!PhraseTileMorph class methodsFor: 'instance creation'!
target: aMorph selector: aSymbol label: aString argument: aNumber
	"car forward by 5: the message forward: for that morph, said
	that way, with that number."
	^self new setTarget: aMorph selector: aSymbol label: aString argument: aNumber! !

!PhraseTileMorph methodsFor: 'initialize'!
setTarget: aMorph selector: aSymbol label: aString argument: aNumber
	target := aMorph.
	selector := aSymbol.
	borderWidth := 1.
	color := Color r: 4/5 g: 1 b: 4/5.
	label := StringMorph contents: aMorph etoyName, ' ', aString.
	argument := NumberTileMorph new.
	argument value: aNumber.
	self addMorph: label; addMorph: argument.
	self fit!

fit
	"Its words, its number after them, itself around the two."
	label position: bounds origin + (4 @ 2).
	argument position: (label bounds right + 6) @ (bounds top + 2).
	self extent: (argument bounds right - bounds left + 4) @ (label bounds height + 4)! !

!PhraseTileMorph methodsFor: 'accessing'!
isPhraseTile
	^true!

argument
	^argument!

run
	"What it says, done: there is no more to a script's meaning."
	target perform: selector with: argument value! !

!PhraseTileMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	"From a viewer, a copy is taken and the viewer keeps its own;
	anywhere else it is picked up itself."
	self world hand grab: ((owner isKindOf: ViewerMorph) ifTrue: [self duplicate] ifFalse: [self])!

justDroppedInto: aMorph
	"On the world itself: a new script, with this phrase in it."
	| script |
	aMorph isWorldOrHand ifFalse: [^self].
	script := ScriptEditorMorph on: target.
	script position: bounds origin - (4 @ 24).
	aMorph addMorph: script.
	script acceptDroppedMorph: self! !

!ScriptEditorMorph class methodsFor: 'instance creation'!
on: aMorph
	^self new setTarget: aMorph! !

!ScriptEditorMorph methodsFor: 'initialize'!
setTarget: aMorph
	target := aMorph.
	ticking := false.
	borderWidth := 1.
	color := Color r: 1 g: 9/10 b: 7/10.
	title := StringMorph contents: aMorph etoyName, ' script'.
	button := SimpleButtonMorph new.
	button label: 'paused'; target: nil selector: #toggle.
	self addMorph: title; addMorph: button.
	self layout! !

!ScriptEditorMorph methodsFor: 'accessing'!
phrases
	"In the order they are done: from the top."
	^submorphs select: [:m | m isPhraseTile]!

isTicking
	^ticking!

toggle
	"Its button: done every cycle, or not at all."
	ticking := ticking not.
	button label: (ticking ifTrue: ['ticking'] ifFalse: ['paused']).
	self layout! !

!ScriptEditorMorph methodsFor: 'dropping'!
wantsDroppedMorph: aMorph
	^aMorph isPhraseTile!

acceptDroppedMorph: aMorph
	"Under the others."
	self addMorphBack: aMorph.
	self layout! !

!ScriptEditorMorph methodsFor: 'layout'!
layout
	"Its name and its button at the top, the phrases under them, and
	itself as big as that takes."
	| y width extent |
	title position: bounds origin + (4 @ 3).
	button position: (title bounds right + 8) @ (bounds top + 2).
	y := bounds top + 26.
	width := button bounds right - bounds left + 4.
	self phrases do: [:p |
		p position: (bounds left + 4) @ y.
		y := p bounds bottom + 2.
		width := width max: p bounds width + 8].
	extent := width @ (y - bounds top + 2).
	extent = bounds extent ifFalse: [self extent: extent]! !

!ScriptEditorMorph methodsFor: 'stepping'!
step
	"A phrase may have been taken out, or its number typed over. And
	if it is ticking, each phrase is done."
	self layout.
	ticking ifTrue: [self phrases do: [:p | p run]]! !

!ViewerMorph class methodsFor: 'instance creation'!
on: aMorph
	^self new setTarget: aMorph! !

!ViewerMorph methodsFor: 'initialize'!
setTarget: aMorph
	"What the morph has, each shown as it is now; then what it can
	do, a phrase each, a !! before it to do it once."
	| y |
	target := aMorph.
	self labelString: aMorph etoyName, ' viewer'.
	self extent: 220 @ 150.
	y := bounds top + self titleHeight + 4.
	#(#x #y #heading) do: [:each | | name value |
		name := StringMorph contents: aMorph etoyName, '''s ', each.
		name position: (bounds left + 8) @ y.
		value := UpdatingStringMorph new.
		value target: aMorph selector: each.
		value position: (bounds left + 140) @ y.
		self addMorph: name; addMorph: value.
		y := y + 19].
	y := self addCommand: #forward: label: 'forward by' at: y + 4.
	self addCommand: #turn: label: 'turn by' at: y!

addCommand: aSymbol label: aString at: y
	"Answers the y under it."
	| phrase bang |
	phrase := PhraseTileMorph target: target selector: aSymbol label: aString argument: 5.
	phrase position: (bounds left + 30) @ y.
	bang := SimpleButtonMorph new.
	bang label: '!!'; target: phrase selector: #run.
	bang position: (bounds left + 6) @ y.
	self addMorph: phrase; addMorph: bang.
	^phrase bounds bottom + 4! !

!ViewerMorph methodsFor: 'accessing'!
target
	^target!

phrases
	^submorphs select: [:m | m isPhraseTile]! !
