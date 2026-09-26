"The display, as the Blue Book's chapters 18 to 20 have it: Forms,
pictures of one bit per pixel; BitBlt, the one primitive that draws
them (St_bitblt.mli); and Pen, a turtle that draws lines with it,
Logo's idea brought to Smalltalk (Papert and Kay were friends).
Display is the Form of the screen: what a Pen draws there is drawn over
the windows, until the screen menu's 'restore display' erases it."!

!Form class methodsFor: 'instance creation'!
extent: aPoint
	^self width: aPoint x height: aPoint y!

width: w height: h
	^self new setBits: (ByteArray new: w + 15 // 16 * 2 * h) width: w height: h! !

!Form methodsFor: 'private'!
setBits: aByteArray width: w height: h
	bits := aByteArray.
	width := w.
	height := h! !

!Form methodsFor: 'accessing'!
width
	^width!

height
	^height!

extent
	^width @ height!

boundingBox
	^0 @ 0 corner: width @ height!

center
	^(width // 2) @ (height // 2)!

bits
	^bits! !

!Form methodsFor: 'displaying'!
fill: aRectangle rule: anInteger
	(BitBlt destForm: self sourceForm: nil halftoneForm: nil combinationRule: anInteger
		destOrigin: aRectangle origin sourceOrigin: 0 @ 0 extent: aRectangle extent clipRect: self boundingBox) copyBits!

fillWhite
	self fill: self boundingBox rule: 0!

fillBlack
	self fill: self boundingBox rule: 15!

reverse
	self fill: self boundingBox rule: 10!

displayOn: aForm at: aPoint rule: anInteger
	(BitBlt destForm: aForm sourceForm: self halftoneForm: nil combinationRule: anInteger
		destOrigin: aPoint sourceOrigin: 0 @ 0 extent: self extent clipRect: aForm boundingBox) copyBits!

displayAt: aPoint
	self displayOn: Display at: aPoint rule: 3!

pixelAt: aPoint
	"0 or 1, read the BitBlt way: the pixel copied onto a Form of one."
	| one |
	one := Form width: 1 height: 1.
	(BitBlt destForm: one sourceForm: self halftoneForm: nil combinationRule: 3
		destOrigin: 0 @ 0 sourceOrigin: aPoint extent: 1 @ 1 clipRect: one boundingBox) copyBits.
	^(one bits at: 1) bitShift: -7! !

!Form methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'a Form('; print: width; nextPut: $x; print: height; nextPut: $)! !

!BitBlt class methodsFor: 'instance creation'!
destForm: df sourceForm: sf halftoneForm: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect
	^self new
		setDestForm: df sourceForm: sf halftoneForm: hf combinationRule: cr
		destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect! !

!BitBlt methodsFor: 'private'!
setDestForm: df sourceForm: sf halftoneForm: hf combinationRule: cr destOrigin: destOrigin sourceOrigin: sourceOrigin extent: extent clipRect: clipRect
	destForm := df.
	sourceForm := sf.
	halftoneForm := hf.
	combinationRule := cr.
	destX := destOrigin x.
	destY := destOrigin y.
	sourceX := sourceOrigin x.
	sourceY := sourceOrigin y.
	width := extent x.
	height := extent y.
	clipX := clipRect origin x.
	clipY := clipRect origin y.
	clipWidth := clipRect width.
	clipHeight := clipRect height! !

!BitBlt methodsFor: 'accessing'!
destForm: aForm
	destForm := aForm!

sourceForm: aForm
	sourceForm := aForm!

halftoneForm: aForm
	halftoneForm := aForm!

combinationRule: anInteger
	combinationRule := anInteger!

destOrigin: aPoint
	destX := aPoint x.
	destY := aPoint y!

destX: x destY: y
	destX := x.
	destY := y!

width: w height: h
	width := w.
	height := h!

clipRect: aRectangle
	clipX := aRectangle origin x.
	clipY := aRectangle origin y.
	clipWidth := aRectangle width.
	clipHeight := aRectangle height! !

!BitBlt methodsFor: 'copying'!
copyBits
	"The one drawing primitive (St_bitblt.mli)."
	<primitive: 96>
	^self error: 'Bad BitBlt arguments'! !

!Pen class methodsFor: 'instance creation'!
new
	"A pen of one pixel on the Display, at its center, pointing up."
	^super new defaultNib: 1! !

!Pen methodsFor: 'initialize'!
defaultNib: n
	destForm := Display.
	sourceForm := (Form width: n height: n) fillBlack; yourself.
	combinationRule := 7.
	width := n.
	height := n.
	sourceX := 0.
	sourceY := 0.
	self clipRect: Display boundingBox.
	frame := Display boundingBox.
	location := Display center.
	direction := 270.
	penDown := true! !

!Pen methodsFor: 'accessing'!
location
	^location!

direction
	^direction! !

!Pen methodsFor: 'moving'!
up
	penDown := false!

down
	penDown := true!

turn: degrees
	"Clockwise, the y axis going down."
	direction := direction + degrees \\ 360!

north
	direction := 270!

home
	location := Display center!

place: aPoint
	location := aPoint!

go: distance
	"Forward, drawing if the pen is down."
	| old radians |
	old := location.
	radians := direction degreesToRadians.
	location := location + ((radians cos * distance) @ (radians sin * distance)).
	penDown ifTrue: [self drawFrom: old to: location]!

goto: aPoint
	| old |
	old := location.
	location := aPoint.
	penDown ifTrue: [self drawFrom: old to: location]!

drawFrom: startPoint to: endPoint
	"Bresenham's line (1965), a copyBits of the nib at each point:
	the error term says when to step across as well as along."
	| x0 y0 x1 y1 dx dy sx sy err e2 |
	x0 := startPoint x rounded.
	y0 := startPoint y rounded.
	x1 := endPoint x rounded.
	y1 := endPoint y rounded.
	dx := (x1 - x0) abs.
	dy := (y1 - y0) abs negated.
	sx := x0 < x1 ifTrue: [1] ifFalse: [-1].
	sy := y0 < y1 ifTrue: [1] ifFalse: [-1].
	err := dx + dy.
	[destX := x0.
	destY := y0.
	self copyBits.
	x0 = x1 and: [y0 = y1]] whileFalse: [
		e2 := 2 * err.
		e2 >= dy ifTrue: [err := err + dy. x0 := x0 + sx].
		e2 <= dx ifTrue: [err := err + dx. y0 := y0 + sy]]! !

!Pen methodsFor: 'geometric designs'!
dragon: n
	"The dragon curve, the Blue Book's (chapter 20): Pen new dragon: 9."
	n = 0
		ifTrue: [self go: 10]
		ifFalse: [
			n > 0
				ifTrue: [self dragon: n - 1; turn: 90; dragon: 1 - n]
				ifFalse: [self dragon: -1 - n; turn: -90; dragon: 1 + n]]!

spiral: n angle: a
	"Pen new spiral: 200 angle: 89."
	1 to: n do: [:i | self go: i; turn: a]!

polygon: sides length: length
	sides timesRepeat: [self go: length; turn: 360 // sides]!

scribble
	"Pen new scribble: press the button and draw, release to stop --
	the mouse read by Smalltalk while it runs."
	self place: Sensor waitButton.
	[Sensor anyButtonPressed] whileTrue: [self goto: Sensor mousePoint]! !

!Number methodsFor: 'converting'!
degreesToRadians
	^self asFloat * 0.017453292519943295! !

!InputSensor methodsFor: 'mouse'!
mousePoint
	"Where the mouse is, on the Display."
	<primitive: 90>
	self primitiveFailed!

cursorPoint
	^self mousePoint!

buttons
	<primitive: 91>
	self primitiveFailed!

redButtonPressed
	^(self buttons bitAnd: 4) ~= 0!

yellowButtonPressed
	^(self buttons bitAnd: 2) ~= 0!

blueButtonPressed
	^(self buttons bitAnd: 1) ~= 0!

anyButtonPressed
	^self buttons ~= 0!

waitButton
	[self anyButtonPressed] whileFalse.
	^self mousePoint!

waitNoButton
	[self anyButtonPressed] whileTrue! !
