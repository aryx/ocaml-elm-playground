"Squeak's kernel, the fourth file: Morphic (John Maloney and Randall
Smith, for Self, 1995; Squeak's from 1997). MiniMorphic
(kernel/morphic/MiniMorphic.st) is its four ideas in one file, to read
first: everything on the screen is a Morph; a morph is alive (step);
nobody redraws, a morph says where it changed and the world redraws
those rectangles; the cycle -- events, steps, display -- is the whole
program. Here is what makes it an environment:

- colour and shapes: a FormCanvas draws rectangles, ovals and text on
  a Form of 32 bits, clipped;
- events: the HandMorph turns the mouse and the keyboard into
  messages. A button going down is offered to the morph under the
  hand, then to its owner, and on up: the first that says
  handlesMouseDown: gets mouseDown:, then every mouseMove: and the
  mouseUp:, wherever the hand goes (it has the mouse focus). If none
  does, the morph is picked up: anything on the screen can be dragged.
  The keys go to the morph that has the keyboard focus;
- the halo: the blue button on a morph puts handles around it -- to
  delete it, pick it up, duplicate it, resize it, inspect it. They
  work on any morph, since they only send it delete, duplicate,
  extent: and inspect. The
  halo is a morph too, and follows its target by its step.

The morphs themselves are in Morphs.st. The interpreter knows none of
this: all it gives is BitBlt, the mouse, the keys and the clock."!

Object subclass: #FormCanvas
	instanceVariableNames: 'form clip blt color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

Object subclass: #MorphicEvent
	instanceVariableNames: 'position buttons key'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

Morph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus lastButtons dragging shadow'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

Morph subclass: #PasteUpMorph
	instanceVariableNames: 'hand canvas damage'
	classVariableNames: 'Current'
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

Morph subclass: #HaloMorph
	instanceVariableNames: 'target'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halo'!

Morph subclass: #HaloHandleMorph
	instanceVariableNames: 'action'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Halo'!

!InputSensor methodsFor: 'keyboard'!
keyboard
	"The next character typed, as its code, taken out of the keys
	waiting; nil when there is none."
	<primitive: 92>
	self primitiveFailed! !

!Rectangle methodsFor: 'morphic'!
left
	^origin x!

top
	^origin y!

right
	^corner x!

bottom
	^corner y!

translateBy: aPoint
	^Rectangle origin: origin + aPoint corner: corner + aPoint!

insetBy: anInteger
	"Smaller by that much on every side."
	^Rectangle origin: origin + anInteger corner: corner - anInteger!

expandBy: anInteger
	^Rectangle origin: origin - anInteger corner: corner + anInteger!

intersects: aRectangle
	"The kernel's, in four comparisons of numbers and no Point made:
	it is asked of every morph for every rectangle redrawn."
	^origin x < aRectangle corner x
		and: [aRectangle origin x < corner x
		and: [origin y < aRectangle corner y
		and: [aRectangle origin y < corner y]]]!

intersect: aRectangle
	"What the two have in common; empty (no width) if nothing."
	^Rectangle origin: (origin max: aRectangle origin) corner: (corner min: aRectangle corner)!

merge: aRectangle
	"The smallest rectangle holding both."
	^Rectangle origin: (origin min: aRectangle origin) corner: (corner max: aRectangle corner)! !

!FormCanvas class methodsFor: 'instance creation'!
on: aForm
	^self new setForm: aForm! !

!FormCanvas methodsFor: 'private'!
setForm: aForm
	"One BitBlt for everything filled, with no source: only its
	halftone (the colour), its rectangle and its clipping change."
	form := aForm.
	clip := aForm boundingBox.
	blt := BitBlt destForm: aForm sourceForm: nil halftoneForm: nil combinationRule: 3
		destOrigin: 0 @ 0 sourceOrigin: 0 @ 0 extent: 0 @ 0 clipRect: clip!

setColor: aColor
	"The colour's pixel as the halftone: stored, or blended if it is
	glass. Not made again for the colour just used."
	aColor == color ifTrue: [^self].
	color := aColor.
	blt halftoneForm: (aColor pixelFormForDepth: form depth);
		combinationRule: ((form depth = 32 and: [aColor isOpaque not]) ifTrue: [24] ifFalse: [3])! !

!FormCanvas methodsFor: 'clipping'!
clip
	^clip!

clip: aRectangle
	"Nothing is drawn outside it, whatever is asked: how the world
	redraws one rectangle of the screen and no more."
	clip := aRectangle.
	blt clipRect: aRectangle!

clipBy: aRectangle during: aBlock
	"A morph that keeps what it draws inside itself."
	| old |
	old := clip.
	self clip: (clip intersect: aRectangle).
	aBlock value.
	self clip: old! !

!FormCanvas methodsFor: 'drawing'!
fillRectangle: aRectangle color: aColor
	self setColor: aColor.
	blt destX: aRectangle left destY: aRectangle top; width: aRectangle width height: aRectangle height.
	blt copyBits!

frameRectangle: r width: w color: aColor
	"Its four sides."
	self fillRectangle: (r origin corner: r right @ (r top + w)) color: aColor.
	self fillRectangle: (r left @ (r bottom - w) corner: r corner) color: aColor.
	self fillRectangle: (r left @ (r top + w) corner: (r left + w) @ (r bottom - w)) color: aColor.
	self fillRectangle: ((r right - w) @ (r top + w) corner: r right @ (r bottom - w)) color: aColor!

fillOval: r color: aColor
	"A row at a time: how far the ellipse comes in from its box at that
	height. From its equation, a and b its width and height, k how
	far the row's middle is from the oval's, in half pixels:

	    half the row = a / 2 * sqrt (1 - (k / b) squared)
	                 = sqrt (b squared - k squared) * a / (2 * b)

	Integers but for the root, and no Integer added to a Float: that
	is a coercion each time."
	| a b scale mid |
	(r intersects: clip) ifFalse: [^self].
	self setColor: aColor.
	a := r width.
	b := r height.
	b > 0 ifFalse: [^self].
	scale := a asFloat / (2 * b) asFloat.
	mid := a asFloat / 2.0.
	(r top max: clip top) to: (r bottom min: clip bottom) - 1 do: [:y | | k inset |
		k := y - r top * 2 + 1 - b.
		inset := (mid - ((b * b - (k * k)) asFloat sqrt * scale) + 0.5) truncated.
		blt destX: r left + inset destY: y; width: a - inset - inset height: 1.
		blt copyBits]!

drawString: aString at: aPoint font: aFont color: aColor
	form drawString: aString at: aPoint font: aFont color: aColor clipRect: clip! !

!MorphicEvent class methodsFor: 'instance creation'!
position: aPoint buttons: anInteger key: aCharacter
	^self new setPosition: aPoint buttons: anInteger key: aCharacter! !

!MorphicEvent methodsFor: 'private'!
setPosition: aPoint buttons: anInteger key: aCharacter
	position := aPoint.
	buttons := anInteger.
	key := aCharacter! !

!MorphicEvent methodsFor: 'accessing'!
position
	"Where the hand was."
	^position!

keyCharacter
	^key!

redButtonPressed
	"The left one, as Smalltalk-80 named them: red, yellow (the
	middle), blue (the right)."
	^(buttons bitAnd: 4) ~= 0!

yellowButtonPressed
	^(buttons bitAnd: 2) ~= 0!

blueButtonPressed
	^(buttons bitAnd: 1) ~= 0! !

!Morph class methodsFor: 'instance creation'!
new
	^super new initialize! !

!Morph methodsFor: 'initialize'!
initialize
	bounds := 0 @ 0 corner: 50 @ 40.
	submorphs := OrderedCollection new.
	color := Color blue! !

!Morph methodsFor: 'accessing'!
bounds
	^bounds!

position
	^bounds origin!

extent
	^bounds extent!

color
	^color!

owner
	^owner!

submorphs
	^submorphs!

fullBounds
	"With what it holds, which may stick out of it."
	| r |
	r := bounds.
	submorphs do: [:m | r := r merge: m fullBounds].
	^r!

world
	"The morph at the top, if it is a world: a morph in nobody's hands
	and on no screen has none."
	^owner == nil ifTrue: [nil] ifFalse: [owner world]!

root
	"The morph that holds it and is itself right in the world, or in
	the hand: what is picked up when no part of it wants the mouse."
	^(owner == nil or: [owner isWorldOrHand]) ifTrue: [self] ifFalse: [owner root]!

isWorldOrHand
	^false!

isMenu
	^false!

isHalo
	^false! !

!Morph methodsFor: 'changing'!
color: aColor
	color := aColor.
	self changed!

position: aPoint
	"Move, with all it holds. The place left and the place taken both
	changed: one rectangle if they touch (a small step, the usual
	case), so that the morph is redrawn once, not once for each."
	| delta old new |
	delta := aPoint - bounds origin.
	(delta x = 0 and: [delta y = 0]) ifTrue: [^self].
	old := self fullBounds.
	self privateMoveBy: delta.
	new := self fullBounds.
	(old intersects: new)
		ifTrue: [self invalidRect: (old merge: new)]
		ifFalse: [self invalidRect: old; invalidRect: new]!

extent: aPoint
	self changed.
	bounds := bounds origin extent: aPoint.
	self extentChanged.
	self changed!

bounds: aRectangle
	self position: aRectangle origin; extent: aRectangle extent!

extentChanged
	"For a morph that places what it holds: its size is new."!

privateMoveBy: delta
	bounds := bounds translateBy: delta.
	submorphs do: [:m | m privateMoveBy: delta]!

changed
	"All a morph does about the screen: say where it is no longer
	right. Its submorphs may stick out of it."
	self invalidRect: bounds.
	submorphs do: [:m | m changed]!

invalidRect: aRectangle
	"Up to the world, which keeps it."
	owner == nil ifFalse: [owner invalidRect: aRectangle]! !

!Morph methodsFor: 'submorphs'!
addMorph: aMorph
	"In front of the others: the first submorph is the frontmost."
	aMorph delete.
	submorphs addFirst: aMorph.
	aMorph privateOwner: self.
	aMorph changed!

addMorphBack: aMorph
	aMorph delete.
	submorphs addLast: aMorph.
	aMorph privateOwner: self.
	aMorph changed!

delete
	"Out of its owner, off the screen."
	owner == nil ifTrue: [^self].
	self changed.
	owner submorphs remove: self ifAbsent: [].
	owner := nil!

privateOwner: aMorph
	owner := aMorph!

comeToFront
	(owner == nil or: [owner submorphs first == self]) ifFalse: [owner addMorph: self]!

containsPoint: aPoint
	"A morph that is not a rectangle says where it really is."
	^bounds containsPoint: aPoint!

morphAt: aPoint
	"The frontmost morph there: a submorph before its owner, the first
	submorph before the others."
	submorphs do: [:m | | found |
		found := m morphAt: aPoint.
		found == nil ifFalse: [^found]].
	^(self containsPoint: aPoint) ifTrue: [self] ifFalse: [nil]!

allMorphsDo: aBlock
	aBlock value: self.
	submorphs do: [:m | m allMorphsDo: aBlock]! !

!Morph methodsFor: 'copying'!
duplicate
	"A copy of it and of all it holds, in nobody's hands."
	| new |
	new := self shallowCopy.
	new privateOwner: nil.
	new privateCopySubmorphs.
	^new!

privateCopySubmorphs
	"Each submorph copied; and an instance variable that was one of
	them (a window's label) is now its copy."
	| old |
	old := submorphs.
	submorphs := OrderedCollection new.
	old do: [:m | | c |
		c := m duplicate.
		c privateOwner: self.
		submorphs addLast: c.
		1 to: self class instSize do: [:i | (self instVarAt: i) == m ifTrue: [self instVarAt: i put: c]]]! !

!Morph methodsFor: 'drawing'!
drawOn: aCanvas
	"What a morph looks like: the one method a new kind of morph
	writes."
	aCanvas fillRectangle: bounds color: color!

fullDrawOn: aCanvas
	"Itself, then what it holds, the frontmost last. A morph outside
	the canvas's clipping rectangle is not drawn at all."
	(bounds intersects: aCanvas clip) ifTrue: [self drawOn: aCanvas].
	submorphs reverseDo: [:m | m fullDrawOn: aCanvas]! !

!Morph methodsFor: 'stepping'!
step
	"Sent every cycle. A morph that does something by itself does it
	here."!

fullStep
	self step.
	submorphs copy do: [:m | m fullStep]! !

!Morph methodsFor: 'events'!
handlesMouseDown: anEvent
	"Does it want the mouse? If neither it nor an owner does, it is
	picked up."
	^false!

mouseDown: anEvent
	"The button went down on it."!

mouseMove: anEvent
	"The hand moved, the button still down."!

mouseUp: anEvent
	"The button went up, wherever the hand now is."!

keyStroke: anEvent
	"A character typed while it has the keyboard focus."! !

!HandMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 0 @ 0.
	lastButtons := 0.
	dragging := false.
	shadow := Color black alpha: 3/10! !

!HandMorph methodsFor: 'accessing'!
isWorldOrHand
	^true!

world
	^owner!

mouseFocus
	^mouseFocus!

keyboardFocus
	^keyboardFocus!

keyboardFocus: aMorph
	"The morph the keys go to. Both the old one and the new one show
	it (a text's caret)."
	| old |
	old := keyboardFocus.
	keyboardFocus := aMorph.
	old == nil ifFalse: [old changed].
	aMorph == nil ifFalse: [aMorph changed]! !

!HandMorph methodsFor: 'changing'!
position: aPoint
	"Carrying nothing, nothing on the screen changes."
	submorphs isEmpty ifFalse: [^super position: aPoint].
	bounds := aPoint corner: aPoint!

changed
	"The hand itself is not drawn: only what it carries."
	submorphs do: [:m | m changed]!

invalidRect: aRectangle
	"What it carries has a shadow."
	owner invalidRect: (aRectangle origin corner: aRectangle corner + 5)! !

!HandMorph methodsFor: 'drawing'!
fullDrawOn: aCanvas
	"What it carries, lifted off the screen: each over its shadow, a
	black glass."
	submorphs reverseDo: [:m |
		aCanvas fillRectangle: (m bounds translateBy: 5 @ 5) color: shadow.
		m fullDrawOn: aCanvas]! !

!HandMorph methodsFor: 'events'!
processEvents
	"The mouse and the keys read once a cycle, and made messages.
	What the buttons were is remembered before a message is sent: if
	it ends in an error, which stops the cycle there, the next cycle
	does not send it again."
	| p buttons before event key |
	p := Sensor mousePoint.
	buttons := Sensor buttons.
	before := lastButtons.
	lastButtons := buttons.
	event := MorphicEvent position: p buttons: buttons key: nil.
	p = bounds origin ifFalse: [
		self position: p.
		mouseFocus == nil ifFalse: [mouseFocus mouseMove: event]].
	(buttons ~= 0 and: [before = 0]) ifTrue: [self mouseDown: event].
	(buttons = 0 and: [before ~= 0]) ifTrue: [self mouseUp: event].
	[(key := Sensor keyboard) == nil] whileFalse: [
		keyboardFocus == nil ifFalse: [
			keyboardFocus keyStroke: (MorphicEvent position: p buttons: buttons key: (Character value: key))]]!

mouseDown: anEvent
	"What it carries is put down. Else, the frontmost morph under it:
	none, the world's menu; with the blue button, its halo; else the
	first of it and its owners that wants the mouse gets it (the
	yellow button too: a text's menu), and if none does the whole is
	picked up."
	| m handler |
	submorphs isEmpty ifFalse: [^self dropAll].
	m := owner morphAt: anEvent position.
	owner closeMenusBut: m.
	anEvent blueButtonPressed ifTrue: [^owner haloOn: m].
	m == nil ifTrue: [^owner openMenuAt: anEvent position].
	handler := m.
	[handler isWorldOrHand or: [handler handlesMouseDown: anEvent]] whileFalse: [handler := handler owner].
	m root comeToFront.
	handler isWorldOrHand ifTrue: [^self grab: m root].
	mouseFocus := handler.
	handler mouseDown: anEvent!

mouseUp: anEvent
	"The focus forgotten before it is told, for the same reason."
	| focus |
	focus := mouseFocus.
	mouseFocus := nil.
	dragging ifTrue: [self dropAll].
	focus == nil ifFalse: [focus mouseUp: anEvent]!

grab: aMorph
	"Picked up, to be put down when the button goes up. It keeps its
	place under the hand."
	self addMorph: aMorph.
	dragging := true!

attachMorph: aMorph
	"Given to the hand, at the hand, to be put down at the next
	click: a morph new from a menu."
	aMorph position: bounds origin.
	self addMorph: aMorph!

dropAll
	"Into the world, in front."
	submorphs copy do: [:m | owner addMorph: m].
	dragging := false! !

!PasteUpMorph class methodsFor: 'instance creation'!
on: aForm
	"A world drawn on that Form, of 8 or 32 bits. The last one made
	is where the tools open."
	^Current := self new setForm: aForm!

current
	^Current! !

!PasteUpMorph methodsFor: 'initialize'!
setForm: aForm
	bounds := aForm boundingBox.
	color := Color r: 4/5 g: 4/5 b: 4/5.
	canvas := FormCanvas on: aForm.
	damage := OrderedCollection new.
	hand := HandMorph new.
	hand privateOwner: self.
	self invalidRect: bounds! !

!PasteUpMorph methodsFor: 'accessing'!
isWorldOrHand
	^true!

world
	^self!

hand
	^hand!

damage
	^damage! !

!PasteUpMorph methodsFor: 'submorphs'!
morphAt: aPoint
	"A morph of the world, never the world itself."
	| found |
	found := super morphAt: aPoint.
	^found == self ifTrue: [nil] ifFalse: [found]! !

!PasteUpMorph methodsFor: 'damage'!
invalidRect: aRectangle
	"Remember a rectangle to redraw. Nothing clever here: this is sent
	twice for every morph that moves."
	damage add: aRectangle!

damageToRedraw
	"The rectangles to redraw this cycle, and none remembered after.
	A few: each of them, and what did not change is not touched.
	Many: the one rectangle that holds them all, drawn once
	(MiniMorphic.st says why)."
	| rects all |
	rects := damage.
	damage := OrderedCollection new.
	rects size <= 8 ifTrue: [^rects].
	all := rects first.
	rects do: [:r | all := all merge: r].
	^OrderedCollection new add: all; yourself! !

!PasteUpMorph methodsFor: 'stepping'!
doOneCycle
	"The whole of Morphic's main loop."
	hand processEvents.
	submorphs copy do: [:m | m fullStep].
	self displayWorld!

displayWorld
	"Each damaged rectangle: the background, then every morph, back to
	front, what the hand carries last -- the canvas clipped to the
	rectangle, so that what did not change is not touched."
	self damageToRedraw do: [:r | | clip |
		clip := r intersect: bounds.
		canvas clip: clip.
		canvas fillRectangle: clip color: color.
		submorphs reverseDo: [:m | m fullDrawOn: canvas].
		hand fullDrawOn: canvas]! !

!PasteUpMorph methodsFor: 'halo'!
haloOn: aMorph
	"The halo around the whole morph that one is part of, and no
	other halo; on nothing, none."
	submorphs copy do: [:m | m isHalo ifTrue: [m delete]].
	(aMorph == nil or: [aMorph root isHalo]) ifTrue: [^self].
	self addMorph: (HaloMorph on: aMorph root)! !

!HaloMorph class methodsFor: 'instance creation'!
on: aMorph
	^self new setTarget: aMorph! !

!HaloMorph methodsFor: 'initialize'!
setTarget: aMorph
	"Its handles, in the order step places them."
	target := aMorph.
	color := Color blue.
	self addHandle: #delete color: (Color r: 1 g: 2/5 b: 2/5).
	self addHandle: #grab color: Color black.
	self addHandle: #duplicate color: Color green.
	self addHandle: #resize color: Color yellow.
	self addHandle: #inspect color: (Color r: 3/5 g: 4/5 b: 1).
	bounds := 0 @ 0 corner: 0 @ 0.
	self step!

addHandle: aSymbol color: aColor
	self addMorphBack: (HaloHandleMorph new setAction: aSymbol color: aColor)! !

!HaloMorph methodsFor: 'accessing'!
isHalo
	^true!

target
	^target!

containsPoint: aPoint
	"Only its handles are there: a click inside goes to the target."
	^false! !

!HaloMorph methodsFor: 'stepping'!
step
	"Follow the target: around it, a handle at the top left (delete),
	the top's middle (pick up), the top right (duplicate), the bottom
	right (resize) and the bottom left (inspect). Gone with it."
	| box |
	target owner == nil ifTrue: [^self delete].
	box := target bounds expandBy: 14.
	box = bounds ifTrue: [^self].
	self changed.
	bounds := box.
	(submorphs at: 1) position: box origin.
	(submorphs at: 2) position: (box left + box right - 14 // 2) @ box top.
	(submorphs at: 3) position: (box right - 14) @ box top.
	(submorphs at: 4) position: box corner - 14.
	(submorphs at: 5) position: box left @ (box bottom - 14).
	self changed! !

!HaloMorph methodsFor: 'drawing'!
drawOn: aCanvas
	aCanvas frameRectangle: (target bounds expandBy: 1) width: 1 color: color! !

!HaloMorph methodsFor: 'handles'!
handle: action down: anEvent
	| hand |
	hand := self world hand.
	action == #delete ifTrue: [
		target delete.
		^self delete].
	action == #inspect ifTrue: [^target inspect].
	action == #grab ifTrue: [^hand grab: target].
	action == #duplicate ifTrue: [
		target := target duplicate.
		target position: target position + 10.
		^hand grab: target]!

handle: action move: anEvent
	action == #resize ifTrue: [target extent: (anEvent position - target position max: 10 @ 10)]! !

!HaloHandleMorph methodsFor: 'initialize'!
setAction: aSymbol color: aColor
	action := aSymbol.
	color := aColor.
	bounds := 0 @ 0 corner: 14 @ 14! !

!HaloHandleMorph methodsFor: 'drawing'!
drawOn: aCanvas
	aCanvas fillOval: bounds color: Color black.
	aCanvas fillOval: (bounds insetBy: 1) color: color! !

!HaloHandleMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	owner handle: action down: anEvent!

mouseMove: anEvent
	owner handle: action move: anEvent! !
