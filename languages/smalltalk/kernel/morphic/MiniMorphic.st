"MiniMorphic: Morphic in one file, to read before the real one.

Morphic (John Maloney and Randall Smith, for Self, 1995; Squeak's
since 1997) has few ideas, all here:

- everything on the screen is a Morph: a rectangle of it, a colour,
  an owner and submorphs. The screen itself is one, the world; so is
  the hand, the mouse made visible, and what it carries are its
  submorphs;
- a morph is alive: the world sends step to every morph, every cycle,
  and a morph that wants to move just changes its position;
- nobody redraws: a morph that changed says where (changed), the
  world remembers those rectangles (its damage), and at the end of
  the cycle redraws only them, back to front, clipped;
- the cycle is the whole program: read the mouse, step, redraw.

What is left out is what makes Squeak's Morphic big, not different:
text, menus, halos, layout, events for the keyboard, colours (the
Display here has one bit a pixel: black, white, and two grays made of
both). The classes: Canvas (a Form to draw on, through one BitBlt),
Morph, AtomMorph (a morph that bounces: the demo), HandMorph,
WorldMorph. The interpreter knows none of them: all it gives is
BitBlt, the mouse and the clock (St_bitblt.mli, St_primitives.mli)."!

Object subclass: #Canvas
	instanceVariableNames: 'form blt clip'
	classVariableNames: 'Gray LightGray'
	poolDictionaries: ''
	category: 'MiniMorphic'!

Object subclass: #Morph
	instanceVariableNames: 'bounds owner submorphs color'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MiniMorphic'!

Morph subclass: #AtomMorph
	instanceVariableNames: 'velocity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MiniMorphic'!

Morph subclass: #HandMorph
	instanceVariableNames: 'wasDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MiniMorphic'!

Morph subclass: #WorldMorph
	instanceVariableNames: 'hand canvas damage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MiniMorphic'!

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

!Canvas class methodsFor: 'instance creation'!
on: aForm
	^self new setForm: aForm! !

!Canvas class methodsFor: 'patterns'!
pattern: evenByte with: oddByte
	"A halftone (St_bitblt.mli): 16 by 16, its even rows of one byte,
	its odd rows of another."
	| f |
	f := Form extent: 16 @ 16.
	1 to: 32 do: [:i | f bits at: i put: (i - 1 // 2 \\ 2 = 0 ifTrue: [evenByte] ifFalse: [oddByte])].
	^f!

gray
	Gray == nil ifTrue: [Gray := self pattern: 16rAA with: 16r55].
	^Gray!

lightGray
	LightGray == nil ifTrue: [LightGray := self pattern: 16r88 with: 16r22].
	^LightGray! !

!Canvas methodsFor: 'private'!
setForm: aForm
	form := aForm.
	clip := aForm boundingBox.
	blt := BitBlt destForm: aForm sourceForm: nil halftoneForm: nil combinationRule: 3
		destOrigin: 0 @ 0 sourceOrigin: 0 @ 0 extent: 0 @ 0 clipRect: clip! !

!Canvas methodsFor: 'clipping'!
clip
	^clip!

clip: aRectangle
	"Nothing is drawn outside it, whatever is asked: how the world
	redraws one rectangle of the screen and no more."
	clip := aRectangle.
	blt clipRect: aRectangle! !

!Canvas methodsFor: 'drawing'!
fill: aRectangle color: aSymbol
	"One BitBlt with no source: the rule alone for black (15: ones) and
	white (0: zeros), a halftone stored (3) for the grays."
	aSymbol == #black
		ifTrue: [blt halftoneForm: nil; combinationRule: 15]
		ifFalse: [aSymbol == #white
			ifTrue: [blt halftoneForm: nil; combinationRule: 0]
			ifFalse: [blt halftoneForm: (aSymbol == #gray ifTrue: [Canvas gray] ifFalse: [Canvas lightGray]); combinationRule: 3]].
	blt destX: aRectangle left destY: aRectangle top; width: aRectangle width height: aRectangle height.
	blt copyBits! !

!Morph class methodsFor: 'instance creation'!
new
	^super new initialize! !

!Morph methodsFor: 'initialize'!
initialize
	bounds := 0 @ 0 corner: 50 @ 40.
	submorphs := OrderedCollection new.
	color := #gray! !

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

world
	"The morph at the top, if it is a world: a morph in nobody's hands
	and on no screen has none."
	^owner == nil ifTrue: [nil] ifFalse: [owner world]! !

!Morph methodsFor: 'changing'!
color: aSymbol
	color := aSymbol.
	self changed!

extent: aPoint
	self changed.
	bounds := bounds origin extent: aPoint.
	self changed!

position: aPoint
	"Move, with all it holds. The place left and the place taken both
	changed."
	| delta |
	delta := aPoint - bounds origin.
	(delta x = 0 and: [delta y = 0]) ifTrue: [^self].
	self changed.
	self privateMoveBy: delta.
	self changed!

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

delete
	"Out of its owner, off the screen."
	owner == nil ifTrue: [^self].
	self changed.
	owner submorphs remove: self.
	owner := nil!

privateOwner: aMorph
	owner := aMorph!

morphAt: aPoint
	"The frontmost morph there: a submorph before its owner, the first
	submorph before the others."
	submorphs do: [:m | | found |
		found := m morphAt: aPoint.
		found == nil ifFalse: [^found]].
	^(bounds containsPoint: aPoint) ifTrue: [self] ifFalse: [nil]! !

!Morph methodsFor: 'drawing'!
drawOn: aCanvas
	"What a morph looks like: the one method a new kind of morph
	writes."
	aCanvas fill: bounds color: #black.
	aCanvas fill: (bounds insetBy: 1) color: color!

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
	submorphs do: [:m | m fullStep]! !

!AtomMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 12 @ 12.
	velocity := 3 @ 2! !

!AtomMorph methodsFor: 'accessing'!
velocity
	^velocity!

velocity: aPoint
	velocity := aPoint! !

!AtomMorph methodsFor: 'stepping'!
step
	"Move by the velocity, and turn back at the owner's walls. In the
	hand, be still."
	| box p vx vy |
	(owner isKindOf: HandMorph) ifTrue: [^self].
	box := owner bounds.
	vx := velocity x.
	vy := velocity y.
	p := bounds origin + velocity.
	(p x < box left or: [p x + bounds width > box right]) ifTrue: [vx := vx negated].
	(p y < box top or: [p y + bounds height > box bottom]) ifTrue: [vy := vy negated].
	velocity := vx @ vy.
	self position: bounds origin + velocity! !

!HandMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 8 @ 8.
	color := #black.
	wasDown := false! !

!HandMorph methodsFor: 'events'!
processEvents
	"The mouse read once a cycle: the hand goes where it is, with what
	it carries; the button going down picks up the morph under it, the
	button going up puts it down, in front."
	| p down |
	p := Sensor mousePoint.
	down := Sensor redButtonPressed.
	self position: p.
	(down and: [wasDown not]) ifTrue: [self grab: (owner morphAt: p)].
	(down not and: [wasDown]) ifTrue: [self dropAll].
	wasDown := down!

grab: aMorph
	(aMorph == nil or: [aMorph == owner]) ifTrue: [^self].
	self addMorph: aMorph!

dropAll
	submorphs copy do: [:m | owner addMorph: m]! !

!WorldMorph class methodsFor: 'instance creation'!
on: aForm
	^self new setForm: aForm! !

!WorldMorph methodsFor: 'initialize'!
setForm: aForm
	bounds := aForm boundingBox.
	color := #white.
	canvas := Canvas on: aForm.
	damage := OrderedCollection new.
	hand := HandMorph new.
	hand privateOwner: self.
	self invalidRect: bounds! !

!WorldMorph methodsFor: 'accessing'!
world
	^self!

hand
	^hand!

damage
	^damage! !

!WorldMorph methodsFor: 'submorphs'!
morphAt: aPoint
	"A morph of the world, never the world itself."
	| found |
	found := super morphAt: aPoint.
	^found == self ifTrue: [nil] ifFalse: [found]! !

!WorldMorph methodsFor: 'damage'!
invalidRect: aRectangle
	"Remember a rectangle to redraw. Nothing clever here: this is sent
	twice for every morph that moves."
	damage add: aRectangle!

damageToRedraw
	"The rectangles to redraw this cycle, and none remembered after.
	A few (one morph dragged): each of them, and what did not change
	is not touched. Many (everything moving): the one rectangle that
	holds them all, drawn once -- going through the morphs for each of
	a hundred small rectangles costs more than drawing them all."
	| rects all |
	rects := damage.
	damage := OrderedCollection new.
	rects size <= 8 ifTrue: [^rects].
	all := rects first.
	rects do: [:r | all := all merge: r].
	^OrderedCollection new add: all; yourself! !

!WorldMorph methodsFor: 'stepping'!
doOneCycle
	"The whole of Morphic's main loop."
	hand processEvents.
	submorphs copy do: [:m | m fullStep].
	self displayWorld!

displayWorld
	"Each damaged rectangle: the background, then every morph, back to
	front, the hand last -- the canvas clipped to the rectangle, so
	that what did not change is not touched."
	self damageToRedraw do: [:r | | clip |
		clip := r intersect: bounds.
		canvas clip: clip.
		canvas fill: clip color: color.
		submorphs reverseDo: [:m | m fullDrawOn: canvas].
		hand fullDrawOn: canvas]! !

!WorldMorph class methodsFor: 'examples'!
bouncingAtoms: n
	"The classic first demo: n atoms in a box. Each cycle moves them
	all, and redraws only where they were and where they are.
	(WorldMorph bouncingAtoms: 50) doOneCycle"
	| world |
	world := self on: Display.
	1 to: n do: [:i | | atom |
		atom := AtomMorph new.
		atom color: (#(#black #gray #lightGray) at: i \\ 3 + 1).
		atom velocity: (i \\ 7 - 3 * 2 + 1) @ (i \\ 5 - 2 * 2 + 1).
		world addMorph: atom.
		atom position: (i * 37 \\ (world bounds width - 20)) @ (i * 53 \\ (world bounds height - 20))].
	^world! !
