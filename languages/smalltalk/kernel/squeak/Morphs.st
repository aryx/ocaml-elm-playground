"Squeak's kernel, the fifth file: the morphs. Morphic.st is the
machinery; these are what one sees. Each is small, because a morph
says little: what it looks like (drawOn:), what it does alone (step),
and what it does with the mouse and the keys, if it wants them.

- BorderedMorph, RectangleMorph, EllipseMorph: a shape and its border;
- AtomMorph: an ellipse that bounces in its owner, the morph that
  steps;
- StringMorph: a line of text; TextMorph: text one types in, selects
  with the mouse, and evaluates (the yellow button's menu: do it,
  print it, inspect it, accept); ListMorph: a column of items, one
  picked;
- SimpleButtonMorph: a click sends a message;
- MenuMorph: a column of items, gone at the next click; the world's
  menu makes new morphs and gives them to the hand;
- SystemWindow: a title, a box to close it, and panes, each placed by
  a rectangle of fractions of the window, so that they follow its
  size.

A window is dragged by its title and comes to the front when touched
without a line here saying so: its label does not want the mouse, so
the hand picks the window up (Morphic.st)."!

Morph subclass: #BorderedMorph
	instanceVariableNames: 'borderWidth borderColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

BorderedMorph subclass: #RectangleMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

BorderedMorph subclass: #EllipseMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

EllipseMorph subclass: #AtomMorph
	instanceVariableNames: 'velocity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Demo'!

Morph subclass: #StringMorph
	instanceVariableNames: 'contents font'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

Morph subclass: #TextMorph
	instanceVariableNames: 'text cursor anchor firstLine font ink highlight receiver acceptTarget acceptSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Basic'!

Morph subclass: #ListMorph
	instanceVariableNames: 'items selection firstItem font ink edge target selector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

RectangleMorph subclass: #SimpleButtonMorph
	instanceVariableNames: 'target selector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

StringMorph subclass: #MenuItemMorph
	instanceVariableNames: 'target selector arguments highlighted'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

RectangleMorph subclass: #MenuMorph
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Menus'!

RectangleMorph subclass: #SystemWindow
	instanceVariableNames: 'label closeBox panes frames titleColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Windows'!

!BorderedMorph methodsFor: 'initialize'!
initialize
	super initialize.
	borderWidth := 2.
	borderColor := Color black! !

!BorderedMorph methodsFor: 'accessing'!
borderWidth
	^borderWidth!

borderWidth: anInteger
	borderWidth := anInteger.
	self changed!

borderColor: aColor
	borderColor := aColor.
	self changed! !

!BorderedMorph methodsFor: 'drawing'!
drawOn: aCanvas
	aCanvas fillRectangle: (bounds insetBy: borderWidth) color: color.
	borderWidth > 0 ifTrue: [aCanvas frameRectangle: bounds width: borderWidth color: borderColor]! !

!EllipseMorph methodsFor: 'initialize'!
initialize
	super initialize.
	color := Color yellow! !

!EllipseMorph methodsFor: 'drawing'!
drawOn: aCanvas
	"The border's ellipse, then the inside's over it."
	borderWidth > 0 ifTrue: [aCanvas fillOval: bounds color: borderColor].
	aCanvas fillOval: (bounds insetBy: borderWidth) color: color! !

!EllipseMorph methodsFor: 'submorphs'!
containsPoint: aPoint
	"Inside the ellipse, not only its box: the equation, multiplied
	out so that the numbers stay whole. A click in a corner goes to
	what is behind."
	| x y a b |
	(bounds containsPoint: aPoint) ifFalse: [^false].
	x := aPoint x * 2 - bounds left - bounds right.
	y := aPoint y * 2 - bounds top - bounds bottom.
	a := bounds width.
	b := bounds height.
	^(x * x * b * b) + (y * y * a * a) <= (a * a * b * b)! !

!AtomMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 14 @ 14.
	borderWidth := 1.
	color := Color red.
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

!StringMorph class methodsFor: 'instance creation'!
contents: aString
	^self new contents: aString! !

!StringMorph methodsFor: 'initialize'!
initialize
	super initialize.
	color := Color black.
	font := StrikeFont default.
	self contents: 'String'! !

!StringMorph methodsFor: 'accessing'!
contents
	^contents!

contents: aString
	"As big as its string."
	self changed.
	contents := aString.
	bounds := bounds origin extent: (font widthOfString: aString) @ font height.
	self changed! !

!StringMorph methodsFor: 'drawing'!
drawOn: aCanvas
	aCanvas drawString: contents at: bounds origin font: font color: color! !

!TextMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 200 @ 80.
	color := Color white.
	ink := Color black.
	highlight := Color r: 7/10 g: 4/5 b: 1.
	font := StrikeFont default.
	text := ''.
	cursor := 0.
	anchor := 0.
	firstLine := 0! !

!TextMorph methodsFor: 'accessing'!
contents
	^text!

contents: aString
	"The lines end with a return, Character value: 13."
	text := aString.
	cursor := 0.
	anchor := 0.
	firstLine := 0.
	self changed!

cursor
	"How many characters are before the caret."
	^cursor!

selection
	"From the anchor, where the button went down, to the cursor."
	^text copyFrom: (cursor min: anchor) + 1 to: (cursor max: anchor)!

selectFrom: start to: stop
	"The characters after the first start, up to the stop-th."
	anchor := start.
	cursor := stop.
	self scrollToCursor.
	self changed!

receiver: anObject
	"Who self is in what is evaluated here: nil in a Workspace, the
	object in its Inspector."
	receiver := anObject!

acceptTarget: anObject selector: aSymbol
	"Who is given the text when it is accepted: a Browser, to
	compile it."
	acceptTarget := anObject.
	acceptSelector := aSymbol!

hasFocus
	| w |
	w := self world.
	^w notNil and: [w hand keyboardFocus == self]! !

!TextMorph methodsFor: 'lines'!
linesDo: aBlock
	"Each line and how many characters are before it."
	| start |
	start := 0.
	1 to: text size do: [:i |
		(text at: i) value = 13 ifTrue: [
			aBlock value: (text copyFrom: start + 1 to: i - 1) value: start.
			start := i]].
	aBlock value: (text copyFrom: start + 1 to: text size) value: start!

visibleLines
	^bounds height - 4 // font height!

cursorRow
	"The line the cursor is on, the first one 0."
	| row |
	row := 0.
	1 to: cursor do: [:i | (text at: i) value = 13 ifTrue: [row := row + 1]].
	^row!

scrollToCursor
	"The first line shown changed so that the cursor's is."
	| row |
	row := self cursorRow.
	row < firstLine ifTrue: [firstLine := row].
	row >= (firstLine + self visibleLines) ifTrue: [firstLine := row - self visibleLines + 1 max: 0]!

cursorPoint
	"The top of the caret: on the line that has the cursor, after the
	width of what is before it."
	| y |
	y := bounds top + 2 - (firstLine * font height).
	self linesDo: [:line :start |
		cursor <= (start + line size) ifTrue: [
			^(bounds left + 3 + (font widthOfString: (line copyFrom: 1 to: cursor - start))) @ y].
		y := y + font height].
	^(bounds left + 3) @ y!

indexAt: aPoint
	"The cursor for a click there: its line, then the characters
	whose middle is left of it."
	| row n |
	row := (aPoint y - bounds top - 2) // font height + firstLine max: 0.
	n := 0.
	self linesDo: [:line :start | | x i |
		n = row ifTrue: [
			x := bounds left + 3.
			i := 0.
			[i < line size and: [x + ((font widthOf: (line at: i + 1)) // 2) < aPoint x]] whileTrue: [
				i := i + 1.
				x := x + (font widthOf: (line at: i))].
			^start + i].
		n := n + 1].
	^text size! !

!TextMorph methodsFor: 'editing'!
replaceSelectionWith: aString
	"What is typed takes the place of what is selected."
	| lo hi |
	lo := cursor min: anchor.
	hi := cursor max: anchor.
	text := (text copyFrom: 1 to: lo), aString, (text copyFrom: hi + 1 to: text size).
	cursor := lo + aString size.
	anchor := cursor!

backspace
	cursor = anchor ifTrue: [
		cursor = 0 ifTrue: [^self].
		anchor := cursor - 1].
	self replaceSelectionWith: ''!

insertAfterSelection: aString
	"After what is selected, and selected itself: an answer printed,
	the compiler's complaint -- a backspace takes it away."
	| end |
	end := cursor max: anchor.
	cursor := end.
	anchor := end.
	self replaceSelectionWith: aString.
	anchor := end.
	self scrollToCursor.
	self changed! !

!TextMorph methodsFor: 'evaluating'!
selectLine
	"Nothing selected: the cursor's line is."
	cursor = anchor ifFalse: [^self].
	self linesDo: [:line :start |
		cursor <= (start + line size) ifTrue: [
			anchor := start.
			cursor := start + line size.
			^self]]!

evaluateThen: aBlock
	"The selection compiled as a method of the receiver's class, DoIt,
	whose answer is the last statement's, and sent: the compiler and
	the interpreter are the ones that run everything else. The
	compiler's complaint, if any, goes after the selection."
	| return compiled |
	self selectLine.
	return := String with: (Character value: 13).
	compiled := receiver class compile: 'DoIt', return, '^[', self selection, return, '] value' classified: 'do its'.
	compiled isSymbol ifFalse: [^self insertAfterSelection: ' ', compiled].
	aBlock value: (receiver perform: #DoIt)!

doIt
	self evaluateThen: [:value | self changed]!

printIt
	self evaluateThen: [:value | self insertAfterSelection: ' ', value printString]!

inspectIt
	self evaluateThen: [:value | value inspect]!

accept
	acceptTarget == nil ifFalse: [acceptTarget perform: acceptSelector with: text]! !

!TextMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	"The keys are now for this text. The red button: the caret where
	the click is, and the selection starts there. The yellow one: its
	menu."
	self world hand keyboardFocus: self.
	anEvent redButtonPressed ifFalse: [^self openMenuAt: anEvent position].
	cursor := self indexAt: anEvent position.
	anchor := cursor.
	self changed!

mouseMove: anEvent
	"The selection follows the hand; past the top or the bottom, the
	text scrolls."
	anEvent redButtonPressed ifFalse: [^self].
	anEvent position y < bounds top ifTrue: [firstLine := firstLine - 1 max: 0].
	anEvent position y > bounds bottom ifTrue: [firstLine := firstLine + 1].
	cursor := self indexAt: anEvent position.
	self scrollToCursor.
	self changed!

openMenuAt: aPoint
	| menu |
	menu := MenuMorph new.
	menu add: 'do it' target: self selector: #doIt.
	menu add: 'print it' target: self selector: #printIt.
	menu add: 'inspect it' target: self selector: #inspectIt.
	acceptTarget == nil ifFalse: [menu add: 'accept' target: self selector: #accept].
	menu popUpAt: aPoint in: self world!

keyStroke: anEvent
	"A character typed, a return, a tab, a backspace (8), the arrows
	(28 left, 29 right, 30 up, 31 down). A character typed in a line
	damages that line only."
	| c first plain |
	c := anEvent keyCharacter value.
	first := firstLine.
	plain := c >= 32 and: [c < 127 and: [cursor = anchor]].
	c = 8 ifTrue: [self backspace].
	c = 28 ifTrue: [cursor := cursor - 1 max: 0. anchor := cursor].
	c = 29 ifTrue: [cursor := cursor + 1 min: text size. anchor := cursor].
	c = 30 ifTrue: [cursor := self indexAt: self cursorPoint - (0 @ font height). anchor := cursor].
	c = 31 ifTrue: [cursor := self indexAt: self cursorPoint + (0 @ font height). anchor := cursor].
	(c = 13 or: [c = 9 or: [c >= 32 and: [c < 127]]]) ifTrue: [
		self replaceSelectionWith: (String with: anEvent keyCharacter)].
	self scrollToCursor.
	(plain and: [first = firstLine])
		ifTrue: [self invalidRect: (bounds left @ self cursorPoint y corner: bounds right @ (self cursorPoint y + font height))]
		ifFalse: [self changed]! !

!TextMorph methodsFor: 'drawing'!
drawOn: aCanvas
	"The paper, then the lines that are in what is redrawn, each over
	its part of the selection, kept inside the paper; the caret if
	the keys come here and nothing is selected."
	| lo hi row |
	aCanvas fillRectangle: bounds color: color.
	lo := cursor min: anchor.
	hi := cursor max: anchor.
	row := 0.
	aCanvas clipBy: bounds during: [
		self linesDo: [:line :start | | y |
			y := bounds top + 2 + (row - firstLine * font height).
			(row >= firstLine and: [y < aCanvas clip bottom and: [y + font height > aCanvas clip top]]) ifTrue: [
				(lo < hi and: [lo <= (start + line size) and: [hi > start]]) ifTrue: [
					aCanvas
						fillRectangle: ((self xIn: line at: lo - start) @ y
							corner: ((self xIn: line at: hi - start) + (hi > (start + line size) ifTrue: [4] ifFalse: [0])) @ (y + font height))
						color: highlight].
				aCanvas drawString: line at: (bounds left + 3) @ y font: font color: ink].
			row := row + 1].
		(lo = hi and: [self hasFocus]) ifTrue: [
			aCanvas fillRectangle: (self cursorPoint extent: 1 @ font height) color: ink]]!

xIn: aLine at: n
	"Where the n-th character of a line ends on the screen; before
	the line's start, its start; past its end, its end."
	^bounds left + 3 + (font widthOfString: (aLine copyFrom: 1 to: ((n max: 0) min: aLine size)))! !

!ListMorph methodsFor: 'initialize'!
initialize
	super initialize.
	bounds := 0 @ 0 corner: 100 @ 100.
	color := Color white.
	font := StrikeFont default.
	ink := Color black.
	edge := Color gray.
	items := #().
	selection := 0.
	firstItem := 0! !

!ListMorph methodsFor: 'accessing'!
items
	^items!

items: aCollection
	"What it lists: Strings or Symbols. Nothing selected."
	items := aCollection asArray.
	selection := 0.
	firstItem := 0.
	self changed!

selection
	"The selected item's place, 0 for none."
	^selection!

selectedItem
	^selection = 0 ifTrue: [nil] ifFalse: [items at: selection]!

target: anObject selector: aSymbol
	"Who is told when an item is picked, with its place."
	target := anObject.
	selector := aSymbol!

visibleRows
	^bounds height - 4 // font height! !

!ListMorph methodsFor: 'selecting'!
highlight: anInteger
	"Shown as selected, scrolled to, nobody told yet."
	selection := (anInteger max: 0) min: items size.
	selection > 0 ifTrue: [
		selection <= firstItem ifTrue: [firstItem := selection - 1].
		selection > (firstItem + self visibleRows) ifTrue: [firstItem := selection - self visibleRows max: 0]].
	self changed!

select: anInteger
	"An item picked: the target told."
	self highlight: anInteger.
	(target == nil or: [selection = 0]) ifFalse: [target perform: selector with: selection]!

selectItem: anObject
	"Picked by what it is, as a click on it would."
	self select: (items indexOf: anObject)! !

!ListMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	self world hand keyboardFocus: self.
	self mouseMove: anEvent!

mouseMove: anEvent
	"The item under the hand shown selected; past the top or the
	bottom, the list scrolls."
	| y |
	y := anEvent position y.
	y < bounds top ifTrue: [^self highlight: firstItem].
	y >= bounds bottom ifTrue: [^self highlight: firstItem + self visibleRows + 1].
	self highlight: (y - bounds top - 2 // font height + firstItem + 1 max: 1)!

mouseUp: anEvent
	"Picked when the button goes up."
	self select: selection!

keyStroke: anEvent
	"The arrows up (30) and down (31)."
	| c |
	c := anEvent keyCharacter value.
	c = 30 ifTrue: [self select: (selection - 1 max: 1)].
	c = 31 ifTrue: [self select: selection + 1]! !

!ListMorph methodsFor: 'drawing'!
drawOn: aCanvas
	"The rows that fit, from the first one shown; the selected one
	white on black."
	aCanvas fillRectangle: bounds color: color.
	aCanvas clipBy: bounds during: [
		firstItem + 1 to: (firstItem + self visibleRows + 1 min: items size) do: [:i | | y |
			y := bounds top + 2 + (i - 1 - firstItem * font height).
			i = selection
				ifTrue: [
					aCanvas fillRectangle: (bounds left @ y corner: bounds right @ (y + font height)) color: ink.
					aCanvas drawString: (items at: i) at: (bounds left + 3) @ y font: font color: color]
				ifFalse: [aCanvas drawString: (items at: i) at: (bounds left + 3) @ y font: font color: ink]]].
	aCanvas frameRectangle: bounds width: 1 color: edge! !

!SimpleButtonMorph methodsFor: 'initialize'!
initialize
	super initialize.
	borderWidth := 1.
	color := Color r: 9/10 g: 9/10 b: 9/10! !

!SimpleButtonMorph methodsFor: 'accessing'!
target: anObject selector: aSymbol
	"What a click sends, and to whom; to nobody, to its owner."
	target := anObject.
	selector := aSymbol!

label: aString
	"A string in its middle, the button as big as needed."
	| s |
	submorphs copy do: [:m | m delete].
	s := StringMorph contents: aString.
	self extent: s extent + (12 @ 4).
	s position: bounds origin + (6 @ 2).
	self addMorph: s! !

!SimpleButtonMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseUp: anEvent
	"A click is the button going up while still on it."
	(self containsPoint: anEvent position) ifFalse: [^self].
	(target == nil ifTrue: [owner] ifFalse: [target]) perform: selector! !

!MenuItemMorph methodsFor: 'initialize'!
initialize
	super initialize.
	highlighted := false!

setTarget: anObject selector: aSymbol arguments: anArray
	target := anObject.
	selector := aSymbol.
	arguments := anArray! !

!MenuItemMorph methodsFor: 'events'!
handlesMouseDown: anEvent
	^true!

mouseDown: anEvent
	highlighted := true.
	self changed!

mouseUp: anEvent
	"Chosen: the menu goes, the message is sent."
	highlighted := false.
	self changed.
	(self containsPoint: anEvent position) ifFalse: [^self].
	owner delete.
	target perform: selector withArguments: arguments! !

!MenuItemMorph methodsFor: 'drawing'!
drawOn: aCanvas
	highlighted ifFalse: [^super drawOn: aCanvas].
	aCanvas fillRectangle: bounds color: color.
	aCanvas drawString: contents at: bounds origin font: font color: Color white! !

!MenuMorph methodsFor: 'initialize'!
initialize
	super initialize.
	borderWidth := 1.
	color := Color white.
	bounds := 0 @ 0 corner: 8 @ 4! !

!MenuMorph methodsFor: 'accessing'!
isMenu
	^true! !

!MenuMorph methodsFor: 'items'!
add: aString target: anObject selector: aSymbol
	self add: aString target: anObject selector: aSymbol arguments: #()!

add: aString target: anObject selector: aSymbol argument: argument
	self add: aString target: anObject selector: aSymbol arguments: (Array with: argument)!

add: aString target: anObject selector: aSymbol arguments: anArray
	"Under the last one; the menu as wide as its widest item."
	| item |
	item := MenuItemMorph contents: aString.
	item setTarget: anObject selector: aSymbol arguments: anArray.
	item position: (bounds left + 4) @ (bounds bottom - 2).
	self addMorphBack: item.
	bounds := bounds origin corner: (bounds right max: item bounds right + 4) @ (item bounds bottom + 2)!

popUpAt: aPoint in: aWorld
	"Every item as wide as the menu: the whole line is the item."
	self position: aPoint.
	submorphs do: [:item | item extent: (bounds width - 8) @ item extent y].
	aWorld addMorph: self! !

!PasteUpMorph methodsFor: 'menus'!
closeMenusBut: aMorph
	"A menu goes at the next click that is not on it."
	submorphs copy do: [:m |
		(m isMenu and: [aMorph == nil or: [aMorph root ~~ m]]) ifTrue: [m delete]]!

openMenuAt: aPoint
	"The world's menu, a click on the world itself."
	| menu |
	menu := MenuMorph new.
	menu add: 'new rectangle' target: self selector: #newMorph: argument: RectangleMorph.
	menu add: 'new ellipse' target: self selector: #newMorph: argument: EllipseMorph.
	menu add: 'new string' target: self selector: #newMorph: argument: StringMorph.
	menu add: 'new text' target: self selector: #newMorph: argument: TextMorph.
	menu add: 'new window' target: self selector: #newMorph: argument: SystemWindow.
	menu add: 'new atom' target: self selector: #newMorph: argument: AtomMorph.
	menu add: 'workspace' target: Workspace selector: #open.
	menu add: 'browser' target: Browser selector: #open.
	menu add: 'transcript' target: Transcript selector: #open.
	menu add: 'restore display' target: self selector: #restoreDisplay.
	menu popUpAt: aPoint in: self!

newMorph: aClass
	"In the hand, to be put down where the next click is."
	hand attachMorph: aClass new!

restoreDisplay
	self invalidRect: bounds! !

!SystemWindow methodsFor: 'initialize'!
initialize
	super initialize.
	borderWidth := 1.
	color := Color r: 9/10 g: 9/10 b: 9/10.
	titleColor := Color r: 3/5 g: 4/5 b: 1.
	bounds := 0 @ 0 corner: 260 @ 160.
	panes := OrderedCollection new.
	frames := OrderedCollection new.
	closeBox := SimpleButtonMorph new.
	closeBox extent: 13 @ 13; color: (Color r: 1 g: 2/5 b: 2/5); target: nil selector: #delete.
	label := StringMorph contents: 'Window'.
	self addMorph: closeBox; addMorph: label.
	self extentChanged! !

!SystemWindow methodsFor: 'accessing'!
labelString: aString
	label contents: aString!

titleHeight
	^label extent y + 4!

paneArea
	"Under the title, inside the border."
	^(bounds left + 1) @ (bounds top + self titleHeight) corner: bounds corner - 1! !

!SystemWindow methodsFor: 'opening'!
openInWorld
	"In the world the tools open in, each a little further than the
	last."
	| world |
	world := PasteUpMorph current.
	self position: world submorphs size * 24 + 30 @ (world submorphs size * 24 + 30).
	world addMorph: self! !

!SystemWindow methodsFor: 'panes'!
addMorph: aMorph frame: aRectangle
	"A pane: where it is, in fractions of the area under the title
	(0 @ 0 corner: 1/2 @ 1, its left half)."
	panes add: aMorph.
	frames add: aRectangle.
	self addMorph: aMorph.
	self extentChanged!

extentChanged
	"Everything put where the window's size says."
	| area |
	closeBox position: bounds origin + 4.
	label position: bounds origin + (24 @ 2).
	area := self paneArea.
	panes with: frames do: [:pane :f |
		pane bounds: ((area left + (area width * f left) rounded) @ (area top + (area height * f top) rounded)
			corner: (area left + (area width * f right) rounded) @ (area top + (area height * f bottom) rounded))]! !

!SystemWindow methodsFor: 'copying'!
privateCopySubmorphs
	"The panes are the copies' too."
	| old |
	old := submorphs.
	super privateCopySubmorphs.
	panes := panes collect: [:p | submorphs at: (old indexOf: p)]! !

!SystemWindow methodsFor: 'drawing'!
drawOn: aCanvas
	super drawOn: aCanvas.
	aCanvas fillRectangle: (bounds origin + 1 corner: (bounds right - 1) @ (bounds top + self titleHeight))
		color: titleColor! !
