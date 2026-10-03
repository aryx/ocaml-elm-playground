"Squeak's kernel, the sixth file: the tools, as morphs. Smalltalk-80's
Workspace, Transcript, Inspector and System Browser (the Blue Book's
were windows of its own Model-View-Controller; TinySmalltalk80's are
OCaml), here windows of Morphic: a SystemWindow whose panes are
ListMorphs and TextMorphs, and an object behind that answers them.

They are made of what the system already has. To evaluate, a text
compiles a method named DoIt in the receiver's class and sends it
(Behavior>>compile:classified:, what the Browser's accept uses): there
is no evaluator, only the compiler. The Browser asks the classes
themselves: Smalltalk for the classes, a class for its category, its
organization (the protocols and their selectors) and its methods'
sources. And what it compiles is in use at once: accept a new
Morph>>drawOn:, and every morph on the screen is drawn that way at the
next redraw -- the Browser too, it is morphs.

An error in what is evaluated stops the process that ran it, the
world's cycle (Object>>error:): the host starts the next cycle and
says why in the Transcript (showError:). A debugger as a morph is to
come."!

Object subclass: #TextCollector
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-Transcript'!

TextMorph subclass: #TranscriptMorph
	instanceVariableNames: 'shown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

Object subclass: #Workspace
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

Object subclass: #Inspector
	instanceVariableNames: 'object window fieldList valuePane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

Object subclass: #Browser
	instanceVariableNames: 'window categoryList classList protocolList selectorList sideButton codePane meta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!

!CompiledMethod methodsFor: 'accessing'!
getSource
	"The text it was compiled from, kept with it (St_bytecode.mli)."
	<primitive: 159>
	self primitiveFailed! !

!ClassDescription methodsFor: 'organization'!
protocols
	"The names its methods are filed under, in order."
	^organization == nil ifTrue: [#()] ifFalse: [organization collect: [:each | each key]]!

selectorsIn: aString
	"The selectors filed under that protocol."
	organization == nil ifTrue: [^#()].
	organization do: [:each | each key = aString ifTrue: [^each value]].
	^#()!

sourceCodeAt: aSymbol
	"Its lines ended by returns, as a text's are: the kernel's files
	end theirs with line feeds."
	^(methodDict at: aSymbol) getSource collect: [:c | c value = 10 ifTrue: [Character value: 13] ifFalse: [c]]! !

!Class methodsFor: 'printing'!
definition
	"The message that defines it, as the Browser shows a class:
	changed and accepted, it defines it again."
	| s quote kind |
	s := WriteStream on: String new.
	quote := Character value: 39.
	kind := format bitAnd: 7.
	s nextPutAll: (superclass == nil ifTrue: ['nil'] ifFalse: [superclass name]).
	s nextPutAll: (kind = 1 ifTrue: [' variableSubclass: #'] ifFalse: [kind = 2 ifTrue: [' variableByteSubclass: #'] ifFalse: [' subclass: #']]).
	s nextPutAll: name; cr; tab; nextPutAll: 'instanceVariableNames: '; nextPut: quote.
	instanceVariables == nil ifFalse: [instanceVariables do: [:each | s nextPutAll: each] separatedBy: [s space]].
	s nextPut: quote; cr; tab; nextPutAll: 'classVariableNames: '; nextPut: quote.
	classPool == nil ifFalse: [classPool do: [:each | s nextPutAll: each key] separatedBy: [s space]].
	s nextPut: quote; cr; tab; nextPutAll: 'poolDictionaries: '; nextPut: quote; nextPut: quote; cr; tab.
	s nextPutAll: 'category: '; nextPut: quote; nextPutAll: category; nextPut: quote.
	^s contents! !

!Object methodsFor: 'system primitives'!
inspect
	"An Inspector on the receiver, a window of Morphic."
	Inspector openOn: self! !

!TextCollector methodsFor: 'accessing'!
show: aString
	"Kept, its last 2,000 characters, for the Transcript's window;
	and given to the host, as before."
	aString isString ifFalse: [^self show: aString printString].
	contents := contents == nil ifTrue: [aString] ifFalse: [contents, aString].
	contents size > 2000 ifTrue: [contents := contents copyFrom: contents size - 1999 to: contents size].
	self primShow: aString!

primShow: aString
	<primitive: 140>
	self primitiveFailed!

contents
	^contents == nil ifTrue: [''] ifFalse: [contents]!

showError: aString
	"What stopped the world's cycle, said by the host: shown, in a
	window opened if there is none."
	self show: aString; cr.
	(TranscriptMorph allInstances anySatisfy: [:each | each world notNil]) ifFalse: [self open]!

open
	"Its window: a text that shows what was shown."
	| window |
	window := SystemWindow new.
	window labelString: 'Transcript'.
	window addMorph: TranscriptMorph new frame: (0 @ 0 corner: 1 @ 1).
	window openInWorld.
	^window! !

!TranscriptMorph methodsFor: 'stepping'!
step
	"Nobody tells it: each cycle it looks whether the Transcript has
	a new text, and shows its end."
	| new |
	new := Transcript contents.
	new == shown ifTrue: [^self].
	shown := new.
	text := new.
	cursor := new size.
	anchor := cursor.
	self scrollToCursor.
	self changed! !

!Workspace class methodsFor: 'instance creation'!
open
	"A window of text to type and evaluate in; self is nil."
	| window |
	window := SystemWindow new.
	window labelString: 'Workspace'.
	window addMorph: TextMorph new frame: (0 @ 0 corner: 1 @ 1).
	window openInWorld.
	^window! !

!Inspector class methodsFor: 'instance creation'!
openOn: anObject
	^self new openOn: anObject! !

!Inspector methodsFor: 'initialize'!
openOn: anObject
	"Its fields on the left; on the right the one picked, printed, in
	a text where self is the object."
	object := anObject.
	window := SystemWindow new.
	window labelString: anObject class name asString.
	fieldList := ListMorph new.
	fieldList target: self selector: #fieldPicked:.
	valuePane := TextMorph new.
	valuePane receiver: anObject.
	window addMorph: fieldList frame: (0 @ 0 corner: 1/3 @ 1).
	window addMorph: valuePane frame: (1/3 @ 0 corner: 1 @ 1).
	fieldList items: self fieldNames.
	fieldList select: 1.
	window openInWorld! !

!Inspector methodsFor: 'accessing'!
object
	^object!

window
	^window!

fieldList
	^fieldList!

valuePane
	^valuePane! !

!Inspector methodsFor: 'fields'!
fieldNames
	"self, its named instance variables, then its indexed ones by
	number, the first 30."
	| names |
	names := OrderedCollection with: 'self'.
	names addAll: object class instanceVariableNames.
	object class isVariable ifTrue: [1 to: (object basicSize min: 30) do: [:i | names add: i printString]].
	^names!

valueAt: anInteger
	| named |
	named := object class instSize.
	anInteger = 1 ifTrue: [^object].
	anInteger - 1 <= named ifTrue: [^object instVarAt: anInteger - 1].
	^object basicAt: anInteger - 1 - named!

fieldPicked: anInteger
	valuePane contents: (self valueAt: anInteger) printString! !

!Browser class methodsFor: 'instance creation'!
open
	^self new open! !

!Browser methodsFor: 'initialize'!
open
	"Four lists -- the categories, a category's classes, a class's
	protocols, a protocol's selectors -- over the text of what is
	picked; and a button, for the methods of the class itself."
	meta := false.
	window := SystemWindow new.
	window labelString: 'System Browser'.
	window extent: 620 @ 380.
	categoryList := self listPicking: #categoryPicked: at: 0.
	classList := self listPicking: #classPicked: at: 1/4.
	protocolList := self listPicking: #protocolPicked: at: 1/2.
	selectorList := self listPicking: #selectorPicked: at: 3/4.
	sideButton := SimpleButtonMorph new.
	sideButton label: 'instance'; target: self selector: #switchSide.
	window addMorph: sideButton frame: (1/4 @ (2/5) corner: 1/2 @ (1/2)).
	codePane := TextMorph new.
	codePane acceptTarget: self selector: #accept:.
	window addMorph: codePane frame: (0 @ (1/2) corner: 1 @ 1).
	categoryList items: self categories.
	window openInWorld!

listPicking: aSymbol at: left
	| list |
	list := ListMorph new.
	list target: self selector: aSymbol.
	window addMorph: list frame: (left @ 0 corner: (left + (1/4)) @ (2/5)).
	^list! !

!Browser methodsFor: 'accessing'!
window
	^window!

categoryList
	^categoryList!

classList
	^classList!

protocolList
	^protocolList!

selectorList
	^selectorList!

codePane
	^codePane! !

!Browser methodsFor: 'the system'!
categories
	"The categories of the classes, each once, in the order met."
	| result |
	result := OrderedCollection new.
	Smalltalk classesDo: [:each | (result includes: each category) ifFalse: [result add: each category]].
	^result!

classNamesIn: aString
	| result |
	result := OrderedCollection new.
	Smalltalk classesDo: [:each | each category = aString ifTrue: [result add: each name]].
	^result!

behavior
	"The class picked, or its metaclass on the class side; nil."
	| class |
	classList selection = 0 ifTrue: [^nil].
	class := Smalltalk at: classList selectedItem.
	^meta ifTrue: [class class] ifFalse: [class]!

template
	"What a new method looks like."
	| return |
	return := String with: (Character value: 13).
	^'message selector and argument names', return, (String with: (Character value: 9)), '"comment stating purpose of message"', return,
		return, (String with: (Character value: 9)), '| temporary variable names |', return, (String with: (Character value: 9)), 'statements'! !

!Browser methodsFor: 'picking'!
categoryPicked: anInteger
	classList items: (self classNamesIn: categoryList selectedItem).
	protocolList items: #().
	selectorList items: #().
	codePane contents: ''!

classPicked: anInteger
	"Its protocols, and its definition to read or change."
	protocolList items: self behavior protocols.
	selectorList items: #().
	codePane contents: (Smalltalk at: classList selectedItem) definition!

protocolPicked: anInteger
	selectorList items: (self behavior selectorsIn: protocolList selectedItem).
	codePane contents: self template!

selectorPicked: anInteger
	codePane contents: (self behavior sourceCodeAt: selectorList selectedItem)!

switchSide
	"The instance's methods, or the class's own."
	meta := meta not.
	sideButton label: (meta ifTrue: ['class'] ifFalse: ['instance']).
	classList selection = 0 ifFalse: [self classPicked: classList selection]! !

!Browser methodsFor: 'accepting'!
accept: aString
	"The code pane's text. With a protocol picked it is a method:
	compiled in the class, filed under the protocol, and from now on
	what the system runs. With only a class, its definition:
	evaluated."
	| class protocol selector name |
	class := self behavior.
	class == nil ifTrue: [^self].
	protocolList selection = 0 ifTrue: [
		name := classList selectedItem.
		codePane selectFrom: 0 to: aString size; doIt.
		self categoryPicked: categoryList selection.
		^classList selectItem: name].
	protocol := protocolList selectedItem.
	selector := class compile: aString classified: protocol.
	selector isSymbol ifFalse: [^codePane insertAfterSelection: ' ', selector].
	selectorList items: (class selectorsIn: protocol).
	selectorList highlight: (selectorList items indexOf: selector)! !
