"Squeak's kernel, the second file: colour. The Blue Book's screen had
one bit a pixel, black or white, and its grays were patterns of both.
Squeak's Forms have a depth, the bits of a pixel, and BitBlt works at
every depth (St_colorblt.mli). Two are here:

- 32: a pixel is four bytes, alpha, red, green, blue, each 0 to 255.
  Alpha is how much of the pixel there is: 255 hides what is under
  it, 0 is not there, and between the two it is glass;
- 8: a pixel is one byte, the number of a colour in a palette of 256,
  what Squeak ran on in 1996, when a screen of 32 bits was four times
  the memory. The palette here: 0 is transparent, then 216 colours,
  6 reds by 6 greens by 6 blues (1 + 36 r + 6 g + b).

A Color is red, green, blue and alpha, and knows its pixel at each
depth. A rectangle is filled with it by a BitBlt with no source whose
halftone is that one pixel; a Form is drawn on a Form of another depth
through a colour map, a table from the pixels of one to the pixels of
the other.

BitBlt gets its fifteenth field, the map; Form its fourth, the depth:
nil for a Form made the Blue Book's way, which has one bit a pixel."!

Object subclass: #Form
	instanceVariableNames: 'bits width height depth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight colorMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Support'!

Object subclass: #Color
	instanceVariableNames: 'red green blue alpha'
	classVariableNames: 'PaletteMap'
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

!Color class methodsFor: 'instance creation'!
r: r g: g b: b
	"Each from 0 to 1, as Squeak's: Color r: 1 g: 1/2 b: 0."
	^self r: r g: g b: b alpha: 1!

r: r g: g b: b alpha: a
	^self new setRed: (r * 255) rounded green: (g * 255) rounded blue: (b * 255) rounded alpha: (a * 255) rounded!

fromIndex: anInteger
	"The colour of that number in the palette of the Forms of 8 bits."
	| i |
	(anInteger < 1 or: [anInteger > 216]) ifTrue: [^self transparent].
	i := anInteger - 1.
	^self new setRed: i // 36 * 51 green: i // 6 \\ 6 * 51 blue: i \\ 6 * 51 alpha: 255! !

!Color class methodsFor: 'named colors'!
black
	^self r: 0 g: 0 b: 0!

white
	^self r: 1 g: 1 b: 1!

gray
	^self r: 1/2 g: 1/2 b: 1/2!

lightGray
	^self r: 4/5 g: 4/5 b: 4/5!

red
	^self r: 1 g: 0 b: 0!

green
	^self r: 0 g: 1 b: 0!

blue
	^self r: 0 g: 0 b: 1!

yellow
	^self r: 1 g: 1 b: 0!

transparent
	^self r: 0 g: 0 b: 0 alpha: 0! !

!Color class methodsFor: 'colour maps'!
paletteMapForDepth: d
	"The map that draws a Form of 8 bits on one of 32: each of the
	256 numbers, its colour's four bytes. Made once."
	d = 32 ifFalse: [^self error: 'A Form of more colours cannot be drawn on one of fewer'].
	PaletteMap == nil ifTrue: [
		PaletteMap := ByteArray new: 256 * 4.
		0 to: 255 do: [:i | (self fromIndex: i) storeOn: PaletteMap at: i * 4 + 1 depth: 32]].
	^PaletteMap! !

!Color methodsFor: 'private'!
setRed: r green: g blue: b alpha: a
	red := r.
	green := g.
	blue := b.
	alpha := a! !

!Color methodsFor: 'accessing'!
red
	"From 0 to 1, as the others."
	^red / 255!

green
	^green / 255!

blue
	^blue / 255!

alpha
	^alpha / 255!

alpha: a
	"The same colour, more or less see-through: Color red alpha: 1/2."
	^Color new setRed: red green: green blue: blue alpha: (a * 255) rounded!

luminance
	"How bright the eye finds it, from 0 to 255: green counts most,
	blue least (the weights of television's, ITU-R 601)."
	^(299 * red) + (587 * green) + (114 * blue) // 1000!

isDark
	^self luminance < 128!

isOpaque
	^alpha = 255!

isTransparent
	^alpha = 0! !

!Color methodsFor: 'comparing'!
= aColor
	^(aColor isKindOf: Color) and: [self asBytes = aColor asBytes]!

hash
	^red * 256 + green * 256 + blue! !

!Color methodsFor: 'pixels'!
index
	"Its number in the palette of 8 bits: the nearest of the six
	levels of each of red, green and blue; 0 if it is not there."
	alpha = 0 ifTrue: [^0].
	^1 + (red + 25 // 51 * 36) + (green + 25 // 51 * 6) + (blue + 25 // 51)!

storeOn: aByteArray at: i depth: d
	"Its pixel's bytes, from i on: four at depth 32, else one."
	d = 32 ifTrue: [
		aByteArray at: i put: alpha; at: i + 1 put: red; at: i + 2 put: green; at: i + 3 put: blue.
		^self].
	aByteArray at: i put: (d = 8 ifTrue: [self index] ifFalse: [self isDark ifTrue: [1] ifFalse: [0]])!

asBytes
	| b |
	b := ByteArray new: 4.
	self storeOn: b at: 1 depth: 32.
	^b!

pixelFormForDepth: d
	"A Form of one pixel, this colour: BitBlt's halftone, repeated
	over what is filled."
	| f |
	f := Form extent: 1 @ 1 depth: d.
	self storeOn: f bits at: 1 depth: d.
	^f!

inkMapForDepth: d
	"The map that draws a Form of one bit in this colour: its 0 is
	nothing (a pixel of zeros: transparent), its 1 this colour. What
	gives text its colour."
	| map |
	map := ByteArray new: (d = 32 ifTrue: [8] ifFalse: [2]).
	self storeOn: map at: map size // 2 + 1 depth: d.
	^map! !

!Color methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'Color('; print: red; space; print: green; space; print: blue.
	alpha = 255 ifFalse: [aStream nextPutAll: ' alpha '; print: alpha].
	aStream nextPut: $)! !

!Form class methodsFor: 'instance creation'!
extent: aPoint depth: d
	^self width: aPoint x height: aPoint y depth: d!

width: w height: h depth: d
	"The bytes of a row: St_colorblt.mli."
	| rowBytes |
	rowBytes := d = 32 ifTrue: [w * 4] ifFalse: [d = 8 ifTrue: [w + 3 // 4 * 4] ifFalse: [w + 15 // 16 * 2]].
	^self new setBits: (ByteArray new: rowBytes * h) width: w height: h depth: d! !

!Form methodsFor: 'private'!
setBits: aByteArray width: w height: h depth: d
	bits := aByteArray.
	width := w.
	height := h.
	depth := d! !

!Form methodsFor: 'accessing'!
depth
	^depth == nil ifTrue: [1] ifFalse: [depth]! !

!Form methodsFor: 'colour'!
fill: aRectangle color: aColor
	"One BitBlt with no source, its halftone the colour's pixel:
	stored (rule 3), or blended with what is there if it is glass
	(rule 24). With one bit a pixel, black or white."
	| d |
	d := self depth.
	d = 1 ifTrue: [^self fill: aRectangle rule: (aColor isDark ifTrue: [15] ifFalse: [0])].
	(BitBlt destForm: self sourceForm: nil halftoneForm: (aColor pixelFormForDepth: d)
		combinationRule: ((d = 32 and: [aColor isOpaque not]) ifTrue: [24] ifFalse: [3])
		destOrigin: aRectangle origin sourceOrigin: 0 @ 0 extent: aRectangle extent clipRect: self boundingBox) copyBits!

fillColor: aColor
	self fill: self boundingBox color: aColor!

colorAt: aPoint
	"Read from the bits, a row after the other."
	| d i |
	d := self depth.
	d = 32 ifTrue: [
		i := aPoint y * width + aPoint x * 4.
		^Color new setRed: (bits at: i + 2) green: (bits at: i + 3) blue: (bits at: i + 4) alpha: (bits at: i + 1)].
	d = 8 ifTrue: [^Color fromIndex: (bits at: width + 3 // 4 * 4 * aPoint y + aPoint x + 1)].
	^(self pixelAt: aPoint) = 1 ifTrue: [Color black] ifFalse: [Color white]!

displayOn: aForm at: aPoint
	"Over what is there, which shows where this Form is transparent
	(blended at depth 32, painted at 8, or-ed at 1). On a Form of
	another depth, through a colour map: one bit a pixel in black, 8
	bits by the palette."
	| blt from to |
	from := self depth.
	to := aForm depth.
	blt := BitBlt destForm: aForm sourceForm: self halftoneForm: nil
		combinationRule: (to = 32 ifTrue: [24] ifFalse: [to = 8 ifTrue: [25] ifFalse: [7]])
		destOrigin: aPoint sourceOrigin: 0 @ 0 extent: self extent clipRect: aForm boundingBox.
	from = to ifFalse: [
		blt colorMap: (from = 1 ifTrue: [Color black inkMapForDepth: to] ifFalse: [Color paletteMapForDepth: to])].
	blt copyBits! !

!Form methodsFor: 'printing'!
printOn: aStream
	aStream nextPutAll: 'a Form('; print: width; nextPut: $x; print: height.
	self depth = 1 ifFalse: [aStream nextPut: $x; print: depth].
	aStream nextPut: $)! !

!BitBlt methodsFor: 'accessing'!
colorMap: aByteArray
	"An entry for each value of a source pixel, each a destination's
	pixel (St_colorblt.mli)."
	colorMap := aByteArray!

sourceX: x
	sourceX := x! !
