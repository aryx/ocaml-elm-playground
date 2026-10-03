"Squeak's kernel, the third file: text. A font is a picture: all its
characters drawn side by side in one Form of one bit a pixel, the
glyphs, and a table saying where each one starts (the strike format,
the Alto's, 1973: a strike is that long row of characters).

          xTable:  0     9      18    27 ...
          glyphs:  |  A  |  B   |  C  |
                   +-----+------+-----+

A string is drawn a character at a time, each one a BitBlt from its
place in the glyphs to the next place on the line: nothing else, text
is BitBlt. Its colour is a colour map (Color.st): the glyphs' 0 is
nothing, their 1 the ink.

The glyphs are not stored: they are drawn the first time the font is
asked for, by a Pen, from Hershey's strokes (A. V. Hershey,
'Calligraphy for Computers', 1967: letters as a few lines, made for
pen plotters). A glyph is a String, each point two characters, their
distances from the letter R (R is 0, S is 1, Q is -1); the first two
are its left and right sides, and a space then R lifts the pen. For A:

    I[RFJ[ RRFZ[ RMTWT     I [  from -9 to 9: 18 wide
                           RF J[   (0, -12) to (-8, 9): the left leg
                           RF Z[   (0, -12) to (8, 9): the right leg
                           MT WT   (-5, 2) to (5, 2): the bar

y goes down, from -16 to 16, 33 rows; the baseline is at 9. A font of
height h is those rows scaled to h rows of pixels: 33 is a pixel a
unit, the default, 17, half of it."!

Object subclass: #StrikeFont
	instanceVariableNames: 'glyphs xTable height'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'Graphics-Text'!

!StrikeFont class methodsFor: 'instance creation'!
height: pixels
	"Drawn now: some 300,000 bytecodes."
	^self new renderHershey: pixels!

default
	Default == nil ifTrue: [Default := self height: 17].
	^Default! !

!StrikeFont class methodsFor: 'strokes'!
hersheyStrokes
	"The Roman simplex font, the characters 32 (the space) to 127."
	^#(
		'JZ' 'MWRFRT RRYQZR[SZRY' 'JZNFNM RVFVM' 'H]SBLb RYBRb RLOZO RKUYU'
		'H\PBP_ RTBT_ RYIWGTFPFMGKIKKLMMNOOUQWRXSYUYXWZT[P[MZKX'
		'F^[FI[ RNFPHPJOLMMKMIKIIJGLFNFPGSHVHYG[F RWTUUTWTYV[X[ZZ[X[VYTWT'
		'E_\O\N[MZMYNXPVUTXRZP[L[JZIYHWHUISJRQNRMSKSIRGPFNGMIMKNNPQUXWZY[[[\Z\Y' 'MWRHQGRFSGSIRKQL'
		'KYVBTDRGPKOPOTPYR]T`Vb' 'KYNBPDRGTKUPUTTYR]P`Nb' 'JZRLRX RMOWU RWOMU' 'E_RIR[ RIR[R'
		'NVSWRXQWRVSWSYQ[' 'E_IR[R' 'NVRVQWRXSWRV' 'G][BIb' 'H\QFNGLJKOKRLWNZQ[S[VZXWYRYOXJVGSFQF'
		'H\NJPISFS[' 'H\LKLJMHNGPFTFVGWHXJXLWNUQK[Y[' 'H\MFXFRNUNWOXPYSYUXXVZS[P[MZLYKW' 'H\UFKTZT RUFU['
		'H\WFMFLOMNPMSMVNXPYSYUXXVZS[P[MZLYKW' 'H\XIWGTFRFOGMJLOLTMXOZR[S[VZXXYUYTXQVOSNRNOOMQLT'
		'H\YFO[ RKFYF' 'H\PFMGLILKMMONSOVPXRYTYWXYWZT[P[MZLYKWKTLRNPQOUNWMXKXIWGTFPF'
		'H\XMWPURRSQSNRLPKMKLLINGQFRFUGWIXMXRWWUZR[P[MZLX' 'NVROQPRQSPRO RRVQWRXSWRV'
		'NVROQPRQSPRO RSWRXQWRVSWSYQ[' 'F^ZIJRZ[' 'E_IO[O RIU[U' 'F^JIZRJ['
		'I[LKLJMHNGPFTFVGWHXJXLWNVORQRT RRYQZR[SZRY'
		'E`WNVLTKQKOLNMMPMSNUPVSVUUVS RQKOMNPNSOUPV RWKVSVUXVZV\T]Q]O\L[JYHWGTFQFNGLHJJILHOHRIUJWLYNZQ[T[WZYYZX RXKWSWUXV'
		'I[RFJ[ RRFZ[ RMTWT' 'G\KFK[ RKFTFWGXHYJYLXNWOTP RKPTPWQXRYTYWXYWZT[K['
		'H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZV' 'G\KFK[ RKFRFUGWIXKYNYSXVWXUZR[K['
		'H[LFL[ RLFYF RLPTP RL[Y[' 'HZLFL[ RLFYF RLPTP' 'H]ZKYIWGUFQFOGMILKKNKSLVMXOZQ[U[WZYXZVZS RUSZS'
		'G]KFK[ RYFY[ RKPYP' 'NVRFR[' 'JZVFVVUYTZR[P[NZMYLVLT' 'G\KFK[ RYFKT RPOY[' 'HYLFL[ RL[X['
		'F^JFJ[ RJFR[ RZFR[ RZFZ[' 'G]KFK[ RKFY[ RYFY[' 'G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF'
		'G\KFK[ RKFTFWGXHYJYMXOWPTQKQ' 'G]PFNGLIKKJNJSKVLXNZP[T[VZXXYVZSZNYKXIVGTFPF RSWY]'
		'G\KFK[ RKFTFWGXHYJYLXNWOTPKP RRPY[' 'H\YIWGTFPFMGKIKKLMMNOOUQWRXSYUYXWZT[P[MZKX' 'JZRFR[ RKFYF'
		'G]KFKULXNZQ[S[VZXXYUYF' 'I[JFR[ RZFR[' 'F^HFM[ RRFM[ RRFW[ R\FW[' 'H\KFY[ RYFK['
		'I[JFRPR[ RZFRP' 'H\YFK[ RKFYF RK[Y[' 'KYOBOb RPBPb ROBVB RObVb' 'KYKFY^'
		'KYTBTb RUBUb RNBUB RNbUb' 'JZRDJR RRDZR' 'I[Ib[b' 'NVSKQMQORPSORNQO'
		'I\XMX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX' 'H[LFL[ RLPNNPMSMUNWPXSXUWXUZS[P[NZLX'
		'I[XPVNTMQMONMPLSLUMXOZQ[T[VZXX' 'I\XFX[ RXPVNTMQMONMPLSLUMXOZQ[T[VZXX'
		'I[LSXSXQWOVNTMQMONMPLSLUMXOZQ[T[VZXX' 'MYWFUFSGRJR[ ROMVM'
		'I\XMX]W`VaTbQbOa RXPVNTMQMONMPLSLUMXOZQ[T[VZXX' 'I\MFM[ RMQPNRMUMWNXQX[' 'NVQFRGSFREQF RRMR['
		'MWRFSGTFSERF RSMS^RaPbNb' 'IZMFM[ RWMMW RQSX[' 'NVRFR[' 'CaGMG[ RGQJNLMOMQNRQR[ RRQUNWMZM\N]Q]['
		'I\MMM[ RMQPNRMUMWNXQX[' 'I\QMONMPLSLUMXOZQ[T[VZXXYUYSXPVNTMQM'
		'H[LMLb RLPNNPMSMUNWPXSXUWXUZS[P[NZLX' 'I\XMXb RXPVNTMQMONMPLSLUMXOZQ[T[VZXX'
		'KXOMO[ ROSPPRNTMWM' 'J[XPWNTMQMNNMPNRPSUTWUXWXXWZT[Q[NZMX' 'MYRFRWSZU[W[ ROMVM'
		'I\MMMWNZP[S[UZXW RXMX[' 'JZLMR[ RXMR[' 'G]JMN[ RRMN[ RRMV[ RZMV[' 'J[MMX[ RXMM['
		'JZLMR[ RXMR[P_NaLbKb' 'J[XMM[ RMMXM RM[X['
		'KYTBRCQDPFPHQJRKSMSOQQ RRCQEQGRISJTLTNSPORSTTVTXSZR[Q]Q_Ra RQSSUSWRYQZP\P^Q`RaTb' 'NVRBRb'
		'KYPBRCSDTFTHSJRKQMQOSQ RRCSESGRIQJPLPNQPURQTPVPXQZR[S]S_Ra RSSQUQWRYSZT\T^S`RaPb'
		'F^IUISJPLONOPPTSVTXTZS[Q RISJQLPNPPQTTVUXUZT[Q[O'
		'JZJFJ[K[KFLFL[M[MFNFN[O[OFPFP[Q[QFRFR[S[SFTFT[U[UFVFV[W[WFXFX[Y[YFZFZ['
	)! !

!StrikeFont methodsFor: 'private'!
renderHershey: pixels
	"The table first, each glyph as wide as its two sides say; then
	the glyphs, a Pen following the strokes."
	| strokes x pen |
	height := pixels.
	strokes := StrikeFont hersheyStrokes.
	xTable := Array new: strokes size + 1.
	x := 0.
	1 to: strokes size do: [:i | | s |
		s := strokes at: i.
		xTable at: i put: x.
		x := x + (self scale: (s at: 2) value - (s at: 1) value)].
	xTable at: strokes size + 1 put: x.
	glyphs := Form extent: x @ height.
	pen := Pen new.
	pen destForm: glyphs.
	1 to: strokes size do: [:i |
		"a stroke that rounds past its glyph's side must not mark the next one"
		pen clipRect: ((xTable at: i) @ 0 corner: (xTable at: i + 1) @ height).
		self draw: (strokes at: i) with: pen at: (xTable at: i)]!

scale: units
	"Hershey's units to pixels, rounded."
	^units * (height - 1) + 16 // 32!

draw: aString with: aPen at: left
	"A glyph's strokes: the pen goes to each point, down but for the
	first of a stroke."
	| side i x y |
	side := (aString at: 1) value.
	aPen up.
	i := 3.
	[i < aString size] whileTrue: [
		x := (aString at: i) value.
		y := (aString at: i + 1) value.
		(x = 32 and: [y = 82])
			ifTrue: [aPen up]
			ifFalse: [
				aPen goto: (left + (self scale: x - side)) @ (self scale: y - 82 + 16).
				aPen down].
		i := i + 2]!

indexOf: aCharacter
	"Its place in the table; a character the font does not have is
	a question mark."
	| v |
	v := aCharacter value.
	^(v < 32 or: [v > 127]) ifTrue: [32] ifFalse: [v - 31]! !

!StrikeFont methodsFor: 'accessing'!
glyphs
	^glyphs!

height
	^height!

ascent
	"From the top of a line down to the baseline, where the letters
	sit: Hershey's 9, of -16 to 16."
	^self scale: 25!

leftOf: aCharacter
	"Where its glyph starts in the glyphs."
	^xTable at: (self indexOf: aCharacter)!

widthOf: aCharacter
	"A tab is four spaces wide, and nothing drawn."
	| i |
	aCharacter value = 9 ifTrue: [^(xTable at: 2) * 4].
	i := self indexOf: aCharacter.
	^(xTable at: i + 1) - (xTable at: i)!

widthOfString: aString
	^aString inject: 0 into: [:sum :c | sum + (self widthOf: c)]! !

!Form methodsFor: 'text'!
drawString: aString at: aPoint font: aFont color: aColor
	^self drawString: aString at: aPoint font: aFont color: aColor clipRect: self boundingBox!

drawString: aString at: aPoint font: aFont color: aColor clipRect: aRectangle
	"From aPoint, the top left of the line; answers the x where it
	ends. One BitBlt, sent copyBits once a character: its source the
	font's glyphs, through the colour's map, blended (painted at 8
	bits), so that the ink may be glass. With one bit a pixel, no
	map: or-ed for black, erased for white."
	| d blt x |
	d := self depth.
	blt := BitBlt destForm: self sourceForm: aFont glyphs halftoneForm: nil
		combinationRule: (d = 32 ifTrue: [24] ifFalse: [d = 8 ifTrue: [25] ifFalse: [aColor isDark ifTrue: [7] ifFalse: [4]]])
		destOrigin: aPoint sourceOrigin: 0 @ 0 extent: 0 @ aFont height clipRect: aRectangle.
	d = 1 ifFalse: [blt colorMap: (aColor inkMapForDepth: d)].
	x := aPoint x.
	aString do: [:c | | w |
		w := aFont widthOf: c.
		c value = 9 ifFalse: [
			blt sourceX: (aFont leftOf: c); destX: x destY: aPoint y; width: w height: aFont height; copyBits].
		x := x + w].
	^x!

drawString: aString at: aPoint
	^self drawString: aString at: aPoint font: StrikeFont default color: Color black! !
