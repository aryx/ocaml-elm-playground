# Hershey fonts

`futural.jhf` is Hershey's "Roman simplex" font (the single-stroke sans
serif, glyphs for ASCII 32 to 127, one per line), used by `../Hershey.ml`
to draw Playground's `words`. Copied as-is from
<https://github.com/kamalmostafa/hershey-fonts> (`hershey-fonts/futural.jhf`).

The font data is distributed under the following use restriction,
from the `hershey.txt` file of the original Usenet distribution:

```
USE RESTRICTION:
	This distribution of the Hershey Fonts may be used by anyone for
	any purpose, commercial or otherwise, providing that:
		1. The following acknowledgements must be distributed with
			the font data:
			- The Hershey Fonts were originally created by Dr.
				A. V. Hershey while working at the U. S.
				National Bureau of Standards.
			- The format of the Font data in this distribution
				was originally created by
					James Hurt
					Cognition, Inc.
					900 Technology Park Drive
					Billerica, MA 01821
					(mit-eddie!ci-dandelion!hurt)
		2. The font data in this distribution may be converted into
			any other format *EXCEPT* the format distributed by
			the U.S. NTIS (which organization holds the rights
			to the distribution and use of the font data in that
			particular format). Not that anybody would really
			*want* to use their format... each point is described
			in eight bytes as "xxx yyy:", where xxx and yyy are
			the coordinate values as ASCII numbers.
```

Acknowledgements, as required:

- The Hershey Fonts were originally created by Dr. A. V. Hershey while
  working at the U. S. National Bureau of Standards.
- The format of the Font data in this distribution was originally
  created by James Hurt, Cognition, Inc., 900 Technology Park Drive,
  Billerica, MA 01821.

Reference: A. V. Hershey, "Calligraphy for Computers", NWL Report
No. 2101, U.S. Naval Weapons Laboratory, Dahlgren, Virginia, 1967.

# The VGA font

`vga8x16.txt` is the IBM VGA's 8 by 16 text-mode font (1987), its 256
characters in code page 437's order, used by `../Vga_font.ml` (tinybox's
code map). One character a line: its code page 437 number, its Unicode
code point, the character itself, and its 16 rows as hex bytes, top
first, the leftmost pixel the byte's high bit:

```
41 0041 A 000010386cc6c6fec6c6c6c600000000
```

The glyphs come from the Linux console's `Uni2-VGA16.psf` (Debian's
console-setup package), each found by its Unicode code point in the
font's table, and for the 24 it lacks (the smileys, the card suits, the
half blocks), from `FullGreek-VGA16.psf`; ► and ◄ are the fonts' ▶ and
◀. Both were made from `u_vga16.bdf`, whose copyright follows (from
console-setup's `copyright.fonts`, which adds: "All console fonts are
public domain by nature."):

```
Copyright © 2000, 2001 by Dmitry Bolkhovityanov.  All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is fur-
nished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FIT-
NESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
XFREE86 PROJECT BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CON-
NECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Dmitry Bolkhovityanov
shall not be used in advertising or otherwise to promote the sale, use
or other dealings in this Software without prior written authorization
from the Dmitry Bolkhovityanov.
```
