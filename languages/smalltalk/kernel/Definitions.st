"The kernel's classes, the Blue Book's hierarchy (its part two), a
subset. St_boot reads these definitions before anything can run: it
makes every class and its metaclass from them, ties the knot at the top
(Object class superclass == Class, Metaclass class class == Metaclass),
then compiles the methods of the other files. The fields the virtual
machine reads -- a class's, a context's, a Character's value -- are in
the order St_class.mli and St_interp.mli say."!

nil subclass: #Object
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Object subclass: #Behavior
	instanceVariableNames: 'superclass methodDict format'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

Behavior subclass: #ClassDescription
	instanceVariableNames: 'instanceVariables organization'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

ClassDescription subclass: #Class
	instanceVariableNames: 'name category classPool comment'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

ClassDescription subclass: #Metaclass
	instanceVariableNames: 'thisClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

Object subclass: #UndefinedObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Object subclass: #Boolean
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Boolean subclass: #True
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Boolean subclass: #False
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Object subclass: #Association
	instanceVariableNames: 'key value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Objects'!

Object subclass: #Message
	instanceVariableNames: 'selector arguments'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

Object subclass: #Magnitude
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Magnitudes'!

Magnitude subclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Magnitudes'!

Magnitude subclass: #Number
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Number subclass: #Integer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Integer subclass: #SmallInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Integer variableByteSubclass: #LargePositiveInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

LargePositiveInteger variableByteSubclass: #LargeNegativeInteger
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Number subclass: #Float
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Number subclass: #Fraction
	instanceVariableNames: 'numerator denominator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Numeric-Numbers'!

Object subclass: #Point
	instanceVariableNames: 'x y'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Object subclass: #Collection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!

Collection subclass: #SequenceableCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!

SequenceableCollection subclass: #ArrayedCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Abstract'!

ArrayedCollection variableSubclass: #Array
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

ArrayedCollection variableByteSubclass: #String
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

String variableByteSubclass: #Symbol
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Text'!

ArrayedCollection variableByteSubclass: #ByteArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

SequenceableCollection subclass: #Interval
	instanceVariableNames: 'start stop step'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

SequenceableCollection variableSubclass: #OrderedCollection
	instanceVariableNames: 'firstIndex lastIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

OrderedCollection variableSubclass: #SortedCollection
	instanceVariableNames: 'sortBlock'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Sequenceable'!

Collection variableSubclass: #Set
	instanceVariableNames: 'tally'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

Set variableSubclass: #Dictionary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

Collection subclass: #Bag
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Unordered'!

Object subclass: #SystemDictionary
	instanceVariableNames: 'associations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

Object subclass: #Stream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

Stream subclass: #PositionableStream
	instanceVariableNames: 'collection position readLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

PositionableStream subclass: #ReadStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

PositionableStream subclass: #WriteStream
	instanceVariableNames: 'writeLimit'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

WriteStream subclass: #ReadWriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

Object subclass: #MethodDictionary
	instanceVariableNames: 'selectors methods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

Object subclass: #CompiledMethod
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

Object variableSubclass: #ContextPart
	instanceVariableNames: 'sender pc stackp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

ContextPart variableSubclass: #MethodContext
	instanceVariableNames: 'method unused receiver'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

ContextPart variableSubclass: #BlockContext
	instanceVariableNames: 'nargs startpc home'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Methods'!

Object subclass: #TextCollector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Interface-Transcript'!

Object subclass: #Time
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

Object subclass: #Form
	instanceVariableNames: 'bits width height'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Display Objects'!

Object subclass: #BitBlt
	instanceVariableNames: 'destForm sourceForm halftoneForm combinationRule destX destY width height sourceX sourceY clipX clipY clipWidth clipHeight'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Support'!

BitBlt subclass: #Pen
	instanceVariableNames: 'frame location direction penDown'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Object subclass: #InputSensor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Processes'!
