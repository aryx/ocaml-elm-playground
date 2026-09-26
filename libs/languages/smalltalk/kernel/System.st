"The last chunks the bootstrap runs, once every method is compiled:
the globals that are objects rather than classes."!

Smalltalk at: #Transcript put: TextCollector new!

Smalltalk at: #Display put: (Form extent: 800 @ 600)!

Smalltalk at: #Sensor put: InputSensor new!

Rectangle comment: 'A rectangle of the screen, from its origin (top left) to its corner (bottom right), each a Point.'!

Point comment: 'A point of the screen, x to the right and y downwards: 3@4.'!

Object comment: 'The root of the classes: what every object answers.'!
